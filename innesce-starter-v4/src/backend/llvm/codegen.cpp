#include "backend/llvm/codegen.hpp"
#include "backend/llvm/ir_builder.hpp"
#include "frontend/sema.hpp" // for enums mapping in future (we'll pass explicit mapping from caller)

#ifdef INNSCE_HAVE_LLVM
#include <llvm/ADT/Triple.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/MC/TargetOptions.h>
#include <system_error>
#include <unordered_map>
#include <variant>
#include <optional>
#endif

namespace innesce::backend::llvm {

#ifndef INNSCE_HAVE_LLVM
std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path) {
    (void)unit; (void)obj_path;
    return "LLVM backend disabled: rebuild with -DINNSCE_ENABLE_LLVM=ON";
}
#else

using namespace ::llvm;

static Value* emit_expr(IRBuilder<>& B,
                        const innesce::ast::Expr& e,
                        std::unordered_map<std::string, Value*>& locals,
                        const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums) {
    using E = innesce::ast::Expr;
    if (std::holds_alternative<E::IntLit>(e.node)) {
        int v = std::get<E::IntLit>(e.node).value;
        return B.getInt32(v);
    }
    if (std::holds_alternative<E::Ident>(e.node)) {
        const auto& I = std::get<E::Ident>(e.node);
        auto it = locals.find(I.name);
        if (it != locals.end()) {
            return B.CreateLoad(B.getInt32Ty(), it->second);
        }
        // maybe enum variant
        for (auto& [ename, vars] : enums) {
            auto vit = vars.find(I.name);
            if (vit != vars.end()) {
                return B.getInt32(vit->second);
            }
        }
        return nullptr;
    }
    if (std::holds_alternative<E::Unary>(e.node)) {
        const auto& U = std::get<E::Unary>(e.node);
        Value* rhs = emit_expr(B, *U.rhs, locals, enums);
        if (!rhs) return nullptr;
        if (U.op == '-') return B.CreateNeg(rhs);
        return rhs;
    }
    const auto& BN = std::get<E::Binary>(e.node);
    Value* L = emit_expr(B, *BN.lhs, locals, enums);
    Value* R = emit_expr(B, *BN.rhs, locals, enums);
    if (!L || !R) return nullptr;
    switch (BN.op) {
        case '+': return B.CreateAdd(L, R);
        case '-': return B.CreateSub(L, R);
        case '*': return B.CreateMul(L, R);
        case '/': return B.CreateSDiv(L, R);
    }
    return nullptr;
}

static bool emit_block(IRBuilder<>& B,
                       Function* F,
                       const std::vector<innesce::ast::Stmt>& body,
                       std::unordered_map<std::string, Value*>& locals,
                       const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums);

static bool emit_match(IRBuilder<>& B,
                       Function* F,
                       const innesce::ast::Stmt::Match& M,
                       std::unordered_map<std::string, Value*>& locals,
                       const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums) {
    Value* scr = emit_expr(B, M.scrutinee, locals, enums);
    if (!scr) return false;

    BasicBlock* contBB = BasicBlock::Create(B.getContext(), "match.cont");
    // build case blocks
    std::vector<BasicBlock*> caseBBs;
    BasicBlock* defaultBB = nullptr;
    for (auto& C : M.cases) {
        if (C.is_default) {
            defaultBB = BasicBlock::Create(B.getContext(), "case.default");
        } else {
            caseBBs.push_back(BasicBlock::Create(B.getContext(), ("case."+C.label).c_str()));
        }
    }
    if (!defaultBB) defaultBB = contBB;

    // switch instruction
    SwitchInst* sw = B.CreateSwitch(scr, defaultBB, (unsigned)caseBBs.size());

    // Attach cases
    unsigned idx = 0;
    for (auto& C : M.cases) {
        if (C.is_default) continue;
        int val = -1;
        // map label to int
        for (auto& [ename, vars] : enums) {
            auto it = vars.find(C.label);
            if (it != vars.end()) { val = it->second; break; }
        }
        if (val < 0) return false;
        BasicBlock* bb = caseBBs[idx++];
        F->getBasicBlockList().push_back(bb);
        sw->addCase(B.getInt32(val), bb);
        B.SetInsertPoint(bb);
        auto locals_case = locals;
        bool returned = emit_block(B, F, C.body, locals_case, enums);
        if (!returned) B.CreateBr(contBB);
    }

    // default
    if (defaultBB != contBB) {
        F->getBasicBlockList().push_back(defaultBB);
        B.SetInsertPoint(defaultBB);
        auto locals_def = locals;
        // empty default => just fallthrough
        for (auto& C : M.cases) {
            if (C.is_default) {
                bool returned = emit_block(B, F, C.body, locals_def, enums);
                if (!returned) B.CreateBr(contBB);
                break;
            }
        }
    }

    // continue
    F->getBasicBlockList().push_back(contBB);
    B.SetInsertPoint(contBB);
    return false;
}

static bool emit_block(IRBuilder<>& B,
                       Function* F,
                       const std::vector<innesce::ast::Stmt>& body,
                       std::unordered_map<std::string, Value*>& locals,
                       const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums) {
    using S = innesce::ast::Stmt;
    for (auto& st : body) {
        if (std::holds_alternative<S::Let>(st.node)) {
            const auto& L = std::get<S::Let>(st.node);
            AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, L.name);
            Value* init = emit_expr(B, L.init, locals, enums);
            if (!init) return false;
            B.CreateStore(init, a);
            locals[L.name] = a;
        } else if (std::holds_alternative<S::Return>(st.node)) {
            const auto& R = std::get<S::Return>(st.node);
            Value* v = emit_expr(B, R.value, locals, enums);
            if (!v) return false;
            B.CreateRet(v);
            return true;
        } else if (std::holds_alternative<S::If>(st.node)) {
            const auto& I = std::get<S::If>(st.node);
            Value* condv = emit_expr(B, I.cond, locals, enums);
            if (!condv) return false;
            Value* istrue = B.CreateICmpNE(condv, B.getInt32(0));
            BasicBlock* thenBB = BasicBlock::Create(B.getContext(), "then", F);
            BasicBlock* elseBB = BasicBlock::Create(B.getContext(), "else");
            BasicBlock* contBB = BasicBlock::Create(B.getContext(), "endif");
            B.CreateCondBr(istrue, thenBB, I.else_body.empty() ? contBB : elseBB);
            // then
            B.SetInsertPoint(thenBB);
            auto locals_then = locals;
            bool thenReturned = emit_block(B, F, I.then_body, locals_then, enums);
            if (!thenReturned) B.CreateBr(contBB);
            if (!I.else_body.empty()) {
                F->getBasicBlockList().push_back(elseBB);
                B.SetInsertPoint(elseBB);
                auto locals_else = locals;
                bool elseReturned = emit_block(B, F, I.else_body, locals_else, enums);
                if (!elseReturned) B.CreateBr(contBB);
            }
            F->getBasicBlockList().push_back(contBB);
            B.SetInsertPoint(contBB);
        } else if (std::holds_alternative<S::Match>(st.node)) {
            if (!emit_match(B, F, std::get<S::Match>(st.node), locals, enums)) return false;
        }
    }
    return false;
}

std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path) {
    // Build enum map from unit
    std::unordered_map<std::string, std::unordered_map<std::string,int>> enums;
    for (auto& e : unit.enums) {
        std::unordered_map<std::string,int> m;
        for (int i=0;i<(int)e.variants.size();++i) m[e.variants[i]] = i;
        enums[e.name] = std::move(m);
    }

    // Find main()
    const innesce::ast::Function* mainFn = nullptr;
    for (auto& f : unit.functions) if (f.name=="main") { mainFn = &f; break; }
    if (!mainFn) return "no main() in unit";

    LLVMContext ctx;
    auto mod = std::make_unique<Module>("innesce_mod", ctx);
    IRBuilder<> B(ctx);

    // int main()
    FunctionType* fty = FunctionType::get(B.getInt32Ty(), false);
    Function* F = Function::Create(fty, Function::ExternalLinkage, "main", mod.get());
    BasicBlock* entry = BasicBlock::Create(ctx, "entry", F);
    B.SetInsertPoint(entry);

    std::unordered_map<std::string, Value*> locals;
    bool returned = emit_block(B, F, mainFn->body, locals, enums);
    if (!returned) {
        B.CreateRet(B.getInt32(0));
    }

    if (verifyFunction(*F, &errs())) return "IR verification failed for main()";
    if (verifyModule(*mod, &errs())) return "Module verification failed";

    // Target setup
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    auto triple = sys::getDefaultTargetTriple();
    std::string err;
    const Target* target = TargetRegistry::lookupTarget(triple, err);
    if (!target) return "lookupTarget failed: " + err;

    TargetOptions opt;
    auto relocModel = std::optional<Reloc::Model>();
    std::unique_ptr<TargetMachine> TM(target->createTargetMachine(triple, "generic", "", opt, relocModel));
    mod->setTargetTriple(triple);
    mod->setDataLayout(TM->createDataLayout());

    std::error_code ec;
    raw_fd_ostream dest(obj_path, ec, sys::fs::OF_None);
    if (ec) return "could not open object file: " + ec.message();

    legacy::PassManager pm;
    if (TM->addPassesToEmitFile(pm, dest, nullptr, CodeGenFileType::CGFT_ObjectFile)) {
        return "TargetMachine cannot emit this file type";
    }
    pm.run(*mod);
    dest.flush();
    return {};
}

#endif // INNSCE_HAVE_LLVM

} // namespace innesce::backend::llvm
