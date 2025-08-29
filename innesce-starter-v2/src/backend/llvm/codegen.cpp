#include "backend/llvm/codegen.hpp"
#include "backend/llvm/ir_builder.hpp"

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
#endif

namespace innesce::backend::llvm {

std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path) {
#ifndef INNSCE_HAVE_LLVM
    (void)unit; (void)obj_path;
    return "LLVM backend disabled: rebuild with -DINNSCE_ENABLE_LLVM=ON";
#else
    using namespace ::llvm;
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

    // Local variables
    std::unordered_map<std::string, Value*> locals;

    for (auto& st : mainFn->body) {
        if (std::holds_alternative<innesce::ast::Stmt::Let>(st.node)) {
            const auto& L = std::get<innesce::ast::Stmt::Let>(st.node);
            AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, L.name);
            if (std::holds_alternative<innesce::ast::Expr::IntLit>(L.init.node)) {
                int v = std::get<innesce::ast::Expr::IntLit>(L.init.node).value;
                B.CreateStore(B.getInt32(v), a);
            } else {
                // identifier init: load from existing
                const auto& I = std::get<innesce::ast::Expr::Ident>(L.init.node);
                auto it = locals.find(I.name);
                if (it == locals.end()) return "undeclared init: " + I.name;
                Value* val = B.CreateLoad(B.getInt32Ty(), it->second);
                B.CreateStore(val, a);
            }
            locals[L.name] = a;
        } else {
            const auto& R = std::get<innesce::ast::Stmt::Return>(st.node);
            Value* retv = nullptr;
            if (std::holds_alternative<innesce::ast::Expr::IntLit>(R.value.node)) {
                int v = std::get<innesce::ast::Expr::IntLit>(R.value.node).value;
                retv = B.getInt32(v);
            } else {
                const auto& I = std::get<innesce::ast::Expr::Ident>(R.value.node);
                auto it = locals.find(I.name);
                if (it == locals.end()) return "undeclared return: " + I.name;
                retv = B.CreateLoad(B.getInt32Ty(), it->second);
            }
            B.CreateRet(retv);
        }
    }
    if (!entry->getTerminator()) {
        // default return 0 if no explicit return
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
#endif
}

} // namespace innesce::backend::llvm
