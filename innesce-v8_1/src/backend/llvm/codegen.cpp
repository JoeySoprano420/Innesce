
#include "backend/llvm/codegen.hpp"
#include "frontend/ast.hpp"
#include "frontend/sema.hpp"
#include <llvm/ADT/Triple.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/raw_ostream.h>
#include <unordered_map>
#include <variant>

using namespace ::llvm;

namespace innesce::backend {

static Value* make_cstring(IRBuilder<>& B, Module* M, std::string_view s) {
    auto& C = B.getContext();
    auto arrTy = ArrayType::get(Type::getInt8Ty(C), s.size()+1);
    auto gv = new GlobalVariable(*M, arrTy, true, GlobalValue::PrivateLinkage, nullptr, ".str");
    gv->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
    gv->setAlignment(Align(1));
    std::vector<Constant*> chars;
    for (char ch : s) chars.push_back(ConstantInt::get(Type::getInt8Ty(C), (uint8_t)ch));
    chars.push_back(ConstantInt::get(Type::getInt8Ty(C), 0));
    gv->setInitializer(ConstantArray::get(arrTy, chars));
    Value* zero = ConstantInt::get(Type::getInt32Ty(C), 0);
    Value* gep = ConstantExpr::getInBoundsGetElementPtr(arrTy, gv, {zero, zero});
    return ConstantExpr::getBitCast(gep, Type::getInt8PtrTy(C));
}

static innesce::ast::Type get_expr_type_simple(const innesce::ast::Expr& e) {
    using E = innesce::ast::Expr;
    innesce::ast::Type t;
    if (std::holds_alternative<E::DurLit>(e.node)) { t.kind = innesce::ast::Type::DUR; t.dur = std::get<E::DurLit>(e.node).unit; return t; }
    if (std::holds_alternative<E::AsmExpr>(e.node)) {
        const auto& A = std::get<E::AsmExpr>(e.node);
        if (A.outs.size()<=1) { t.kind = innesce::ast::Type::I32; return t; }
        t.kind = innesce::ast::Type::TUPLE; t.tuple_elems.assign(A.outs.size(), innesce::ast::Type{.kind=innesce::ast::Type::I32});
        return t;
    }
    t.kind = innesce::ast::Type::I32; return t;
}

static Value* emit_expr(IRBuilder<>& B,
                        const innesce::ast::Expr& e,
                        std::unordered_map<std::string, Value*>& locals,
                        std::unordered_map<std::string, Value*>& qflags,
                        const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums,
                        std::unordered_map<std::string, innesce::ast::Type>& localsTy);

static bool emit_block(IRBuilder<>& B,
                       Function* F,
                       const std::vector<innesce::ast::Stmt>& body,
                       std::unordered_map<std::string, Value*>& locals,
                       std::unordered_map<std::string, Value*>& qflags,
                       const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums,
                       std::unordered_map<std::string, innesce::ast::Type>& localsTy);

static Value* emit_match_expr(IRBuilder<>& B,
                              Function* F,
                              const innesce::ast::Expr::MatchExpr& M,
                              std::unordered_map<std::string, Value*>& locals,
                              std::unordered_map<std::string, Value*>& qflags,
                              const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums,
                              std::unordered_map<std::string, innesce::ast::Type>& localsTy) {
    using P = innesce::ast::Expr::Pattern;
    auto scrT = get_expr_type_simple(M.scrutinee);
    Value* scr = emit_expr(B, M.scrutinee, locals, qflags, enums, localsTy); if (!scr) return nullptr;

    BasicBlock* endBB = BasicBlock::Create(B.getContext(), "match.end", F);
    PHINode* phi = nullptr;

    // Prepare tuple extraction if needed
    AllocaInst* tupAlloca = nullptr;
    auto i32 = B.getInt32Ty();
    if (scrT.kind == innesce::ast::Type::TUPLE) {
        tupAlloca = B.CreateAlloca(scr->getType(), nullptr, "tup.scr");
        B.CreateStore(scr, tupAlloca);
    }

    BasicBlock* curCont = endBB;
    std::vector<std::pair<BasicBlock*, Value*>> incoming;

    for (int i=(int)M.cases.size()-1; i>=0; --i) {
        const auto& C = M.cases[(size_t)i];
        // Body block
        BasicBlock* bodyBB = BasicBlock::Create(B.getContext(), "case.body", F);
        B.SetInsertPoint(bodyBB);
        // Bind locals into stack slots just-in-time
        std::unordered_map<std::string, Value*> savedLocals = locals;
        std::unordered_map<std::string, innesce::ast::Type> savedLocalsTy = localsTy;
        auto bind_extract = [&](unsigned idx, std::string name){
            Value* vload = nullptr;
            if (tupAlloca) {
                Value* tupv = B.CreateLoad(tupAlloca->getAllocatedType(), tupAlloca);
                vload = B.CreateExtractValue(tupv, {idx});
            } else {
                vload = B.getInt32(0);
            }
            AllocaInst* a = B.CreateAlloca(i32, nullptr, name);
            B.CreateStore(vload, a);
            locals[name] = a;
            localsTy[name] = (scrT.kind==innesce::ast::Type::TUPLE) ? scrT.tuple_elems[idx] : innesce::ast::Type{.kind=innesce::ast::Type::I32};
        };
        if (C.is_tuple) {
            for (unsigned idx=0; idx<C.tpat.size(); ++idx) {
                if (std::holds_alternative<P::Bind>(C.tpat[idx].node)) {
                    bind_extract(idx, std::get<P::Bind>(C.tpat[idx].node).name);
                }
            }
        }
        Value* val = emit_expr(B, *C.value, locals, qflags, enums, localsTy);
        locals = savedLocals; localsTy = savedLocalsTy;
        if (!val) return nullptr;
        if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(endBB);
        BasicBlock* doneBody = B.GetInsertBlock();
        incoming.push_back({doneBody, val});

        // Test block
        BasicBlock* testBB = BasicBlock::Create(B.getContext(), "case.test", F);
        B.SetInsertPoint(testBB);
        Value* cond = nullptr;
        if (C.is_default) {
            cond = B.getInt1(true);
        } else if (C.is_tuple) {
            for (unsigned idx=0; idx<C.tpat.size(); ++idx) {
                Value* el = nullptr;
                if (tupAlloca) {
                    Value* tupv = B.CreateLoad(tupAlloca->getAllocatedType(), tupAlloca);
                    el = B.CreateExtractValue(tupv, {idx});
                } else {
                    el = scr;
                }
                Value* c = nullptr;
                if (std::holds_alternative<P::Wild>(C.tpat[idx].node)) c = B.getInt1(true);
                else if (std::holds_alternative<P::Int>(C.tpat[idx].node)) {
                    int v = std::get<P::Int>(C.tpat[idx].node).value;
                    c = B.CreateICmpEQ(el, B.getInt32(v));
                } else if (std::holds_alternative<P::Dur>(C.tpat[idx].node)) {
                    int v = std::get<P::Dur>(C.tpat[idx].node).value;
                    c = B.CreateICmpEQ(el, B.getInt32(v));
                } else if (std::holds_alternative<P::Bind>(C.tpat[idx].node)) {
                    c = B.getInt1(true);
                }
                cond = cond ? B.CreateAnd(cond, c) : c;
            }
        } else {
            cond = B.getInt1(false); // enum paths not implemented for expr form here
        }
        // Guard
        if (C.guard) {
            Value* gv = emit_expr(B, *C.guard, locals, qflags, enums, localsTy);
            gv = B.CreateICmpNE(gv, B.getInt32(0));
            cond = cond ? B.CreateAnd(cond, gv) : gv;
        }
        B.CreateCondBr(cond, bodyBB, curCont);
        curCont = testBB;
    }
    // entry into first test
    B.SetInsertPoint(curCont);
    B.CreateBr(endBB);
    B.SetInsertPoint(endBB);

    // PHI node with unified i32 (durations are represented as i32 as well)
    if (!incoming.empty()) {
        phi = B.CreatePHI(B.getInt32Ty(), incoming.size(), "match.val");
        for (auto& inc : incoming) phi->addIncoming(inc.second, inc.first);
        return phi;
    }
    return B.getInt32(0);
}

static Value* emit_expr(IRBuilder<>& B,
                        const innesce::ast::Expr& e,
                        std::unordered_map<std::string, Value*>& locals,
                        std::unordered_map<std::string, Value*>& qflags,
                        const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums,
                        std::unordered_map<std::string, innesce::ast::Type>& localsTy) {
    using E = innesce::ast::Expr;
    if (std::holds_alternative<E::IntLit>(e.node)) { return B.getInt32(std::get<E::IntLit>(e.node).value); }
    if (std::holds_alternative<E::StringLit>(e.node)) { return make_cstring(B, B.GetInsertBlock()->getModule(), std::get<E::StringLit>(e.node).value); }
    if (std::holds_alternative<E::DurLit>(e.node)) { return B.getInt32(std::get<E::DurLit>(e.node).value); }
    if (std::holds_alternative<E::Ident>(e.node)) {
        const auto& nm = std::get<E::Ident>(e.node).name;
        auto it = locals.find(nm); if (it!=locals.end()) return B.CreateLoad(B.getInt32Ty(), it->second);
        for (auto& [ename, vars] : enums) { (void)ename; auto vit = vars.find(nm); if (vit != vars.end()) return B.getInt32(vit->second); }
        return nullptr;
    }
    if (std::holds_alternative<E::IsFailed>(e.node)) {
        const auto& IS = std::get<E::IsFailed>(e.node);
        auto it = qflags.find(IS.name); if (it==qflags.end()) return nullptr;
        auto flag = B.CreateLoad(B.getInt1Ty(), it->second);
        return B.CreateZExt(flag, B.getInt32Ty());
    }
    if (std::holds_alternative<E::Unary>(e.node)) {
        const auto& U = std::get<E::Unary>(e.node);
        Value* R = emit_expr(B, *U.rhs, locals, qflags, enums, localsTy); if (!R) return nullptr;
        if (U.op=='-') return B.CreateNeg(R);
        return R;
    }
    if (std::holds_alternative<E::Binary>(e.node)) {
        const auto& BN = std::get<E::Binary>(e.node);
        Value* L = emit_expr(B, *BN.lhs, locals, qflags, enums, localsTy); if (!L) return nullptr;
        Value* R = emit_expr(B, *BN.rhs, locals, qflags, enums, localsTy); if (!R) return nullptr;
        switch (BN.op) { case '+': return B.CreateAdd(L, R); case '-': return B.CreateSub(L, R); case '*': return B.CreateMul(L, R); case '/': return B.CreateSDiv(L, R); }
        return nullptr;
    }
    if (std::holds_alternative<E::Cast>(e.node)) {
        const auto& Cst = std::get<E::Cast>(e.node);
        if (std::holds_alternative<E::DurLit>(Cst.inner->node)) {
            auto dl = std::get<E::DurLit>(Cst.inner->node);
            int v = dl.value;
            if (dl.unit == innesce::ast::DurUnit::MS && Cst.target.kind==innesce::ast::Type::DUR && Cst.target.dur==innesce::ast::DurUnit::SEC) return B.getInt32(v / 1000);
            if (dl.unit == innesce::ast::DurUnit::SEC && Cst.target.kind==innesce::ast::Type::DUR && Cst.target.dur==innesce::ast::DurUnit::MS) return B.getInt32(v * 1000);
            return B.getInt32(v);
        }
        Value* inner = emit_expr(B, *Cst.inner, locals, qflags, enums, localsTy); if (!inner) return nullptr;
        // runtime unit conversion if needed
        return inner;
    }
    if (std::holds_alternative<E::Call>(e.node)) {
        const auto& C = std::get<E::Call>(e.node);
        Module* M = B.GetInsertBlock()->getModule();
        auto i32 = B.getInt32Ty(); auto i8p = Type::getInt8PtrTy(B.getContext());
        auto marshal = [&](const innesce::ast::Expr& a)->Value*{ return emit_expr(B, a, locals, qflags, enums, localsTy); };
        if (C.name == "fs_open") {
            Value* v = marshal(C.args[0]); if (v->getType()!=i8p) v = B.CreateBitCast(v, i8p);
            FunctionType* FT = FunctionType::get(i32, {i8p}, false);
            return B.CreateCall(M->getOrInsertFunction("fs_open_str", FT), {v});
        } else if (C.name == "net_tcp") {
            Value* host = marshal(C.args[0]); if (host->getType()!=i8p) host = B.CreateBitCast(host, i8p);
            Value* port = marshal(C.args[1]); if (port->getType()!=i32) port = B.CreateTruncOrBitCast(port, i32);
            FunctionType* FT = FunctionType::get(i32, {i8p, i32}, false);
            return B.CreateCall(M->getOrInsertFunction("net_tcp_str_i32", FT), {host, port});
        } else if (C.name == "rand_range") {
            Value* lo = marshal(C.args[0]); if (lo->getType()!=i32) lo = B.CreateTruncOrBitCast(lo, i32);
            Value* hi = marshal(C.args[1]); if (hi->getType()!=i32) hi = B.CreateTruncOrBitCast(hi, i32);
            FunctionType* FT = FunctionType::get(i32, {i32, i32}, false);
            return B.CreateCall(M->getOrInsertFunction("rand_range_i32", FT), {lo, hi});
        } else {
            std::vector<Type*> argtys; std::vector<Value*> args;
            for (auto& a : C.args) { Value* v = marshal(a); if (v->getType()!=i32) v = B.CreateTruncOrBitCast(v, i32); argtys.push_back(i32); args.push_back(v); }
            FunctionType* FT = FunctionType::get(i32, argtys, false);
            FunctionCallee cal = M->getOrInsertFunction(C.name, FT);
            return B.CreateCall(cal, args);
        }
    }
    if (std::holds_alternative<E::AsmExpr>(e.node)) {
        const auto& A = std::get<E::AsmExpr>(e.node);
        std::vector<Type*> outtys; std::vector<Type*> argtys; std::vector<Value*> args;
        for (auto& op : A.ins) {
            Value* v = nullptr; if (op.is_immediate) v = B.getInt32(op.imm_val);
            else { auto it = locals.find(op.name); if (it == locals.end()) return nullptr; v = B.CreateLoad(B.getInt32Ty(), it->second); }
            if (v->getType()!=B.getInt32Ty()) v = B.CreateTruncOrBitCast(v, B.getInt32Ty());
            argtys.push_back(B.getInt32Ty()); args.push_back(v);
        }
        for (auto& op : A.outs) outtys.push_back(B.getInt32Ty());
        Type* retTy = Type::getVoidTy(B.getContext());
        if (outtys.size()==1) retTy = outtys[0];
        else if (!outtys.empty()) retTy = StructType::get(B.getContext(), outtys);
        std::string cons;
        for (size_t i=0;i<A.outs.size();++i) { cons += "="+A.outs[i].constraint; if (i+1<A.outs.size()) cons += ","; }
        if (!A.outs.empty() && !A.ins.empty()) cons += ",";
        for (size_t i=0;i<A.ins.size();++i) { cons += A.ins[i].constraint; if (i+1<A.ins.size()) cons += ","; }
        for (auto& c : A.clobbers) { cons += ",~{"+c+"}"; }
        auto IA = InlineAsm::get(FunctionType::get(retTy, argtys, false), A.body, cons, true, false, A.intel ? InlineAsm::AD_Intel : InlineAsm::AD_ATT);
        CallInst* call = B.CreateCall(IA, args);
        if (outtys.size()==1) return call;
        if (!outtys.empty()) return call;
        return B.getInt32(0);
    }
    if (std::holds_alternative<E::MatchExpr>(e.node)) {
        return emit_match_expr(B, F, std::get<E::MatchExpr>(e.node), locals, qflags, enums, localsTy);
    }
    return nullptr;
}

static bool emit_block(IRBuilder<>& B,
                       Function* F,
                       const std::vector<innesce::ast::Stmt>& body,
                       std::unordered_map<std::string, Value*>& locals,
                       std::unordered_map<std::string, Value*>& qflags,
                       const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums,
                       std::unordered_map<std::string, innesce::ast::Type>& localsTy) {
    using S = innesce::ast::Stmt;
    for (auto& st : body) {
        if (std::holds_alternative<S::Let>(st.node)) {
            const auto& L = std::get<S::Let>(st.node);
            Value* init = emit_expr(B, L.init, locals, qflags, enums, localsTy); if (!init) return false;
            AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, L.name);
            B.CreateStore(init, a);
            locals[L.name] = a; localsTy[L.name] = L.type;
        } else if (std::holds_alternative<S::LetTuple>(st.node)) {
            const auto& LT = std::get<S::LetTuple>(st.node);
            Value* init = emit_expr(B, LT.init, locals, qflags, enums, localsTy); if (!init) return false;
            if (LT.names.size()==1) {
                AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, LT.names[0]); B.CreateStore(init, a);
                locals[LT.names[0]] = a; localsTy[LT.names[0]] = LT.types[0];
            } else {
                for (size_t i=0;i<LT.names.size();++i) {
                    Value* v = B.CreateExtractValue(init, {unsigned(i)});
                    AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, LT.names[i]);
                    B.CreateStore(v, a);
                    locals[LT.names[i]] = a; localsTy[LT.names[i]] = LT.types[i];
                }
            }
        } else if (std::holds_alternative<S::Return>(st.node)) {
            const auto& R = std::get<S::Return>(st.node);
            Value* v = emit_expr(B, R.value, locals, qflags, enums, localsTy); if (!v) return false;
            B.CreateRet(v); return true;
        } else if (std::holds_alternative<S::If>(st.node)) {
            const auto& I = std::get<S::If>(st.node);
            Value* c = emit_expr(B, I.cond, locals, qflags, enums, localsTy); if (!c) return false;
            c = B.CreateICmpNE(c, B.getInt32(0));
            BasicBlock* thenBB = BasicBlock::Create(B.getContext(), "then", F);
            BasicBlock* elseBB = BasicBlock::Create(B.getContext(), "else", F);
            BasicBlock* contBB = BasicBlock::Create(B.getContext(), "endif", F);
            B.CreateCondBr(c, thenBB, elseBB);
            B.SetInsertPoint(thenBB); emit_block(B, F, I.then_body, locals, qflags, enums, localsTy); if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(contBB);
            B.SetInsertPoint(elseBB); emit_block(B, F, I.else_body, locals, qflags, enums, localsTy); if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(contBB);
            B.SetInsertPoint(contBB);
        } else if (std::holds_alternative<S::Match>(st.node)) {
            // Lower as a side-effect-only chain (no value)
            const auto& M = std::get<S::Match>(st.node);
            // Reuse expression lowering with dummy value by emitting a constant in each body via return to end block
            // For brevity, we just create bodies and tests similarly to expr form but discard the result.
            // We'll call emit_match_expr to generate value and then ignore it.
            using E = innesce::ast::Expr;
            E::MatchExpr mx; mx.scrutinee = M.scrutinee;
            for (auto& c : M.cases) {
                E::MatchExpr::Case ec;
                ec.is_default=c.is_default; ec.is_tuple=c.is_tuple; ec.label=c.label; ec.tpat=c.tpat; ec.guard=c.guard;
                // Wrap the body as value: if it has a trailing 'return x;' we can't easily introspect.
                // Emit a constant 0 for each case body; but to execute the user's statements, we need a separate lowering.
                // To keep consistency, we'll inline a trivial 0 value and then actually emit the body before jumping end.
                // Handled by duplication isn't trivial here; for simplicity, ignore stmt form in this minimal demo.
                ec.value = std::make_unique<E>(E{E::IntLit{0}});
                mx.cases.push_back(std::move(ec));
            }
            (void)emit_match_expr(B, F, mx, locals, qflags, enums, localsTy);
        } else if (std::holds_alternative<S::Sleep>(st.node)) {
            const auto& SL = std::get<S::Sleep>(st.node);
            Value* amt = emit_expr(B, SL.amount, locals, qflags, enums, localsTy); if (!amt) return false;
            auto i64 = Type::getInt64Ty(B.getContext());
            auto FT = FunctionType::get(Type::getVoidTy(B.getContext()), {i64}, false);
            const char* fname = SL.is_ms ? "inn_sleep_ms" : "inn_sleep_sec";
            Function* fs = cast<Function>(B.GetInsertBlock()->getModule()->getOrInsertFunction(fname, FT).getCallee());
            Value* amt64 = B.CreateSExt(amt, i64);
            B.CreateCall(fs, {amt64});
        } else if (std::holds_alternative<S::Fail>(st.node)) {
            if (qflags.empty()) return false;
            auto it = qflags.end(); --it;
            B.CreateStore(B.getInt1(true), it->second);
        } else if (std::holds_alternative<S::Quarantine>(st.node)) {
            const auto& Q = std::get<S::Quarantine>(st.node);
            AllocaInst* flag = B.CreateAlloca(B.getInt1Ty(), nullptr, (Q.name + std::string("_failed")).c_str());
            B.CreateStore(B.getInt1(false), flag);
            qflags[Q.name] = flag;
            emit_block(B, F, Q.body, locals, qflags, enums, localsTy);
        } else if (std::holds_alternative<S::Asm>(st.node)) {
            // no-op for demo
        }
    }
    return true;
}

bool compile_to_object(const innesce::ast::Unit& unit,
                       const innesce::front::Sema& sema,
                       const std::string& out_obj_path,
                       std::string& err) {
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();

    LLVMContext Ctx;
    auto mod = std::make_unique<Module>("inn", Ctx);
    IRBuilder<> B(Ctx);

    const auto& enums = sema.enums();

    for (auto& f : unit.functions) {
        FunctionType* fty = FunctionType::get(Type::getInt32Ty(Ctx), {}, false);
        Function* F = Function::Create(fty, Function::ExternalLinkage, f.name, mod.get());
        if (f.hot) F->addFnAttr(Attribute::Hot);
        BasicBlock* entry = BasicBlock::Create(Ctx, "entry", F);
        B.SetInsertPoint(entry);
        std::unordered_map<std::string, Value*> locals;
        std::unordered_map<std::string, Value*> qflags;
        std::unordered_map<std::string, innesce::ast::Type> localsTy;
        emit_block(B, F, f.body, locals, qflags, enums, localsTy);
        if (!entry->getTerminator()) B.CreateRet(B.getInt32(0));
    }

    std::string verr; raw_string_ostream os(verr);
    if (verifyModule(*mod, &os)) { err = os.str(); return false; }

    auto targetTriple = sys::getDefaultTargetTriple();
    std::string errStr;
    auto target = TargetRegistry::lookupTarget(targetTriple, errStr);
    if (!target) { err = errStr; return false; }

    TargetOptions opt; auto RM = std::optional<Reloc::Model>();
    auto tm = std::unique_ptr<TargetMachine>(target->createTargetMachine(targetTriple, "generic", "", opt, RM));

    mod->setDataLayout(tm->createDataLayout()); mod->setTargetTriple(targetTriple);

    std::error_code EC; raw_fd_ostream dest(out_obj_path, EC, sys::fs::OF_None);
    if (EC) { err = "Could not open output file: " + EC.message(); return false; }

    legacy::PassManager pass;
    if (tm->addPassesToEmitFile(pass, dest, nullptr, CGFT_ObjectFile)) { err = "TargetMachine can't emit an object file"; return false; }
    pass.run(*mod); dest.flush(); return true;
}

} // namespace innesce::backend
