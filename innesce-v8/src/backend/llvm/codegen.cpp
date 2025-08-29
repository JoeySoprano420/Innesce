
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
    auto gv = new GlobalVariable(*M, arrTy, /*isConst*/true, GlobalValue::PrivateLinkage, nullptr, ".str");
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

static innesce::ast::Type get_expr_type(const innesce::ast::Expr& e,
    const std::unordered_map<std::string, innesce::ast::Type>& localsTy,
    const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums) {
    using E = innesce::ast::Expr;
    innesce::ast::Type t;
    if (std::holds_alternative<E::IntLit>(e.node)) { t.kind = innesce::ast::Type::I32; return t; }
    if (std::holds_alternative<E::StringLit>(e.node)) { t.kind = innesce::ast::Type::STR; return t; }
    if (std::holds_alternative<E::DurLit>(e.node)) { t.kind = innesce::ast::Type::DUR; t.dur = std::get<E::DurLit>(e.node).unit; return t; }
    if (std::holds_alternative<E::Ident>(e.node)) {
        const auto& id = std::get<E::Ident>(e.node).name;
        auto it = localsTy.find(id);
        if (it != localsTy.end()) return it->second;
        for (auto& [ename, vars] : enums) { (void)ename; if (vars.count(id)) { t.kind = innesce::ast::Type::I32; return t; } }
        t.kind = innesce::ast::Type::I32; return t;
    }
    if (std::holds_alternative<E::Unary>(e.node)) { t.kind = innesce::ast::Type::I32; return t; }
    if (std::holds_alternative<E::Binary>(e.node)) { t.kind = innesce::ast::Type::I32; return t; }
    if (std::holds_alternative<E::IsFailed>(e.node)) { t.kind = innesce::ast::Type::I32; return t; }
    if (std::holds_alternative<E::Cast>(e.node)) { return std::get<E::Cast>(e.node).target; }
    if (std::holds_alternative<E::Call>(e.node)) { t.kind = innesce::ast::Type::I32; return t; }
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
                        const std::unordered_map<std::string, Value*>& qflags,
                        const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums,
                        const std::unordered_map<std::string, innesce::ast::Type>& localsTy);

static bool emit_block(IRBuilder<>& B,
                       Function* F,
                       const std::vector<innesce::ast::Stmt>& body,
                       std::unordered_map<std::string, Value*>& locals,
                       std::unordered_map<std::string, Value*>& qflags,
                       const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums,
                       std::unordered_map<std::string, innesce::ast::Type>& localsTy);

static Value* emit_expr(IRBuilder<>& B,
                        const innesce::ast::Expr& e,
                        std::unordered_map<std::string, Value*>& locals,
                        const std::unordered_map<std::string, Value*>& qflags,
                        const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums,
                        const std::unordered_map<std::string, innesce::ast::Type>& localsTy) {
    using E = innesce::ast::Expr;
    if (std::holds_alternative<E::IntLit>(e.node)) {
        int v = std::get<E::IntLit>(e.node).value; return B.getInt32(v);
    }
    if (std::holds_alternative<E::StringLit>(e.node)) {
        Module* M = B.GetInsertBlock()->getModule();
        return make_cstring(B, M, std::get<E::StringLit>(e.node).value);
    }
    if (std::holds_alternative<E::DurLit>(e.node)) {
        int v = std::get<E::DurLit>(e.node).value; return B.getInt32(v);
    }
    if (std::holds_alternative<E::Ident>(e.node)) {
        const auto& nm = std::get<E::Ident>(e.node).name;
        auto it = locals.find(nm);
        if (it != locals.end()) return B.CreateLoad(B.getInt32Ty(), it->second);
        for (auto& [ename, vars] : enums) { (void)ename;
            auto vit = vars.find(nm); if (vit != vars.end()) return B.getInt32(vit->second);
        }
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
        switch (BN.op) {
            case '+': return B.CreateAdd(L, R);
            case '-': return B.CreateSub(L, R);
            case '*': return B.CreateMul(L, R);
            case '/': return B.CreateSDiv(L, R);
        }
        return nullptr;
    }
    if (std::holds_alternative<E::Cast>(e.node)) {
        const auto& Cst = std::get<E::Cast>(e.node);
        if (std::holds_alternative<E::DurLit>(Cst.inner->node)) {
            auto dl = std::get<E::DurLit>(Cst.inner->node);
            int v = dl.value;
            if (dl.unit == innesce::ast::DurUnit::MS && Cst.target.kind==innesce::ast::Type::DUR && Cst.target.dur==innesce::ast::DurUnit::SEC) {
                return B.getInt32(v / 1000);
            } else if (dl.unit == innesce::ast::DurUnit::SEC && Cst.target.kind==innesce::ast::Type::DUR && Cst.target.dur==innesce::ast::DurUnit::MS) {
                return B.getInt32(v * 1000);
            } else {
                return B.getInt32(v);
            }
        }
        Value* inner = emit_expr(B, *Cst.inner, locals, qflags, enums, localsTy); if (!inner) return nullptr;
        auto srcTy = get_expr_type(*Cst.inner, localsTy, enums);
        if (srcTy.kind==innesce::ast::Type::DUR && Cst.target.kind==innesce::ast::Type::DUR) {
            if (srcTy.dur == innesce::ast::DurUnit::MS && Cst.target.dur == innesce::ast::DurUnit::SEC) {
                return B.CreateSDiv(inner, B.getInt32(1000));
            } else if (srcTy.dur == innesce::ast::DurUnit::SEC && Cst.target.dur == innesce::ast::DurUnit::MS) {
                return B.CreateMul(inner, B.getInt32(1000));
            } else {
                return inner;
            }
        }
        return inner;
    }
    if (std::holds_alternative<E::Call>(e.node)) {
        const auto& C = std::get<E::Call>(e.node);
        Module* M = B.GetInsertBlock()->getModule();
        std::vector<Type*> argtys;
        std::vector<Value*> args;
        auto i32 = B.getInt32Ty();
        auto i8p = Type::getInt8PtrTy(B.getContext());
        auto marshal = [&](const innesce::ast::Expr& a)->Value*{
            return emit_expr(B, a, locals, qflags, enums, localsTy);
        };
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
            for (auto& a : C.args) {
                Value* v = marshal(a);
                if (v->getType() != i32) v = B.CreateTruncOrBitCast(v, i32);
                argtys.push_back(i32); args.push_back(v);
            }
            FunctionType* FT = FunctionType::get(i32, argtys, false);
            FunctionCallee cal = M->getOrInsertFunction(C.name, FT);
            return B.CreateCall(cal, args);
        }
    }
    if (std::holds_alternative<E::AsmExpr>(e.node)) {
        const auto& A = std::get<E::AsmExpr>(e.node);
        std::vector<Type*> outtys;
        std::vector<Type*> argtys;
        std::vector<Value*> args;
        for (auto& op : A.ins) {
            Value* v = nullptr;
            if (op.is_immediate) v = B.getInt32(op.imm_val);
            else {
                auto it = locals.find(op.name);
                if (it == locals.end()) return nullptr;
                v = B.CreateLoad(B.getInt32Ty(), it->second);
            }
            if (v->getType() != B.getInt32Ty()) v = B.CreateTruncOrBitCast(v, B.getInt32Ty());
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
    return nullptr;
}

static bool emit_match(IRBuilder<>& B,
                       Function* F,
                       const innesce::ast::Stmt::Match& M,
                       std::unordered_map<std::string, Value*>& locals,
                       std::unordered_map<std::string, Value*>& qflags,
                       const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums,
                       std::unordered_map<std::string, innesce::ast::Type>& localsTy) {
    auto scrTy = get_expr_type(M.scrutinee, localsTy, enums);
    Value* scr = emit_expr(B, M.scrutinee, locals, qflags, enums, localsTy); if (!scr) return false;

    BasicBlock* endBB = BasicBlock::Create(B.getContext(), "match.end", F);

    if (scrTy.kind == innesce::ast::Type::I32 || scrTy.kind == innesce::ast::Type::ENUM) {
        SwitchInst* swi = B.CreateSwitch(scr, endBB, M.cases.size());
        std::vector<BasicBlock*> bodyBBs; bodyBBs.reserve(M.cases.size());
        int defaultIdx = -1;
        for (size_t i=0;i<M.cases.size();++i) {
            auto& C = M.cases[i];
            BasicBlock* bb = BasicBlock::Create(B.getContext(), "case", F);
            bodyBBs.push_back(bb);
            if (C.is_default) { defaultIdx = int(i); continue; }
            int val = 0;
            if (!C.is_tuple) {
                for (auto& [ename, vars] : enums) {
                    auto it = vars.find(C.label);
                    if (it != vars.end()) { val = it->second; break; }
                }
            }
            swi->addCase(B.getInt32(val), bb);
        }
        if (defaultIdx >= 0) swi->setDefaultDest(bodyBBs[defaultIdx]);
        for (size_t i=0;i<M.cases.size();++i) {
            B.SetInsertPoint(bodyBBs[i]);
            emit_block(B, F, M.cases[i].body, locals, qflags, enums, localsTy);
            if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(endBB);
        }
        B.SetInsertPoint(endBB);
        return true;
    }

    // Tuple: create temp alloca to extract elements multiple times
    AllocaInst* tupAlloca = B.CreateAlloca(scr->getType(), nullptr, "tup.scr");
    B.CreateStore(scr, tupAlloca);

    auto extract = [&](unsigned idx)->Value*{
        Value* v = B.CreateLoad(tupAlloca->getAllocatedType(), tupAlloca);
        return B.CreateExtractValue(v, {idx});
    };

    BasicBlock* curCont = endBB;
    for (int i=int(M.cases.size())-1; i>=0; --i) {
        auto& C = M.cases[size_t(i)];
        BasicBlock* bodyBB = BasicBlock::Create(B.getContext(), "case.body", F);
        B.SetInsertPoint(bodyBB);
        emit_block(B, F, C.body, locals, qflags, enums, localsTy);
        if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(endBB);

        BasicBlock* testBB = BasicBlock::Create(B.getContext(), "case.test", F);
        B.SetInsertPoint(testBB);
        Value* cond = nullptr;
        if (C.is_default) {
            cond = B.getInt1(true);
        } else if (C.is_tuple) {
            for (unsigned idx=0; idx<C.tpat.size(); ++idx) {
                auto el = extract(idx);
                Value* c = nullptr;
                if (std::holds_alternative<innesce::ast::Pattern::Wild>(C.tpat[idx].node)) {
                    c = B.getInt1(true);
                } else if (std::holds_alternative<innesce::ast::Pattern::Int>(C.tpat[idx].node)) {
                    int v = std::get<innesce::ast::Pattern::Int>(C.tpat[idx].node).value;
                    c = B.CreateICmpEQ(el, B.getInt32(v));
                } else if (std::holds_alternative<innesce::ast::Pattern::Dur>(C.tpat[idx].node)) {
                    int v = std::get<innesce::ast::Pattern::Dur>(C.tpat[idx].node).value;
                    c = B.CreateICmpEQ(el, B.getInt32(v));
                }
                cond = cond ? B.CreateAnd(cond, c) : c;
            }
        } else {
            cond = B.getInt1(false);
        }
        B.CreateCondBr(cond, bodyBB, curCont);
        curCont = testBB;
    }
    B.SetInsertPoint(curCont);
    B.CreateBr(endBB);
    B.SetInsertPoint(endBB);
    return true;
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
                AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, LT.names[0]);
                B.CreateStore(init, a);
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
            B.CreateRet(v);
            return true;
        } else if (std::holds_alternative<S::If>(st.node)) {
            const auto& I = std::get<S::If>(st.node);
            Value* c = emit_expr(B, I.cond, locals, qflags, enums, localsTy); if (!c) return false;
            c = B.CreateICmpNE(c, B.getInt32(0));
            BasicBlock* thenBB = BasicBlock::Create(B.getContext(), "then", F);
            BasicBlock* elseBB = BasicBlock::Create(B.getContext(), "else", F);
            BasicBlock* contBB = BasicBlock::Create(B.getContext(), "endif", F);
            B.CreateCondBr(c, thenBB, elseBB);
            B.SetInsertPoint(thenBB);
            emit_block(B, F, I.then_body, locals, qflags, enums, localsTy);
            if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(contBB);
            B.SetInsertPoint(elseBB);
            emit_block(B, F, I.else_body, locals, qflags, enums, localsTy);
            if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(contBB);
            B.SetInsertPoint(contBB);
        } else if (std::holds_alternative<S::Match>(st.node)) {
            if (!emit_match(B, F, std::get<S::Match>(st.node), locals, qflags, enums, localsTy)) return false;
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
            const auto& A = std::get<S::Asm>(st.node);
            std::vector<Type*> outtys;
            std::vector<Type*> argtys;
            std::vector<Value*> args;
            for (auto& op : A.ins) {
                Value* v = nullptr;
                if (op.is_immediate) v = B.getInt32(op.imm_val);
                else {
                    auto it = locals.find(op.name);
                    if (it == locals.end()) return false;
                    v = B.CreateLoad(B.getInt32Ty(), it->second);
                }
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
            if (outtys.size()==1) {
                auto& op = A.outs[0];
                auto it = locals.find(op.name); if (it==locals.end()) return false;
                B.CreateStore(call, it->second);
            } else if (outtys.size()>1) {
                for (size_t i=0;i<outtys.size();++i) {
                    auto& op = A.outs[i];
                    auto it = locals.find(op.name); if (it==locals.end()) return false;
                    Value* v = B.CreateExtractValue(call, {unsigned(i)});
                    B.CreateStore(v, it->second);
                }
            }
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
        if (!entry->getTerminator()) {
            B.CreateRet(B.getInt32(0));
        }
    }

    std::string verr;
    raw_string_ostream os(verr);
    if (verifyModule(*mod, &os)) { err = os.str(); return false; }

    auto targetTriple = sys::getDefaultTargetTriple();
    std::string errStr;
    auto target = TargetRegistry::lookupTarget(targetTriple, errStr);
    if (!target) { err = errStr; return false; }

    TargetOptions opt;
    auto RM = std::optional<Reloc::Model>();
    auto tm = std::unique_ptr<TargetMachine>(target->createTargetMachine(targetTriple, "generic", "", opt, RM));

    mod->setDataLayout(tm->createDataLayout());
    mod->setTargetTriple(targetTriple);

    std::error_code EC;
    raw_fd_ostream dest(out_obj_path, EC, sys::fs::OF_None);
    if (EC) { err = "Could not open output file: " + EC.message(); return false; }

    legacy::PassManager pass;
    if (tm->addPassesToEmitFile(pass, dest, nullptr, CGFT_ObjectFile)) { err = "TargetMachine can't emit an object file"; return false; }
    pass.run(*mod);
    dest.flush();
    return true;
}

} // namespace innesce::backend
