
#include "frontend/sema.hpp"
#include <unordered_map>
#include <variant>
#include <set>
#include <algorithm>
#include <iostream>

namespace innesce::front {

using Type = innesce::ast::Type;
using DurUnit = innesce::ast::DurUnit;

static bool is_i32(const Type& t){ return t.kind==Type::I32; }
static bool is_str(const Type& t){ return t.kind==Type::STR; }
static bool is_tuple(const Type& t){ return t.kind==Type::TUPLE; }
static bool is_dur(const Type& t, DurUnit* u=nullptr){ if (t.kind==Type::DUR){ if(u)*u=t.dur; return true;} return false; }
static bool is_enum(const Type& t){ return t.kind==Type::ENUM; }

static bool same_type(const Type& a, const Type& b){
    if (a.kind != b.kind) return false;
    if (a.kind==Type::ENUM) return a.enum_name==b.enum_name;
    if (a.kind==Type::DUR) return a.dur==b.dur;
    if (a.kind==Type::TUPLE) {
        if (a.tuple_elems.size()!=b.tuple_elems.size()) return false;
        for (size_t i=0;i<a.tuple_elems.size();++i) if (!same_type(a.tuple_elems[i], b.tuple_elems[i])) return false;
    }
    return true;
}

static bool has_gate(const std::vector<std::string>& gates, const std::string& g) {
    return std::find(gates.begin(), gates.end(), g) != gates.end();
}

static bool check_expr(const innesce::ast::Expr& e,
                       std::unordered_map<std::string, Type> locals,
                       const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums,
                       const std::vector<std::string>& fn_gates,
                       std::string& err,
                       Type* outTy = nullptr);

static void bind_tuple_pattern_locals(const std::vector<innesce::ast::Expr::Pattern>& pat,
                                      const Type& scrTy,
                                      std::unordered_map<std::string, Type>& locals,
                                      std::string& err) {
    if (scrTy.kind != Type::TUPLE) { err="bind on non-tuple"; return; }
    for (size_t i=0;i<pat.size();++i) {
        if (std::holds_alternative<innesce::ast::Expr::Pattern::Bind>(pat[i].node)) {
            auto name = std::get<innesce::ast::Expr::Pattern::Bind>(pat[i].node).name;
            locals[name] = scrTy.tuple_elems[i];
        }
    }
}

static bool check_expr(const innesce::ast::Expr& e,
                       std::unordered_map<std::string, Type> locals,
                       const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums,
                       const std::vector<std::string>& fn_gates,
                       std::string& err,
                       Type* outTy) {
    using E = innesce::ast::Expr;
    if (std::holds_alternative<E::IntLit>(e.node)) { if (outTy) outTy->kind = Type::I32; return true; }
    if (std::holds_alternative<E::StringLit>(e.node)) { if (outTy) outTy->kind = Type::STR; return true; }
    if (std::holds_alternative<E::DurLit>(e.node)) { if (outTy){ outTy->kind = Type::DUR; outTy->dur = std::get<E::DurLit>(e.node).unit; } return true; }
    if (std::holds_alternative<E::Ident>(e.node)) {
        const auto& I = std::get<E::Ident>(e.node);
        auto it = locals.find(I.name);
        if (it != locals.end()) { if (outTy) *outTy = it->second; return true; }
        for (auto& [ename, vars] : enums) if (vars.count(I.name)) { if (outTy){ outTy->kind = Type::ENUM; outTy->enum_name = ename; } return true; }
        err = "use of undeclared identifier: " + I.name; return false;
    }
    if (std::holds_alternative<E::IsFailed>(e.node)) { if (outTy) outTy->kind = Type::I32; return true; }
    if (std::holds_alternative<E::Unary>(e.node)) {
        const auto& U = std::get<E::Unary>(e.node);
        Type t; if (!check_expr(*U.rhs, locals, enums, fn_gates, err, &t)) return false;
        if (!is_i32(t)) { err = "unary '-' requires i32"; return false; }
        if (outTy) outTy->kind = Type::I32; return true;
    }
    if (std::holds_alternative<E::Cast>(e.node)) {
        const auto& C = std::get<E::Cast>(e.node);
        Type it; if (!check_expr(*C.inner, locals, enums, fn_gates, err, &it)) return false;
        if (is_dur(it) && C.target.kind==Type::DUR) { if (outTy){ *outTy = C.target; } return true; }
        err = "invalid cast: only duration unit conversions are supported"; return false;
    }
    if (std::holds_alternative<E::AsmExpr>(e.node)) {
        const auto& A = std::get<E::AsmExpr>(e.node);
        if (A.outs.empty()) { if (outTy) outTy->kind = Type::I32; return true; }
        if (A.outs.size()==1) { if (outTy) outTy->kind = Type::I32; return true; }
        if (outTy) { outTy->kind = Type::TUPLE; outTy->tuple_elems.assign(A.outs.size(), Type{.kind=Type::I32}); }
        return true;
    }
    if (std::holds_alternative<E::MatchExpr>(e.node)) {
        const auto& M = std::get<E::MatchExpr>(e.node);
        Type scr; if (!check_expr(M.scrutinee, locals, enums, fn_gates, err, &scr)) return false;
        Type rty; bool rty_set=false;
        for (auto& C : M.cases) {
            auto l2 = locals;
            if (C.is_tuple) {
                if (scr.kind != Type::TUPLE) { err = "tuple match on non-tuple expression"; return false; }
                if (C.tpat.size()!=scr.tuple_elems.size()) { err = "tuple pattern arity mismatch"; return false; }
                // validate pattern types and bind names
                for (size_t i=0;i<C.tpat.size();++i) {
                    const auto& et = scr.tuple_elems[i];
                    if (std::holds_alternative<E::Pattern::Wild>(C.tpat[i].node)) {}
                    else if (std::holds_alternative<E::Pattern::Int>(C.tpat[i].node)) { if (et.kind != Type::I32) { err = "int literal pattern on non-i32"; return false; } }
                    else if (std::holds_alternative<E::Pattern::Dur>(C.tpat[i].node)) { if (et.kind != Type::DUR) { err = "duration literal pattern on non-duration"; return false; } }
                    else if (std::holds_alternative<E::Pattern::Bind>(C.tpat[i].node)) { /* bind ok */ }
                }
                bind_tuple_pattern_locals(C.tpat, scr, l2, err);
            }
            if (C.guard) {
                Type gt; if (!check_expr(*C.guard, l2, enums, fn_gates, err, &gt)) return false;
                if (!is_i32(gt)) { err = "guard must be i32 (truthy)"; return false; }
            }
            Type vt; if (!check_expr(*C.value, l2, enums, fn_gates, err, &vt)) return false;
            if (!rty_set) { rty = vt; rty_set = true; }
            else if (!same_type(rty, vt)) { err = "match expression case types differ"; return false; }
        }
        if (outTy) *outTy = rty_set ? rty : Type{.kind=Type::I32};
        return true;
    }
    if (std::holds_alternative<E::Call>(e.node)) {
        const auto& C = std::get<E::Call>(e.node);
        int argc = (int)C.args.size();
        if (C.name == "fs_open") {
            if (!has_gate(fn_gates, "fs.open")) { err = "missing gate 'fs.open' to call fs_open"; return false; }
            if (argc != 1) { err = "fs_open(path) takes 1 arg"; return false; }
            Type t0; if (!check_expr(C.args[0], locals, enums, fn_gates, err, &t0)) return false;
            if (!is_str(t0)) { err = "fs_open(path) expects str"; return false; }
            if (outTy) outTy->kind = Type::I32; return true;
        } else if (C.name == "net_tcp") {
            if (!has_gate(fn_gates, "net.tcp")) { err = "missing gate 'net.tcp' to call net_tcp"; return false; }
            if (argc != 2) { err = "net_tcp(host, port) takes 2 args"; return false; }
            Type t0,t1; if (!check_expr(C.args[0], locals, enums, fn_gates, err, &t0)) return false; if (!check_expr(C.args[1], locals, enums, fn_gates, err, &t1)) return false;
            if (!is_str(t0) || !is_i32(t1)) { err = "net_tcp(host, port) expects (str, i32)"; return false; }
            if (outTy) outTy->kind = Type::I32; return true;
        } else if (C.name == "rand_range") {
            if (!has_gate(fn_gates, "rand")) { err = "missing gate 'rand' to call rand_range"; return false; }
            if (argc != 2) { err = "rand_range(lo, hi) takes 2 args"; return false; }
            Type t0,t1; if (!check_expr(C.args[0], locals, enums, fn_gates, err, &t0)) return false; if (!check_expr(C.args[1], locals, enums, fn_gates, err, &t1)) return false;
            if (!is_i32(t0) || !is_i32(t1)) { err = "rand_range(lo, hi) expects (i32, i32)"; return false; }
            if (outTy) outTy->kind = Type::I32; return true;
        } else { if (outTy) outTy->kind = Type::I32; return true; }
    }
    const auto& B = std::get<E::Binary>(e.node);
    Type lt, rt;
    if (!check_expr(*B.lhs, locals, enums, fn_gates, err, &lt)) return false;
    if (!check_expr(*B.rhs, locals, enums, fn_gates, err, &rt)) return false;
    if (B.op=='+' || B.op=='-') {
        if (is_i32(lt) && is_i32(rt)) { if(outTy) outTy->kind = Type::I32; return true; }
        DurUnit ul, ur; if (is_dur(lt,&ul) && is_dur(rt,&ur) && ul==ur) { if(outTy){ outTy->kind=Type::DUR; outTy->dur=ul; } return true; }
        err = "duration addition/subtraction requires same units"; return false;
    }
    if (B.op=='*') {
        DurUnit u; if (is_dur(lt,&u) && is_i32(rt)) { if(outTy){ outTy->kind=Type::DUR; outTy->dur=u; } return true; }
        if (is_i32(lt) && is_dur(rt,&u)) { if(outTy){ outTy->kind=Type::DUR; outTy->dur=u; } return true; }
        if (is_i32(lt) && is_i32(rt)) { if(outTy) outTy->kind=Type::I32; return true; }
        err = "invalid duration multiplication"; return false;
    }
    if (B.op=='/') {
        DurUnit u; if (is_dur(lt,&u) && is_i32(rt)) { if(outTy){ outTy->kind=Type::DUR; outTy->dur=u; } return true; }
        if (is_i32(lt) && is_i32(rt)) { if(outTy) outTy->kind=Type::I32; return true; }
        err = "invalid duration division"; return false;
    }
    err = "unknown binary operator"; return false;
}

static bool check_block(const std::vector<innesce::ast::Stmt>& body,
                        std::unordered_map<std::string, Type> locals,
                        const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums,
                        std::string& err,
                        bool in_quarantine,
                        std::unordered_map<std::string,bool>& declared_quarantines,
                        const std::vector<std::string>& fn_gates) {
    using S = innesce::ast::Stmt;
    for (auto& st : body) {
        if (std::holds_alternative<S::Let>(st.node)) {
            const auto& L = std::get<S::Let>(st.node);
            Type t; if (!check_expr(L.init, locals, enums, fn_gates, err, &t)) return false;
            if (!same_type(L.type, t)) { err = "type mismatch in let"; return false; }
            locals[L.name] = L.type;
        } else if (std::holds_alternative<S::LetTuple>(st.node)) {
            const auto& LT = std::get<S::LetTuple>(st.node);
            Type t; if (!check_expr(LT.init, locals, enums, fn_gates, err, &t)) return false;
            if (!is_tuple(t) || t.tuple_elems.size() != LT.names.size()) { err = "tuple arity mismatch in let"; return false; }
            if (LT.types.size() != LT.names.size()) { err = "tuple type list mismatch"; return false; }
            for (size_t i=0;i<LT.names.size();++i) {
                if (!same_type(LT.types[i], t.tuple_elems[i])) {
                    bool ok=false; if (t.tuple_elems[i].kind==Type::I32 && LT.types[i].kind==Type::DUR) ok=true;
                    if (!ok) { err = "tuple element type mismatch"; return false; }
                }
                locals[LT.names[i]] = LT.types[i];
            }
        } else if (std::holds_alternative<S::Return>(st.node)) {
            const auto& R = std::get<S::Return>(st.node);
            Type t; if (!check_expr(R.value, locals, enums, fn_gates, err, &t)) return false;
        } else if (std::holds_alternative<S::If>(st.node)) {
            const auto& I = std::get<S::If>(st.node);
            Type t; if (!check_expr(I.cond, locals, enums, fn_gates, err, &t)) return false;
            if (!is_i32(t)) { err = "if condition must be i32"; return false; }
            if (!check_block(I.then_body, locals, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
            if (!check_block(I.else_body, locals, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
        } else if (std::holds_alternative<S::Match>(st.node)) {
            const auto& M = std::get<S::Match>(st.node);
            Type scrTy; if (!check_expr(M.scrutinee, locals, enums, fn_gates, err, &scrTy)) return false;
            for (auto& C : M.cases) {
                auto l2 = locals;
                if (C.is_default) { /* ok */ }
                else if (C.is_tuple) {
                    if (scrTy.kind != Type::TUPLE) { err = "match tuple pattern on non-tuple"; return false; }
                    if (C.tpat.size() != scrTy.tuple_elems.size()) { err = "tuple pattern arity mismatch"; return false; }
                    for (size_t i=0;i<C.tpat.size();++i) {
                        const auto& et = scrTy.tuple_elems[i];
                        if (std::holds_alternative<innesce::ast::Expr::Pattern::Wild>(C.tpat[i].node)) {}
                        else if (std::holds_alternative<innesce::ast::Expr::Pattern::Int>(C.tpat[i].node)) { if (et.kind != Type::I32) { err = "int pattern requires i32 element"; return false; } }
                        else if (std::holds_alternative<innesce::ast::Expr::Pattern::Dur>(C.tpat[i].node)) { if (et.kind != Type::DUR) { err = "duration pattern requires duration element"; return false; } }
                        else if (std::holds_alternative<innesce::ast::Expr::Pattern::Bind>(C.tpat[i].node)) { /* bind ok */ }
                    }
                    bind_tuple_pattern_locals(C.tpat, scrTy, l2, err);
                } else {
                    if (!is_enum(scrTy)) { err = "match label requires enum scrutinee"; return false; }
                }
                if (C.guard) {
                    Type gt; if (!check_expr(*C.guard, l2, enums, fn_gates, err, &gt)) return false;
                    if (!is_i32(gt)) { err = "guard must be i32"; return false; }
                }
                if (!check_block(C.body, l2, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
            }
        } else if (std::holds_alternative<S::Sleep>(st.node)) {
            const auto& SL = std::get<S::Sleep>(st.node);
            Type t; if (!check_expr(SL.amount, locals, enums, fn_gates, err, &t)) return false;
            if (t.kind != Type::DUR) { err = "sleep expects duration value (ms or sec)"; return false; }
            if (!has_gate(fn_gates, "time")) { err = "missing gate 'time' for sleep"; return false; }
        } else if (std::holds_alternative<S::Fail>(st.node)) {
            if (!in_quarantine) { err = "fail; outside of quarantine"; return false; }
        } else if (std::holds_alternative<S::Quarantine>(st.node)) {
            const auto& Q = std::get<S::Quarantine>(st.node);
            std::unordered_map<std::string,bool> declared;
            if (!check_block(Q.body, locals, enums, err, true, declared, fn_gates)) return false;
        } else if (std::holds_alternative<S::Asm>(st.node)) {
            // ok
        }
    }
    return true;
}

SemaResult Sema::check(const ast::Unit& u) {
    enums_.clear();
    for (auto& e : u.enums) {
        std::unordered_map<std::string,int> m; for (int i=0;i<(int)e.variants.size();++i) m[e.variants[i]] = i;
        enums_[e.name] = std::move(m);
    }
    bool has_main = false;
    for (auto& f : u.functions) {
        if (f.name == "main") has_main = true;
        std::unordered_map<std::string, Type> locals;
        std::string err;
        std::unordered_map<std::string,bool> declared;
        if (!check_block(f.body, locals, enums_, err, false, declared, f.gates)) return {err};
    }
    if (!has_main) return {"missing 'main' function"};
    return {};
}

} // namespace innesce::front
