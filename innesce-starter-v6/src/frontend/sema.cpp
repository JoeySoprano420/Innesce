#include "frontend/sema.hpp"
#include <unordered_map>
#include <variant>
#include <set>
#include <algorithm>

namespace innesce::front {

using Type = innesce::ast::Type;
using DurUnit = innesce::ast::DurUnit;

static bool is_i32(const Type& t){ return t.kind==Type::I32; }
static bool is_dur(const Type& t, DurUnit* u=nullptr){ if (t.kind==Type::DUR){ if(u)*u=t.dur; return true;} return false; }
static bool is_enum(const Type& t){ return t.kind==Type::ENUM; }

static bool check_expr(const innesce::ast::Expr& e,
                       const std::unordered_map<std::string, Type>& locals,
                       const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums,
                       std::string& err,
                       Type* outTy = nullptr)
{
    using E = innesce::ast::Expr;
    if (std::holds_alternative<E::IntLit>(e.node)) {
        if (outTy) outTy->kind = Type::I32; return true;
    }
    if (std::holds_alternative<E::DurLit>(e.node)) {
        if (outTy){ outTy->kind = Type::DUR; outTy->dur = std::get<E::DurLit>(e.node).unit; } return true;
    }
    if (std::holds_alternative<E::Ident>(e.node)) {
        const auto& I = std::get<E::Ident>(e.node);
        auto it = locals.find(I.name);
        if (it != locals.end()) { if (outTy) *outTy = it->second; return true; }
        // enum variant?
        for (auto& [ename, vars] : enums) {
            if (vars.count(I.name)) { if (outTy){ outTy->kind = Type::ENUM; outTy->enum_name = ename; } return true; }
        }
        err = "use of undeclared identifier: " + I.name; return false;
    }
    if (std::holds_alternative<E::IsFailed>(e.node)) {
        if (outTy) outTy->kind = Type::I32; return true;
    }
    if (std::holds_alternative<E::Unary>(e.node)) {
        const auto& U = std::get<E::Unary>(e.node);
        Type t; if (!check_expr(*U.rhs, locals, enums, err, &t)) return false;
        if (!is_i32(t)) { err = "unary '-' requires i32"; return false; }
        if (outTy) outTy->kind = Type::I32; return true;
    }
    const auto& B = std::get<E::Binary>(e.node);
    Type lt, rt;
    if (!check_expr(*B.lhs, locals, enums, err, &lt)) return false;
    if (!check_expr(*B.rhs, locals, enums, err, &rt)) return false;
    // arithmetic typing
    if (B.op=='+' || B.op=='-') {
        if (is_i32(lt) && is_i32(rt)) { if(outTy) outTy->kind = Type::I32; return true; }
        DurUnit ul, ur;
        if (is_dur(lt,&ul) && is_dur(rt,&ur) && ul==ur) { if(outTy){ outTy->kind=Type::DUR; outTy->dur=ul; } return true; }
        err = "duration addition/subtraction requires same units"; return false;
    }
    if (B.op=='*') {
        DurUnit u;
        if (is_dur(lt,&u) && is_i32(rt)) { if(outTy){ outTy->kind=Type::DUR; outTy->dur=u; } return true; }
        if (is_i32(lt) && is_dur(rt,&u)) { if(outTy){ outTy->kind=Type::DUR; outTy->dur=u; } return true; }
        if (is_i32(lt) && is_i32(rt)) { if(outTy) outTy->kind=Type::I32; return true; }
        err = "invalid duration multiplication"; return false;
    }
    if (B.op=='/') {
        DurUnit u;
        if (is_dur(lt,&u) && is_i32(rt)) { if(outTy){ outTy->kind=Type::DUR; outTy->dur=u; } return true; }
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
            Type t;
            if (!check_expr(L.init, locals, enums, err, &t)) return false;
            // type match
            if (L.type.kind != t.kind) { err = "type mismatch in let"; return false; }
            if (L.type.kind==Type::ENUM && L.type.enum_name!=t.enum_name) { err = "enum type mismatch in let"; return false; }
            if (L.type.kind==Type::DUR && L.type.dur!=t.dur) { err = "duration unit mismatch in let"; return false; }
            locals[L.name] = L.type;
        } else if (std::holds_alternative<S::Return>(st.node)) {
            const auto& R = std::get<S::Return>(st.node);
            Type t; if (!check_expr(R.value, locals, enums, err, &t)) return false;
        } else if (std::holds_alternative<S::If>(st.node)) {
            const auto& I = std::get<S::If>(st.node);
            Type t; if (!check_expr(I.cond, locals, enums, err, &t)) return false;
            if (!is_i32(t)) { err = "if condition must be i32"; return false; }
            if (!check_block(I.then_body, locals, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
            if (!check_block(I.else_body, locals, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
        } else if (std::holds_alternative<S::Match>(st.node)) {
            const auto& M = std::get<S::Match>(st.node);
            Type t; if (!check_expr(M.scrutinee, locals, enums, err, &t)) return false;
            std::set<std::string> enum_variants;
            bool has_default = false;
            if (t.kind == Type::ENUM) {
                auto it = enums.find(t.enum_name);
                if (it == enums.end()) { err = "unknown enum in match"; return false; }
                for (auto& kv : it->second) enum_variants.insert(kv.first);
            }
            std::set<std::string> seen;
            for (auto& C : M.cases) {
                if (C.is_default) { has_default = true; }
                else {
                    if (seen.count(C.label)) { err = "duplicate case label: " + C.label; return false; }
                    seen.insert(C.label);
                    if (!enum_variants.empty() && !enum_variants.count(C.label)) { err = "label not in enum " + t.enum_name + ": " + C.label; return false; }
                }
                if (!check_block(C.body, locals, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
            }
            if (!has_default && !enum_variants.empty()) {
                for (auto& v : enum_variants) if (!seen.count(v)) { err = "non-exhaustive match, missing: " + v; return false; }
            }
        } else if (std::holds_alternative<S::Sleep>(st.node)) {
            const auto& SL = std::get<S::Sleep>(st.node);
            Type t; if (!check_expr(SL.amount, locals, enums, err, &t)) return false;
            if (t.kind != Type::DUR) { err = "sleep expects duration value (ms or sec)"; return false; }
            // gate check
            if (std::find(fn_gates.begin(), fn_gates.end(), "time") == fn_gates.end()) {
                err = "missing gate 'time' for sleep"; return false;
            }
            // annotate is_ms via const_cast (since sema owns AST here)
            const_cast<innesce::ast::Stmt::Sleep&>(SL).is_ms = (t.dur == DurUnit::MS);
        } else if (std::holds_alternative<S::Fail>(st.node)) {
            if (!in_quarantine) { err = "fail; outside of quarantine"; return false; }
        } else if (std::holds_alternative<S::Quarantine>(st.node)) {
            const auto& Q = std::get<S::Quarantine>(st.node);
            declared_quarantines[Q.name] = true;
            if (!check_block(Q.body, locals, enums, err, true, declared_quarantines, fn_gates)) return false;
        } else if (std::holds_alternative<S::Asm>(st.node)) {
            // no checks
        }
    }
    return true;
}

SemaResult Sema::check(const ast::Unit& u) {
    // Load enums
    enums_.clear();
    for (auto& e : u.enums) {
        std::unordered_map<std::string,int> m;
        for (int i=0;i<(int)e.variants.size();++i) m[e.variants[i]] = i;
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
