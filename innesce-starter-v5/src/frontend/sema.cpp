#include "frontend/sema.hpp"
#include <unordered_map>
#include <variant>
#include <set>

namespace innesce::front {

static bool check_expr(const innesce::ast::Expr& e,
                       const std::unordered_map<std::string, innesce::ast::Type>& locals,
                       const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums,
                       std::string& err,
                       innesce::ast::Type* outTy = nullptr)
{
    using E = innesce::ast::Expr;
    if (std::holds_alternative<E::IntLit>(e.node)) {
        if (outTy) { outTy->kind = innesce::ast::Type::I32; }
        return true;
    }
    if (std::holds_alternative<E::Ident>(e.node)) {
        const auto& I = std::get<E::Ident>(e.node);
        auto it = locals.find(I.name);
        if (it != locals.end()) {
            if (outTy) *outTy = it->second;
            return true;
        }
        // Maybe it's an enum variant: find which enum defines it
        for (auto& [ename, vars] : enums) {
            auto vit = vars.find(I.name);
            if (vit != vars.end()) {
                if (outTy) { outTy->kind = innesce::ast::Type::ENUM; outTy->enum_name = ename; }
                return true;
            }
        }
        err = "use of undeclared identifier: " + I.name; return false;
    }
    if (std::holds_alternative<E::Unary>(e.node)) {
        const auto& U = std::get<E::Unary>(e.node);
        innesce::ast::Type t;
        if (!check_expr(*U.rhs, locals, enums, err, &t)) return false;
        if (t.kind != innesce::ast::Type::I32) { err = "unary operator on non-i32"; return false; }
        if (outTy) { outTy->kind = innesce::ast::Type::I32; }
        return true;
    }
    const auto& B = std::get<E::Binary>(e.node);
    innesce::ast::Type lt, rt;
    if (!check_expr(*B.lhs, locals, enums, err, &lt)) return false;
    if (!check_expr(*B.rhs, locals, enums, err, &rt)) return false;
    if (lt.kind != innesce::ast::Type::I32 || rt.kind != innesce::ast::Type::I32) {
        err = "binary arithmetic requires i32"; return false;
    }
    if (outTy) { outTy->kind = innesce::ast::Type::I32; }
    return true;
}

static bool check_block(const std::vector<innesce::ast::Stmt>& body,
                        std::unordered_map<std::string, innesce::ast::Type> locals,
                        const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums,
                        std::string& err) {
    using S = innesce::ast::Stmt;
    for (auto& st : body) {
        if (std::holds_alternative<S::Let>(st.node)) {
            const auto& L = std::get<S::Let>(st.node);
            innesce::ast::Type t;
            if (!check_expr(L.init, locals, enums, err, &t)) return false;
            // allow assigning enum variant to enum var or int to i32
            if (L.type.kind == innesce::ast::Type::I32 && t.kind != innesce::ast::Type::I32) {
                err = "type mismatch: expected i32"; return false;
            }
            if (L.type.kind == innesce::ast::Type::ENUM) {
                if (t.kind != innesce::ast::Type::ENUM || t.enum_name != L.type.enum_name) {
                    err = "type mismatch: expected enum " + L.type.enum_name; return false;
                }
            }
            locals[L.name] = L.type;
        } else if (std::holds_alternative<S::Return>(st.node)) {
            const auto& R = std::get<S::Return>(st.node);
            innesce::ast::Type t;
            if (!check_expr(R.value, locals, enums, err, &t)) return false;
            // function return type is validated elsewhere or trusted i32 for now
        } else if (std::holds_alternative<S::If>(st.node)) {
            const auto& I = std::get<S::If>(st.node);
            innesce::ast::Type t;
            if (!check_expr(I.cond, locals, enums, err, &t)) return false;
            if (!check_block(I.then_body, locals, enums, err)) return false;
            if (!check_block(I.else_body, locals, enums, err)) return false;
        } else if (std::holds_alternative<S::Match>(st.node)) {
            const auto& M = std::get<S::Match>(st.node);
            innesce::ast::Type t;
            if (!check_expr(M.scrutinee, locals, enums, err, &t)) return false;
            // Determine enum type and variant set
            std::set<std::string> enum_variants;
            bool has_default = false;
            if (t.kind == innesce::ast::Type::ENUM) {
                auto it = enums.find(t.enum_name);
                if (it == enums.end()) { err = "unknown enum type in match"; return false; }
                for (auto& kv : it->second) enum_variants.insert(kv.first);
            }
            std::set<std::string> seen;
            for (auto& C : M.cases) {
                if (C.is_default) { has_default = true; }
                else {
                    if (seen.count(C.label)) { err = "duplicate case label: " + C.label; return false; }
                    seen.insert(C.label);
                    if (!enum_variants.empty() && !enum_variants.count(C.label)) {
                        err = "label not in enum " + t.enum_name + ": " + C.label; return false;
                    }
                }
                if (!check_block(C.body, locals, enums, err)) return false;
            }
            if (!has_default && !enum_variants.empty()) {
                for (auto& v : enum_variants) if (!seen.count(v)) { err = "non-exhaustive match, missing: " + v; return false; }
            }
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
        std::unordered_map<std::string, innesce::ast::Type> locals;
        std::string err;
        if (!check_block(f.body, locals, enums_, err)) return {err};
    }
    if (!has_main) return {"missing 'main' function"};
    return {};
}

} // namespace innesce::front
