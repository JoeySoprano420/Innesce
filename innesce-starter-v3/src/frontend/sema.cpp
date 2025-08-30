#include "frontend/sema.hpp"
#include <unordered_map>
#include <variant>

namespace innesce::front {

static bool check_expr(const innesce::ast::Expr& e, const std::unordered_map<std::string,bool>& locals, std::string& err) {
    using E = innesce::ast::Expr;
    if (std::holds_alternative<E::IntLit>(e.node)) return true;
    if (std::holds_alternative<E::Ident>(e.node)) {
        const auto& I = std::get<E::Ident>(e.node);
        if (!locals.count(I.name)) { err = "use of undeclared identifier: " + I.name; return false; }
        return true;
    }
    if (std::holds_alternative<E::Unary>(e.node)) {
        const auto& U = std::get<E::Unary>(e.node);
        return check_expr(*U.rhs, locals, err);
    }
    const auto& B = std::get<E::Binary>(e.node);
    return check_expr(*B.lhs, locals, err) && check_expr(*B.rhs, locals, err);
}

static bool check_block(const std::vector<innesce::ast::Stmt>& body, std::unordered_map<std::string,bool> locals, std::string& err) {
    using S = innesce::ast::Stmt;
    for (auto& st : body) {
        if (std::holds_alternative<S::Let>(st.node)) {
            const auto& L = std::get<S::Let>(st.node);
            if (!check_expr(L.init, locals, err)) return false;
            locals[L.name] = true;
        } else if (std::holds_alternative<S::Return>(st.node)) {
            const auto& R = std::get<S::Return>(st.node);
            if (!check_expr(R.value, locals, err)) return false;
        } else if (std::holds_alternative<S::If>(st.node)) {
            const auto& I = std::get<S::If>(st.node);
            if (!check_expr(I.cond, locals, err)) return false;
            auto locals_then = locals;
            if (!check_block(I.then_body, locals_then, err)) return false;
            auto locals_else = locals;
            if (!check_block(I.else_body, locals_else, err)) return false;
        }
    }
    return true;
}

SemaResult Sema::check(const ast::Unit& u) {
    bool has_main = false;
    for (auto& f : u.functions) {
        if (f.name == "main") has_main = true;
        std::unordered_map<std::string, bool> locals;
        std::string err;
        if (!check_block(f.body, locals, err)) return {err};
    }
    if (!has_main) return {"missing 'main' function"};
    return {};
}

} // namespace innesce::front
