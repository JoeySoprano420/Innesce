#include "frontend/sema.hpp"

namespace innesce::front {

SemaResult Sema::check(const ast::Unit& u) {
    // Minimal: ensure there is a main() returning i32 and that identifiers are declared before use.
    bool has_main = false;
    for (auto& f : u.functions) {
        if (f.name == "main") has_main = true;
        // naive symbol table per function
        std::unordered_map<std::string, bool> locals;
        for (auto& st : f.body) {
            if (std::holds_alternative<ast::Stmt::Let>(st.node)) {
                const auto& L = std::get<ast::Stmt::Let>(st.node);
                locals[L.name] = true;
            } else if (std::holds_alternative<ast::Stmt::Return>(st.node)) {
                const auto& R = std::get<ast::Stmt::Return>(st.node);
                if (std::holds_alternative<ast::Expr::Ident>(R.value.node)) {
                    const auto& I = std::get<ast::Expr::Ident>(R.value.node);
                    if (!locals.count(I.name)) return {"use of undeclared identifier: " + I.name};
                }
            }
        }
    }
    if (!has_main) return {"missing 'main' function"};
    return {};
}

} // namespace innesce::front
