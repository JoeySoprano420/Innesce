#include "backend/llvm/ir_builder.hpp"
#include "frontend/ast.hpp"
#include <iostream>

using namespace innesce::backend::llvm;
using namespace innesce;

void IrBuilder::compile(const ast::Program& prog) {
    std::cout << "; Innesce IR dump (stub)\n";
    for (auto& st : prog.stmts) {
        if (std::holds_alternative<ast::LetStmt>(st->node)) {
            auto& ls = std::get<ast::LetStmt>(st->node);
            if (std::holds_alternative<ast::IntLit>(ls.init->node)) {
                auto v = std::get<ast::IntLit>(ls.init->node).value;
                std::cout << "%"<<ls.name<<" = const i64 " << v << "\n";
            }
        } else if (std::holds_alternative<ast::ReturnStmt>(st->node)) {
            auto& rs = std::get<ast::ReturnStmt>(st->node);
            if (std::holds_alternative<ast::IntLit>(rs->node)) {
                auto v = std::get<ast::IntLit>(rs->node).value;
                std::cout << "ret i64 " << v << "\n";
            }
        }
    }
}
