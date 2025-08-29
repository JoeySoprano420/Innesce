#pragma once
#include "frontend/lexer.hpp"
#include "frontend/ast.hpp"
#include <optional>

namespace innesce::front {

class Parser {
public:
    explicit Parser(std::string_view src);
    std::optional<ast::Unit> parse_unit();

private:
    Lexer lex_;
    Token cur_;

    void bump();
    bool accept(TokKind k);
    bool expect(TokKind k, const char* what);

    std::optional<ast::Function> parse_function();
    std::optional<ast::Stmt> parse_stmt();
    std::optional<ast::Expr> parse_expr();
};

} // namespace innesce::front
