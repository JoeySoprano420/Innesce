#include "frontend/parser.hpp"
#include <iostream>

namespace innesce::front {

Parser::Parser(std::string_view src) : lex_(src) { bump(); }

void Parser::bump() { cur_ = lex_.next(); }
bool Parser::accept(TokKind k) { if (cur_.kind==k) { bump(); return true; } return false; }
bool Parser::expect(TokKind k, const char* what) {
    if (!accept(k)) { std::cerr << "Expected " << what << "\n"; return false; }
    return true;
}

std::optional<ast::Unit> Parser::parse_unit() {
    ast::Unit u;
    while (cur_.kind != TokKind::End) {
        auto fn = parse_function();
        if (!fn) return std::nullopt;
        u.functions.push_back(std::move(*fn));
    }
    return u;
}

std::optional<ast::Function> Parser::parse_function() {
    if (!expect(TokKind::KwFn, "'fn'")) return std::nullopt;
    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected function name\n"; return std::nullopt; }
    std::string name = cur_.text; bump();
    if (!expect(TokKind::LParen, "'('")) return std::nullopt;
    if (!expect(TokKind::RParen, "')'")) return std::nullopt;
    if (!expect(TokKind::Arrow, "'->'")) return std::nullopt;
    ast::Type ret;
    if (!expect(TokKind::KwI32, "'i32'")) return std::nullopt;
    if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;

    ast::Function f; f.name = std::move(name); f.ret = ret;
    while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
        auto st = parse_stmt();
        if (!st) return std::nullopt;
        f.body.push_back(std::move(*st));
    }
    if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
    return f;
}

std::optional<ast::Stmt> Parser::parse_stmt() {
    if (cur_.kind == TokKind::KwLet) {
        bump();
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected identifier after let\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::Colon, "':'")) return std::nullopt;
        if (!expect(TokKind::KwI32, "'i32'")) return std::nullopt;
        if (!expect(TokKind::Assign, "':='")) return std::nullopt;
        auto e = parse_expr(); if (!e) return std::nullopt;
        if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
        ast::Stmt st; st.node = ast::Stmt::Let{ std::move(name), ast::Type{}, std::move(*e) };
        return st;
    }
    if (cur_.kind == TokKind::KwReturn) {
        bump();
        auto e = parse_expr(); if (!e) return std::nullopt;
        if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
        ast::Stmt st; st.node = ast::Stmt::Return{ std::move(*e) };
        return st;
    }
    std::cerr << "Unknown statement\n";
    return std::nullopt;
}

std::optional<ast::Expr> Parser::parse_expr() {
    if (cur_.kind == TokKind::Int) {
        ast::Expr e; e.node = ast::Expr::IntLit{ cur_.int_val }; bump(); return e;
    }
    if (cur_.kind == TokKind::Ident) {
        ast::Expr e; e.node = ast::Expr::Ident{ cur_.text }; bump(); return e;
    }
    std::cerr << "Expected expression\n";
    return std::nullopt;
}

} // namespace innesce::front
