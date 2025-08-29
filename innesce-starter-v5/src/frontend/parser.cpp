#include "frontend/parser.hpp"
#include <iostream>
#include <memory>

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
        if (cur_.kind == TokKind::KwType) {
            auto e = parse_enum_decl();
            if (!e) return std::nullopt;
            u.enums.push_back(std::move(*e));
            continue;
        }
        auto fn = parse_function();
        if (!fn) return std::nullopt;
        u.functions.push_back(std::move(*fn));
    }
    return u;
}

std::optional<ast::EnumDecl> Parser::parse_enum_decl() {
    if (!expect(TokKind::KwType, "'type'")) return std::nullopt;
    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected enum name\n"; return std::nullopt; }
    std::string name = cur_.text; bump();
    if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
    if (!expect(TokKind::KwEnum, "'enum'")) return std::nullopt;
    if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
    ast::EnumDecl d; d.name = std::move(name);
    // variants
    bool first = true;
    while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
        if (!first) expect(TokKind::Comma, "','");
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected variant name\n"; return std::nullopt; }
        d.variants.push_back(cur_.text);
        bump();
        first = false;
    }
    if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
    expect(TokKind::Semicolon, "';'"); // optional; won't error if missing
    return d;
}

std::optional<ast::Function> Parser::parse_function() {
    if (!expect(TokKind::KwFn, "'fn'")) return std::nullopt;
    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected function name\n"; return std::nullopt; }
    std::string name = cur_.text; bump();
    if (!expect(TokKind::LParen, "'('")) return std::nullopt;
    if (!expect(TokKind::RParen, "')'")) return std::nullopt;
    if (!expect(TokKind::Arrow, "'->'")) return std::nullopt;
    ast::Type ret;
    if (accept(TokKind::KwI32)) {
        ret.kind = ast::Type::I32;
    } else if (cur_.kind == TokKind::Ident) {
        ret.kind = ast::Type::ENUM; ret.enum_name = cur_.text; bump();
    } else {
        std::cerr << "Expected return type\n"; return std::nullopt;
    }
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

std::optional<ast::Stmt> Parser::parse_match() {
    // match <expr> is ... end
    if (!expect(TokKind::KwMatch, "'match'")) return std::nullopt;
    auto scrut = parse_expr(); if (!scrut) return std::nullopt;
    if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
    ast::Stmt::Match m; m.scrutinee = std::move(*scrut);
    while (cur_.kind == TokKind::KwCase) {
        bump();
        bool is_default = false;
        std::string label;
        if (accept(TokKind::KwDefault)) {
            is_default = true;
        } else {
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected case label\n"; return std::nullopt; }
            label = cur_.text; bump();
        }
        if (!expect(TokKind::FatArrow, "'=>'")) return std::nullopt;
        auto body = parse_case_body();
        m.cases.push_back(ast::Stmt::Match::Case{ std::move(label), std::move(body), is_default });
    }
    if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
    ast::Stmt st; st.node = std::move(m);
    return st;
}

std::vector<ast::Stmt> Parser::parse_case_body() {
    std::vector<ast::Stmt> body;
    while (cur_.kind != TokKind::KwCase && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
        auto st = parse_stmt();
        if (!st) break;
        body.push_back(std::move(*st));
    }
    return body;
}

std::optional<ast::Stmt> Parser::parse_stmt() {
    if (cur_.kind == TokKind::KwLet) {
        bump();
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected identifier after let\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::Colon, "':'")) return std::nullopt;
        ast::Type ty;
        if (accept(TokKind::KwI32)) {
            ty.kind = ast::Type::I32;
        } else if (cur_.kind == TokKind::Ident) {
            ty.kind = ast::Type::ENUM; ty.enum_name = cur_.text; bump();
        } else {
            std::cerr << "Expected type name after ':'\n"; return std::nullopt;
        }
        if (!expect(TokKind::Assign, "':='")) return std::nullopt;
        auto e = parse_expr(); if (!e) return std::nullopt;
        if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
        ast::Stmt st; st.node = ast::Stmt::Let{ std::move(name), ty, std::move(*e) };
        return st;
    }
    if (cur_.kind == TokKind::KwReturn) {
        bump();
        auto e = parse_expr(); if (!e) return std::nullopt;
        if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
        ast::Stmt st; st.node = ast::Stmt::Return{ std::move(*e) };
        return st;
    }
    if (cur_.kind == TokKind::KwIf) {
        bump();
        auto cond = parse_expr(); if (!cond) return std::nullopt;
        if (!expect(TokKind::KwThen, "'then'")) return std::nullopt;
        // then body until 'else' or 'end'
        std::vector<ast::Stmt> then_body;
        while (cur_.kind != TokKind::KwElse && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt();
            if (!st) return std::nullopt;
            then_body.push_back(std::move(*st));
        }
        std::vector<ast::Stmt> else_body;
        if (accept(TokKind::KwElse)) {
            while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                auto st = parse_stmt();
                if (!st) return std::nullopt;
                else_body.push_back(std::move(*st));
            }
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        ast::Stmt st; st.node = ast::Stmt::If{ std::move(*cond), std::move(then_body), std::move(else_body) };
        return st;
    }
    if (cur_.kind == TokKind::KwMatch) {
        return parse_match();
    }
    if (cur_.kind == TokKind::KwSleep) {
        bump();
        if (!expect(TokKind::LParen, "'('")) return std::nullopt;
        auto amt = parse_expr(); if (!amt) return std::nullopt;
        bool is_ms = true;
        if (accept(TokKind::KwMS)) { is_ms = true; }
        else if (accept(TokKind::KwSEC)) { is_ms = false; }
        else { std::cerr << "Expected unit ms or sec in sleep()\n"; return std::nullopt; }
        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
        if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
        ast::Stmt st; st.node = ast::Stmt::Sleep{ std::move(*amt), is_ms };
        return st;
    }
    if (cur_.kind == TokKind::KwFail) {
        bump();
        if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
        ast::Stmt st; st.node = ast::Stmt::Fail{};
        return st;
    }
    if (cur_.kind == TokKind::KwQuarantine) {
        bump();
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected quarantine name"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        std::vector<ast::Stmt> body;
        while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt(); if (!st) return std::nullopt; body.push_back(std::move(*st));
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        ast::Stmt st; st.node = ast::Stmt::Quarantine{ std::move(name), std::move(body) };
        return st;
    }
    std::cerr << "Unknown statement\n";
    return std::nullopt;
}

std::optional<ast::Expr> Parser::parse_expr() { return parse_add(); }

std::optional<ast::Expr> Parser::parse_add() {
    auto lhs = parse_mul(); if (!lhs) return std::nullopt;
    while (cur_.kind == TokKind::Plus || cur_.kind == TokKind::Minus) {
        char op = (cur_.kind == TokKind::Plus) ? '+' : '-';
        bump();
        auto rhs = parse_mul(); if (!rhs) return std::nullopt;
        ast::Expr e;
        e.node = ast::Expr::Binary{ op, std::make_unique<ast::Expr>(std::move(*lhs)), std::make_unique<ast::Expr>(std::move(*rhs)) };
        lhs = std::move(e);
    }
    return lhs;
}

std::optional<ast::Expr> Parser::parse_mul() {
    auto lhs = parse_unary(); if (!lhs) return std::nullopt;
    while (cur_.kind == TokKind::Star || cur_.kind == TokKind::Slash) {
        char op = (cur_.kind == TokKind::Star) ? '*' : '/';
        bump();
        auto rhs = parse_unary(); if (!rhs) return std::nullopt;
        ast::Expr e;
        e.node = ast::Expr::Binary{ op, std::make_unique<ast::Expr>(std::move(*lhs)), std::make_unique<ast::Expr>(std::move(*rhs)) };
        lhs = std::move(e);
    }
    return lhs;
}

std::optional<ast::Expr> Parser::parse_unary() {
    if (cur_.kind == TokKind::Minus) {
        bump();
        auto rhs = parse_unary(); if (!rhs) return std::nullopt;
        ast::Expr e;
        e.node = ast::Expr::Unary{ '-', std::make_unique<ast::Expr>(std::move(*rhs)) };
        return e;
    }
    return parse_primary();
}

std::optional<ast::Expr> Parser::parse_primary() {
    if (cur_.kind == TokKind::Int) {
        ast::Expr e; e.node = ast::Expr::IntLit{ cur_.int_val }; bump(); return e;
    }
    if (cur_.kind == TokKind::Ident) {
        ast::Expr e; e.node = ast::Expr::Ident{ cur_.text }; bump(); return e;
    }
    if (cur_.kind == TokKind::KwIsFailed) {
        bump();
        if (!expect(TokKind::LParen, "'('")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected name in isfailed()"; return std::nullopt; }
        std::string q = cur_.text; bump();
        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
        ast::Expr e; e.node = ast::Expr::IsFailed{ std::move(q) }; return e;
    }
    if (accept(TokKind::LParen)) {
        auto e = parse_expr(); if (!e) return std::nullopt;
        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
        return e;
    }
    std::cerr << "Expected expression\n";
    return std::nullopt;
}

} // namespace innesce::front
