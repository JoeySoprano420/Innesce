
#include "frontend/parser.hpp"
#include <iostream>
#include <memory>

namespace innesce::front {

Parser::Parser(std::string_view src) : lex_(src) { bump(); }
void Parser::bump() { cur_ = lex_.next(); }
bool Parser::accept(TokKind k) { if (cur_.kind==k) { bump(); return true; } return false; }
bool Parser::expect(TokKind k, const char* what) { if (!accept(k)) { std::cerr << "Expected " << what << "\n"; return false; } return true; }

std::optional<ast::Type> Parser::parse_type_name() {
    ast::Type t;
    if (accept(TokKind::KwI32)) { t.kind = ast::Type::I32; return t; }
    if (accept(TokKind::KwStr)) { t.kind = ast::Type::STR; return t; }
    if (accept(TokKind::KwMS))  { t.kind = ast::Type::DUR; t.dur = ast::DurUnit::MS; return t; }
    if (accept(TokKind::KwSEC)) { t.kind = ast::Type::DUR; t.dur = ast::DurUnit::SEC; return t; }
    if (accept(TokKind::LParen)) {
        std::vector<ast::Type> elems;
        if (!accept(TokKind::RParen)) {
            while (true) {
                auto et = parse_type_name(); if (!et) return std::nullopt;
                elems.push_back(*et);
                if (accept(TokKind::RParen)) break;
                if (!expect(TokKind::Comma, "','")) return std::nullopt;
            }
        }
        t.kind = ast::Type::TUPLE; t.tuple_elems = std::move(elems); return t;
    }
    if (cur_.kind == TokKind::Ident) { t.kind = ast::Type::ENUM; t.enum_name = cur_.text; bump(); return t; }
    return std::nullopt;
}

std::optional<ast::Unit> Parser::parse_unit() {
    ast::Unit u;
    while (cur_.kind != TokKind::End) {
        if (cur_.kind == TokKind::KwType) {
            auto e = parse_enum_decl(); if (!e) return std::nullopt; u.enums.push_back(std::move(*e)); continue;
        }
        auto fn = parse_function(); if (!fn) return std::nullopt; u.functions.push_back(std::move(*fn));
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
    bool first = true;
    while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
        if (!first) expect(TokKind::Comma, "','");
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected variant name\n"; return std::nullopt; }
        d.variants.push_back(cur_.text); bump();
        first = false;
    }
    if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
    accept(TokKind::Semicolon);
    return d;
}

std::optional<ast::Function> Parser::parse_function() {
    if (!expect(TokKind::KwFn, "'fn'")) return std::nullopt;
    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected function name\n"; return std::nullopt; }
    std::string name = cur_.text; bump();
    if (!expect(TokKind::LParen, "'('")) return std::nullopt;
    if (!expect(TokKind::RParen, "')'")) return std::nullopt;
    if (!expect(TokKind::Arrow, "'->'")) return std::nullopt;
    auto rty = parse_type_name(); if (!rty) { std::cerr << "Expected return type\n"; return std::nullopt; }

    std::vector<std::string> gates; bool hot=false;
    if (accept(TokKind::KwWith)) {
        if (!expect(TokKind::LBracket, "'['")) return std::nullopt;
        bool first = true;
        while (cur_.kind != TokKind::RBracket && cur_.kind != TokKind::End) {
            if (!first) expect(TokKind::Comma, "','");
            if (accept(TokKind::KwHot)) { hot = true; }
            else if (cur_.kind == TokKind::Ident) { gates.push_back(cur_.text); bump(); }
            else { std::cerr << "Expected gate name or Hot\n"; return std::nullopt; }
            first = false;
        }
        if (!expect(TokKind::RBracket, "']'")) return std::nullopt;
    }

    if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;

    ast::Function f; f.name = std::move(name); f.ret = *rty; f.gates = std::move(gates); f.hot = hot;
    while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
        auto st = parse_stmt(); if (!st) return std::nullopt; f.body.push_back(std::move(*st));
    }
    if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
    return f;
}

std::optional<ast::Expr::Pattern> Parser::parse_tuple_pat_elem() {
    ast::Expr::Pattern p;
    if (cur_.kind == TokKind::Ident && cur_.text == "_") { p.node = ast::Expr::Pattern::Wild{}; bump(); return p; }
    if (cur_.kind == TokKind::Int) {
        int v = cur_.int_val; bump();
        if (accept(TokKind::KwMS)) { p.node = ast::Expr::Pattern::Dur{ v, ast::DurUnit::MS }; return p; }
        if (accept(TokKind::KwSEC)) { p.node = ast::Expr::Pattern::Dur{ v, ast::DurUnit::SEC }; return p; }
        p.node = ast::Expr::Pattern::Int{ v }; return p;
    }
    if (cur_.kind == TokKind::Ident) {
        std::string name = cur_.text; bump();
        p.node = ast::Expr::Pattern::Bind{ std::move(name) }; return p;
    }
    return std::nullopt;
}

std::optional<ast::Stmt> Parser::parse_match_stmt() {
    if (!expect(TokKind::KwMatch, "'match'")) return std::nullopt;
    auto scrut = parse_expr(); if (!scrut) return std::nullopt;
    if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
    ast::Stmt::Match m; m.scrutinee = std::move(*scrut);
    while (cur_.kind == TokKind::KwCase) {
        bump();
        ast::Stmt::Match::Case c;
        if (accept(TokKind::KwDefault)) {
            c.is_default = true;
        } else if (accept(TokKind::LParen)) {
            c.is_tuple = true;
            if (!accept(TokKind::RParen)) {
                while (true) {
                    auto pe = parse_tuple_pat_elem(); if (!pe) return std::nullopt;
                    c.tpat.push_back(std::move(*pe));
                    if (accept(TokKind::RParen)) break;
                    if (!expect(TokKind::Comma, "','")) return std::nullopt;
                }
            }
        } else {
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected case label or tuple pattern\n"; return std::nullopt; }
            c.label = cur_.text; bump();
        }
        if (accept(TokKind::KwWhen)) {
            auto g = parse_expr(); if (!g) return std::nullopt; c.guard = std::move(*g);
        }
        if (!expect(TokKind::FatArrow, "'=>'")) return std::nullopt;
        c.body = parse_case_body();
        m.cases.push_back(std::move(c));
    }
    if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
    ast::Stmt st; st.node = std::move(m); return st;
}

std::vector<ast::Stmt> Parser::parse_case_body() {
    std::vector<ast::Stmt> body;
    while (cur_.kind != TokKind::KwCase && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
        auto st = parse_stmt(); if (!st) break; body.push_back(std::move(*st));
    }
    return body;
}

std::optional<ast::Expr> Parser::parse_case_value() {
    // A single expression until end-of-line case separator (we rely on 'case'/'end'/'default'/'case' boundaries)
    return parse_expr();
}

std::optional<ast::Stmt> Parser::parse_stmt() {
    if (cur_.kind == TokKind::KwLet) {
        bump();
        if (accept(TokKind::LParen)) {
            std::vector<std::string> names;
            if (!accept(TokKind::RParen)) {
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected name in tuple pattern\n"; return std::nullopt; }
                    names.push_back(cur_.text); bump();
                    if (accept(TokKind::RParen)) break;
                    if (!expect(TokKind::Comma, "','")) return std::nullopt;
                }
            }
            if (!expect(TokKind::Colon, "':'")) return std::nullopt;
            if (!accept(TokKind::LParen)) { std::cerr << "Expected '(' for tuple types\n"; return std::nullopt; }
            std::vector<ast::Type> tys;
            if (!accept(TokKind::RParen)) {
                while (true) {
                    auto t = parse_type_name(); if (!t) return std::nullopt;
                    tys.push_back(*t);
                    if (accept(TokKind::RParen)) break;
                    if (!expect(TokKind::Comma, "','")) return std::nullopt;
                }
            }
            if (!expect(TokKind::Assign, "':='")) return std::nullopt;
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::LetTuple{ std::move(names), std::move(tys), std::move(*e) };
            return st;
        } else {
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected identifier after let\n"; return std::nullopt; }
            std::string name = cur_.text; bump();
            if (!expect(TokKind::Colon, "':'")) return std::nullopt;
            auto ty = parse_type_name(); if (!ty) { std::cerr << "Expected type name\n"; return std::nullopt; }
            if (!expect(TokKind::Assign, "':='")) return std::nullopt;
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Let{ std::move(name), *ty, std::move(*e) };
            return st;
        }
    }
    if (cur_.kind == TokKind::KwReturn) {
        bump(); auto e = parse_expr(); if (!e) return std::nullopt;
        if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
        ast::Stmt st; st.node = ast::Stmt::Return{ std::move(*e) }; return st;
    }
    if (cur_.kind == TokKind::KwIf) {
        bump(); auto cond = parse_expr(); if (!cond) return std::nullopt;
        if (!expect(TokKind::KwThen, "'then'")) return std::nullopt;
        std::vector<ast::Stmt> then_body;
        while (cur_.kind != TokKind::KwElse && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) { auto st = parse_stmt(); if (!st) return std::nullopt; then_body.push_back(std::move(*st)); }
        std::vector<ast::Stmt> else_body;
        if (accept(TokKind::KwElse)) { while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) { auto st = parse_stmt(); if (!st) return std::nullopt; else_body.push_back(std::move(*st)); } }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        ast::Stmt st; st.node = ast::Stmt::If{ std::move(*cond), std::move(then_body), std::move(else_body) }; return st;
    }
    if (cur_.kind == TokKind::KwMatch) return parse_match_stmt();
    if (cur_.kind == TokKind::KwSleep) { bump(); if (!expect(TokKind::LParen, "'('")) return std::nullopt; auto amt = parse_expr(); if (!amt) return std::nullopt; if (!expect(TokKind::RParen, "')'")) return std::nullopt; if (!expect(TokKind::Semicolon, "';'")) return std::nullopt; ast::Stmt st; st.node = ast::Stmt::Sleep{ std::move(*amt), true }; return st; }
    if (cur_.kind == TokKind::KwFail) { bump(); if (!expect(TokKind::Semicolon, "';'")) return std::nullopt; ast::Stmt st; st.node = ast::Stmt::Fail{}; return st; }
    if (cur_.kind == TokKind::KwQuarantine) {
        bump();
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected quarantine name"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        std::vector<ast::Stmt> body;
        while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) { auto st = parse_stmt(); if (!st) return std::nullopt; body.push_back(std::move(*st)); }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        ast::Stmt st; st.node = ast::Stmt::Quarantine{ std::move(name), std::move(body) }; return st;
    }
    if (cur_.kind == TokKind::KwASM) return parse_asm_block();
    std::cerr << "Unknown statement\n"; return std::nullopt;
}

std::optional<ast::Expr> Parser::parse_expr() {
    auto e = parse_add(); if (!e) return std::nullopt;
    while (accept(TokKind::KwAs)) {
        auto ty = parse_type_name(); if (!ty) { std::cerr << "Expected type after 'as'\n"; return std::nullopt; }
        ast::Expr ce; ce.node = ast::Expr::Cast{ std::make_unique<ast::Expr>(std::move(*e)), *ty }; e = std::move(ce);
    }
    return e;
}

std::optional<ast::Expr> Parser::parse_add() {
    auto lhs = parse_mul(); if (!lhs) return std::nullopt;
    while (cur_.kind == TokKind::Plus || cur_.kind == TokKind::Minus) {
        char op = (cur_.kind == TokKind::Plus) ? '+' : '-'; bump();
        auto rhs = parse_mul(); if (!rhs) return std::nullopt;
        ast::Expr e; e.node = ast::Expr::Binary{ op, std::make_unique<ast::Expr>(std::move(*lhs)), std::make_unique<ast::Expr>(std::move(*rhs)) }; lhs = std::move(e);
    }
    return lhs;
}

std::optional<ast::Expr> Parser::parse_mul() {
    auto lhs = parse_unary(); if (!lhs) return std::nullopt;
    while (cur_.kind == TokKind::Star || cur_.kind == TokKind::Slash) {
        char op = (cur_.kind == TokKind::Star) ? '*' : '/'; bump();
        auto rhs = parse_unary(); if (!rhs) return std::nullopt;
        ast::Expr e; e.node = ast::Expr::Binary{ op, std::make_unique<ast::Expr>(std::move(*lhs)), std::make_unique<ast::Expr>(std::move(*rhs)) }; lhs = std::move(e);
    }
    return lhs;
}

std::optional<ast::Expr> Parser::parse_unary() {
    if (cur_.kind == TokKind::Minus) { bump(); auto rhs = parse_unary(); if (!rhs) return std::nullopt; ast::Expr e; e.node = ast::Expr::Unary{ '-', std::make_unique<ast::Expr>(std::move(*rhs)) }; return e; }
    return parse_postfix();
}

std::optional<ast::Expr> Parser::parse_postfix() {
    auto p = parse_primary(); if (!p) return std::nullopt;
    while (accept(TokKind::LParen)) {
        std::vector<ast::Expr> args;
        if (!accept(TokKind::RParen)) {
            while (true) { auto a = parse_expr(); if (!a) return std::nullopt; args.push_back(std::move(*a)); if (accept(TokKind::RParen)) break; if (!expect(TokKind::Comma, "','")) return std::nullopt; }
        }
        if (!std::holds_alternative<ast::Expr::Ident>(p->node)) { std::cerr << "call target must be identifier\n"; return std::nullopt; }
        std::string name = std::get<ast::Expr::Ident>(p->node).name;
        ast::Expr call; call.node = ast::Expr::Call{ std::move(name), std::move(args) }; p = std::move(call);
    }
    return p;
}

std::optional<ast::Expr> Parser::parse_primary() {
    if (cur_.kind == TokKind::Int) { int v = cur_.int_val; bump(); if (accept(TokKind::KwMS)){ ast::Expr e; e.node = ast::Expr::DurLit{ v, ast::DurUnit::MS }; return e; } if (accept(TokKind::KwSEC)){ ast::Expr e; e.node = ast::Expr::DurLit{ v, ast::DurUnit::SEC }; return e; } ast::Expr e; e.node = ast::Expr::IntLit{ v }; return e; }
    if (cur_.kind == TokKind::String) { ast::Expr e; e.node = ast::Expr::StringLit{ cur_.text }; bump(); return e; }
    if (cur_.kind == TokKind::Ident) { ast::Expr e; e.node = ast::Expr::Ident{ cur_.text }; bump(); return e; }
    if (cur_.kind == TokKind::KwASM) { return parse_asm_expr(); }
    if (cur_.kind == TokKind::KwIsFailed) { bump(); if (!expect(TokKind::LParen, "'('")) return std::nullopt; if (cur_.kind!=TokKind::Ident) return std::nullopt; std::string q=cur_.text; bump(); if (!expect(TokKind::RParen, "')'")) return std::nullopt; ast::Expr e; e.node = ast::Expr::IsFailed{ std::move(q) }; return e; }
    if (cur_.kind == TokKind::KwMatch) { return parse_match_expr(); }
    if (accept(TokKind::LParen)) { auto e = parse_expr(); if (!e) return std::nullopt; if (!expect(TokKind::RParen, "')'")) return std::nullopt; return e; }
    std::cerr << "Expected expression\n"; return std::nullopt;
}

std::optional<ast::Expr> Parser::parse_match_expr() {
    if (!expect(TokKind::KwMatch, "'match'")) return std::nullopt;
    auto scrut = parse_expr(); if (!scrut) return std::nullopt;
    if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
    ast::Expr M; ast::Expr::MatchExpr mx; mx.scrutinee = std::move(*scrut);
    while (cur_.kind == TokKind::KwCase) {
        bump();
        ast::Expr::MatchExpr::Case c;
        if (accept(TokKind::KwDefault)) {
            c.is_default = true;
        } else if (accept(TokKind::LParen)) {
            c.is_tuple = true;
            if (!accept(TokKind::RParen)) {
                while (true) {
                    auto pe = parse_tuple_pat_elem(); if (!pe) return std::nullopt;
                    c.tpat.push_back(std::move(*pe));
                    if (accept(TokKind::RParen)) break;
                    if (!expect(TokKind::Comma, "','")) return std::nullopt;
                }
            }
        } else {
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected case label or tuple pattern\n"; return std::nullopt; }
            c.label = cur_.text; bump();
        }
        if (accept(TokKind::KwWhen)) {
            auto g = parse_expr(); if (!g) return std::nullopt; c.guard = std::move(*g);
        }
        if (!expect(TokKind::FatArrow, "'=>'")) return std::nullopt;
        auto val = parse_case_value(); if (!val) return std::nullopt;
        c.value = std::make_unique<ast::Expr>(std::move(*val));
        mx.cases.push_back(std::move(c));
    }
    if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
    M.node = std::move(mx);
    return M;
}

std::optional<ast::Stmt> Parser::parse_asm_block() {
    if (!expect(TokKind::KwASM, "'asm'")) return std::nullopt;
    if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
    ast::Stmt::Asm A;
    while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
        if (cur_.kind == TokKind::Ident && cur_.text == "intel") { bump(); A.intel = true; continue; }
        if (cur_.kind == TokKind::Ident && cur_.text == "att")   { bump(); A.intel = false; continue; }
        if (cur_.kind == TokKind::Ident && cur_.text == "outs") {
            bump();
            while (true) {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected constraint ident after outs\n"; return std::nullopt; }
                std::string cons = cur_.text; bump();
                if (!expect(TokKind::LParen, "'('")) return std::nullopt;
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected variable name in outs()\n"; return std::nullopt; }
                std::string name = cur_.text; bump();
                if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                A.outs.push_back({true, cons, name, false, 0});
                if (accept(TokKind::Comma)) continue;
                if (accept(TokKind::Semicolon)) break;
                std::cerr << "Expected ',' or ';' after outs entry\n"; return std::nullopt;
            }
            continue;
        }
        if (cur_.kind == TokKind::Ident && cur_.text == "ins") {
            bump();
            while (true) {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected constraint ident after ins\n"; return std::nullopt; }
                std::string cons = cur_.text; bump();
                if (!expect(TokKind::LParen, "'('")) return std::nullopt;
                if (cur_.kind == TokKind::Ident) {
                    std::string name = cur_.text; bump();
                    if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                    A.ins.push_back({false, cons, name, false, 0});
                } else if (cur_.kind == TokKind::Int) {
                    int v = cur_.int_val; bump();
                    if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                    A.ins.push_back({false, cons, "", true, v});
                } else { std::cerr << "Expected ident or int in ins()\n"; return std::nullopt; }
                if (accept(TokKind::Comma)) continue;
                if (accept(TokKind::Semicolon)) break;
                std::cerr << "Expected ',' or ';' after ins entry\n"; return std::nullopt;
            }
            continue;
        }
        if (cur_.kind == TokKind::Ident && cur_.text == "clobbers") {
            bump();
            while (true) {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected clobber ident\n"; return std::nullopt; }
                A.clobbers.push_back(cur_.text); bump();
                if (accept(TokKind::Comma)) continue;
                if (accept(TokKind::Semicolon)) break;
                std::cerr << "Expected ',' or ';' after clobbers entry\n"; return std::nullopt;
            }
            continue;
        }
        if (cur_.kind == TokKind::Ident && cur_.text == "body") {
            bump();
            if (!expect(TokKind::Colon, "':'")) return std::nullopt;
            while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
                if (cur_.kind == TokKind::Semicolon) { A.body.push_back('\n'); bump(); continue; }
                if (cur_.kind == TokKind::Ident && (cur_.text=="outs" || cur_.text=="ins" || cur_.text=="clobbers" || cur_.text=="intel" || cur_.text=="att")) break;
                if (!cur_.text.empty()) { A.body += cur_.text; A.body.push_back(' '); }
                bump();
            }
            continue;
        }
        if (!cur_.text.empty()) { A.body += cur_.text; A.body.push_back(' '); }
        bump();
    }
    if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
    if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
    ast::Stmt st; st.node = std::move(A); return st;
}

std::optional<ast::Expr> Parser::parse_asm_expr() {
    if (!expect(TokKind::KwASM, "'asm'")) return std::nullopt;
    if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
    ast::Expr A; ast::Expr::AsmExpr AX;
    while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
        if (cur_.kind == TokKind::Ident && cur_.text == "intel") { bump(); AX.intel = true; continue; }
        if (cur_.kind == TokKind::Ident && cur_.text == "att")   { bump(); AX.intel = false; continue; }
        if (cur_.kind == TokKind::Ident && cur_.text == "outs") {
            bump();
            while (true) {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected constraint ident after outs\n"; return std::nullopt; }
                std::string cons = cur_.text; bump();
                if (!expect(TokKind::LParen, "'('")) return std::nullopt;
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected name in outs() (use '_' for placeholder)\n"; return std::nullopt; }
                std::string name = cur_.text; bump();
                if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                AX.outs.push_back({true, cons, false, 0, name});
                if (accept(TokKind::Comma)) continue;
                if (accept(TokKind::Semicolon)) break;
                std::cerr << "Expected ',' or ';' after outs entry\n"; return std::nullopt;
            }
            continue;
        }
        if (cur_.kind == TokKind::Ident && cur_.text == "ins") {
            bump();
            while (true) {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected constraint ident after ins\n"; return std::nullopt; }
                std::string cons = cur_.text; bump();
                if (!expect(TokKind::LParen, "'('")) return std::nullopt;
                if (cur_.kind == TokKind::Ident) {
                    std::string name = cur_.text; bump();
                    if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                    AX.ins.push_back({false, cons, false, 0, name});
                } else if (cur_.kind == TokKind::Int) {
                    int v = cur_.int_val; bump();
                    if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                    AX.ins.push_back({false, cons, true, v, ""});
                } else { std::cerr << "Expected ident or int in ins()\n"; return std::nullopt; }
                if (accept(TokKind::Comma)) continue;
                if (accept(TokKind::Semicolon)) break;
                std::cerr << "Expected ',' or ';' after ins entry\n"; return std::nullopt;
            }
            continue;
        }
        if (cur_.kind == TokKind::Ident && cur_.text == "clobbers") {
            bump();
            while (true) {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected clobber ident\n"; return std::nullopt; }
                AX.clobbers.push_back(cur_.text); bump();
                if (accept(TokKind::Comma)) continue;
                if (accept(TokKind::Semicolon)) break;
                std::cerr << "Expected ',' or ';' after clobbers entry\n"; return std::nullopt;
            }
            continue;
        }
        if (cur_.kind == TokKind::Ident && cur_.text == "body") {
            bump();
            if (!expect(TokKind::Colon, "':'")) return std::nullopt;
            while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
                if (cur_.kind == TokKind::Semicolon) { AX.body.push_back('\n'); bump(); continue; }
                if (cur_.kind == TokKind::Ident && (cur_.text=="outs" || cur_.text=="ins" || cur_.text=="clobbers" || cur_.text=="intel" || cur_.text=="att")) break;
                if (!cur_.text.empty()) { AX.body += cur_.text; AX.body.push_back(' '); }
                bump();
            }
            continue;
        }
        if (!cur_.text.empty()) { AX.body += cur_.text; AX.body.push_back(' '); }
        bump();
    }
    if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
    A.node = std::move(AX);
    return A;
}

} // namespace innesce::front
