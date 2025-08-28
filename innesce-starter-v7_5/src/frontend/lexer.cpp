
#include "frontend/lexer.hpp"
#include <cctype>

namespace innesce::front {

Lexer::Lexer(std::string_view src) : s_(src) {}

char Lexer::peek() const { return i_ < s_.size() ? s_[i_] : '\0'; }
char Lexer::get() {
    if (i_ >= s_.size()) return '\0';
    char c = s_[i_++];
    if (c == '\n') { line_++; col_ = 1; } else { col_++; }
    return c;
}

void Lexer::skip_ws_and_comments() {
    while (true) {
        char c = peek();
        if (c == '\0') break;
        if (std::isspace(static_cast<unsigned char>(c))) { get(); continue; }
        if (c == '-' && i_+1 < s_.size() && s_[i_+1] == '-') {
            while (c != '\n' && c != '\0') c = get();
            continue;
        }
        break;
    }
}

Token Lexer::make(TokKind k, std::string t) {
    Token x{ k, std::move(t), 0, line_, col_ };
    return x;
}

Token Lexer::next() {
    skip_ws_and_comments();
    char c = peek();
    if (c == '\0') return make(TokKind::End, "");

    // punctuation/operators
    if (c == '(') { get(); return make(TokKind::LParen, "("); }
    if (c == ')') { get(); return make(TokKind::RParen, ")"); }
    if (c == '{') { get(); return make(TokKind::LBrace, "{"); }
    if (c == '}') { get(); return make(TokKind::RBrace, "}"); }
    if (c == '[') { get(); return make(TokKind::LBracket, "["); }
    if (c == ']') { get(); return make(TokKind::RBracket, "]"); }
    if (c == '+') { get(); return make(TokKind::Plus, "+"); }
    if (c == '-') {
        if (i_+1 < s_.size() && s_[i_+1] == '>') { get(); get(); return make(TokKind::Arrow, "->"); }
        get(); return make(TokKind::Minus, "-");
    }
    if (c == '*') { get(); return make(TokKind::Star, "*"); }
    if (c == '/') { get(); return make(TokKind::Slash, "/"); }
    if (c == ':') {
        get();
        if (peek() == '=') { get(); return make(TokKind::Assign, ":="); }
        return make(TokKind::Colon, ":");
    }
    if (c == ';') { get(); return make(TokKind::Semicolon, ";"); }
    if (c == ',') { get(); return make(TokKind::Comma, ","); }
    if (c == '=') {
        if (i_+1 < s_.size() && s_[i_+1] == '>') { get(); get(); return make(TokKind::FatArrow, "=>"); }
    }

    // number
    if (std::isdigit(static_cast<unsigned char>(c))) {
        int v = 0;
        while (std::isdigit(static_cast<unsigned char>(peek()))) {
            v = v*10 + (get() - '0');
        }
        Token t = make(TokKind::Int, "");
        t.int_val = v;
        return t;
    }

    // identifier/keyword
    if (std::isalpha(static_cast<unsigned char>(c)) || c=='_') {
        std::string id;
        while (std::isalnum(static_cast<unsigned char>(peek())) || peek()=='_') {
            id.push_back(get());
        }
        if (id == "fn") return make(TokKind::KwFn, id);
        if (id == "is") return make(TokKind::KwIs, id);
        if (id == "end") return make(TokKind::KwEnd, id);
        if (id == "return") return make(TokKind::KwReturn, id);
        if (id == "let") return make(TokKind::KwLet, id);
        if (id == "i32") return make(TokKind::KwI32, id);
        if (id == "if") return make(TokKind::KwIf, id);
        if (id == "then") return make(TokKind::KwThen, id);
        if (id == "else") return make(TokKind::KwElse, id);
        if (id == "type") return make(TokKind::KwType, id);
        if (id == "enum") return make(TokKind::KwEnum, id);
        if (id == "match") return make(TokKind::KwMatch, id);
        if (id == "case") return make(TokKind::KwCase, id);
        if (id == "default") return make(TokKind::KwDefault, id);
        if (id == "sleep") return make(TokKind::KwSleep, id);
        if (id == "quarantine") return make(TokKind::KwQuarantine, id);
        if (id == "fail") return make(TokKind::KwFail, id);
        if (id == "isfailed") return make(TokKind::KwIsFailed, id);
        if (id == "ms") return make(TokKind::KwMS, id);
        if (id == "sec") return make(TokKind::KwSEC, id);
        if (id == "asm") return make(TokKind::KwASM, id);
        if (id == "with") return make(TokKind::KwWith, id);
        if (id == "Hot") return make(TokKind::KwHot, id);
        if (id == "as") return make(TokKind::KwAs, id);
        Token t = make(TokKind::Ident, id);
        t.text = id;
        return t;
    }

    // unknown char -> skip
    get();
    return next();
}

} // namespace innesce::front
