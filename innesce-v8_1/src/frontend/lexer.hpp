
#pragma once
#include <string>

namespace innesce::front {

enum class TokKind {
    End, Ident, Int, String,
    KwFn, KwIs, KwEnd, KwReturn, KwLet,
    KwI32, KwStr, KwIf, KwThen, KwElse,
    KwType, KwEnum, KwMatch, KwCase, KwDefault,
    KwSleep, KwQuarantine, KwFail, KwIsFailed,
    KwMS, KwSEC, KwASM, KwWith, KwHot, KwAs, KwWhen,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Colon, Semicolon, Comma,
    Arrow, Assign, FatArrow,
    Plus, Minus, Star, Slash,
};

struct Token {
    TokKind kind;
    std::string text;
    int int_val{0};
    int line{1}, col{1};
};

struct Lexer {
    explicit Lexer(std::string_view src);
    Token next();
private:
    std::string s_;
    size_t i_{0};
    int line_{1}, col_{1};
    char peek() const;
    char get();
    void skip_ws_and_comments();
    Token make(TokKind k, std::string t);
};

} // namespace innesce::front
