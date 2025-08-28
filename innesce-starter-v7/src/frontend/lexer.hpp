
#pragma once
#include <string>
#include <string_view>

namespace innesce::front {

enum class TokKind {
    End,
    Ident,
    Int,
    // keywords
    KwFn, KwIs, KwEnd, KwReturn, KwLet,
    KwI32, KwIf, KwThen, KwElse,
    KwType, KwEnum, KwMatch, KwCase, KwDefault,
    KwSleep, KwQuarantine, KwFail, KwIsFailed,
    KwMS, KwSEC, KwASM, KwWith, KwHot, KwAs,
    // punctuation
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Colon, Semicolon, Comma,
    Arrow, Assign, FatArrow,
    // ops
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
    std::string_view s_;
    size_t i_{0};
    int line_{1}, col_{1};
    char peek() const;
    char get();
    void skip_ws_and_comments();
    Token make(TokKind k, std::string t);
};

} // namespace innesce::front
