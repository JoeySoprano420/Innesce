#pragma once
#include <string>
#include <string_view>
#include <vector>

namespace innesce::front {

enum class TokKind {
    End,
    Ident,
    Int,
    KwFn, KwIs, KwEnd, KwReturn, KwLet,
    KwI32,
    LParen, RParen,
    Colon, Semicolon,
    Arrow, Assign,
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
