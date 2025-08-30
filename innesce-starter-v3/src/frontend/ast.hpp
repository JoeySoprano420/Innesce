#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <variant>

namespace innesce::ast {

struct Type {
    enum Kind { I32 } kind{I32};
};

struct Expr {
    struct IntLit { int value; };
    struct Ident  { std::string name; };
    struct Unary  { char op; std::unique_ptr<Expr> rhs; };          // -x
    struct Binary { char op; std::unique_ptr<Expr> lhs, rhs; };     // x+y,* ,/ ,-
    std::variant<IntLit, Ident, Unary, Binary> node;
};

struct Stmt {
    struct Let {
        std::string name;
        Type type;
        Expr init;
    };
    struct Return {
        Expr value;
    };
    struct If {
        Expr cond;
        std::vector<Stmt> then_body;
        std::vector<Stmt> else_body; // empty if no else
    };
    std::variant<Let, Return, If> node;
};

struct Function {
    std::string name;
    Type ret;
    std::vector<Stmt> body;
};

struct Unit {
    std::vector<Function> functions;
};

} // namespace innesce::ast
