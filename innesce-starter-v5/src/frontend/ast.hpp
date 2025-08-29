#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <variant>

namespace innesce::ast {

struct Type {
    enum Kind { I32, ENUM } kind{I32};
    std::string enum_name; // if kind==ENUM
};

struct Expr {
    struct IntLit { int value; };
    struct Ident  { std::string name; };     // local var or enum variant
    struct Unary  { char op; std::unique_ptr<Expr> rhs; };
    struct Binary { char op; std::unique_ptr<Expr> lhs, rhs; };
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
        std::vector<Stmt> else_body;
    };
    struct Match {
        Expr scrutinee;
        struct Case { std::string label; std::vector<Stmt> body; bool is_default{false}; };
        std::vector<Case> cases;
    };
    std::variant<Let, Return, If, Match> node;
};

struct EnumDecl {
    std::string name;
    std::vector<std::string> variants;
};

struct Function {
    std::string name;
    Type ret;
    std::vector<Stmt> body;
};

struct Unit {
    std::vector<EnumDecl> enums;
    std::vector<Function> functions;
};

} // namespace innesce::ast
