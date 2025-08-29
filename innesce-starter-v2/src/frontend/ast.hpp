#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>

namespace innesce::ast {

struct Type {
    enum Kind { I32 } kind{I32};
};

struct Expr {
    struct IntLit { int value; };
    struct Ident  { std::string name; };
    std::variant<IntLit, Ident> node;
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
    std::variant<Let, Return> node;
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
