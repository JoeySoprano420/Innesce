
#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <variant>

namespace innesce::ast {

enum class DurUnit { MS, SEC };

struct Type {
    enum Kind { I32, ENUM, DUR } kind{I32};
    std::string enum_name; // if kind==ENUM
    DurUnit dur{};         // if kind==DUR
};

struct Expr {
    struct IntLit { int value; };
    struct Ident  { std::string name; };
    struct Unary  { char op; std::unique_ptr<Expr> rhs; };
    struct Binary { char op; std::unique_ptr<Expr> lhs, rhs; };
    struct IsFailed { std::string name; };
    struct DurLit { int value; DurUnit unit; };
    struct Cast { std::unique_ptr<Expr> inner; Type target; };
    struct Call { std::string name; std::vector<Expr> args; };

    std::variant<IntLit, Ident, Unary, Binary, IsFailed, DurLit, Cast, Call> node;
};

struct Stmt {
    struct Let { std::string name; Type type; Expr init; };
    struct Return { Expr value; };
    struct If { Expr cond; std::vector<Stmt> then_body; std::vector<Stmt> else_body; };
    struct Match {
        Expr scrutinee;
        struct Case { std::string label; std::vector<Stmt> body; bool is_default{false}; };
        std::vector<Case> cases;
    };
    struct Sleep { Expr amount; bool is_ms{true}; };
    struct Fail {};
    struct Quarantine { std::string name; std::vector<Stmt> body; };

    struct Asm {
        std::string body;
        bool intel{true};
        struct Operand { bool is_output{false}; std::string constraint; std::string name; bool is_immediate{false}; int imm_val{0}; };
        std::vector<Operand> ins;
        std::vector<std::string> clobbers;
    };

    std::variant<Let, Return, If, Match, Sleep, Fail, Quarantine, Asm> node;
};

struct EnumDecl { std::string name; std::vector<std::string> variants; };

struct Function {
    std::string name;
    Type ret;
    std::vector<std::string> gates; // e.g., "time", "fs.read"
    bool hot{false};
    std::vector<Stmt> body;
};

struct Unit { std::vector<EnumDecl> enums; std::vector<Function> functions; };

} // namespace innesce::ast
