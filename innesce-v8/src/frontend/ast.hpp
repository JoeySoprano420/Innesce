
#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <variant>

namespace innesce::ast {

enum class DurUnit { MS, SEC };

struct Type {
    enum Kind { I32, ENUM, DUR, STR, TUPLE } kind{I32};
    std::string enum_name; // ENUM
    DurUnit dur{};         // DUR
    std::vector<Type> tuple_elems; // TUPLE
};

struct Expr {
    struct IntLit { int value; };
    struct StringLit { std::string value; };
    struct Ident  { std::string name; };
    struct Unary  { char op; std::unique_ptr<Expr> rhs; };
    struct Binary { char op; std::unique_ptr<Expr> lhs, rhs; };
    struct IsFailed { std::string name; };
    struct DurLit { int value; DurUnit unit; };
    struct Cast { std::unique_ptr<Expr> inner; Type target; };
    struct Call { std::string name; std::vector<Expr> args; };

    struct AsmExpr {
        std::string body;
        bool intel{true};
        struct Operand { bool is_output{false}; std::string constraint; bool is_immediate{false}; int imm_val{0}; std::string name; };
        std::vector<Operand> outs;
        std::vector<Operand> ins;
        std::vector<std::string> clobbers;
    };

    std::variant<IntLit, StringLit, Ident, Unary, Binary, IsFailed, DurLit, Cast, Call, AsmExpr> node;
};

struct Pattern {
    struct Wild {};
    struct Int { int value; };
    struct Dur { int value; DurUnit unit; };
    using Node = std::variant<Wild, Int, Dur>;
    Node node;
};

struct Stmt {
    struct Let { std::string name; Type type; Expr init; };
    struct LetTuple { std::vector<std::string> names; std::vector<Type> types; Expr init; };
    struct Return { Expr value; };
    struct If { Expr cond; std::vector<Stmt> then_body; std::vector<Stmt> else_body; };
    struct Match {
        Expr scrutinee;
        struct Case { bool is_default{false}; bool is_tuple{false}; std::string label; std::vector<Pattern> tpat; std::vector<Stmt> body; };
        std::vector<Case> cases;
    };
    struct Sleep { Expr amount; bool is_ms{true}; };
    struct Fail {};
    struct Quarantine { std::string name; std::vector<Stmt> body; };

    struct Asm {
        std::string body;
        bool intel{true};
        struct Operand { bool is_output{false}; std::string constraint; std::string name; bool is_immediate{false}; int imm_val{0}; };
        std::vector<Operand> outs;
        std::vector<Operand> ins;
        std::vector<std::string> clobbers;
    };

    std::variant<Let, LetTuple, Return, If, Match, Sleep, Fail, Quarantine, Asm> node;
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
