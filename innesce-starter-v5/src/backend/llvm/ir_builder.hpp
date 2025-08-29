#pragma once
#include <string>
#include "frontend/ast.hpp"

namespace innesce::backend::llvm {

struct IrBuilderConfig {
    bool optimize{true};
};

class IrBuilder {
public:
    explicit IrBuilder(IrBuilderConfig cfg = {}) : cfg_(cfg) {}
    std::string banner() const { return "Innesce LLVM IR Builder (stub)"; }

    void compile(const innesce::ast::Program& prog);
private:
    IrBuilderConfig cfg_;
};

} // namespace innesce::backend::llvm
