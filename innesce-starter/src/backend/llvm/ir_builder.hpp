#pragma once
#include <string>

namespace innesce::backend::llvm {

struct IrBuilderConfig {
    bool optimize{true};
};

class IrBuilder {
public:
    explicit IrBuilder(IrBuilderConfig cfg = {}) : cfg_(cfg) {}
    std::string banner() const { return "Innesce LLVM IR Builder (stub)"; }
private:
    IrBuilderConfig cfg_;
};

} // namespace innesce::backend::llvm
