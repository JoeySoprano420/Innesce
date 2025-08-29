
#pragma once
#include "frontend/ast.hpp"
#include <string>
#include <unordered_map>
#include <optional>

namespace innesce::front {

struct SemaResult {
    std::string error;
    bool ok() const { return error.empty(); }
};

class Sema {
public:
    Sema() = default;
    SemaResult check(const ast::Unit& u);

    const std::unordered_map<std::string, std::unordered_map<std::string,int>>& enums() const { return enums_; }
private:
    std::unordered_map<std::string, std::unordered_map<std::string,int>> enums_;
};

} // namespace innesce::front
