#pragma once
#include "frontend/ast.hpp"
#include <string>

namespace innesce::backend::llvm {

// Returns empty string on success; otherwise an error message.
std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path);

} // namespace innesce::backend::llvm
