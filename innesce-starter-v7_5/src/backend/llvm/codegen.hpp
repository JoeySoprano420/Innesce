
#pragma once
#include "frontend/ast.hpp"
#include "frontend/sema.hpp"
#include <string>

namespace innesce::backend {
bool compile_to_object(const innesce::ast::Unit& unit,
                       const innesce::front::Sema& sema,
                       const std::string& out_obj_path,
                       std::string& err);
}
