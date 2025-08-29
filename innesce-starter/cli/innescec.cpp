#include <iostream>
#include "core/durations.hpp"
#include "core/truth.hpp"
#include "core/checkpoint.hpp"
#ifdef INNSCE_HAVE_LLVM
#include "backend/llvm/ir_builder.hpp"
#endif

int main(int argc, char** argv) {
    (void)argc; (void)argv;
    std::cout << "Innesce (N-S) compiler skeleton\n";
#ifdef INNSCE_HAVE_LLVM
    innesce::backend::llvm::IrBuilder b{};
    std::cout << b.banner() << "\n";
#else
    std::cout << "(LLVM backend disabled)\n";
#endif
    innesce::checkpoint("startup");
    return 0;
}
