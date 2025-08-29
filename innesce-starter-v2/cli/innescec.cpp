#include <iostream>
#include <fstream>
#include <string>
#include <vector>

#include "core/checkpoint.hpp"
#include "frontend/lexer.hpp"
#include "frontend/parser.hpp"
#include "frontend/sema.hpp"

#ifdef INNSCE_HAVE_LLVM
#include "backend/llvm/codegen.hpp"
#endif

static std::string read_all(const std::string& path) {
    std::ifstream ifs(path, std::ios::binary);
    if (!ifs) return {};
    std::string s((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
    return s;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        std::cerr << "usage: innescec <input.inn> [-o out.o]\n";
        return 1;
    }
    std::string input = argv[1];
    std::string out = "a.o";
    for (int i=2;i<argc;i++) {
        std::string a = argv[i];
        if (a == "-o" && i+1 < argc) { out = argv[++i]; continue; }
    }

    auto src = read_all(input);
    if (src.empty()) {
        std::cerr << "error: cannot read " << input << "\n";
        return 1;
    }

    innesce::checkpoint("parse");
    innesce::front::Parser p(src);
    auto unit = p.parse_unit();
    if (!unit) { std::cerr << "parse failed\n"; return 1; }

    innesce::checkpoint("sema");
    innesce::front::Sema sema;
    auto ok = sema.check(*unit);
    if (!ok.ok()) { std::cerr << "sema error: " << ok.error << "\n"; return 1; }

#ifdef INNSCE_HAVE_LLVM
    innesce::checkpoint("codegen");
    auto err = innesce::backend::llvm::compile_to_object(*unit, out);
    if (!err.empty()) { std::cerr << "codegen error: " << err << "\n"; return 1; }
    std::cout << "wrote object: " << out << "\n";
#else
    std::cout << "LLVM backend disabled. Reconfigure with -DINNSCE_ENABLE_LLVM=ON to emit objects.\n";
#endif
    return 0;
}
