
#include "frontend/lexer.hpp"
#include "frontend/parser.hpp"
#include "frontend/sema.hpp"
#include <fstream>
#include <sstream>
#include <iostream>

#ifdef INNSCE_ENABLE_LLVM
#include "backend/llvm/codegen.hpp"
#endif

int main(int argc, char** argv) {
    if (argc < 2) { std::cerr << "usage: innescec <file.inn> [-o out.o]\n"; return 2; }
    std::string srcpath = argv[1];
    std::string outobj;
    for (int i=2;i<argc;i++) { std::string a = argv[i]; if (a=="-o" && i+1<argc) outobj = argv[++i]; }
    std::ifstream in(srcpath); if (!in) { std::cerr << "cannot open " << srcpath << "\n"; return 2; }
    std::ostringstream ss; ss << in.rdbuf();
    innesce::front::Parser p(ss.str());
    auto unit = p.parse_unit(); if (!unit) { std::cerr << "Parse error.\n"; return 3; }
    innesce::front::Sema s; auto res = s.check(*unit); if (!res.ok()) { std::cerr << "Sema error: " << res.error << "\n"; return 4; }
#ifdef INNSCE_ENABLE_LLVM
    if (!outobj.empty()) {
        std::string err;
        if (!innesce::backend::compile_to_object(*unit, s, outobj, err)) { std::cerr << "Codegen error: " << err << "\n"; return 5; }
        std::cout << "Wrote " << outobj << "\n";
        return 0;
    }
#endif
    std::cout << "OK\n"; return 0;
}
