
#include "frontend/lexer.hpp"
#include "frontend/parser.hpp"
#include "frontend/sema.hpp"
#include <fstream>
#include <sstream>
#include <iostream>

int main(int argc, char** argv) {
    if (argc < 2) { std::cerr << "usage: innescec <file.inn>\n"; return 2; }
    std::ifstream in(argv[1]);
    if (!in) { std::cerr << "cannot open " << argv[1] << "\n"; return 2; }
    std::ostringstream ss; ss << in.rdbuf();
    innesce::front::Parser p(ss.str());
    auto unit = p.parse_unit();
    if (!unit) { std::cerr << "Parse error.\n"; return 3; }
    innesce::front::Sema s;
    auto res = s.check(*unit);
    if (!res.ok()) { std::cerr << "Sema error: " << res.error << "\n"; return 4; }
    std::cout << "OK\n";
    return 0;
}
