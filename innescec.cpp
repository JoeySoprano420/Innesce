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
#include "backend/llvm/ir_builder.hpp"
// Stubbed out; integrate real LLVM IRBuilder wiring later.
#pragma once
#include <string_view>

namespace innesce {

    // Compile-time/no-op checkpoint markers; can be wired to profiling later.
    inline void checkpoint(std::string_view) noexcept {
        // no-op (placeholders for future instrumentation)
    }

} // namespace innesce
#pragma once
#include <cstdint>
#include <concepts>
#include <compare>
#include <type_traits>

namespace innesce {

    // Strong unit duration (integral count of base ticks), similar to std::chrono but minimal.
    // We intentionally do NOT mix units implicitly; conversions are explicit.
    template <typename UnitTag>
    struct Duration {
        using rep = std::int64_t;
        rep value{ 0 };

        constexpr Duration() = default;
        constexpr explicit Duration(rep v) : value(v) {}

        // Comparisons
        constexpr auto operator<=>(const Duration&) const = default;

        // Arithmetic (same-unit only)
        constexpr Duration operator+(Duration other) const { return Duration{ value + other.value }; }
        constexpr Duration operator-(Duration other) const { return Duration{ value - other.value }; }
        constexpr Duration& operator+=(Duration other) { value += other.value; return *this; }
        constexpr Duration& operator-=(Duration other) { value -= other.value; return *this; }

        // Scale by scalar
        template <std::integral I>
        constexpr Duration operator*(I k) const { return Duration{ value * static_cast<rep>(k) }; }

        template <std::integral I>
        constexpr Duration operator/(I k) const { return Duration{ value / static_cast<rep>(k) }; }
    };

    // Unit tags
    struct ns_tag {}; struct us_tag {}; struct ms_tag {}; struct sec_tag {}; struct min_tag {}; struct hr_tag {};

    using ns = Duration<ns_tag>;
    using us = Duration<us_tag>;
    using ms = Duration<ms_tag>;
    using sec = Duration<sec_tag>;
    using min = Duration<min_tag>;
    using hr = Duration<hr_tag>;

    // Explicit converters (compile-time factors)
    constexpr us to_us(ns x) { return us{ x.value / 1000 }; }
    constexpr ms to_ms(us x) { return ms{ x.value / 1000 }; }
    constexpr sec to_sec(ms x) { return sec{ x.value / 1000 }; }
    constexpr ms  to_ms(sec x) { return ms{ x.value * 1000 }; }
    constexpr us  to_us(ms x) { return us{ x.value * 1000 }; }
    constexpr ns  to_ns(us x) { return ns{ x.value * 1000 }; }

    // Literals helpers
    constexpr ns  operator""_ns(unsigned long long v) { return ns{ static_cast<ns::rep>(v) }; }
    constexpr us  operator""_us(unsigned long long v) { return us{ static_cast<us::rep>(v) * 1000 }; }
    constexpr ms  operator""_ms(unsigned long long v) { return ms{ static_cast<ms::rep>(v) * 1000 * 1000 }; }
    constexpr sec operator""_sec(unsigned long long v) { return sec{ static_cast<sec::rep>(v) }; }

} // namespace innesce
#include "core/lanes.hpp"
// Implementation currently header-only for simplicity; TU exists for future expansion.
#pragma once
#include <string>
#include <vector>
#include <utility>
#include <optional>

namespace innesce {

    struct Quarantine {
        bool failed{ false };
        std::vector<std::string> notes;

        void note(std::string msg) {
            notes.emplace_back(std::move(msg));
        }

        // Attempt wrapper: F must return std::optional<T>. If empty => failure recorded.
        template <typename F>
        auto attempt(F&& f) -> decltype(f()) {
            auto r = f();
            if (!r.has_value()) {
                failed = true;
                note("operation failed");
            }
            return r;
        }
    };

} // namespace innesce
#pragma once
#include <cstdint>

namespace innesce {

    enum class Truth : std::uint8_t { False = 0, True = 1, Unknown = 2, Both = 3 };

    // 4-valued truth operations (Kleene-like; tweak as desired)
    constexpr Truth t_not(Truth a) {
        switch (a) {
        case Truth::False:   return Truth::True;
        case Truth::True:    return Truth::False;
        case Truth::Unknown: return Truth::Unknown;
        case Truth::Both:    return Truth::Unknown; // design choice: ¬Both = Unknown
        }
        return Truth::Unknown;
    }

    constexpr Truth t_and(Truth a, Truth b) {
        // truth table encoded; symmetric
        if (a == Truth::False || b == Truth::False) return Truth::False;
        if (a == Truth::True && b == Truth::True)  return Truth::True;
        if (a == Truth::Both && b == Truth::True)  return Truth::Both;
        if (b == Truth::Both && a == Truth::True)  return Truth::Both;
        if (a == Truth::Both && b == Truth::Both)  return Truth::Both;
        // any combo with Unknown that isn't already decided:
        return Truth::Unknown;
    }

    constexpr Truth t_or(Truth a, Truth b) {
        if (a == Truth::True || b == Truth::True) return Truth::True;
        if (a == Truth::False && b == Truth::False) return Truth::False;
        if (a == Truth::Both || b == Truth::Both) return Truth::Both;
        return Truth::Unknown;
    }

} // namespace innesce
#pragma once
#include <cstdint>

namespace innesce {

    enum class Truth : std::uint8_t { False = 0, True = 1, Unknown = 2, Both = 3 };

    // 4-valued truth operations (Kleene-like; tweak as desired)
    constexpr Truth t_not(Truth a) {
        switch (a) {
        case Truth::False:   return Truth::True;
        case Truth::True:    return Truth::False;
        case Truth::Unknown: return Truth::Unknown;
        case Truth::Both:    return Truth::Unknown; // design choice: ¬Both = Unknown
        }
        return Truth::Unknown;
    }

    constexpr Truth t_and(Truth a, Truth b) {
        // truth table encoded; symmetric
        if (a == Truth::False || b == Truth::False) return Truth::False;
        if (a == Truth::True && b == Truth::True)  return Truth::True;
        if (a == Truth::Both && b == Truth::True)  return Truth::Both;
        if (b == Truth::Both && a == Truth::True)  return Truth::Both;
        if (a == Truth::Both && b == Truth::Both)  return Truth::Both;
        // any combo with Unknown that isn't already decided:
        return Truth::Unknown;
    }

    constexpr Truth t_or(Truth a, Truth b) {
        if (a == Truth::True || b == Truth::True) return Truth::True;
        if (a == Truth::False && b == Truth::False) return Truth::False;
        if (a == Truth::Both || b == Truth::Both) return Truth::Both;
        return Truth::Unknown;
    }

} // namespace innesce
#pragma once
#include <string>

namespace innesce::backend::llvm {

    struct IrBuilderConfig {
        bool optimize{ true };
    };

    class IrBuilder {
    public:
        explicit IrBuilder(IrBuilderConfig cfg = {}) : cfg_(cfg) {}
        std::string banner() const { return "Innesce LLVM IR Builder (stub)"; }
    private:
        IrBuilderConfig cfg_;
    };

} // namespace innesce::backend::llvm
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
    for (int i = 2; i < argc; i++) {
        std::string a = argv[i];
        if (a == "-o" && i + 1 < argc) { out = argv[++i]; continue; }
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
#include "backend/llvm/codegen.hpp"
#include "backend/llvm/ir_builder.hpp"

#ifdef INNSCE_HAVE_LLVM
#include <llvm/ADT/Triple.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/MC/TargetOptions.h>
#include <system_error>
#endif

namespace innesce::backend::llvm {

    std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path) {
#ifndef INNSCE_HAVE_LLVM
        (void)unit; (void)obj_path;
        return "LLVM backend disabled: rebuild with -DINNSCE_ENABLE_LLVM=ON";
#else
        using namespace ::llvm;
        // Find main()
        const innesce::ast::Function* mainFn = nullptr;
        for (auto& f : unit.functions) if (f.name == "main") { mainFn = &f; break; }
        if (!mainFn) return "no main() in unit";

        LLVMContext ctx;
        auto mod = std::make_unique<Module>("innesce_mod", ctx);
        IRBuilder<> B(ctx);

        // int main()
        FunctionType* fty = FunctionType::get(B.getInt32Ty(), false);
        Function* F = Function::Create(fty, Function::ExternalLinkage, "main", mod.get());
        BasicBlock* entry = BasicBlock::Create(ctx, "entry", F);
        B.SetInsertPoint(entry);

        // Local variables
        std::unordered_map<std::string, Value*> locals;

        for (auto& st : mainFn->body) {
            if (std::holds_alternative<innesce::ast::Stmt::Let>(st.node)) {
                const auto& L = std::get<innesce::ast::Stmt::Let>(st.node);
                AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, L.name);
                if (std::holds_alternative<innesce::ast::Expr::IntLit>(L.init.node)) {
                    int v = std::get<innesce::ast::Expr::IntLit>(L.init.node).value;
                    B.CreateStore(B.getInt32(v), a);
                }
                else {
                    // identifier init: load from existing
                    const auto& I = std::get<innesce::ast::Expr::Ident>(L.init.node);
                    auto it = locals.find(I.name);
                    if (it == locals.end()) return "undeclared init: " + I.name;
                    Value* val = B.CreateLoad(B.getInt32Ty(), it->second);
                    B.CreateStore(val, a);
                }
                locals[L.name] = a;
            }
            else {
                const auto& R = std::get<innesce::ast::Stmt::Return>(st.node);
                Value* retv = nullptr;
                if (std::holds_alternative<innesce::ast::Expr::IntLit>(R.value.node)) {
                    int v = std::get<innesce::ast::Expr::IntLit>(R.value.node).value;
                    retv = B.getInt32(v);
                }
                else {
                    const auto& I = std::get<innesce::ast::Expr::Ident>(R.value.node);
                    auto it = locals.find(I.name);
                    if (it == locals.end()) return "undeclared return: " + I.name;
                    retv = B.CreateLoad(B.getInt32Ty(), it->second);
                }
                B.CreateRet(retv);
            }
        }
        if (!entry->getTerminator()) {
            // default return 0 if no explicit return
            B.CreateRet(B.getInt32(0));
        }

        if (verifyFunction(*F, &errs())) return "IR verification failed for main()";
        if (verifyModule(*mod, &errs())) return "Module verification failed";

        // Target setup
        InitializeAllTargetInfos();
        InitializeAllTargets();
        InitializeAllTargetMCs();
        InitializeAllAsmParsers();
        InitializeAllAsmPrinters();

        auto triple = sys::getDefaultTargetTriple();
        std::string err;
        const Target* target = TargetRegistry::lookupTarget(triple, err);
        if (!target) return "lookupTarget failed: " + err;

        TargetOptions opt;
        auto relocModel = std::optional<Reloc::Model>();
        std::unique_ptr<TargetMachine> TM(target->createTargetMachine(triple, "generic", "", opt, relocModel));
        mod->setTargetTriple(triple);
        mod->setDataLayout(TM->createDataLayout());

        std::error_code ec;
        raw_fd_ostream dest(obj_path, ec, sys::fs::OF_None);
        if (ec) return "could not open object file: " + ec.message();

        legacy::PassManager pm;
        if (TM->addPassesToEmitFile(pm, dest, nullptr, CodeGenFileType::CGFT_ObjectFile)) {
            return "TargetMachine cannot emit this file type";
        }
        pm.run(*mod);
        dest.flush();
        return {};
#endif
    }

} // namespace innesce::backend::llvm
#pragma once
#include "frontend/ast.hpp"
#include <string>

namespace innesce::backend::llvm {

    // Returns empty string on success; otherwise an error message.
    std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path);

} // namespace innesce::backend::llvm
#include "backend/llvm/ir_builder.hpp"
#include "frontend/ast.hpp"
#include <iostream>

using namespace innesce::backend::llvm;
using namespace innesce;

void IrBuilder::compile(const ast::Program& prog) {
    std::cout << "; Innesce IR dump (stub)\n";
    for (auto& st : prog.stmts) {
        if (std::holds_alternative<ast::LetStmt>(st->node)) {
            auto& ls = std::get<ast::LetStmt>(st->node);
            if (std::holds_alternative<ast::IntLit>(ls.init->node)) {
                auto v = std::get<ast::IntLit>(ls.init->node).value;
                std::cout << "%" << ls.name << " = const i64 " << v << "\n";
            }
        }
        else if (std::holds_alternative<ast::ReturnStmt>(st->node)) {
            auto& rs = std::get<ast::ReturnStmt>(st->node);
            if (std::holds_alternative<ast::IntLit>(rs->node)) {
                auto v = std::get<ast::IntLit>(rs->node).value;
                std::cout << "ret i64 " << v << "\n";
            }
        }
    }
}
#pragma once
#include <string>
#include "frontend/ast.hpp"

namespace innesce::backend::llvm {

    struct IrBuilderConfig {
        bool optimize{ true };
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
#pragma once
#include <string_view>

namespace innesce {

    // Compile-time/no-op checkpoint markers; can be wired to profiling later.
    inline void checkpoint(std::string_view) noexcept {
        // no-op (placeholders for future instrumentation)
    }

} // namespace innesce
#pragma once
#include <cstdint>
#include <concepts>
#include <compare>
#include <type_traits>

namespace innesce {

    // Strong unit duration (integral count of base ticks), similar to std::chrono but minimal.
    // We intentionally do NOT mix units implicitly; conversions are explicit.
    template <typename UnitTag>
    struct Duration {
        using rep = std::int64_t;
        rep value{ 0 };

        constexpr Duration() = default;
        constexpr explicit Duration(rep v) : value(v) {}

        // Comparisons
        constexpr auto operator<=>(const Duration&) const = default;

        // Arithmetic (same-unit only)
        constexpr Duration operator+(Duration other) const { return Duration{ value + other.value }; }
        constexpr Duration operator-(Duration other) const { return Duration{ value - other.value }; }
        constexpr Duration& operator+=(Duration other) { value += other.value; return *this; }
        constexpr Duration& operator-=(Duration other) { value -= other.value; return *this; }

        // Scale by scalar
        template <std::integral I>
        constexpr Duration operator*(I k) const { return Duration{ value * static_cast<rep>(k) }; }

        template <std::integral I>
        constexpr Duration operator/(I k) const { return Duration{ value / static_cast<rep>(k) }; }
    };

    // Unit tags
    struct ns_tag {}; struct us_tag {}; struct ms_tag {}; struct sec_tag {}; struct min_tag {}; struct hr_tag {};

    using ns = Duration<ns_tag>;
    using us = Duration<us_tag>;
    using ms = Duration<ms_tag>;
    using sec = Duration<sec_tag>;
    using min = Duration<min_tag>;
    using hr = Duration<hr_tag>;

    // Explicit converters (compile-time factors)
    constexpr us to_us(ns x) { return us{ x.value / 1000 }; }
    constexpr ms to_ms(us x) { return ms{ x.value / 1000 }; }
    constexpr sec to_sec(ms x) { return sec{ x.value / 1000 }; }
    constexpr ms  to_ms(sec x) { return ms{ x.value * 1000 }; }
    constexpr us  to_us(ms x) { return us{ x.value * 1000 }; }
    constexpr ns  to_ns(us x) { return ns{ x.value * 1000 }; }

    // Literals helpers
    constexpr ns  operator""_ns(unsigned long long v) { return ns{ static_cast<ns::rep>(v) }; }
    constexpr us  operator""_us(unsigned long long v) { return us{ static_cast<us::rep>(v) * 1000 }; }
    constexpr ms  operator""_ms(unsigned long long v) { return ms{ static_cast<ms::rep>(v) * 1000 * 1000 }; }
    constexpr sec operator""_sec(unsigned long long v) { return sec{ static_cast<sec::rep>(v) }; }

} // namespace innesce
#include "core/lanes.hpp"
// Implementation currently header-only for simplicity; TU exists for future expansion.
#pragma once
#include <vector>
#include <thread>
#include <functional>
#include <barrier>
#include <atomic>
#include <cstddef>
#include <cassert>

namespace innesce {

    // Deterministic lanes: fixed number of worker threads; tasks are partitioned by index.
    class Lanes {
    public:
        explicit Lanes(std::size_t count)
            : count_(count)
        {
            assert(count_ > 0);
        }

        std::size_t size() const noexcept { return count_; }

        // Parallel for over [begin, end) with step 'chunk'.
        // Each lane operates on indices i where (i / chunk) % lane_count == lane_id.
        void parallel_for(std::size_t begin, std::size_t end, std::size_t chunk,
            const std::function<void(std::size_t, std::size_t)>& body) const
        {
            const std::size_t lanes = count_;
            std::vector<std::jthread> threads;
            threads.reserve(lanes);
            for (std::size_t lane = 0; lane < lanes; ++lane) {
                threads.emplace_back([=]() {
                    for (std::size_t i = begin; i < end; i += chunk) {
                        std::size_t slice = (i - begin) / chunk;
                        if ((slice % lanes) == lane) {
                            std::size_t hi = i + chunk;
                            if (hi > end) hi = end;
                            body(i, hi);
                        }
                    }
                    });
            }
            // join on destruction of jthreads
        }

        // Packetize into frames; barrier after each frame.
        void packetize_frames(std::size_t begin, std::size_t end, std::size_t frame,
            const std::function<void(std::size_t, std::size_t)>& body) const
        {
            const std::size_t lanes = count_;
            std::atomic<std::size_t> next{ begin };
            std::barrier sync(static_cast<std::ptrdiff_t>(lanes));
            std::vector<std::jthread> threads;
            threads.reserve(lanes);
            for (std::size_t lane = 0; lane < lanes; ++lane) {
                threads.emplace_back([&]() {
                    while (true) {
                        std::size_t i = next.fetch_add(frame);
                        if (i >= end) break;
                        std::size_t hi = i + frame;
                        if (hi > end) hi = end;
                        body(i, hi);
                        sync.arrive_and_wait();
                    }
                    });
            }
        }

    private:
        std::size_t count_;
    };

} // namespace innesce
#pragma once
#include <string>
#include <vector>
#include <utility>
#include <optional>

namespace innesce {

    struct Quarantine {
        bool failed{ false };
        std::vector<std::string> notes;

        void note(std::string msg) {
            notes.emplace_back(std::move(msg));
        }

        // Attempt wrapper: F must return std::optional<T>. If empty => failure recorded.
        template <typename F>
        auto attempt(F&& f) -> decltype(f()) {
            auto r = f();
            if (!r.has_value()) {
                failed = true;
                note("operation failed");
            }
            return r;
        }
    };

} // namespace innesce
#pragma once
#include <cstdint>

namespace innesce {

    enum class Truth : std::uint8_t { False = 0, True = 1, Unknown = 2, Both = 3 };

    // 4-valued truth operations (Kleene-like; tweak as desired)
    constexpr Truth t_not(Truth a) {
        switch (a) {
        case Truth::False:   return Truth::True;
        case Truth::True:    return Truth::False;
        case Truth::Unknown: return Truth::Unknown;
        case Truth::Both:    return Truth::Unknown; // design choice: ¬Both = Unknown
        }
        return Truth::Unknown;
    }

    constexpr Truth t_and(Truth a, Truth b) {
        // truth table encoded; symmetric
        if (a == Truth::False || b == Truth::False) return Truth::False;
        if (a == Truth::True && b == Truth::True)  return Truth::True;
        if (a == Truth::Both && b == Truth::True)  return Truth::Both;
        if (b == Truth::Both && a == Truth::True)  return Truth::Both;
        if (a == Truth::Both && b == Truth::Both)  return Truth::Both;
        // any combo with Unknown that isn't already decided:
        return Truth::Unknown;
    }

    constexpr Truth t_or(Truth a, Truth b) {
        if (a == Truth::True || b == Truth::True) return Truth::True;
        if (a == Truth::False && b == Truth::False) return Truth::False;
        if (a == Truth::Both || b == Truth::Both) return Truth::Both;
        return Truth::Unknown;
    }

} // namespace innesce
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
    for (int i = 2; i < argc; i++) {
        std::string a = argv[i];
        if (a == "-o" && i + 1 < argc) { out = argv[++i]; continue; }
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
#include "backend/llvm/codegen.hpp"
#include "backend/llvm/ir_builder.hpp"

#ifdef INNSCE_HAVE_LLVM
#include <llvm/ADT/Triple.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/MC/TargetOptions.h>
#include <system_error>
#include <unordered_map>
#include <variant>
#include <optional>
#endif

namespace innesce::backend::llvm {

#ifndef INNSCE_HAVE_LLVM
    std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path) {
        (void)unit; (void)obj_path;
        return "LLVM backend disabled: rebuild with -DINNSCE_ENABLE_LLVM=ON";
    }
#else

    using namespace ::llvm;

    static Value* emit_expr(IRBuilder<>& B,
        const innesce::ast::Expr& e,
        std::unordered_map<std::string, Value*>& locals) {
        using E = innesce::ast::Expr;
        if (std::holds_alternative<E::IntLit>(e.node)) {
            int v = std::get<E::IntLit>(e.node).value;
            return B.getInt32(v);
        }
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& I = std::get<E::Ident>(e.node);
            auto it = locals.find(I.name);
            if (it == locals.end()) return nullptr;
            return B.CreateLoad(B.getInt32Ty(), it->second);
        }
        if (std::holds_alternative<E::Unary>(e.node)) {
            const auto& U = std::get<E::Unary>(e.node);
            Value* rhs = emit_expr(B, *U.rhs, locals);
            if (!rhs) return nullptr;
            if (U.op == '-') return B.CreateNeg(rhs);
            return rhs;
        }
        const auto& BN = std::get<E::Binary>(e.node);
        Value* L = emit_expr(B, *BN.lhs, locals);
        Value* R = emit_expr(B, *BN.rhs, locals);
        if (!L || !R) return nullptr;
        switch (BN.op) {
        case '+': return B.CreateAdd(L, R);
        case '-': return B.CreateSub(L, R);
        case '*': return B.CreateMul(L, R);
        case '/': return B.CreateSDiv(L, R);
        }
        return nullptr;
    }

    static bool emit_block(IRBuilder<>& B,
        Function* F,
        const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Value*>& locals) {
        using S = innesce::ast::Stmt;
        for (auto& st : body) {
            if (std::holds_alternative<S::Let>(st.node)) {
                const auto& L = std::get<S::Let>(st.node);
                AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, L.name);
                Value* init = emit_expr(B, L.init, locals);
                if (!init) return false;
                B.CreateStore(init, a);
                locals[L.name] = a;
            }
            else if (std::holds_alternative<S::Return>(st.node)) {
                const auto& R = std::get<S::Return>(st.node);
                Value* v = emit_expr(B, R.value, locals);
                if (!v) return false;
                B.CreateRet(v);
                // After a return, no more code should be emitted in this block.
                return true;
            }
            else if (std::holds_alternative<S::If>(st.node)) {
                const auto& I = std::get<S::If>(st.node);
                Value* condv = emit_expr(B, I.cond, locals);
                if (!condv) return false;
                // cond != 0
                Value* istrue = B.CreateICmpNE(condv, B.getInt32(0));

                BasicBlock* thenBB = BasicBlock::Create(B.getContext(), "then", F);
                BasicBlock* elseBB = BasicBlock::Create(B.getContext(), "else");
                BasicBlock* contBB = BasicBlock::Create(B.getContext(), "endif");

                B.CreateCondBr(istrue, thenBB, I.else_body.empty() ? contBB : elseBB);

                // then
                B.SetInsertPoint(thenBB);
                auto locals_then = locals;
                bool thenReturned = emit_block(B, F, I.then_body, locals_then);
                if (!thenReturned) {
                    B.CreateBr(contBB);
                }

                if (!I.else_body.empty()) {
                    // else
                    F->getBasicBlockList().push_back(elseBB);
                    B.SetInsertPoint(elseBB);
                    auto locals_else = locals;
                    bool elseReturned = emit_block(B, F, I.else_body, locals_else);
                    if (!elseReturned) {
                        B.CreateBr(contBB);
                    }
                }

                // continue
                F->getBasicBlockList().push_back(contBB);
                B.SetInsertPoint(contBB);
            }
        }
        return false; // block flow falls through without guaranteed return
    }

    std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path) {
        // Find main()
        const innesce::ast::Function* mainFn = nullptr;
        for (auto& f : unit.functions) if (f.name == "main") { mainFn = &f; break; }
        if (!mainFn) return "no main() in unit";

        LLVMContext ctx;
        auto mod = std::make_unique<Module>("innesce_mod", ctx);
        IRBuilder<> B(ctx);

        // int main()
        FunctionType* fty = FunctionType::get(B.getInt32Ty(), false);
        Function* F = Function::Create(fty, Function::ExternalLinkage, "main", mod.get());
        BasicBlock* entry = BasicBlock::Create(ctx, "entry", F);
        B.SetInsertPoint(entry);

        std::unordered_map<std::string, Value*> locals;
        bool returned = emit_block(B, F, mainFn->body, locals);
        if (!returned) {
            // default return 0 if not already returned
            B.CreateRet(B.getInt32(0));
        }

        if (verifyFunction(*F, &errs())) return "IR verification failed for main()";
        if (verifyModule(*mod, &errs())) return "Module verification failed";

        // Target setup
        InitializeAllTargetInfos();
        InitializeAllTargets();
        InitializeAllTargetMCs();
        InitializeAllAsmParsers();
        InitializeAllAsmPrinters();

        auto triple = sys::getDefaultTargetTriple();
        std::string err;
        const Target* target = TargetRegistry::lookupTarget(triple, err);
        if (!target) return "lookupTarget failed: " + err;

        TargetOptions opt;
        auto relocModel = std::optional<Reloc::Model>();
        std::unique_ptr<TargetMachine> TM(target->createTargetMachine(triple, "generic", "", opt, relocModel));
        mod->setTargetTriple(triple);
        mod->setDataLayout(TM->createDataLayout());

        std::error_code ec;
        raw_fd_ostream dest(obj_path, ec, sys::fs::OF_None);
        if (ec) return "could not open object file: " + ec.message();

        legacy::PassManager pm;
        if (TM->addPassesToEmitFile(pm, dest, nullptr, CodeGenFileType::CGFT_ObjectFile)) {
            return "TargetMachine cannot emit this file type";
        }
        pm.run(*mod);
        dest.flush();
        return {};
    }

#endif // INNSCE_HAVE_LLVM

} // namespace innesce::backend::llvm
#pragma once
#include "frontend/ast.hpp"
#include <string>

namespace innesce::backend::llvm {

    // Returns empty string on success; otherwise an error message.
    std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path);

} // namespace innesce::backend::llvm
#include "backend/llvm/ir_builder.hpp"
#include "frontend/ast.hpp"
#include <iostream>

using namespace innesce::backend::llvm;
using namespace innesce;

void IrBuilder::compile(const ast::Program& prog) {
    std::cout << "; Innesce IR dump (stub)\n";
    for (auto& st : prog.stmts) {
        if (std::holds_alternative<ast::LetStmt>(st->node)) {
            auto& ls = std::get<ast::LetStmt>(st->node);
            if (std::holds_alternative<ast::IntLit>(ls.init->node)) {
                auto v = std::get<ast::IntLit>(ls.init->node).value;
                std::cout << "%" << ls.name << " = const i64 " << v << "\n";
            }
        }
        else if (std::holds_alternative<ast::ReturnStmt>(st->node)) {
            auto& rs = std::get<ast::ReturnStmt>(st->node);
            if (std::holds_alternative<ast::IntLit>(rs->node)) {
                auto v = std::get<ast::IntLit>(rs->node).value;
                std::cout << "ret i64 " << v << "\n";
            }
        }
    }
}
#pragma once
#include <string>
#include "frontend/ast.hpp"

namespace innesce::backend::llvm {

    struct IrBuilderConfig {
        bool optimize{ true };
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
#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <variant>

namespace innesce::ast {

    struct Type {
        enum Kind { I32 } kind{ I32 };
    };

    struct Expr {
        struct IntLit { int value; };
        struct Ident { std::string name; };
        struct Unary { char op; std::unique_ptr<Expr> rhs; };          // -x
        struct Binary { char op; std::unique_ptr<Expr> lhs, rhs; };     // x+y,* ,/ ,-
        std::variant<IntLit, Ident, Unary, Binary> node;
    };

    struct Stmt {
        struct Let {
            std::string name;
            Type type;
            Expr init;
        };
        struct Return {
            Expr value;
        };
        struct If {
            Expr cond;
            std::vector<Stmt> then_body;
            std::vector<Stmt> else_body; // empty if no else
        };
        std::variant<Let, Return, If> node;
    };

    struct Function {
        std::string name;
        Type ret;
        std::vector<Stmt> body;
    };

    struct Unit {
        std::vector<Function> functions;
    };

} // namespace innesce::ast
#include "frontend/lexer.hpp"
#include <cctype>

namespace innesce::front {

    Lexer::Lexer(std::string_view src) : s_(src) {}

    char Lexer::peek() const { return i_ < s_.size() ? s_[i_] : '\0'; }
    char Lexer::get() {
        if (i_ >= s_.size()) return '\0';
        char c = s_[i_++];
        if (c == '\n') { line_++; col_ = 1; }
        else { col_++; }
        return c;
    }

    void Lexer::skip_ws_and_comments() {
        while (true) {
            char c = peek();
            if (c == '\0') break;
            if (std::isspace(static_cast<unsigned char>(c))) { get(); continue; }
            if (c == '-' && i_ + 1 < s_.size() && s_[i_ + 1] == '-') {
                while (c != '\n' && c != '\0') c = get();
                continue;
            }
            break;
        }
    }

    Token Lexer::make(TokKind k, std::string t) {
        Token x{ k, std::move(t), 0, line_, col_ };
        return x;
    }

    Token Lexer::next() {
        skip_ws_and_comments();
        char c = peek();
        if (c == '\0') return make(TokKind::End, "");

        // punctuation/operators
        if (c == '(') { get(); return make(TokKind::LParen, "("); }
        if (c == ')') { get(); return make(TokKind::RParen, ")"); }
        if (c == '+') { get(); return make(TokKind::Plus, "+"); }
        if (c == '-') {
            if (i_ + 1 < s_.size() && s_[i_ + 1] == '>') { get(); get(); return make(TokKind::Arrow, "->"); }
            get(); return make(TokKind::Minus, "-");
        }
        if (c == '*') { get(); return make(TokKind::Star, "*"); }
        if (c == '/') { get(); return make(TokKind::Slash, "/"); }
        if (c == ':') {
            get();
            if (peek() == '=') { get(); return make(TokKind::Assign, ":="); }
            return make(TokKind::Colon, ":");
        }
        if (c == ';') { get(); return make(TokKind::Semicolon, ";"); }

        // number
        if (std::isdigit(static_cast<unsigned char>(c))) {
            int v = 0;
            while (std::isdigit(static_cast<unsigned char>(peek()))) {
                v = v * 10 + (get() - '0');
            }
            Token t = make(TokKind::Int, "");
            t.int_val = v;
            return t;
        }

        // identifier/keyword
        if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
            std::string id;
            while (std::isalnum(static_cast<unsigned char>(peek())) || peek() == '_') {
                id.push_back(get());
            }
            if (id == "fn") return make(TokKind::KwFn, id);
            if (id == "is") return make(TokKind::KwIs, id);
            if (id == "end") return make(TokKind::KwEnd, id);
            if (id == "return") return make(TokKind::KwReturn, id);
            if (id == "let") return make(TokKind::KwLet, id);
            if (id == "i32") return make(TokKind::KwI32, id);
            if (id == "if") return make(TokKind::KwIf, id);
            if (id == "then") return make(TokKind::KwThen, id);
            if (id == "else") return make(TokKind::KwElse, id);
            Token t = make(TokKind::Ident, id);
            t.text = id;
            return t;
        }

        // unknown char -> skip
        get();
        return next();
    }

} // namespace innesce::front
#pragma once
#include <string>
#include <string_view>
#include <vector>

namespace innesce::front {

    enum class TokKind {
        End,
        Ident,
        Int,
        KwFn, KwIs, KwEnd, KwReturn, KwLet,
        KwI32, KwIf, KwThen, KwElse,
        LParen, RParen,
        Colon, Semicolon,
        Arrow, Assign,
        Plus, Minus, Star, Slash,
    };

    struct Token {
        TokKind kind;
        std::string text;
        int int_val{ 0 };
        int line{ 1 }, col{ 1 };
    };

    struct Lexer {
        explicit Lexer(std::string_view src);
        Token next();
    private:
        std::string_view s_;
        size_t i_{ 0 };
        int line_{ 1 }, col_{ 1 };
        char peek() const;
        char get();
        void skip_ws_and_comments();
        Token make(TokKind k, std::string t);
    };

} // namespace innesce::front
#include "frontend/parser.hpp"
#include <iostream>
#include <memory>

namespace innesce::front {

    Parser::Parser(std::string_view src) : lex_(src) { bump(); }

    void Parser::bump() { cur_ = lex_.next(); }
    bool Parser::accept(TokKind k) { if (cur_.kind == k) { bump(); return true; } return false; }
    bool Parser::expect(TokKind k, const char* what) {
        if (!accept(k)) { std::cerr << "Expected " << what << "\n"; return false; }
        return true;
    }

    std::optional<ast::Unit> Parser::parse_unit() {
        ast::Unit u;
        while (cur_.kind != TokKind::End) {
            auto fn = parse_function();
            if (!fn) return std::nullopt;
            u.functions.push_back(std::move(*fn));
        }
        return u;
    }

    std::optional<ast::Function> Parser::parse_function() {
        if (!expect(TokKind::KwFn, "'fn'")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected function name\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::LParen, "'('")) return std::nullopt;
        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
        if (!expect(TokKind::Arrow, "'->'")) return std::nullopt;
        ast::Type ret;
        if (!expect(TokKind::KwI32, "'i32'")) return std::nullopt;
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;

        ast::Function f; f.name = std::move(name); f.ret = ret;
        while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt();
            if (!st) return std::nullopt;
            f.body.push_back(std::move(*st));
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        return f;
    }

    std::vector<ast::Stmt> Parser::parse_block_until(TokKind terminator) {
        std::vector<ast::Stmt> body;
        while (cur_.kind != terminator && cur_.kind != TokKind::End) {
            auto st = parse_stmt();
            if (!st) return {};
            body.push_back(std::move(*st));
        }
        return body;
    }

    std::optional<ast::Stmt> Parser::parse_stmt() {
        if (cur_.kind == TokKind::KwLet) {
            bump();
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected identifier after let\n"; return std::nullopt; }
            std::string name = cur_.text; bump();
            if (!expect(TokKind::Colon, "':'")) return std::nullopt;
            if (!expect(TokKind::KwI32, "'i32'")) return std::nullopt;
            if (!expect(TokKind::Assign, "':='")) return std::nullopt;
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Let{ std::move(name), ast::Type{}, std::move(*e) };
            return st;
        }
        if (cur_.kind == TokKind::KwReturn) {
            bump();
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Return{ std::move(*e) };
            return st;
        }
        if (cur_.kind == TokKind::KwIf) {
            bump();
            auto cond = parse_expr(); if (!cond) return std::nullopt;
            if (!expect(TokKind::KwThen, "'then'")) return std::nullopt;
            auto then_body = parse_block_until(TokKind::KwElse);
            if (cur_.kind == TokKind::KwElse) { bump(); } // consume else
            std::vector<ast::Stmt> else_body;
            if (cur_.kind != TokKind::KwEnd) {
                else_body = parse_block_until(TokKind::KwEnd);
            }
            if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::If{ std::move(*cond), std::move(then_body), std::move(else_body) };
            return st;
        }
        std::cerr << "Unknown statement\n";
        return std::nullopt;
    }

    std::optional<ast::Expr> Parser::parse_expr() { return parse_add(); }

    std::optional<ast::Expr> Parser::parse_add() {
        auto lhs = parse_mul(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Plus || cur_.kind == TokKind::Minus) {
            char op = (cur_.kind == TokKind::Plus) ? '+' : '-';
            bump();
            auto rhs = parse_mul(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Binary{ op, std::make_unique<ast::Expr>(std::move(*lhs)), std::make_unique<ast::Expr>(std::move(*rhs)) };
            lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_mul() {
        auto lhs = parse_unary(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Star || cur_.kind == TokKind::Slash) {
            char op = (cur_.kind == TokKind::Star) ? '*' : '/';
            bump();
            auto rhs = parse_unary(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Binary{ op, std::make_unique<ast::Expr>(std::move(*lhs)), std::make_unique<ast::Expr>(std::move(*rhs)) };
            lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_unary() {
        if (cur_.kind == TokKind::Minus) {
            bump();
            auto rhs = parse_unary(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Unary{ '-', std::make_unique<ast::Expr>(std::move(*rhs)) };
            return e;
        }
        return parse_primary();
    }

    std::optional<ast::Expr> Parser::parse_primary() {
        if (cur_.kind == TokKind::Int) {
            ast::Expr e; e.node = ast::Expr::IntLit{ cur_.int_val }; bump(); return e;
        }
        if (cur_.kind == TokKind::Ident) {
            ast::Expr e; e.node = ast::Expr::Ident{ cur_.text }; bump(); return e;
        }
        if (accept(TokKind::LParen)) {
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            return e;
        }
        std::cerr << "Expected expression\n";
        return std::nullopt;
    }

} // namespace innesce::front
#pragma once
#include "frontend/lexer.hpp"
#include "frontend/ast.hpp"
#include <optional>

namespace innesce::front {

    class Parser {
    public:
        explicit Parser(std::string_view src);
        std::optional<ast::Unit> parse_unit();

    private:
        Lexer lex_;
        Token cur_;

        void bump();
        bool accept(TokKind k);
        bool expect(TokKind k, const char* what);

        std::optional<ast::Function> parse_function();
        std::optional<ast::Stmt> parse_stmt();
        std::optional<ast::Expr> parse_expr();
        std::optional<ast::Expr> parse_add();
        std::optional<ast::Expr> parse_mul();
        std::optional<ast::Expr> parse_unary();
        std::optional<ast::Expr> parse_primary();

        std::vector<ast::Stmt> parse_block_until(TokKind terminator);
    };

} // namespace innesce::front
#include "frontend/sema.hpp"
#include <unordered_map>
#include <variant>

namespace innesce::front {

    static bool check_expr(const innesce::ast::Expr& e, const std::unordered_map<std::string, bool>& locals, std::string& err) {
        using E = innesce::ast::Expr;
        if (std::holds_alternative<E::IntLit>(e.node)) return true;
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& I = std::get<E::Ident>(e.node);
            if (!locals.count(I.name)) { err = "use of undeclared identifier: " + I.name; return false; }
            return true;
        }
        if (std::holds_alternative<E::Unary>(e.node)) {
            const auto& U = std::get<E::Unary>(e.node);
            return check_expr(*U.rhs, locals, err);
        }
        const auto& B = std::get<E::Binary>(e.node);
        return check_expr(*B.lhs, locals, err) && check_expr(*B.rhs, locals, err);
    }

    static bool check_block(const std::vector<innesce::ast::Stmt>& body, std::unordered_map<std::string, bool> locals, std::string& err) {
        using S = innesce::ast::Stmt;
        for (auto& st : body) {
            if (std::holds_alternative<S::Let>(st.node)) {
                const auto& L = std::get<S::Let>(st.node);
                if (!check_expr(L.init, locals, err)) return false;
                locals[L.name] = true;
            }
            else if (std::holds_alternative<S::Return>(st.node)) {
                const auto& R = std::get<S::Return>(st.node);
                if (!check_expr(R.value, locals, err)) return false;
            }
            else if (std::holds_alternative<S::If>(st.node)) {
                const auto& I = std::get<S::If>(st.node);
                if (!check_expr(I.cond, locals, err)) return false;
                auto locals_then = locals;
                if (!check_block(I.then_body, locals_then, err)) return false;
                auto locals_else = locals;
                if (!check_block(I.else_body, locals_else, err)) return false;
            }
        }
        return true;
    }

    SemaResult Sema::check(const ast::Unit& u) {
        bool has_main = false;
        for (auto& f : u.functions) {
            if (f.name == "main") has_main = true;
            std::unordered_map<std::string, bool> locals;
            std::string err;
            if (!check_block(f.body, locals, err)) return { err };
        }
        if (!has_main) return { "missing 'main' function" };
        return {};
    }

} // namespace innesce::front
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
    };

} // namespace innesce::front
#pragma once
#include <string_view>

namespace innesce {

    // Compile-time/no-op checkpoint markers; can be wired to profiling later.
    inline void checkpoint(std::string_view) noexcept {
        // no-op (placeholders for future instrumentation)
    }

} // namespace innesce
#pragma once
#include <cstdint>
#include <concepts>
#include <compare>
#include <type_traits>

namespace innesce {

    // Strong unit duration (integral count of base ticks), similar to std::chrono but minimal.
    // We intentionally do NOT mix units implicitly; conversions are explicit.
    template <typename UnitTag>
    struct Duration {
        using rep = std::int64_t;
        rep value{ 0 };

        constexpr Duration() = default;
        constexpr explicit Duration(rep v) : value(v) {}

        // Comparisons
        constexpr auto operator<=>(const Duration&) const = default;

        // Arithmetic (same-unit only)
        constexpr Duration operator+(Duration other) const { return Duration{ value + other.value }; }
        constexpr Duration operator-(Duration other) const { return Duration{ value - other.value }; }
        constexpr Duration& operator+=(Duration other) { value += other.value; return *this; }
        constexpr Duration& operator-=(Duration other) { value -= other.value; return *this; }

        // Scale by scalar
        template <std::integral I>
        constexpr Duration operator*(I k) const { return Duration{ value * static_cast<rep>(k) }; }

        template <std::integral I>
        constexpr Duration operator/(I k) const { return Duration{ value / static_cast<rep>(k) }; }
    };

    // Unit tags
    struct ns_tag {}; struct us_tag {}; struct ms_tag {}; struct sec_tag {}; struct min_tag {}; struct hr_tag {};

    using ns = Duration<ns_tag>;
    using us = Duration<us_tag>;
    using ms = Duration<ms_tag>;
    using sec = Duration<sec_tag>;
    using min = Duration<min_tag>;
    using hr = Duration<hr_tag>;

    // Explicit converters (compile-time factors)
    constexpr us to_us(ns x) { return us{ x.value / 1000 }; }
    constexpr ms to_ms(us x) { return ms{ x.value / 1000 }; }
    constexpr sec to_sec(ms x) { return sec{ x.value / 1000 }; }
    constexpr ms  to_ms(sec x) { return ms{ x.value * 1000 }; }
    constexpr us  to_us(ms x) { return us{ x.value * 1000 }; }
    constexpr ns  to_ns(us x) { return ns{ x.value * 1000 }; }

    // Literals helpers
    constexpr ns  operator""_ns(unsigned long long v) { return ns{ static_cast<ns::rep>(v) }; }
    constexpr us  operator""_us(unsigned long long v) { return us{ static_cast<us::rep>(v) * 1000 }; }
    constexpr ms  operator""_ms(unsigned long long v) { return ms{ static_cast<ms::rep>(v) * 1000 * 1000 }; }
    constexpr sec operator""_sec(unsigned long long v) { return sec{ static_cast<sec::rep>(v) }; }

} // namespace innesce
#include "core/lanes.hpp"
// Implementation currently header-only for simplicity; TU exists for future expansion.
#pragma once
#include <vector>
#include <thread>
#include <functional>
#include <barrier>
#include <atomic>
#include <cstddef>
#include <cassert>

namespace innesce {

    // Deterministic lanes: fixed number of worker threads; tasks are partitioned by index.
    class Lanes {
    public:
        explicit Lanes(std::size_t count)
            : count_(count)
        {
            assert(count_ > 0);
        }

        std::size_t size() const noexcept { return count_; }

        // Parallel for over [begin, end) with step 'chunk'.
        // Each lane operates on indices i where (i / chunk) % lane_count == lane_id.
        void parallel_for(std::size_t begin, std::size_t end, std::size_t chunk,
            const std::function<void(std::size_t, std::size_t)>& body) const
        {
            const std::size_t lanes = count_;
            std::vector<std::jthread> threads;
            threads.reserve(lanes);
            for (std::size_t lane = 0; lane < lanes; ++lane) {
                threads.emplace_back([=]() {
                    for (std::size_t i = begin; i < end; i += chunk) {
                        std::size_t slice = (i - begin) / chunk;
                        if ((slice % lanes) == lane) {
                            std::size_t hi = i + chunk;
                            if (hi > end) hi = end;
                            body(i, hi);
                        }
                    }
                    });
            }
            // join on destruction of jthreads
        }

        // Packetize into frames; barrier after each frame.
        void packetize_frames(std::size_t begin, std::size_t end, std::size_t frame,
            const std::function<void(std::size_t, std::size_t)>& body) const
        {
            const std::size_t lanes = count_;
            std::atomic<std::size_t> next{ begin };
            std::barrier sync(static_cast<std::ptrdiff_t>(lanes));
            std::vector<std::jthread> threads;
            threads.reserve(lanes);
            for (std::size_t lane = 0; lane < lanes; ++lane) {
                threads.emplace_back([&]() {
                    while (true) {
                        std::size_t i = next.fetch_add(frame);
                        if (i >= end) break;
                        std::size_t hi = i + frame;
                        if (hi > end) hi = end;
                        body(i, hi);
                        sync.arrive_and_wait();
                    }
                    });
            }
        }

    private:
        std::size_t count_;
    };

} // namespace innesce
#pragma once
#include <string>
#include <vector>
#include <utility>
#include <optional>

namespace innesce {

    struct Quarantine {
        bool failed{ false };
        std::vector<std::string> notes;

        void note(std::string msg) {
            notes.emplace_back(std::move(msg));
        }

        // Attempt wrapper: F must return std::optional<T>. If empty => failure recorded.
        template <typename F>
        auto attempt(F&& f) -> decltype(f()) {
            auto r = f();
            if (!r.has_value()) {
                failed = true;
                note("operation failed");
            }
            return r;
        }
    };

} // namespace innesce
#pragma once
#include <cstdint>

namespace innesce {

    enum class Truth : std::uint8_t { False = 0, True = 1, Unknown = 2, Both = 3 };

    // 4-valued truth operations (Kleene-like; tweak as desired)
    constexpr Truth t_not(Truth a) {
        switch (a) {
        case Truth::False:   return Truth::True;
        case Truth::True:    return Truth::False;
        case Truth::Unknown: return Truth::Unknown;
        case Truth::Both:    return Truth::Unknown; // design choice: ¬Both = Unknown
        }
        return Truth::Unknown;
    }

    constexpr Truth t_and(Truth a, Truth b) {
        // truth table encoded; symmetric
        if (a == Truth::False || b == Truth::False) return Truth::False;
        if (a == Truth::True && b == Truth::True)  return Truth::True;
        if (a == Truth::Both && b == Truth::True)  return Truth::Both;
        if (b == Truth::Both && a == Truth::True)  return Truth::Both;
        if (a == Truth::Both && b == Truth::Both)  return Truth::Both;
        // any combo with Unknown that isn't already decided:
        return Truth::Unknown;
    }

    constexpr Truth t_or(Truth a, Truth b) {
        if (a == Truth::True || b == Truth::True) return Truth::True;
        if (a == Truth::False && b == Truth::False) return Truth::False;
        if (a == Truth::Both || b == Truth::Both) return Truth::Both;
        return Truth::Unknown;
    }

} // namespace innesce
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
    for (int i = 2; i < argc; i++) {
        std::string a = argv[i];
        if (a == "-o" && i + 1 < argc) { out = argv[++i]; continue; }
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
#include "backend/llvm/codegen.hpp"
#include "backend/llvm/ir_builder.hpp"
#include "frontend/sema.hpp" // for enums mapping in future (we'll pass explicit mapping from caller)

#ifdef INNSCE_HAVE_LLVM
#include <llvm/ADT/Triple.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/MC/TargetOptions.h>
#include <system_error>
#include <unordered_map>
#include <variant>
#include <optional>
#endif

namespace innesce::backend::llvm {

#ifndef INNSCE_HAVE_LLVM
    std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path) {
        (void)unit; (void)obj_path;
        return "LLVM backend disabled: rebuild with -DINNSCE_ENABLE_LLVM=ON";
    }
#else

    using namespace ::llvm;

    static Value* emit_expr(IRBuilder<>& B,
        const innesce::ast::Expr& e,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums) {
        using E = innesce::ast::Expr;
        if (std::holds_alternative<E::IntLit>(e.node)) {
            int v = std::get<E::IntLit>(e.node).value;
            return B.getInt32(v);
        }
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& I = std::get<E::Ident>(e.node);
            auto it = locals.find(I.name);
            if (it != locals.end()) {
                return B.CreateLoad(B.getInt32Ty(), it->second);
            }
            // maybe enum variant
            for (auto& [ename, vars] : enums) {
                auto vit = vars.find(I.name);
                if (vit != vars.end()) {
                    return B.getInt32(vit->second);
                }
            }
            return nullptr;
        }
        if (std::holds_alternative<E::Unary>(e.node)) {
            const auto& U = std::get<E::Unary>(e.node);
            Value* rhs = emit_expr(B, *U.rhs, locals, enums);
            if (!rhs) return nullptr;
            if (U.op == '-') return B.CreateNeg(rhs);
            return rhs;
        }
        const auto& BN = std::get<E::Binary>(e.node);
        Value* L = emit_expr(B, *BN.lhs, locals, enums);
        Value* R = emit_expr(B, *BN.rhs, locals, enums);
        if (!L || !R) return nullptr;
        switch (BN.op) {
        case '+': return B.CreateAdd(L, R);
        case '-': return B.CreateSub(L, R);
        case '*': return B.CreateMul(L, R);
        case '/': return B.CreateSDiv(L, R);
        }
        return nullptr;
    }

    static bool emit_block(IRBuilder<>& B,
        Function* F,
        const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums);

    static bool emit_match(IRBuilder<>& B,
        Function* F,
        const innesce::ast::Stmt::Match& M,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums) {
        Value* scr = emit_expr(B, M.scrutinee, locals, enums);
        if (!scr) return false;

        BasicBlock* contBB = BasicBlock::Create(B.getContext(), "match.cont");
        // build case blocks
        std::vector<BasicBlock*> caseBBs;
        BasicBlock* defaultBB = nullptr;
        for (auto& C : M.cases) {
            if (C.is_default) {
                defaultBB = BasicBlock::Create(B.getContext(), "case.default");
            }
            else {
                caseBBs.push_back(BasicBlock::Create(B.getContext(), ("case." + C.label).c_str()));
            }
        }
        if (!defaultBB) defaultBB = contBB;

        // switch instruction
        SwitchInst* sw = B.CreateSwitch(scr, defaultBB, (unsigned)caseBBs.size());

        // Attach cases
        unsigned idx = 0;
        for (auto& C : M.cases) {
            if (C.is_default) continue;
            int val = -1;
            // map label to int
            for (auto& [ename, vars] : enums) {
                auto it = vars.find(C.label);
                if (it != vars.end()) { val = it->second; break; }
            }
            if (val < 0) return false;
            BasicBlock* bb = caseBBs[idx++];
            F->getBasicBlockList().push_back(bb);
            sw->addCase(B.getInt32(val), bb);
            B.SetInsertPoint(bb);
            auto locals_case = locals;
            bool returned = emit_block(B, F, C.body, locals_case, enums);
            if (!returned) B.CreateBr(contBB);
        }

        // default
        if (defaultBB != contBB) {
            F->getBasicBlockList().push_back(defaultBB);
            B.SetInsertPoint(defaultBB);
            auto locals_def = locals;
            // empty default => just fallthrough
            for (auto& C : M.cases) {
                if (C.is_default) {
                    bool returned = emit_block(B, F, C.body, locals_def, enums);
                    if (!returned) B.CreateBr(contBB);
                    break;
                }
            }
        }

        // continue
        F->getBasicBlockList().push_back(contBB);
        B.SetInsertPoint(contBB);
        return false;
    }

    static bool emit_block(IRBuilder<>& B,
        Function* F,
        const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums) {
        using S = innesce::ast::Stmt;
        for (auto& st : body) {
            if (std::holds_alternative<S::Let>(st.node)) {
                const auto& L = std::get<S::Let>(st.node);
                AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, L.name);
                Value* init = emit_expr(B, L.init, locals, enums);
                if (!init) return false;
                B.CreateStore(init, a);
                locals[L.name] = a;
            }
            else if (std::holds_alternative<S::Return>(st.node)) {
                const auto& R = std::get<S::Return>(st.node);
                Value* v = emit_expr(B, R.value, locals, enums);
                if (!v) return false;
                B.CreateRet(v);
                return true;
            }
            else if (std::holds_alternative<S::If>(st.node)) {
                const auto& I = std::get<S::If>(st.node);
                Value* condv = emit_expr(B, I.cond, locals, enums);
                if (!condv) return false;
                Value* istrue = B.CreateICmpNE(condv, B.getInt32(0));
                BasicBlock* thenBB = BasicBlock::Create(B.getContext(), "then", F);
                BasicBlock* elseBB = BasicBlock::Create(B.getContext(), "else");
                BasicBlock* contBB = BasicBlock::Create(B.getContext(), "endif");
                B.CreateCondBr(istrue, thenBB, I.else_body.empty() ? contBB : elseBB);
                // then
                B.SetInsertPoint(thenBB);
                auto locals_then = locals;
                bool thenReturned = emit_block(B, F, I.then_body, locals_then, enums);
                if (!thenReturned) B.CreateBr(contBB);
                if (!I.else_body.empty()) {
                    F->getBasicBlockList().push_back(elseBB);
                    B.SetInsertPoint(elseBB);
                    auto locals_else = locals;
                    bool elseReturned = emit_block(B, F, I.else_body, locals_else, enums);
                    if (!elseReturned) B.CreateBr(contBB);
                }
                F->getBasicBlockList().push_back(contBB);
                B.SetInsertPoint(contBB);
            }
            else if (std::holds_alternative<S::Match>(st.node)) {
                if (!emit_match(B, F, std::get<S::Match>(st.node), locals, enums)) return false;
            }
        }
        return false;
    }

    std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path) {
        // Build enum map from unit
        std::unordered_map<std::string, std::unordered_map<std::string, int>> enums;
        for (auto& e : unit.enums) {
            std::unordered_map<std::string, int> m;
            for (int i = 0; i < (int)e.variants.size(); ++i) m[e.variants[i]] = i;
            enums[e.name] = std::move(m);
        }

        // Find main()
        const innesce::ast::Function* mainFn = nullptr;
        for (auto& f : unit.functions) if (f.name == "main") { mainFn = &f; break; }
        if (!mainFn) return "no main() in unit";

        LLVMContext ctx;
        auto mod = std::make_unique<Module>("innesce_mod", ctx);
        IRBuilder<> B(ctx);

        // int main()
        FunctionType* fty = FunctionType::get(B.getInt32Ty(), false);
        Function* F = Function::Create(fty, Function::ExternalLinkage, "main", mod.get());
        BasicBlock* entry = BasicBlock::Create(ctx, "entry", F);
        B.SetInsertPoint(entry);

        std::unordered_map<std::string, Value*> locals;
        bool returned = emit_block(B, F, mainFn->body, locals, enums);
        if (!returned) {
            B.CreateRet(B.getInt32(0));
        }

        if (verifyFunction(*F, &errs())) return "IR verification failed for main()";
        if (verifyModule(*mod, &errs())) return "Module verification failed";

        // Target setup
        InitializeAllTargetInfos();
        InitializeAllTargets();
        InitializeAllTargetMCs();
        InitializeAllAsmParsers();
        InitializeAllAsmPrinters();

        auto triple = sys::getDefaultTargetTriple();
        std::string err;
        const Target* target = TargetRegistry::lookupTarget(triple, err);
        if (!target) return "lookupTarget failed: " + err;

        TargetOptions opt;
        auto relocModel = std::optional<Reloc::Model>();
        std::unique_ptr<TargetMachine> TM(target->createTargetMachine(triple, "generic", "", opt, relocModel));
        mod->setTargetTriple(triple);
        mod->setDataLayout(TM->createDataLayout());

        std::error_code ec;
        raw_fd_ostream dest(obj_path, ec, sys::fs::OF_None);
        if (ec) return "could not open object file: " + ec.message();

        legacy::PassManager pm;
        if (TM->addPassesToEmitFile(pm, dest, nullptr, CodeGenFileType::CGFT_ObjectFile)) {
            return "TargetMachine cannot emit this file type";
        }
        pm.run(*mod);
        dest.flush();
        return {};
    }

#endif // INNSCE_HAVE_LLVM

} // namespace innesce::backend::llvm
#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <variant>

namespace innesce::ast {

    struct Type {
        enum Kind { I32, ENUM } kind{ I32 };
        std::string enum_name; // if kind==ENUM
    };

    struct Expr {
        struct IntLit { int value; };
        struct Ident { std::string name; };     // local var or enum variant
        struct Unary { char op; std::unique_ptr<Expr> rhs; };
        struct Binary { char op; std::unique_ptr<Expr> lhs, rhs; };
        std::variant<IntLit, Ident, Unary, Binary> node;
    };

    struct Stmt {
        struct Let {
            std::string name;
            Type type;
            Expr init;
        };
        struct Return {
            Expr value;
        };
        struct If {
            Expr cond;
            std::vector<Stmt> then_body;
            std::vector<Stmt> else_body;
        };
        struct Match {
            Expr scrutinee;
            struct Case { std::string label; std::vector<Stmt> body; bool is_default{ false }; };
            std::vector<Case> cases;
        };
        std::variant<Let, Return, If, Match> node;
    };

    struct EnumDecl {
        std::string name;
        std::vector<std::string> variants;
    };

    struct Function {
        std::string name;
        Type ret;
        std::vector<Stmt> body;
    };

    struct Unit {
        std::vector<EnumDecl> enums;
        std::vector<Function> functions;
    };

} // namespace innesce::ast
#include "frontend/lexer.hpp"
#include <cctype>

namespace innesce::front {

    Lexer::Lexer(std::string_view src) : s_(src) {}

    char Lexer::peek() const { return i_ < s_.size() ? s_[i_] : '\0'; }
    char Lexer::get() {
        if (i_ >= s_.size()) return '\0';
        char c = s_[i_++];
        if (c == '\n') { line_++; col_ = 1; }
        else { col_++; }
        return c;
    }

    void Lexer::skip_ws_and_comments() {
        while (true) {
            char c = peek();
            if (c == '\0') break;
            if (std::isspace(static_cast<unsigned char>(c))) { get(); continue; }
            if (c == '-' && i_ + 1 < s_.size() && s_[i_ + 1] == '-') {
                while (c != '\n' && c != '\0') c = get();
                continue;
            }
            break;
        }
    }

    Token Lexer::make(TokKind k, std::string t) {
        Token x{ k, std::move(t), 0, line_, col_ };
        return x;
    }

    Token Lexer::next() {
        skip_ws_and_comments();
        char c = peek();
        if (c == '\0') return make(TokKind::End, "");

        // punctuation/operators
        if (c == '(') { get(); return make(TokKind::LParen, "("); }
        if (c == ')') { get(); return make(TokKind::RParen, ")"); }
        if (c == '{') { get(); return make(TokKind::LBrace, "{"); }
        if (c == '}') { get(); return make(TokKind::RBrace, "}"); }
        if (c == '+') { get(); return make(TokKind::Plus, "+"); }
        if (c == '-') {
            if (i_ + 1 < s_.size() && s_[i_ + 1] == '>') { get(); get(); return make(TokKind::Arrow, "->"); }
            get(); return make(TokKind::Minus, "-");
        }
        if (c == '*') { get(); return make(TokKind::Star, "*"); }
        if (c == '/') { get(); return make(TokKind::Slash, "/"); }
        if (c == ':') {
            get();
            if (peek() == '=') { get(); return make(TokKind::Assign, ":="); }
            return make(TokKind::Colon, ":");
        }
        if (c == ';') { get(); return make(TokKind::Semicolon, ";"); }
        if (c == ',') { get(); return make(TokKind::Comma, ","); }
        if (c == '=') {
            if (i_ + 1 < s_.size() && s_[i_ + 1] == '>') { get(); get(); return make(TokKind::FatArrow, "=>"); }
        }

        // number
        if (std::isdigit(static_cast<unsigned char>(c))) {
            int v = 0;
            while (std::isdigit(static_cast<unsigned char>(peek()))) {
                v = v * 10 + (get() - '0');
            }
            Token t = make(TokKind::Int, "");
            t.int_val = v;
            return t;
        }

        // identifier/keyword
        if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
            std::string id;
            while (std::isalnum(static_cast<unsigned char>(peek())) || peek() == '_') {
                id.push_back(get());
            }
            if (id == "fn") return make(TokKind::KwFn, id);
            if (id == "is") return make(TokKind::KwIs, id);
            if (id == "end") return make(TokKind::KwEnd, id);
            if (id == "return") return make(TokKind::KwReturn, id);
            if (id == "let") return make(TokKind::KwLet, id);
            if (id == "i32") return make(TokKind::KwI32, id);
            if (id == "if") return make(TokKind::KwIf, id);
            if (id == "then") return make(TokKind::KwThen, id);
            if (id == "else") return make(TokKind::KwElse, id);
            if (id == "type") return make(TokKind::KwType, id);
            if (id == "enum") return make(TokKind::KwEnum, id);
            if (id == "match") return make(TokKind::KwMatch, id);
            if (id == "case") return make(TokKind::KwCase, id);
            if (id == "default") return make(TokKind::KwDefault, id);
            Token t = make(TokKind::Ident, id);
            t.text = id;
            return t;
        }

        // unknown char -> skip
        get();
        return next();
    }

} // namespace innesce::front
#pragma once
#include <string>
#include <string_view>
#include <vector>

namespace innesce::front {

    enum class TokKind {
        End,
        Ident,
        Int,
        // keywords
        KwFn, KwIs, KwEnd, KwReturn, KwLet,
        KwI32, KwIf, KwThen, KwElse,
        KwType, KwEnum, KwMatch, KwCase, KwDefault,
        // punctuation
        LParen, RParen, LBrace, RBrace,
        Colon, Semicolon, Comma,
        Arrow, Assign, FatArrow, // "->" and ":=" and "=>"
        // ops
        Plus, Minus, Star, Slash,
    };

    struct Token {
        TokKind kind;
        std::string text;
        int int_val{ 0 };
        int line{ 1 }, col{ 1 };
    };

    struct Lexer {
        explicit Lexer(std::string_view src);
        Token next();
    private:
        std::string_view s_;
        size_t i_{ 0 };
        int line_{ 1 }, col_{ 1 };
        char peek() const;
        char get();
        void skip_ws_and_comments();
        Token make(TokKind k, std::string t);
    };

} // namespace innesce::front
#include "frontend/parser.hpp"
#include <iostream>
#include <memory>

namespace innesce::front {

    Parser::Parser(std::string_view src) : lex_(src) { bump(); }

    void Parser::bump() { cur_ = lex_.next(); }
    bool Parser::accept(TokKind k) { if (cur_.kind == k) { bump(); return true; } return false; }
    bool Parser::expect(TokKind k, const char* what) {
        if (!accept(k)) { std::cerr << "Expected " << what << "\n"; return false; }
        return true;
    }

    std::optional<ast::Unit> Parser::parse_unit() {
        ast::Unit u;
        while (cur_.kind != TokKind::End) {
            if (cur_.kind == TokKind::KwType) {
                auto e = parse_enum_decl();
                if (!e) return std::nullopt;
                u.enums.push_back(std::move(*e));
                continue;
            }
            auto fn = parse_function();
            if (!fn) return std::nullopt;
            u.functions.push_back(std::move(*fn));
        }
        return u;
    }

    std::optional<ast::EnumDecl> Parser::parse_enum_decl() {
        if (!expect(TokKind::KwType, "'type'")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected enum name\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        if (!expect(TokKind::KwEnum, "'enum'")) return std::nullopt;
        if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
        ast::EnumDecl d; d.name = std::move(name);
        // variants
        bool first = true;
        while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
            if (!first) expect(TokKind::Comma, "','");
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected variant name\n"; return std::nullopt; }
            d.variants.push_back(cur_.text);
            bump();
            first = false;
        }
        if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
        expect(TokKind::Semicolon, "';'"); // optional; won't error if missing
        return d;
    }

    std::optional<ast::Function> Parser::parse_function() {
        if (!expect(TokKind::KwFn, "'fn'")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected function name\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::LParen, "'('")) return std::nullopt;
        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
        if (!expect(TokKind::Arrow, "'->'")) return std::nullopt;
        ast::Type ret;
        if (accept(TokKind::KwI32)) {
            ret.kind = ast::Type::I32;
        }
        else if (cur_.kind == TokKind::Ident) {
            ret.kind = ast::Type::ENUM; ret.enum_name = cur_.text; bump();
        }
        else {
            std::cerr << "Expected return type\n"; return std::nullopt;
        }
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;

        ast::Function f; f.name = std::move(name); f.ret = ret;
        while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt();
            if (!st) return std::nullopt;
            f.body.push_back(std::move(*st));
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        return f;
    }

    std::optional<ast::Stmt> Parser::parse_match() {
        // match <expr> is ... end
        if (!expect(TokKind::KwMatch, "'match'")) return std::nullopt;
        auto scrut = parse_expr(); if (!scrut) return std::nullopt;
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        ast::Stmt::Match m; m.scrutinee = std::move(*scrut);
        while (cur_.kind == TokKind::KwCase) {
            bump();
            bool is_default = false;
            std::string label;
            if (accept(TokKind::KwDefault)) {
                is_default = true;
            }
            else {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected case label\n"; return std::nullopt; }
                label = cur_.text; bump();
            }
            if (!expect(TokKind::FatArrow, "'=>'")) return std::nullopt;
            auto body = parse_case_body();
            m.cases.push_back(ast::Stmt::Match::Case{ std::move(label), std::move(body), is_default });
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        ast::Stmt st; st.node = std::move(m);
        return st;
    }

    std::vector<ast::Stmt> Parser::parse_case_body() {
        std::vector<ast::Stmt> body;
        while (cur_.kind != TokKind::KwCase && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt();
            if (!st) break;
            body.push_back(std::move(*st));
        }
        return body;
    }

    std::optional<ast::Stmt> Parser::parse_stmt() {
        if (cur_.kind == TokKind::KwLet) {
            bump();
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected identifier after let\n"; return std::nullopt; }
            std::string name = cur_.text; bump();
            if (!expect(TokKind::Colon, "':'")) return std::nullopt;
            ast::Type ty;
            if (accept(TokKind::KwI32)) {
                ty.kind = ast::Type::I32;
            }
            else if (cur_.kind == TokKind::Ident) {
                ty.kind = ast::Type::ENUM; ty.enum_name = cur_.text; bump();
            }
            else {
                std::cerr << "Expected type name after ':'\n"; return std::nullopt;
            }
            if (!expect(TokKind::Assign, "':='")) return std::nullopt;
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Let{ std::move(name), ty, std::move(*e) };
            return st;
        }
        if (cur_.kind == TokKind::KwReturn) {
            bump();
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Return{ std::move(*e) };
            return st;
        }
        if (cur_.kind == TokKind::KwIf) {
            bump();
            auto cond = parse_expr(); if (!cond) return std::nullopt;
            if (!expect(TokKind::KwThen, "'then'")) return std::nullopt;
            // then body until 'else' or 'end'
            std::vector<ast::Stmt> then_body;
            while (cur_.kind != TokKind::KwElse && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                auto st = parse_stmt();
                if (!st) return std::nullopt;
                then_body.push_back(std::move(*st));
            }
            std::vector<ast::Stmt> else_body;
            if (accept(TokKind::KwElse)) {
                while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                    auto st = parse_stmt();
                    if (!st) return std::nullopt;
                    else_body.push_back(std::move(*st));
                }
            }
            if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::If{ std::move(*cond), std::move(then_body), std::move(else_body) };
            return st;
        }
        if (cur_.kind == TokKind::KwMatch) {
            return parse_match();
        }
        std::cerr << "Unknown statement\n";
        return std::nullopt;
    }

    std::optional<ast::Expr> Parser::parse_expr() { return parse_add(); }

    std::optional<ast::Expr> Parser::parse_add() {
        auto lhs = parse_mul(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Plus || cur_.kind == TokKind::Minus) {
            char op = (cur_.kind == TokKind::Plus) ? '+' : '-';
            bump();
            auto rhs = parse_mul(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Binary{ op, std::make_unique<ast::Expr>(std::move(*lhs)), std::make_unique<ast::Expr>(std::move(*rhs)) };
            lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_mul() {
        auto lhs = parse_unary(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Star || cur_.kind == TokKind::Slash) {
            char op = (cur_.kind == TokKind::Star) ? '*' : '/';
            bump();
            auto rhs = parse_unary(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Binary{ op, std::make_unique<ast::Expr>(std::move(*lhs)), std::make_unique<ast::Expr>(std::move(*rhs)) };
            lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_unary() {
        if (cur_.kind == TokKind::Minus) {
            bump();
            auto rhs = parse_unary(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Unary{ '-', std::make_unique<ast::Expr>(std::move(*rhs)) };
            return e;
        }
        return parse_primary();
    }

    std::optional<ast::Expr> Parser::parse_primary() {
        if (cur_.kind == TokKind::Int) {
            ast::Expr e; e.node = ast::Expr::IntLit{ cur_.int_val }; bump(); return e;
        }
        if (cur_.kind == TokKind::Ident) {
            ast::Expr e; e.node = ast::Expr::Ident{ cur_.text }; bump(); return e;
        }
        if (accept(TokKind::LParen)) {
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            return e;
        }
        std::cerr << "Expected expression\n";
        return std::nullopt;
    }

} // namespace innesce::front
#pragma once
#include "frontend/lexer.hpp"
#include "frontend/ast.hpp"
#include <optional>

namespace innesce::front {

    class Parser {
    public:
        explicit Parser(std::string_view src);
        std::optional<ast::Unit> parse_unit();

    private:
        Lexer lex_;
        Token cur_;

        void bump();
        bool accept(TokKind k);
        bool expect(TokKind k, const char* what);

        std::optional<ast::EnumDecl> parse_enum_decl();
        std::optional<ast::Function> parse_function();
        std::optional<ast::Stmt> parse_stmt();
        std::optional<ast::Stmt> parse_match();
        std::optional<ast::Expr> parse_expr();
        std::optional<ast::Expr> parse_add();
        std::optional<ast::Expr> parse_mul();
        std::optional<ast::Expr> parse_unary();
        std::optional<ast::Expr> parse_primary();

        std::vector<ast::Stmt> parse_case_body();
    };

} // namespace innesce::front
#include "frontend/sema.hpp"
#include <unordered_map>
#include <variant>
#include <set>

namespace innesce::front {

    static bool check_expr(const innesce::ast::Expr& e,
        const std::unordered_map<std::string, innesce::ast::Type>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::string& err,
        innesce::ast::Type* outTy = nullptr)
    {
        using E = innesce::ast::Expr;
        if (std::holds_alternative<E::IntLit>(e.node)) {
            if (outTy) { outTy->kind = innesce::ast::Type::I32; }
            return true;
        }
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& I = std::get<E::Ident>(e.node);
            auto it = locals.find(I.name);
            if (it != locals.end()) {
                if (outTy) *outTy = it->second;
                return true;
            }
            // Maybe it's an enum variant: find which enum defines it
            for (auto& [ename, vars] : enums) {
                auto vit = vars.find(I.name);
                if (vit != vars.end()) {
                    if (outTy) { outTy->kind = innesce::ast::Type::ENUM; outTy->enum_name = ename; }
                    return true;
                }
            }
            err = "use of undeclared identifier: " + I.name; return false;
        }
        if (std::holds_alternative<E::Unary>(e.node)) {
            const auto& U = std::get<E::Unary>(e.node);
            innesce::ast::Type t;
            if (!check_expr(*U.rhs, locals, enums, err, &t)) return false;
            if (t.kind != innesce::ast::Type::I32) { err = "unary operator on non-i32"; return false; }
            if (outTy) { outTy->kind = innesce::ast::Type::I32; }
            return true;
        }
        const auto& B = std::get<E::Binary>(e.node);
        innesce::ast::Type lt, rt;
        if (!check_expr(*B.lhs, locals, enums, err, &lt)) return false;
        if (!check_expr(*B.rhs, locals, enums, err, &rt)) return false;
        if (lt.kind != innesce::ast::Type::I32 || rt.kind != innesce::ast::Type::I32) {
            err = "binary arithmetic requires i32"; return false;
        }
        if (outTy) { outTy->kind = innesce::ast::Type::I32; }
        return true;
    }

    static bool check_block(const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, innesce::ast::Type> locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::string& err) {
        using S = innesce::ast::Stmt;
        for (auto& st : body) {
            if (std::holds_alternative<S::Let>(st.node)) {
                const auto& L = std::get<S::Let>(st.node);
                innesce::ast::Type t;
                if (!check_expr(L.init, locals, enums, err, &t)) return false;
                // allow assigning enum variant to enum var or int to i32
                if (L.type.kind == innesce::ast::Type::I32 && t.kind != innesce::ast::Type::I32) {
                    err = "type mismatch: expected i32"; return false;
                }
                if (L.type.kind == innesce::ast::Type::ENUM) {
                    if (t.kind != innesce::ast::Type::ENUM || t.enum_name != L.type.enum_name) {
                        err = "type mismatch: expected enum " + L.type.enum_name; return false;
                    }
                }
                locals[L.name] = L.type;
            }
            else if (std::holds_alternative<S::Return>(st.node)) {
                const auto& R = std::get<S::Return>(st.node);
                innesce::ast::Type t;
                if (!check_expr(R.value, locals, enums, err, &t)) return false;
                // function return type is validated elsewhere or trusted i32 for now
            }
            else if (std::holds_alternative<S::If>(st.node)) {
                const auto& I = std::get<S::If>(st.node);
                innesce::ast::Type t;
                if (!check_expr(I.cond, locals, enums, err, &t)) return false;
                if (!check_block(I.then_body, locals, enums, err)) return false;
                if (!check_block(I.else_body, locals, enums, err)) return false;
            }
            else if (std::holds_alternative<S::Match>(st.node)) {
                const auto& M = std::get<S::Match>(st.node);
                innesce::ast::Type t;
                if (!check_expr(M.scrutinee, locals, enums, err, &t)) return false;
                // Determine enum type and variant set
                std::set<std::string> enum_variants;
                bool has_default = false;
                if (t.kind == innesce::ast::Type::ENUM) {
                    auto it = enums.find(t.enum_name);
                    if (it == enums.end()) { err = "unknown enum type in match"; return false; }
                    for (auto& kv : it->second) enum_variants.insert(kv.first);
                }
                std::set<std::string> seen;
                for (auto& C : M.cases) {
                    if (C.is_default) { has_default = true; }
                    else {
                        if (seen.count(C.label)) { err = "duplicate case label: " + C.label; return false; }
                        seen.insert(C.label);
                        if (!enum_variants.empty() && !enum_variants.count(C.label)) {
                            err = "label not in enum " + t.enum_name + ": " + C.label; return false;
                        }
                    }
                    if (!check_block(C.body, locals, enums, err)) return false;
                }
                if (!has_default && !enum_variants.empty()) {
                    for (auto& v : enum_variants) if (!seen.count(v)) { err = "non-exhaustive match, missing: " + v; return false; }
                }
            }
        }
        return true;
    }

    SemaResult Sema::check(const ast::Unit& u) {
        // Load enums
        enums_.clear();
        for (auto& e : u.enums) {
            std::unordered_map<std::string, int> m;
            for (int i = 0; i < (int)e.variants.size(); ++i) m[e.variants[i]] = i;
            enums_[e.name] = std::move(m);
        }

        bool has_main = false;
        for (auto& f : u.functions) {
            if (f.name == "main") has_main = true;
            std::unordered_map<std::string, innesce::ast::Type> locals;
            std::string err;
            if (!check_block(f.body, locals, enums_, err)) return { err };
        }
        if (!has_main) return { "missing 'main' function" };
        return {};
    }

} // namespace innesce::front
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

        // Expose enum info for codegen
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums() const { return enums_; }
    private:
        std::unordered_map<std::string, std::unordered_map<std::string, int>> enums_;
    };

} // namespace innesce::front
#pragma once
#include <string_view>

namespace innesce {

    // Compile-time/no-op checkpoint markers; can be wired to profiling later.
    inline void checkpoint(std::string_view) noexcept {
        // no-op (placeholders for future instrumentation)
    }

} // namespace innesce
#pragma once
#include <cstdint>
#include <concepts>
#include <compare>
#include <type_traits>

namespace innesce {

    // Strong unit duration (integral count of base ticks), similar to std::chrono but minimal.
    // We intentionally do NOT mix units implicitly; conversions are explicit.
    template <typename UnitTag>
    struct Duration {
        using rep = std::int64_t;
        rep value{ 0 };

        constexpr Duration() = default;
        constexpr explicit Duration(rep v) : value(v) {}

        // Comparisons
        constexpr auto operator<=>(const Duration&) const = default;

        // Arithmetic (same-unit only)
        constexpr Duration operator+(Duration other) const { return Duration{ value + other.value }; }
        constexpr Duration operator-(Duration other) const { return Duration{ value - other.value }; }
        constexpr Duration& operator+=(Duration other) { value += other.value; return *this; }
        constexpr Duration& operator-=(Duration other) { value -= other.value; return *this; }

        // Scale by scalar
        template <std::integral I>
        constexpr Duration operator*(I k) const { return Duration{ value * static_cast<rep>(k) }; }

        template <std::integral I>
        constexpr Duration operator/(I k) const { return Duration{ value / static_cast<rep>(k) }; }
    };

    // Unit tags
    struct ns_tag {}; struct us_tag {}; struct ms_tag {}; struct sec_tag {}; struct min_tag {}; struct hr_tag {};

    using ns = Duration<ns_tag>;
    using us = Duration<us_tag>;
    using ms = Duration<ms_tag>;
    using sec = Duration<sec_tag>;
    using min = Duration<min_tag>;
    using hr = Duration<hr_tag>;

    // Explicit converters (compile-time factors)
    constexpr us to_us(ns x) { return us{ x.value / 1000 }; }
    constexpr ms to_ms(us x) { return ms{ x.value / 1000 }; }
    constexpr sec to_sec(ms x) { return sec{ x.value / 1000 }; }
    constexpr ms  to_ms(sec x) { return ms{ x.value * 1000 }; }
    constexpr us  to_us(ms x) { return us{ x.value * 1000 }; }
    constexpr ns  to_ns(us x) { return ns{ x.value * 1000 }; }

    // Literals helpers
    constexpr ns  operator""_ns(unsigned long long v) { return ns{ static_cast<ns::rep>(v) }; }
    constexpr us  operator""_us(unsigned long long v) { return us{ static_cast<us::rep>(v) * 1000 }; }
    constexpr ms  operator""_ms(unsigned long long v) { return ms{ static_cast<ms::rep>(v) * 1000 * 1000 }; }
    constexpr sec operator""_sec(unsigned long long v) { return sec{ static_cast<sec::rep>(v) }; }

} // namespace innesce
#include "core/lanes.hpp"
// Implementation currently header-only for simplicity; TU exists for future expansion.
#pragma once
#include <vector>
#include <thread>
#include <functional>
#include <barrier>
#include <atomic>
#include <cstddef>
#include <cassert>

namespace innesce {

    // Deterministic lanes: fixed number of worker threads; tasks are partitioned by index.
    class Lanes {
    public:
        explicit Lanes(std::size_t count)
            : count_(count)
        {
            assert(count_ > 0);
        }

        std::size_t size() const noexcept { return count_; }

        // Parallel for over [begin, end) with step 'chunk'.
        // Each lane operates on indices i where (i / chunk) % lane_count == lane_id.
        void parallel_for(std::size_t begin, std::size_t end, std::size_t chunk,
            const std::function<void(std::size_t, std::size_t)>& body) const
        {
            const std::size_t lanes = count_;
            std::vector<std::jthread> threads;
            threads.reserve(lanes);
            for (std::size_t lane = 0; lane < lanes; ++lane) {
                threads.emplace_back([=]() {
                    for (std::size_t i = begin; i < end; i += chunk) {
                        std::size_t slice = (i - begin) / chunk;
                        if ((slice % lanes) == lane) {
                            std::size_t hi = i + chunk;
                            if (hi > end) hi = end;
                            body(i, hi);
                        }
                    }
                    });
            }
            // join on destruction of jthreads
        }

        // Packetize into frames; barrier after each frame.
        void packetize_frames(std::size_t begin, std::size_t end, std::size_t frame,
            const std::function<void(std::size_t, std::size_t)>& body) const
        {
            const std::size_t lanes = count_;
            std::atomic<std::size_t> next{ begin };
            std::barrier sync(static_cast<std::ptrdiff_t>(lanes));
            std::vector<std::jthread> threads;
            threads.reserve(lanes);
            for (std::size_t lane = 0; lane < lanes; ++lane) {
                threads.emplace_back([&]() {
                    while (true) {
                        std::size_t i = next.fetch_add(frame);
                        if (i >= end) break;
                        std::size_t hi = i + frame;
                        if (hi > end) hi = end;
                        body(i, hi);
                        sync.arrive_and_wait();
                    }
                    });
            }
        }

    private:
        std::size_t count_;
    };

} // namespace innesce
#pragma once
#include <string>
#include <vector>
#include <utility>
#include <optional>

namespace innesce {

    struct Quarantine {
        bool failed{ false };
        std::vector<std::string> notes;

        void note(std::string msg) {
            notes.emplace_back(std::move(msg));
        }

        // Attempt wrapper: F must return std::optional<T>. If empty => failure recorded.
        template <typename F>
        auto attempt(F&& f) -> decltype(f()) {
            auto r = f();
            if (!r.has_value()) {
                failed = true;
                note("operation failed");
            }
            return r;
        }
    };

} // namespace innesce
#pragma once
#include <cstdint>

namespace innesce {

    enum class Truth : std::uint8_t { False = 0, True = 1, Unknown = 2, Both = 3 };

    // 4-valued truth operations (Kleene-like; tweak as desired)
    constexpr Truth t_not(Truth a) {
        switch (a) {
        case Truth::False:   return Truth::True;
        case Truth::True:    return Truth::False;
        case Truth::Unknown: return Truth::Unknown;
        case Truth::Both:    return Truth::Unknown; // design choice: ¬Both = Unknown
        }
        return Truth::Unknown;
    }

    constexpr Truth t_and(Truth a, Truth b) {
        // truth table encoded; symmetric
        if (a == Truth::False || b == Truth::False) return Truth::False;
        if (a == Truth::True && b == Truth::True)  return Truth::True;
        if (a == Truth::Both && b == Truth::True)  return Truth::Both;
        if (b == Truth::Both && a == Truth::True)  return Truth::Both;
        if (a == Truth::Both && b == Truth::Both)  return Truth::Both;
        // any combo with Unknown that isn't already decided:
        return Truth::Unknown;
    }

    constexpr Truth t_or(Truth a, Truth b) {
        if (a == Truth::True || b == Truth::True) return Truth::True;
        if (a == Truth::False && b == Truth::False) return Truth::False;
        if (a == Truth::Both || b == Truth::Both) return Truth::Both;
        return Truth::Unknown;
    }

} // namespace innesce
#pragma once
#include <string>
#include "frontend/ast.hpp"

namespace innesce::backend::llvm {

    struct IrBuilderConfig {
        bool optimize{ true };
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
#include "backend/llvm/ir_builder.hpp"
#include "frontend/ast.hpp"
#include <iostream>

using namespace innesce::backend::llvm;
using namespace innesce;

void IrBuilder::compile(const ast::Program& prog) {
    std::cout << "; Innesce IR dump (stub)\n";
    for (auto& st : prog.stmts) {
        if (std::holds_alternative<ast::LetStmt>(st->node)) {
            auto& ls = std::get<ast::LetStmt>(st->node);
            if (std::holds_alternative<ast::IntLit>(ls.init->node)) {
                auto v = std::get<ast::IntLit>(ls.init->node).value;
                std::cout << "%" << ls.name << " = const i64 " << v << "\n";
            }
        }
        else if (std::holds_alternative<ast::ReturnStmt>(st->node)) {
            auto& rs = std::get<ast::ReturnStmt>(st->node);
            if (std::holds_alternative<ast::IntLit>(rs->node)) {
                auto v = std::get<ast::IntLit>(rs->node).value;
                std::cout << "ret i64 " << v << "\n";
            }
        }
    }
}
#pragma once
#include "frontend/ast.hpp"
#include <string>

namespace innesce::backend::llvm {

    // Returns empty string on success; otherwise an error message.
    std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path);

} // namespace innesce::backend::llvm
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
    for (int i = 2; i < argc; i++) {
        std::string a = argv[i];
        if (a == "-o" && i + 1 < argc) { out = argv[++i]; continue; }
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
#include "backend/llvm/codegen.hpp"
#include "backend/llvm/ir_builder.hpp"
#include "frontend/sema.hpp" // for enums mapping in future (we'll pass explicit mapping from caller)

#ifdef INNSCE_HAVE_LLVM
#include <llvm/ADT/Triple.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/MC/TargetOptions.h>
#include <system_error>
#include <unordered_map>
#include <variant>
#include <optional>
#endif

namespace innesce::backend::llvm {

#ifndef INNSCE_HAVE_LLVM
    std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path) {
        (void)unit; (void)obj_path;
        return "LLVM backend disabled: rebuild with -DINNSCE_ENABLE_LLVM=ON";
    }
#else

    using namespace ::llvm;

    static Value* emit_expr(IRBuilder<>& B,
        const innesce::ast::Expr& e,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums) {
        using E = innesce::ast::Expr;
        if (std::holds_alternative<E::IntLit>(e.node)) {
            int v = std::get<E::IntLit>(e.node).value;
            return B.getInt32(v);
        }
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& I = std::get<E::Ident>(e.node);
            auto it = locals.find(I.name);
            if (it != locals.end()) {
                return B.CreateLoad(B.getInt32Ty(), it->second);
            }
            // maybe enum variant
            for (auto& [ename, vars] : enums) {
                auto vit = vars.find(I.name);
                if (vit != vars.end()) {
                    return B.getInt32(vit->second);
                }
            }
            return nullptr;
        }
        if (std::holds_alternative<E::Unary>(e.node)) {
            const auto& U = std::get<E::Unary>(e.node);
            Value* rhs = emit_expr(B, *U.rhs, locals, enums);
            if (!rhs) return nullptr;
            if (U.op == '-') return B.CreateNeg(rhs);
            return rhs;
        }
        const auto& BN = std::get<E::Binary>(e.node);
        Value* L = emit_expr(B, *BN.lhs, locals, enums);
        Value* R = emit_expr(B, *BN.rhs, locals, enums);
        if (!L || !R) return nullptr;
        switch (BN.op) {
        case '+': return B.CreateAdd(L, R);
        case '-': return B.CreateSub(L, R);
        case '*': return B.CreateMul(L, R);
        case '/': return B.CreateSDiv(L, R);
        }
        return nullptr;
    }

    static bool emit_block(IRBuilder<>& B,
        Function* F,
        const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums);

    static bool emit_match(IRBuilder<>& B,
        Function* F,
        const innesce::ast::Stmt::Match& M,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums) {
        Value* scr = emit_expr(B, M.scrutinee, locals, enums);
        if (!scr) return false;

        BasicBlock* contBB = BasicBlock::Create(B.getContext(), "match.cont");
        // build case blocks
        std::vector<BasicBlock*> caseBBs;
        BasicBlock* defaultBB = nullptr;
        for (auto& C : M.cases) {
            if (C.is_default) {
                defaultBB = BasicBlock::Create(B.getContext(), "case.default");
            }
            else {
                caseBBs.push_back(BasicBlock::Create(B.getContext(), ("case." + C.label).c_str()));
            }
        }
        if (!defaultBB) defaultBB = contBB;

        // switch instruction
        SwitchInst* sw = B.CreateSwitch(scr, defaultBB, (unsigned)caseBBs.size());

        // Attach cases
        unsigned idx = 0;
        for (auto& C : M.cases) {
            if (C.is_default) continue;
            int val = -1;
            // map label to int
            for (auto& [ename, vars] : enums) {
                auto it = vars.find(C.label);
                if (it != vars.end()) { val = it->second; break; }
            }
            if (val < 0) return false;
            BasicBlock* bb = caseBBs[idx++];
            F->getBasicBlockList().push_back(bb);
            sw->addCase(B.getInt32(val), bb);
            B.SetInsertPoint(bb);
            auto locals_case = locals;
            bool returned = emit_block(B, F, C.body, locals_case, enums);
            if (!returned) B.CreateBr(contBB);
        }

        // default
        if (defaultBB != contBB) {
            F->getBasicBlockList().push_back(defaultBB);
            B.SetInsertPoint(defaultBB);
            auto locals_def = locals;
            // empty default => just fallthrough
            for (auto& C : M.cases) {
                if (C.is_default) {
                    bool returned = emit_block(B, F, C.body, locals_def, enums);
                    if (!returned) B.CreateBr(contBB);
                    break;
                }
            }
        }

        // continue
        F->getBasicBlockList().push_back(contBB);
        B.SetInsertPoint(contBB);
        return false;
    }

    static bool emit_block(IRBuilder<>& B,
        Function* F,
        const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums) {
        using S = innesce::ast::Stmt;
        for (auto& st : body) {
            if (std::holds_alternative<S::Let>(st.node)) {
                const auto& L = std::get<S::Let>(st.node);
                AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, L.name);
                Value* init = emit_expr(B, L.init, locals, enums);
                if (!init) return false;
                B.CreateStore(init, a);
                locals[L.name] = a;
            }
            else if (std::holds_alternative<S::Return>(st.node)) {
                const auto& R = std::get<S::Return>(st.node);
                Value* v = emit_expr(B, R.value, locals, enums);
                if (!v) return false;
                B.CreateRet(v);
                return true;
            }
            else if (std::holds_alternative<S::If>(st.node)) {
                const auto& I = std::get<S::If>(st.node);
                Value* condv = emit_expr(B, I.cond, locals, enums);
                if (!condv) return false;
                Value* istrue = B.CreateICmpNE(condv, B.getInt32(0));
                BasicBlock* thenBB = BasicBlock::Create(B.getContext(), "then", F);
                BasicBlock* elseBB = BasicBlock::Create(B.getContext(), "else");
                BasicBlock* contBB = BasicBlock::Create(B.getContext(), "endif");
                B.CreateCondBr(istrue, thenBB, I.else_body.empty() ? contBB : elseBB);
                // then
                B.SetInsertPoint(thenBB);
                auto locals_then = locals;
                bool thenReturned = emit_block(B, F, I.then_body, locals_then, enums);
                if (!thenReturned) B.CreateBr(contBB);
                if (!I.else_body.empty()) {
                    F->getBasicBlockList().push_back(elseBB);
                    B.SetInsertPoint(elseBB);
                    auto locals_else = locals;
                    bool elseReturned = emit_block(B, F, I.else_body, locals_else, enums);
                    if (!elseReturned) B.CreateBr(contBB);
                }
                F->getBasicBlockList().push_back(contBB);
                B.SetInsertPoint(contBB);
            }
            else if (std::holds_alternative<S::Match>(st.node)) {
                if (!emit_match(B, F, std::get<S::Match>(st.node), locals, enums)) return false;
            }
        }
        return false;
    }

    std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path) {
        // Build enum map from unit
        std::unordered_map<std::string, std::unordered_map<std::string, int>> enums;
        for (auto& e : unit.enums) {
            std::unordered_map<std::string, int> m;
            for (int i = 0; i < (int)e.variants.size(); ++i) m[e.variants[i]] = i;
            enums[e.name] = std::move(m);
        }

        // Find main()
        const innesce::ast::Function* mainFn = nullptr;
        for (auto& f : unit.functions) if (f.name == "main") { mainFn = &f; break; }
        if (!mainFn) return "no main() in unit";

        LLVMContext ctx;
        auto mod = std::make_unique<Module>("innesce_mod", ctx);
        IRBuilder<> B(ctx);

        // int main()
        FunctionType* fty = FunctionType::get(B.getInt32Ty(), false);
        Function* F = Function::Create(fty, Function::ExternalLinkage, "main", mod.get());
        BasicBlock* entry = BasicBlock::Create(ctx, "entry", F);
        B.SetInsertPoint(entry);

        std::unordered_map<std::string, Value*> locals;
        bool returned = emit_block(B, F, mainFn->body, locals, enums);
        if (!returned) {
            B.CreateRet(B.getInt32(0));
        }

        if (verifyFunction(*F, &errs())) return "IR verification failed for main()";
        if (verifyModule(*mod, &errs())) return "Module verification failed";

        // Target setup
        InitializeAllTargetInfos();
        InitializeAllTargets();
        InitializeAllTargetMCs();
        InitializeAllAsmParsers();
        InitializeAllAsmPrinters();

        auto triple = sys::getDefaultTargetTriple();
        std::string err;
        const Target* target = TargetRegistry::lookupTarget(triple, err);
        if (!target) return "lookupTarget failed: " + err;

        TargetOptions opt;
        auto relocModel = std::optional<Reloc::Model>();
        std::unique_ptr<TargetMachine> TM(target->createTargetMachine(triple, "generic", "", opt, relocModel));
        mod->setTargetTriple(triple);
        mod->setDataLayout(TM->createDataLayout());

        std::error_code ec;
        raw_fd_ostream dest(obj_path, ec, sys::fs::OF_None);
        if (ec) return "could not open object file: " + ec.message();

        legacy::PassManager pm;
        if (TM->addPassesToEmitFile(pm, dest, nullptr, CodeGenFileType::CGFT_ObjectFile)) {
            return "TargetMachine cannot emit this file type";
        }
        pm.run(*mod);
        dest.flush();
        return {};
    }

#endif // INNSCE_HAVE_LLVM

} // namespace innesce::backend::llvm
#pragma once
#include "frontend/ast.hpp"
#include <string>

namespace innesce::backend::llvm {

    // Returns empty string on success; otherwise an error message.
    std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path);

} // namespace innesce::backend::llvm
#include "backend/llvm/ir_builder.hpp"
#include "frontend/ast.hpp"
#include <iostream>

using namespace innesce::backend::llvm;
using namespace innesce;

void IrBuilder::compile(const ast::Program& prog) {
    std::cout << "; Innesce IR dump (stub)\n";
    for (auto& st : prog.stmts) {
        if (std::holds_alternative<ast::LetStmt>(st->node)) {
            auto& ls = std::get<ast::LetStmt>(st->node);
            if (std::holds_alternative<ast::IntLit>(ls.init->node)) {
                auto v = std::get<ast::IntLit>(ls.init->node).value;
                std::cout << "%" << ls.name << " = const i64 " << v << "\n";
            }
        }
        else if (std::holds_alternative<ast::ReturnStmt>(st->node)) {
            auto& rs = std::get<ast::ReturnStmt>(st->node);
            if (std::holds_alternative<ast::IntLit>(rs->node)) {
                auto v = std::get<ast::IntLit>(rs->node).value;
                std::cout << "ret i64 " << v << "\n";
            }
        }
    }
}
#pragma once
#include <string>
#include "frontend/ast.hpp"

namespace innesce::backend::llvm {

    struct IrBuilderConfig {
        bool optimize{ true };
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
#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <variant>

namespace innesce::ast {

    struct Type {
        enum Kind { I32, ENUM } kind{ I32 };
        std::string enum_name; // if kind==ENUM
    };

    struct Expr {
        struct IntLit { int value; };
        struct Ident { std::string name; };     // local var or enum variant
        struct Unary { char op; std::unique_ptr<Expr> rhs; };
        struct Binary { char op; std::unique_ptr<Expr> lhs, rhs; };
        std::variant<IntLit, Ident, Unary, Binary> node;
    };

    struct Stmt {
        struct Let {
            std::string name;
            Type type;
            Expr init;
        };
        struct Return {
            Expr value;
        };
        struct If {
            Expr cond;
            std::vector<Stmt> then_body;
            std::vector<Stmt> else_body;
        };
        struct Match {
            Expr scrutinee;
            struct Case { std::string label; std::vector<Stmt> body; bool is_default{ false }; };
            std::vector<Case> cases;
        };
        std::variant<Let, Return, If, Match> node;
    };

    struct EnumDecl {
        std::string name;
        std::vector<std::string> variants;
    };

    struct Function {
        std::string name;
        Type ret;
        std::vector<Stmt> body;
    };

    struct Unit {
        std::vector<EnumDecl> enums;
        std::vector<Function> functions;
    };

} // namespace innesce::ast
#include "frontend/lexer.hpp"
#include <cctype>

namespace innesce::front {

    Lexer::Lexer(std::string_view src) : s_(src) {}

    char Lexer::peek() const { return i_ < s_.size() ? s_[i_] : '\0'; }
    char Lexer::get() {
        if (i_ >= s_.size()) return '\0';
        char c = s_[i_++];
        if (c == '\n') { line_++; col_ = 1; }
        else { col_++; }
        return c;
    }

    void Lexer::skip_ws_and_comments() {
        while (true) {
            char c = peek();
            if (c == '\0') break;
            if (std::isspace(static_cast<unsigned char>(c))) { get(); continue; }
            if (c == '-' && i_ + 1 < s_.size() && s_[i_ + 1] == '-') {
                while (c != '\n' && c != '\0') c = get();
                continue;
            }
            break;
        }
    }

    Token Lexer::make(TokKind k, std::string t) {
        Token x{ k, std::move(t), 0, line_, col_ };
        return x;
    }

    Token Lexer::next() {
        skip_ws_and_comments();
        char c = peek();
        if (c == '\0') return make(TokKind::End, "");

        // punctuation/operators
        if (c == '(') { get(); return make(TokKind::LParen, "("); }
        if (c == ')') { get(); return make(TokKind::RParen, ")"); }
        if (c == '{') { get(); return make(TokKind::LBrace, "{"); }
        if (c == '}') { get(); return make(TokKind::RBrace, "}"); }
        if (c == '+') { get(); return make(TokKind::Plus, "+"); }
        if (c == '-') {
            if (i_ + 1 < s_.size() && s_[i_ + 1] == '>') { get(); get(); return make(TokKind::Arrow, "->"); }
            get(); return make(TokKind::Minus, "-");
        }
        if (c == '*') { get(); return make(TokKind::Star, "*"); }
        if (c == '/') { get(); return make(TokKind::Slash, "/"); }
        if (c == ':') {
            get();
            if (peek() == '=') { get(); return make(TokKind::Assign, ":="); }
            return make(TokKind::Colon, ":");
        }
        if (c == ';') { get(); return make(TokKind::Semicolon, ";"); }
        if (c == ',') { get(); return make(TokKind::Comma, ","); }
        if (c == '=') {
            if (i_ + 1 < s_.size() && s_[i_ + 1] == '>') { get(); get(); return make(TokKind::FatArrow, "=>"); }
        }

        // number
        if (std::isdigit(static_cast<unsigned char>(c))) {
            int v = 0;
            while (std::isdigit(static_cast<unsigned char>(peek()))) {
                v = v * 10 + (get() - '0');
            }
            Token t = make(TokKind::Int, "");
            t.int_val = v;
            return t;
        }

        // identifier/keyword
        if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
            std::string id;
            while (std::isalnum(static_cast<unsigned char>(peek())) || peek() == '_') {
                id.push_back(get());
            }
            if (id == "fn") return make(TokKind::KwFn, id);
            if (id == "is") return make(TokKind::KwIs, id);
            if (id == "end") return make(TokKind::KwEnd, id);
            if (id == "return") return make(TokKind::KwReturn, id);
            if (id == "let") return make(TokKind::KwLet, id);
            if (id == "i32") return make(TokKind::KwI32, id);
            if (id == "if") return make(TokKind::KwIf, id);
            if (id == "then") return make(TokKind::KwThen, id);
            if (id == "else") return make(TokKind::KwElse, id);
            if (id == "type") return make(TokKind::KwType, id);
            if (id == "enum") return make(TokKind::KwEnum, id);
            if (id == "match") return make(TokKind::KwMatch, id);
            if (id == "case") return make(TokKind::KwCase, id);
            if (id == "default") return make(TokKind::KwDefault, id);
            Token t = make(TokKind::Ident, id);
            t.text = id;
            return t;
        }

        // unknown char -> skip
        get();
        return next();
    }

} // namespace innesce::front
#pragma once
#include <string>
#include <string_view>
#include <vector>

namespace innesce::front {

    enum class TokKind {
        End,
        Ident,
        Int,
        // keywords
        KwFn, KwIs, KwEnd, KwReturn, KwLet,
        KwI32, KwIf, KwThen, KwElse,
        KwType, KwEnum, KwMatch, KwCase, KwDefault,
        // punctuation
        LParen, RParen, LBrace, RBrace,
        Colon, Semicolon, Comma,
        Arrow, Assign, FatArrow, // "->" and ":=" and "=>"
        // ops
        Plus, Minus, Star, Slash,
    };

    struct Token {
        TokKind kind;
        std::string text;
        int int_val{ 0 };
        int line{ 1 }, col{ 1 };
    };

    struct Lexer {
        explicit Lexer(std::string_view src);
        Token next();
    private:
        std::string_view s_;
        size_t i_{ 0 };
        int line_{ 1 }, col_{ 1 };
        char peek() const;
        char get();
        void skip_ws_and_comments();
        Token make(TokKind k, std::string t);
    };

} // namespace innesce::front
#include "frontend/parser.hpp"
#include <iostream>
#include <memory>

namespace innesce::front {

    Parser::Parser(std::string_view src) : lex_(src) { bump(); }

    void Parser::bump() { cur_ = lex_.next(); }
    bool Parser::accept(TokKind k) { if (cur_.kind == k) { bump(); return true; } return false; }
    bool Parser::expect(TokKind k, const char* what) {
        if (!accept(k)) { std::cerr << "Expected " << what << "\n"; return false; }
        return true;
    }

    std::optional<ast::Unit> Parser::parse_unit() {
        ast::Unit u;
        while (cur_.kind != TokKind::End) {
            if (cur_.kind == TokKind::KwType) {
                auto e = parse_enum_decl();
                if (!e) return std::nullopt;
                u.enums.push_back(std::move(*e));
                continue;
            }
            auto fn = parse_function();
            if (!fn) return std::nullopt;
            u.functions.push_back(std::move(*fn));
        }
        return u;
    }

    std::optional<ast::EnumDecl> Parser::parse_enum_decl() {
        if (!expect(TokKind::KwType, "'type'")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected enum name\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        if (!expect(TokKind::KwEnum, "'enum'")) return std::nullopt;
        if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
        ast::EnumDecl d; d.name = std::move(name);
        // variants
        bool first = true;
        while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
            if (!first) expect(TokKind::Comma, "','");
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected variant name\n"; return std::nullopt; }
            d.variants.push_back(cur_.text);
            bump();
            first = false;
        }
        if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
        expect(TokKind::Semicolon, "';'"); // optional; won't error if missing
        return d;
    }

    std::optional<ast::Function> Parser::parse_function() {
        if (!expect(TokKind::KwFn, "'fn'")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected function name\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::LParen, "'('")) return std::nullopt;
        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
        if (!expect(TokKind::Arrow, "'->'")) return std::nullopt;
        ast::Type ret;
        if (accept(TokKind::KwI32)) {
            ret.kind = ast::Type::I32;
        }
        else if (cur_.kind == TokKind::Ident) {
            ret.kind = ast::Type::ENUM; ret.enum_name = cur_.text; bump();
        }
        else {
            std::cerr << "Expected return type\n"; return std::nullopt;
        }
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;

        ast::Function f; f.name = std::move(name); f.ret = ret;
        while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt();
            if (!st) return std::nullopt;
            f.body.push_back(std::move(*st));
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        return f;
    }

    std::optional<ast::Stmt> Parser::parse_match() {
        // match <expr> is ... end
        if (!expect(TokKind::KwMatch, "'match'")) return std::nullopt;
        auto scrut = parse_expr(); if (!scrut) return std::nullopt;
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        ast::Stmt::Match m; m.scrutinee = std::move(*scrut);
        while (cur_.kind == TokKind::KwCase) {
            bump();
            bool is_default = false;
            std::string label;
            if (accept(TokKind::KwDefault)) {
                is_default = true;
            }
            else {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected case label\n"; return std::nullopt; }
                label = cur_.text; bump();
            }
            if (!expect(TokKind::FatArrow, "'=>'")) return std::nullopt;
            auto body = parse_case_body();
            m.cases.push_back(ast::Stmt::Match::Case{ std::move(label), std::move(body), is_default });
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        ast::Stmt st; st.node = std::move(m);
        return st;
    }

    std::vector<ast::Stmt> Parser::parse_case_body() {
        std::vector<ast::Stmt> body;
        while (cur_.kind != TokKind::KwCase && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt();
            if (!st) break;
            body.push_back(std::move(*st));
        }
        return body;
    }

    std::optional<ast::Stmt> Parser::parse_stmt() {
        if (cur_.kind == TokKind::KwLet) {
            bump();
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected identifier after let\n"; return std::nullopt; }
            std::string name = cur_.text; bump();
            if (!expect(TokKind::Colon, "':'")) return std::nullopt;
            ast::Type ty;
            if (accept(TokKind::KwI32)) {
                ty.kind = ast::Type::I32;
            }
            else if (cur_.kind == TokKind::Ident) {
                ty.kind = ast::Type::ENUM; ty.enum_name = cur_.text; bump();
            }
            else {
                std::cerr << "Expected type name after ':'\n"; return std::nullopt;
            }
            if (!expect(TokKind::Assign, "':='")) return std::nullopt;
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Let{ std::move(name), ty, std::move(*e) };
            return st;
        }
        if (cur_.kind == TokKind::KwReturn) {
            bump();
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Return{ std::move(*e) };
            return st;
        }
        if (cur_.kind == TokKind::KwIf) {
            bump();
            auto cond = parse_expr(); if (!cond) return std::nullopt;
            if (!expect(TokKind::KwThen, "'then'")) return std::nullopt;
            // then body until 'else' or 'end'
            std::vector<ast::Stmt> then_body;
            while (cur_.kind != TokKind::KwElse && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                auto st = parse_stmt();
                if (!st) return std::nullopt;
                then_body.push_back(std::move(*st));
            }
            std::vector<ast::Stmt> else_body;
            if (accept(TokKind::KwElse)) {
                while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                    auto st = parse_stmt();
                    if (!st) return std::nullopt;
                    else_body.push_back(std::move(*st));
                }
            }
            if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::If{ std::move(*cond), std::move(then_body), std::move(else_body) };
            return st;
        }
        if (cur_.kind == TokKind::KwMatch) {
            return parse_match();
        }
        if (cur_.kind == TokKind::KwSleep) {
            bump();
            if (!expect(TokKind::LParen, "'('")) return std::nullopt;
            auto amt = parse_expr(); if (!amt) return std::nullopt;
            bool is_ms = true;
            if (accept(TokKind::KwMS)) { is_ms = true; }
            else if (accept(TokKind::KwSEC)) { is_ms = false; }
            else { std::cerr << "Expected unit ms or sec in sleep()\n"; return std::nullopt; }
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Sleep{ std::move(*amt), is_ms };
            return st;
        }
        if (cur_.kind == TokKind::KwFail) {
            bump();
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Fail{};
            return st;
        }
        if (cur_.kind == TokKind::KwQuarantine) {
            bump();
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected quarantine name"; return std::nullopt; }
            std::string name = cur_.text; bump();
            if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
            std::vector<ast::Stmt> body;
            while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                auto st = parse_stmt(); if (!st) return std::nullopt; body.push_back(std::move(*st));
            }
            if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Quarantine{ std::move(name), std::move(body) };
            return st;
        }
        std::cerr << "Unknown statement\n";
        return std::nullopt;
    }

    std::optional<ast::Expr> Parser::parse_expr() { return parse_add(); }

    std::optional<ast::Expr> Parser::parse_add() {
        auto lhs = parse_mul(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Plus || cur_.kind == TokKind::Minus) {
            char op = (cur_.kind == TokKind::Plus) ? '+' : '-';
            bump();
            auto rhs = parse_mul(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Binary{ op, std::make_unique<ast::Expr>(std::move(*lhs)), std::make_unique<ast::Expr>(std::move(*rhs)) };
            lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_mul() {
        auto lhs = parse_unary(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Star || cur_.kind == TokKind::Slash) {
            char op = (cur_.kind == TokKind::Star) ? '*' : '/';
            bump();
            auto rhs = parse_unary(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Binary{ op, std::make_unique<ast::Expr>(std::move(*lhs)), std::make_unique<ast::Expr>(std::move(*rhs)) };
            lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_unary() {
        if (cur_.kind == TokKind::Minus) {
            bump();
            auto rhs = parse_unary(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Unary{ '-', std::make_unique<ast::Expr>(std::move(*rhs)) };
            return e;
        }
        return parse_primary();
    }

    std::optional<ast::Expr> Parser::parse_primary() {
        if (cur_.kind == TokKind::Int) {
            ast::Expr e; e.node = ast::Expr::IntLit{ cur_.int_val }; bump(); return e;
        }
        if (cur_.kind == TokKind::Ident) {
            ast::Expr e; e.node = ast::Expr::Ident{ cur_.text }; bump(); return e;
        }
        if (cur_.kind == TokKind::KwIsFailed) {
            bump();
            if (!expect(TokKind::LParen, "'('")) return std::nullopt;
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected name in isfailed()"; return std::nullopt; }
            std::string q = cur_.text; bump();
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            ast::Expr e; e.node = ast::Expr::IsFailed{ std::move(q) }; return e;
        }
        if (accept(TokKind::LParen)) {
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            return e;
        }
        std::cerr << "Expected expression\n";
        return std::nullopt;
    }

} // namespace innesce::front
#pragma once
#include "frontend/lexer.hpp"
#include "frontend/ast.hpp"
#include <optional>

namespace innesce::front {

    class Parser {
    public:
        explicit Parser(std::string_view src);
        std::optional<ast::Unit> parse_unit();

    private:
        Lexer lex_;
        Token cur_;

        void bump();
        bool accept(TokKind k);
        bool expect(TokKind k, const char* what);

        std::optional<ast::EnumDecl> parse_enum_decl();
        std::optional<ast::Function> parse_function();
        std::optional<ast::Stmt> parse_stmt();
        std::optional<ast::Stmt> parse_match();
        std::optional<ast::Expr> parse_expr();
        std::optional<ast::Expr> parse_add();
        std::optional<ast::Expr> parse_mul();
        std::optional<ast::Expr> parse_unary();
        std::optional<ast::Expr> parse_primary();

        std::vector<ast::Stmt> parse_case_body();
    };

} // namespace innesce::front
#include "frontend/sema.hpp"
#include <unordered_map>
#include <variant>
#include <set>

namespace innesce::front {

    static bool check_expr(const innesce::ast::Expr& e,
        const std::unordered_map<std::string, innesce::ast::Type>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::string& err,
        innesce::ast::Type* outTy = nullptr)
    {
        using E = innesce::ast::Expr;
        if (std::holds_alternative<E::IntLit>(e.node)) {
            if (outTy) { outTy->kind = innesce::ast::Type::I32; }
            return true;
        }
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& I = std::get<E::Ident>(e.node);
            auto it = locals.find(I.name);
            if (it != locals.end()) {
                if (outTy) *outTy = it->second;
                return true;
            }
            // Maybe it's an enum variant: find which enum defines it
            for (auto& [ename, vars] : enums) {
                auto vit = vars.find(I.name);
                if (vit != vars.end()) {
                    if (outTy) { outTy->kind = innesce::ast::Type::ENUM; outTy->enum_name = ename; }
                    return true;
                }
            }
            err = "use of undeclared identifier: " + I.name; return false;
        }
        if (std::holds_alternative<E::Unary>(e.node)) {
            const auto& U = std::get<E::Unary>(e.node);
            innesce::ast::Type t;
            if (!check_expr(*U.rhs, locals, enums, err, &t)) return false;
            if (t.kind != innesce::ast::Type::I32) { err = "unary operator on non-i32"; return false; }
            if (outTy) { outTy->kind = innesce::ast::Type::I32; }
            return true;
        }
        const auto& B = std::get<E::Binary>(e.node);
        innesce::ast::Type lt, rt;
        if (!check_expr(*B.lhs, locals, enums, err, &lt)) return false;
        if (!check_expr(*B.rhs, locals, enums, err, &rt)) return false;
        if (lt.kind != innesce::ast::Type::I32 || rt.kind != innesce::ast::Type::I32) {
            err = "binary arithmetic requires i32"; return false;
        }
        if (outTy) { outTy->kind = innesce::ast::Type::I32; }
        return true;
    }

    static bool check_block(const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, innesce::ast::Type> locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::string& err) {
        using S = innesce::ast::Stmt;
        for (auto& st : body) {
            if (std::holds_alternative<S::Let>(st.node)) {
                const auto& L = std::get<S::Let>(st.node);
                innesce::ast::Type t;
                if (!check_expr(L.init, locals, enums, err, &t)) return false;
                // allow assigning enum variant to enum var or int to i32
                if (L.type.kind == innesce::ast::Type::I32 && t.kind != innesce::ast::Type::I32) {
                    err = "type mismatch: expected i32"; return false;
                }
                if (L.type.kind == innesce::ast::Type::ENUM) {
                    if (t.kind != innesce::ast::Type::ENUM || t.enum_name != L.type.enum_name) {
                        err = "type mismatch: expected enum " + L.type.enum_name; return false;
                    }
                }
                locals[L.name] = L.type;
            }
            else if (std::holds_alternative<S::Return>(st.node)) {
                const auto& R = std::get<S::Return>(st.node);
                innesce::ast::Type t;
                if (!check_expr(R.value, locals, enums, err, &t)) return false;
                // function return type is validated elsewhere or trusted i32 for now
            }
            else if (std::holds_alternative<S::If>(st.node)) {
                const auto& I = std::get<S::If>(st.node);
                innesce::ast::Type t;
                if (!check_expr(I.cond, locals, enums, err, &t)) return false;
                if (!check_block(I.then_body, locals, enums, err)) return false;
                if (!check_block(I.else_body, locals, enums, err)) return false;
            }
            else if (std::holds_alternative<S::Match>(st.node)) {
                const auto& M = std::get<S::Match>(st.node);
                innesce::ast::Type t;
                if (!check_expr(M.scrutinee, locals, enums, err, &t)) return false;
                // Determine enum type and variant set
                std::set<std::string> enum_variants;
                bool has_default = false;
                if (t.kind == innesce::ast::Type::ENUM) {
                    auto it = enums.find(t.enum_name);
                    if (it == enums.end()) { err = "unknown enum type in match"; return false; }
                    for (auto& kv : it->second) enum_variants.insert(kv.first);
                }
                std::set<std::string> seen;
                for (auto& C : M.cases) {
                    if (C.is_default) { has_default = true; }
                    else {
                        if (seen.count(C.label)) { err = "duplicate case label: " + C.label; return false; }
                        seen.insert(C.label);
                        if (!enum_variants.empty() && !enum_variants.count(C.label)) {
                            err = "label not in enum " + t.enum_name + ": " + C.label; return false;
                        }
                    }
                    if (!check_block(C.body, locals, enums, err)) return false;
                }
                if (!has_default && !enum_variants.empty()) {
                    for (auto& v : enum_variants) if (!seen.count(v)) { err = "non-exhaustive match, missing: " + v; return false; }
                }
            }
        }
        return true;
    }

    SemaResult Sema::check(const ast::Unit& u) {
        // Load enums
        enums_.clear();
        for (auto& e : u.enums) {
            std::unordered_map<std::string, int> m;
            for (int i = 0; i < (int)e.variants.size(); ++i) m[e.variants[i]] = i;
            enums_[e.name] = std::move(m);
        }

        bool has_main = false;
        for (auto& f : u.functions) {
            if (f.name == "main") has_main = true;
            std::unordered_map<std::string, innesce::ast::Type> locals;
            std::string err;
            if (!check_block(f.body, locals, enums_, err)) return { err };
        }
        if (!has_main) return { "missing 'main' function" };
        return {};
    }

} // namespace innesce::front
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

        // Expose enum info for codegen
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums() const { return enums_; }
    private:
        std::unordered_map<std::string, std::unordered_map<std::string, int>> enums_;
    };

} // namespace innesce::front
#pragma once
#include <string_view>

namespace innesce {

    // Compile-time/no-op checkpoint markers; can be wired to profiling later.
    inline void checkpoint(std::string_view) noexcept {
        // no-op (placeholders for future instrumentation)
    }

} // namespace innesce
#pragma once
#include <cstdint>
#include <concepts>
#include <compare>
#include <type_traits>

namespace innesce {

    // Strong unit duration (integral count of base ticks), similar to std::chrono but minimal.
    // We intentionally do NOT mix units implicitly; conversions are explicit.
    template <typename UnitTag>
    struct Duration {
        using rep = std::int64_t;
        rep value{ 0 };

        constexpr Duration() = default;
        constexpr explicit Duration(rep v) : value(v) {}

        // Comparisons
        constexpr auto operator<=>(const Duration&) const = default;

        // Arithmetic (same-unit only)
        constexpr Duration operator+(Duration other) const { return Duration{ value + other.value }; }
        constexpr Duration operator-(Duration other) const { return Duration{ value - other.value }; }
        constexpr Duration& operator+=(Duration other) { value += other.value; return *this; }
        constexpr Duration& operator-=(Duration other) { value -= other.value; return *this; }

        // Scale by scalar
        template <std::integral I>
        constexpr Duration operator*(I k) const { return Duration{ value * static_cast<rep>(k) }; }

        template <std::integral I>
        constexpr Duration operator/(I k) const { return Duration{ value / static_cast<rep>(k) }; }
    };

    // Unit tags
    struct ns_tag {}; struct us_tag {}; struct ms_tag {}; struct sec_tag {}; struct min_tag {}; struct hr_tag {};

    using ns = Duration<ns_tag>;
    using us = Duration<us_tag>;
    using ms = Duration<ms_tag>;
    using sec = Duration<sec_tag>;
    using min = Duration<min_tag>;
    using hr = Duration<hr_tag>;

    // Explicit converters (compile-time factors)
    constexpr us to_us(ns x) { return us{ x.value / 1000 }; }
    constexpr ms to_ms(us x) { return ms{ x.value / 1000 }; }
    constexpr sec to_sec(ms x) { return sec{ x.value / 1000 }; }
    constexpr ms  to_ms(sec x) { return ms{ x.value * 1000 }; }
    constexpr us  to_us(ms x) { return us{ x.value * 1000 }; }
    constexpr ns  to_ns(us x) { return ns{ x.value * 1000 }; }

    // Literals helpers
    constexpr ns  operator""_ns(unsigned long long v) { return ns{ static_cast<ns::rep>(v) }; }
    constexpr us  operator""_us(unsigned long long v) { return us{ static_cast<us::rep>(v) * 1000 }; }
    constexpr ms  operator""_ms(unsigned long long v) { return ms{ static_cast<ms::rep>(v) * 1000 * 1000 }; }
    constexpr sec operator""_sec(unsigned long long v) { return sec{ static_cast<sec::rep>(v) }; }

} // namespace innesce
#include "core/lanes.hpp"
// Implementation currently header-only for simplicity; TU exists for future expansion.
#pragma once
#include <vector>
#include <thread>
#include <functional>
#include <barrier>
#include <atomic>
#include <cstddef>
#include <cassert>

namespace innesce {

    // Deterministic lanes: fixed number of worker threads; tasks are partitioned by index.
    class Lanes {
    public:
        explicit Lanes(std::size_t count)
            : count_(count)
        {
            assert(count_ > 0);
        }

        std::size_t size() const noexcept { return count_; }

        // Parallel for over [begin, end) with step 'chunk'.
        // Each lane operates on indices i where (i / chunk) % lane_count == lane_id.
        void parallel_for(std::size_t begin, std::size_t end, std::size_t chunk,
            const std::function<void(std::size_t, std::size_t)>& body) const
        {
            const std::size_t lanes = count_;
            std::vector<std::jthread> threads;
            threads.reserve(lanes);
            for (std::size_t lane = 0; lane < lanes; ++lane) {
                threads.emplace_back([=]() {
                    for (std::size_t i = begin; i < end; i += chunk) {
                        std::size_t slice = (i - begin) / chunk;
                        if ((slice % lanes) == lane) {
                            std::size_t hi = i + chunk;
                            if (hi > end) hi = end;
                            body(i, hi);
                        }
                    }
                    });
            }
            // join on destruction of jthreads
        }

        // Packetize into frames; barrier after each frame.
        void packetize_frames(std::size_t begin, std::size_t end, std::size_t frame,
            const std::function<void(std::size_t, std::size_t)>& body) const
        {
            const std::size_t lanes = count_;
            std::atomic<std::size_t> next{ begin };
            std::barrier sync(static_cast<std::ptrdiff_t>(lanes));
            std::vector<std::jthread> threads;
            threads.reserve(lanes);
            for (std::size_t lane = 0; lane < lanes; ++lane) {
                threads.emplace_back([&]() {
                    while (true) {
                        std::size_t i = next.fetch_add(frame);
                        if (i >= end) break;
                        std::size_t hi = i + frame;
                        if (hi > end) hi = end;
                        body(i, hi);
                        sync.arrive_and_wait();
                    }
                    });
            }
        }

    private:
        std::size_t count_;
    };

} // namespace innesce
#pragma once
#include <string>
#include <vector>
#include <utility>
#include <optional>

namespace innesce {

    struct Quarantine {
        bool failed{ false };
        std::vector<std::string> notes;

        void note(std::string msg) {
            notes.emplace_back(std::move(msg));
        }

        // Attempt wrapper: F must return std::optional<T>. If empty => failure recorded.
        template <typename F>
        auto attempt(F&& f) -> decltype(f()) {
            auto r = f();
            if (!r.has_value()) {
                failed = true;
                note("operation failed");
            }
            return r;
        }
    };

} // namespace innesce
#pragma once
#include <cstdint>

namespace innesce {

    enum class Truth : std::uint8_t { False = 0, True = 1, Unknown = 2, Both = 3 };

    // 4-valued truth operations (Kleene-like; tweak as desired)
    constexpr Truth t_not(Truth a) {
        switch (a) {
        case Truth::False:   return Truth::True;
        case Truth::True:    return Truth::False;
        case Truth::Unknown: return Truth::Unknown;
        case Truth::Both:    return Truth::Unknown; // design choice: ¬Both = Unknown
        }
        return Truth::Unknown;
    }

    constexpr Truth t_and(Truth a, Truth b) {
        // truth table encoded; symmetric
        if (a == Truth::False || b == Truth::False) return Truth::False;
        if (a == Truth::True && b == Truth::True)  return Truth::True;
        if (a == Truth::Both && b == Truth::True)  return Truth::Both;
        if (b == Truth::Both && a == Truth::True)  return Truth::Both;
        if (a == Truth::Both && b == Truth::Both)  return Truth::Both;
        // any combo with Unknown that isn't already decided:
        return Truth::Unknown;
    }

    constexpr Truth t_or(Truth a, Truth b) {
        if (a == Truth::True || b == Truth::True) return Truth::True;
        if (a == Truth::False && b == Truth::False) return Truth::False;
        if (a == Truth::Both || b == Truth::Both) return Truth::Both;
        return Truth::Unknown;
    }

} // namespace innesce
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
    for (int i = 2; i < argc; i++) {
        std::string a = argv[i];
        if (a == "-o" && i + 1 < argc) { out = argv[++i]; continue; }
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
#include "backend/llvm/codegen.hpp"
#include "backend/llvm/ir_builder.hpp"
#include "frontend/sema.hpp" // for enums mapping in future (we'll pass explicit mapping from caller)

#ifdef INNSCE_HAVE_LLVM
#include <llvm/ADT/Triple.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/MC/TargetOptions.h>
#include <system_error>
#include <unordered_map>
#include <variant>
#include <optional>
#endif

namespace innesce::backend::llvm {

#ifndef INNSCE_HAVE_LLVM
    std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path) {
        (void)unit; (void)obj_path;
        return "LLVM backend disabled: rebuild with -DINNSCE_ENABLE_LLVM=ON";
    }
#else

    using namespace ::llvm;

    static Value* emit_expr(IRBuilder<>& B,
        const innesce::ast::Expr& e,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums) {
        using E = innesce::ast::Expr;
        if (std::holds_alternative<E::IntLit>(e.node)) {
            int v = std::get<E::IntLit>(e.node).value;
            return B.getInt32(v);
        }
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& I = std::get<E::Ident>(e.node);
            auto it = locals.find(I.name);
            if (it != locals.end()) {
                return B.CreateLoad(B.getInt32Ty(), it->second);
            }
            // maybe enum variant
            for (auto& [ename, vars] : enums) {
                auto vit = vars.find(I.name);
                if (vit != vars.end()) {
                    return B.getInt32(vit->second);
                }
            }
            return nullptr;
        }
        if (std::holds_alternative<E::DurLit>(e.node)) {
            int v = std::get<E::DurLit>(e.node).value; return B.getInt32(v);
        }
        if (std::holds_alternative<E::Unary>(e.node)) {
            const auto& U = std::get<E::Unary>(e.node);
            Value* rhs = emit_expr(B, *U.rhs, locals, enums);
            if (!rhs) return nullptr;
            if (U.op == '-') return B.CreateNeg(rhs);
            return rhs;
        }
        const auto& BN = std::get<E::Binary>(e.node);
        Value* L = emit_expr(B, *BN.lhs, locals, enums);
        Value* R = emit_expr(B, *BN.rhs, locals, enums);
        if (!L || !R) return nullptr;
        switch (BN.op) {
        case '+': return B.CreateAdd(L, R);
        case '-': return B.CreateSub(L, R);
        case '*': return B.CreateMul(L, R);
        case '/': return B.CreateSDiv(L, R);
        }
        return nullptr;
    }

    static bool emit_block(IRBuilder<>& B,
        Function* F,
        const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums);

    static bool emit_match(IRBuilder<>& B,
        Function* F,
        const innesce::ast::Stmt::Match& M,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums) {
        Value* scr = emit_expr(B, M.scrutinee, locals, enums);
        if (!scr) return false;

        BasicBlock* contBB = BasicBlock::Create(B.getContext(), "match.cont");
        // build case blocks
        std::vector<BasicBlock*> caseBBs;
        BasicBlock* defaultBB = nullptr;
        for (auto& C : M.cases) {
            if (C.is_default) {
                defaultBB = BasicBlock::Create(B.getContext(), "case.default");
            }
            else {
                caseBBs.push_back(BasicBlock::Create(B.getContext(), ("case." + C.label).c_str()));
            }
        }
        if (!defaultBB) defaultBB = contBB;

        // switch instruction
        SwitchInst* sw = B.CreateSwitch(scr, defaultBB, (unsigned)caseBBs.size());

        // Attach cases
        unsigned idx = 0;
        for (auto& C : M.cases) {
            if (C.is_default) continue;
            int val = -1;
            // map label to int
            for (auto& [ename, vars] : enums) {
                auto it = vars.find(C.label);
                if (it != vars.end()) { val = it->second; break; }
            }
            if (val < 0) return false;
            BasicBlock* bb = caseBBs[idx++];
            F->getBasicBlockList().push_back(bb);
            sw->addCase(B.getInt32(val), bb);
            B.SetInsertPoint(bb);
            auto locals_case = locals;
            bool returned = emit_block(B, F, C.body, locals_case, enums);
            if (!returned) B.CreateBr(contBB);
        }

        // default
        if (defaultBB != contBB) {
            F->getBasicBlockList().push_back(defaultBB);
            B.SetInsertPoint(defaultBB);
            auto locals_def = locals;
            // empty default => just fallthrough
            for (auto& C : M.cases) {
                if (C.is_default) {
                    bool returned = emit_block(B, F, C.body, locals_def, enums);
                    if (!returned) B.CreateBr(contBB);
                    break;
                }
            }
        }

        // continue
        F->getBasicBlockList().push_back(contBB);
        B.SetInsertPoint(contBB);
        return false;
    }

    static bool emit_block(IRBuilder<>& B,
        Function* F,
        const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums) {
        using S = innesce::ast::Stmt;
        for (auto& st : body) {
            if (std::holds_alternative<S::Let>(st.node)) {
                const auto& L = std::get<S::Let>(st.node);
                AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, L.name);
                Value* init = emit_expr(B, L.init, locals, enums);
                if (!init) return false;
                B.CreateStore(init, a);
                locals[L.name] = a;
            }
            else if (std::holds_alternative<S::Return>(st.node)) {
                const auto& R = std::get<S::Return>(st.node);
                Value* v = emit_expr(B, R.value, locals, enums);
                if (!v) return false;
                B.CreateRet(v);
                return true;
            }
            else if (std::holds_alternative<S::If>(st.node)) {
                const auto& I = std::get<S::If>(st.node);
                Value* condv = emit_expr(B, I.cond, locals, enums);
                if (!condv) return false;
                Value* istrue = B.CreateICmpNE(condv, B.getInt32(0));
                BasicBlock* thenBB = BasicBlock::Create(B.getContext(), "then", F);
                BasicBlock* elseBB = BasicBlock::Create(B.getContext(), "else");
                BasicBlock* contBB = BasicBlock::Create(B.getContext(), "endif");
                B.CreateCondBr(istrue, thenBB, I.else_body.empty() ? contBB : elseBB);
                // then
                B.SetInsertPoint(thenBB);
                auto locals_then = locals;
                bool thenReturned = emit_block(B, F, I.then_body, locals_then, enums);
                if (!thenReturned) B.CreateBr(contBB);
                if (!I.else_body.empty()) {
                    F->getBasicBlockList().push_back(elseBB);
                    B.SetInsertPoint(elseBB);
                    auto locals_else = locals;
                    bool elseReturned = emit_block(B, F, I.else_body, locals_else, enums);
                    if (!elseReturned) B.CreateBr(contBB);
                }
                F->getBasicBlockList().push_back(contBB);
                B.SetInsertPoint(contBB);

            }
            else if (std::holds_alternative<S::Asm>(st.node)) {
                const auto& A = std::get<S::Asm>(st.node);
                auto& Ctx = B.getContext();
                Module* M = B.GetInsertBlock()->getModule();
                auto FT = FunctionType::get(Type::getVoidTy(Ctx), {}, false);
                bool isIntel = true;
                auto IA = InlineAsm::get(FT, A.text, "", /*sideeffect*/ true, /*alignstack*/ false,
                    isIntel ? InlineAsm::AD_Intel : InlineAsm::AD_ATT);
                B.CreateCall(IA);
            }
            else if (std::holds_alternative<S::Match>(st.node)) {
                if (!emit_match(B, F, std::get<S::Match>(st.node), locals, enums)) return false;
            }
        }
        return false;
    }

    std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path) {
        // Build enum map from unit
        std::unordered_map<std::string, std::unordered_map<std::string, int>> enums;
        for (auto& e : unit.enums) {
            std::unordered_map<std::string, int> m;
            for (int i = 0; i < (int)e.variants.size(); ++i) m[e.variants[i]] = i;
            enums[e.name] = std::move(m);
        }

        // Find main()
        const innesce::ast::Function* mainFn = nullptr;
        for (auto& f : unit.functions) if (f.name == "main") { mainFn = &f; break; }
        if (!mainFn) return "no main() in unit";

        LLVMContext ctx;
        auto mod = std::make_unique<Module>("innesce_mod", ctx);
        IRBuilder<> B(ctx);

        // int main()
        FunctionType* fty = FunctionType::get(B.getInt32Ty(), false);
        Function* F = Function::Create(fty, Function::ExternalLinkage, "main", mod.get());
        // Apply Hot attribute if requested
        for (auto& ff : unit.functions) if (ff.name == "main" && ff.hot) { F->addFnAttr(Attribute::Hot); }
        BasicBlock* entry = BasicBlock::Create(ctx, "entry", F);
        B.SetInsertPoint(entry);

        std::unordered_map<std::string, Value*> locals;
        bool returned = emit_block(B, F, mainFn->body, locals, enums);
        if (!returned) {
            B.CreateRet(B.getInt32(0));
        }

        if (verifyFunction(*F, &errs())) return "IR verification failed for main()";
        if (verifyModule(*mod, &errs())) return "Module verification failed";

        // Target setup
        InitializeAllTargetInfos();
        InitializeAllTargets();
        InitializeAllTargetMCs();
        InitializeAllAsmParsers();
        InitializeAllAsmPrinters();

        auto triple = sys::getDefaultTargetTriple();
        std::string err;
        const Target* target = TargetRegistry::lookupTarget(triple, err);
        if (!target) return "lookupTarget failed: " + err;

        TargetOptions opt;
        auto relocModel = std::optional<Reloc::Model>();
        std::unique_ptr<TargetMachine> TM(target->createTargetMachine(triple, "generic", "", opt, relocModel));
        mod->setTargetTriple(triple);
        mod->setDataLayout(TM->createDataLayout());

        std::error_code ec;
        raw_fd_ostream dest(obj_path, ec, sys::fs::OF_None);
        if (ec) return "could not open object file: " + ec.message();

        legacy::PassManager pm;
        if (TM->addPassesToEmitFile(pm, dest, nullptr, CodeGenFileType::CGFT_ObjectFile)) {
            return "TargetMachine cannot emit this file type";
        }
        pm.run(*mod);
        dest.flush();
        return {};
    }

#endif // INNSCE_HAVE_LLVM

} // namespace innesce::backend::llvm
#pragma once
#include "frontend/ast.hpp"
#include <string>

namespace innesce::backend::llvm {

    // Returns empty string on success; otherwise an error message.
    std::string compile_to_object(const innesce::ast::Unit& unit, const std::string& obj_path);

} // namespace innesce::backend::llvm
#include "backend/llvm/ir_builder.hpp"
#include "frontend/ast.hpp"
#include <iostream>

using namespace innesce::backend::llvm;
using namespace innesce;

void IrBuilder::compile(const ast::Program& prog) {
    std::cout << "; Innesce IR dump (stub)\n";
    for (auto& st : prog.stmts) {
        if (std::holds_alternative<ast::LetStmt>(st->node)) {
            auto& ls = std::get<ast::LetStmt>(st->node);
            if (std::holds_alternative<ast::IntLit>(ls.init->node)) {
                auto v = std::get<ast::IntLit>(ls.init->node).value;
                std::cout << "%" << ls.name << " = const i64 " << v << "\n";
            }
        }
        else if (std::holds_alternative<ast::ReturnStmt>(st->node)) {
            auto& rs = std::get<ast::ReturnStmt>(st->node);
            if (std::holds_alternative<ast::IntLit>(rs->node)) {
                auto v = std::get<ast::IntLit>(rs->node).value;
                std::cout << "ret i64 " << v << "\n";
            }
        }
    }
}
#pragma once
#include <string>
#include "frontend/ast.hpp"

namespace innesce::backend::llvm {

    struct IrBuilderConfig {
        bool optimize{ true };
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
#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <variant>

namespace innesce::ast {

    enum class DurUnit { MS, SEC };

    struct Type {
        enum Kind { I32, ENUM, DUR } kind{ I32 };
        std::string enum_name; // if kind==ENUM
        DurUnit dur{};         // if kind==DUR
    };

    struct Expr {
        struct IntLit { int value; };
        struct Ident { std::string name; };
        struct Unary { char op; std::unique_ptr<Expr> rhs; };
        struct Binary { char op; std::unique_ptr<Expr> lhs, rhs; };
        struct IsFailed { std::string name; }; // isfailed(name) -> i32
        struct DurLit { int value; DurUnit unit; }; // 100 ms / 2 sec
        std::variant<IntLit, Ident, Unary, Binary, IsFailed, DurLit> node;
    };

    struct Stmt {
        struct Let { std::string name; Type type; Expr init; };
        struct Return { Expr value; };
        struct If { Expr cond; std::vector<Stmt> then_body; std::vector<Stmt> else_body; };
        struct Match {
            Expr scrutinee;
            struct Case { std::string label; std::vector<Stmt> body; bool is_default{ false }; };
            std::vector<Case> cases;
        };
        struct Sleep { Expr amount; bool is_ms{ true }; }; // sema sets is_ms based on amount type
        struct Fail {};
        struct Quarantine { std::string name; std::vector<Stmt> body; };
        struct Asm { std::string text; bool intel{ true }; };

        std::variant<Let, Return, If, Match, Sleep, Fail, Quarantine, Asm> node;
    };

    struct EnumDecl { std::string name; std::vector<std::string> variants; };

    struct Function {
        std::string name;
        Type ret;
        std::vector<std::string> gates; // e.g., "time", "fs.read"
        bool hot{ false };
        std::vector<Stmt> body;
    };

    struct Unit { std::vector<EnumDecl> enums; std::vector<Function> functions; };

} // namespace innesce::ast
#include "frontend/lexer.hpp"
#include <cctype>

namespace innesce::front {

    Lexer::Lexer(std::string_view src) : s_(src) {}

    char Lexer::peek() const { return i_ < s_.size() ? s_[i_] : '\0'; }
    char Lexer::get() {
        if (i_ >= s_.size()) return '\0';
        char c = s_[i_++];
        if (c == '\n') { line_++; col_ = 1; }
        else { col_++; }
        return c;
    }

    void Lexer::skip_ws_and_comments() {
        while (true) {
            char c = peek();
            if (c == '\0') break;
            if (std::isspace(static_cast<unsigned char>(c))) { get(); continue; }
            if (c == '-' && i_ + 1 < s_.size() && s_[i_ + 1] == '-') {
                while (c != '\n' && c != '\0') c = get();
                continue;
            }
            break;
        }
    }

    Token Lexer::make(TokKind k, std::string t) {
        Token x{ k, std::move(t), 0, line_, col_ };
        return x;
    }

    Token Lexer::next() {
        skip_ws_and_comments();
        char c = peek();
        if (c == '\0') return make(TokKind::End, "");

        // punctuation/operators
        if (c == '(') { get(); return make(TokKind::LParen, "("); }
        if (c == ')') { get(); return make(TokKind::RParen, ")"); }
        if (c == '{') { get(); return make(TokKind::LBrace, "{"); }
        if (c == '}') { get(); return make(TokKind::RBrace, "}"); }
        if (c == '[') { get(); return make(TokKind::LBracket, "["); }
        if (c == ']') { get(); return make(TokKind::RBracket, "]"); }
        if (c == '+') { get(); return make(TokKind::Plus, "+"); }
        if (c == '-') {
            if (i_ + 1 < s_.size() && s_[i_ + 1] == '>') { get(); get(); return make(TokKind::Arrow, "->"); }
            get(); return make(TokKind::Minus, "-");
        }
        if (c == '*') { get(); return make(TokKind::Star, "*"); }
        if (c == '/') { get(); return make(TokKind::Slash, "/"); }
        if (c == ':') {
            get();
            if (peek() == '=') { get(); return make(TokKind::Assign, ":="); }
            return make(TokKind::Colon, ":");
        }
        if (c == ';') { get(); return make(TokKind::Semicolon, ";"); }
        if (c == ',') { get(); return make(TokKind::Comma, ","); }


        // number
        if (std::isdigit(static_cast<unsigned char>(c))) {
            int v = 0;
            while (std::isdigit(static_cast<unsigned char>(peek()))) {
                v = v * 10 + (get() - '0');
            }
            Token t = make(TokKind::Int, "");
            t.int_val = v;
            return t;
        }

        // identifier/keyword
        if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
            std::string id;
            while (std::isalnum(static_cast<unsigned char>(peek())) || peek() == '_') {
                id.push_back(get());
            }
            if (id == "fn") return make(TokKind::KwFn, id);
            if (id == "is") return make(TokKind::KwIs, id);
            if (id == "end") return make(TokKind::KwEnd, id);
            if (id == "return") return make(TokKind::KwReturn, id);
            if (id == "let") return make(TokKind::KwLet, id);
            if (id == "i32") return make(TokKind::KwI32, id);
            if (id == "if") return make(TokKind::KwIf, id);
            if (id == "then") return make(TokKind::KwThen, id);
            if (id == "else") return make(TokKind::KwElse, id);
            if (id == "type") return make(TokKind::KwType, id);
            if (id == "enum") return make(TokKind::KwEnum, id);
            if (id == "match") return make(TokKind::KwMatch, id);
            if (id == "case") return make(TokKind::KwCase, id);
            if (id == "default") return make(TokKind::KwDefault, id);
            if (id == "sleep") return make(TokKind::KwSleep, id);
            if (id == "quarantine") return make(TokKind::KwQuarantine, id);
            if (id == "fail") return make(TokKind::KwFail, id);
            if (id == "isfailed") return make(TokKind::KwIsFailed, id);
            if (id == "ms") return make(TokKind::KwMS, id);
            if (id == "sec") return make(TokKind::KwSEC, id);
            if (id == "asm") return make(TokKind::KwASM, id);
            if (id == "with") return make(TokKind::KwWith, id);
            if (id == "Hot") return make(TokKind::KwHot, id);
            Token t = make(TokKind::Ident, id);
            t.text = id;
            return t;
        }

        // unknown char -> skip
        get();
        return next();
    }

} // namespace innesce::front
#pragma once
#include <string>
#include <string_view>
#include <vector>

namespace innesce::front {

    enum class TokKind {
        End,
        Ident,
        Int,
        // keywords
        KwFn, KwIs, KwEnd, KwReturn, KwLet,
        KwI32, KwIf, KwThen, KwElse,
        KwType, KwEnum, KwMatch, KwCase, KwDefault,
        KwSleep, KwQuarantine, KwFail, KwIsFailed,
        KwMS, KwSEC, KwASM, KwWith, KwHot,
        // punctuation
        LParen, RParen, LBrace, RBrace, LBracket, RBracket,
        Colon, Semicolon, Comma,
        Arrow, Assign, FatArrow,
        // ops
        Plus, Minus, Star, Slash,
    };

    struct Token {
        TokKind kind;
        std::string text;
        int int_val{ 0 };
        int line{ 1 }, col{ 1 };
    };

    struct Lexer {
        explicit Lexer(std::string_view src);
        Token next();
    private:
        std::string_view s_;
        size_t i_{ 0 };
        int line_{ 1 }, col_{ 1 };
        char peek() const;
        char get();
        void skip_ws_and_comments();
        Token make(TokKind k, std::string t);
    };

} // namespace innesce::front
#include "frontend/parser.hpp"
#include <iostream>
#include <memory>

namespace innesce::front {

    Parser::Parser(std::string_view src) : lex_(src) { bump(); }

    void Parser::bump() { cur_ = lex_.next(); }
    bool Parser::accept(TokKind k) { if (cur_.kind == k) { bump(); return true; } return false; }
    bool Parser::expect(TokKind k, const char* what) {
        if (!accept(k)) { std::cerr << "Expected " << what << "\n"; return false; }
        return true;
    }

    std::optional<ast::Type> Parser::parse_type_name() {
        ast::Type t;
        if (accept(TokKind::KwI32)) { t.kind = ast::Type::I32; return t; }
        if (accept(TokKind::KwMS)) { t.kind = ast::Type::DUR; t.dur = ast::DurUnit::MS; return t; }
        if (accept(TokKind::KwSEC)) { t.kind = ast::Type::DUR; t.dur = ast::DurUnit::SEC; return t; }
        if (cur_.kind == TokKind::Ident) { t.kind = ast::Type::ENUM; t.enum_name = cur_.text; bump(); return t; }
        return std::nullopt;
    }

    std::optional<ast::Unit> Parser::parse_unit() {
        ast::Unit u;
        while (cur_.kind != TokKind::End) {
            if (cur_.kind == TokKind::KwType) {
                auto e = parse_enum_decl();
                if (!e) return std::nullopt;
                u.enums.push_back(std::move(*e));
                continue;
            }
            auto fn = parse_function();
            if (!fn) return std::nullopt;
            u.functions.push_back(std::move(*fn));
        }
        return u;
    }

    std::optional<ast::EnumDecl> Parser::parse_enum_decl() {
        if (!expect(TokKind::KwType, "'type'")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected enum name\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        if (!expect(TokKind::KwEnum, "'enum'")) return std::nullopt;
        if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
        ast::EnumDecl d; d.name = std::move(name);
        bool first = true; // we'll fix this to true after writing
        while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
            if (!first) expect(TokKind::Comma, "','");
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected variant name\n"; return std::nullopt; }
            d.variants.push_back(cur_.text);
            bump();
            first = false;
        }
        if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
        accept(TokKind::Semicolon);
        return d;
    }

    std::optional<ast::Function> Parser::parse_function() {
        if (!expect(TokKind::KwFn, "'fn'")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected function name\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::LParen, "'('")) return std::nullopt;
        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
        if (!expect(TokKind::Arrow, "'->'")) return std::nullopt;
        auto rty = parse_type_name(); if (!rty) { std::cerr << "Expected return type\n"; return std::nullopt; }

        // optional: with [ items ]
        std::vector<std::string> gates; bool hot = false;
        if (accept(TokKind::KwWith)) {
            if (!expect(TokKind::LBracket, "'['")) return std::nullopt;
            bool first = true;
            while (cur_.kind != TokKind::RBracket && cur_.kind != TokKind::End) {
                if (!first) expect(TokKind::Comma, "','");
                if (accept(TokKind::KwHot)) { hot = true; }
                else if (cur_.kind == TokKind::Ident) { gates.push_back(cur_.text); bump(); }
                else { std::cerr << "Expected gate name or Hot\n"; return std::nullopt; }
                first = false;
            }
            if (!expect(TokKind::RBracket, "']'")) return std::nullopt;
        }

        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;

        ast::Function f; f.name = std::move(name); f.ret = *rty; f.gates = std::move(gates); f.hot = hot;
        while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt();
            if (!st) return std::nullopt;
            f.body.push_back(std::move(*st));
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        return f;
    }

    std::optional<ast::Stmt> Parser::parse_match() {
        if (!expect(TokKind::KwMatch, "'match'")) return std::nullopt;
        auto scrut = parse_expr(); if (!scrut) return std::nullopt;
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        ast::Stmt::Match m; m.scrutinee = std::move(*scrut);
        while (cur_.kind == TokKind::KwCase) {
            bump();
            bool is_default = false;
            std::string label;
            if (accept(TokKind::KwDefault)) {
                is_default = true;
            }
            else {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected case label\n"; return std::nullopt; }
                label = cur_.text; bump();
            }
            if (!expect(TokKind::FatArrow, "'=>'")) return std::nullopt;
            auto body = parse_case_body();
            m.cases.push_back(ast::Stmt::Match::Case{ std::move(label), std::move(body), is_default });
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        ast::Stmt st; st.node = std::move(m);
        return st;
    }

    std::vector<ast::Stmt> Parser::parse_case_body() {
        std::vector<ast::Stmt> body;
        while (cur_.kind != TokKind::KwCase && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt();
            if (!st) break;
            body.push_back(std::move(*st));
        }
        return body;
    }

    std::optional<ast::Stmt> Parser::parse_stmt() {
        if (cur_.kind == TokKind::KwLet) {
            bump();
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected identifier after let\n"; return std::nullopt; }
            std::string name = cur_.text; bump();
            if (!expect(TokKind::Colon, "':'")) return std::nullopt;
            auto ty = parse_type_name(); if (!ty) { std::cerr << "Expected type name\n"; return std::nullopt; }
            if (!expect(TokKind::Assign, "':='")) return std::nullopt;
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Let{ std::move(name), *ty, std::move(*e) };
            return st;
        }
        if (cur_.kind == TokKind::KwReturn) {
            bump();
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Return{ std::move(*e) };
            return st;
        }
        if (cur_.kind == TokKind::KwIf) {
            bump();
            auto cond = parse_expr(); if (!cond) return std::nullopt;
            if (!expect(TokKind::KwThen, "'then'")) return std::nullopt;
            std::vector<ast::Stmt> then_body;
            while (cur_.kind != TokKind::KwElse && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                auto st = parse_stmt(); if (!st) return std::nullopt; then_body.push_back(std::move(*st));
            }
            std::vector<ast::Stmt> else_body;
            if (accept(TokKind::KwElse)) {
                while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                    auto st = parse_stmt(); if (!st) return std::nullopt; else_body.push_back(std::move(*st));
                }
            }
            if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::If{ std::move(*cond), std::move(then_body), std::move(else_body) };
            return st;
        }
        if (cur_.kind == TokKind::KwMatch) return parse_match();
        if (cur_.kind == TokKind::KwSleep) {
            bump();
            if (!expect(TokKind::LParen, "'('")) return std::nullopt;
            auto amt = parse_expr(); if (!amt) return std::nullopt;
            // optional: legacy units inside parens are now part of expr, so we just expect ')'
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Sleep{ std::move(*amt), true /*placeholder; sema will set*/ };
            return st;
        }
        if (cur_.kind == TokKind::KwFail) {
            bump();
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Fail{};
            return st;
        }
        if (cur_.kind == TokKind::KwQuarantine) {
            bump();
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected quarantine name"; return std::nullopt; }
            std::string name = cur_.text; bump();
            if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
            std::vector<ast::Stmt> body;
            while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                auto st = parse_stmt(); if (!st) return std::nullopt; body.push_back(std::move(*st));
            }
            if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Quarantine{ std::move(name), std::move(body) };
            return st;
        }
        if (cur_.kind == TokKind::KwASM) {
            bump();
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected assembly mnemonic (identifier)\n"; return std::nullopt; }
            std::string text = cur_.text; bump();
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Asm{ std::move(text), true };
            return st;
        }
        std::cerr << "Unknown statement\n";
        return std::nullopt;
    }

    std::optional<ast::Expr> Parser::parse_expr() { return parse_add(); }

    std::optional<ast::Expr> Parser::parse_add() {
        auto lhs = parse_mul(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Plus || cur_.kind == TokKind::Minus) {
            char op = (cur_.kind == TokKind::Plus) ? '+' : '-';
            bump();
            auto rhs = parse_mul(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Binary{ op, std::make_unique<ast::Expr>(std::move(*lhs)), std::make_unique<ast::Expr>(std::move(*rhs)) };
            lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_mul() {
        auto lhs = parse_unary(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Star || cur_.kind == TokKind::Slash) {
            char op = (cur_.kind == TokKind::Star) ? '*' : '/';
            bump();
            auto rhs = parse_unary(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Binary{ op, std::make_unique<ast::Expr>(std::move(*lhs)), std::make_unique<ast::Expr>(std::move(*rhs)) };
            lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_unary() {
        if (cur_.kind == TokKind::Minus) {
            bump();
            auto rhs = parse_unary(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Unary{ '-', std::make_unique<ast::Expr>(std::move(*rhs)) };
            return e;
        }
        return parse_primary();
    }

    std::optional<ast::Expr> Parser::parse_primary() {
        if (cur_.kind == TokKind::Int) {
            int v = cur_.int_val; bump();
            // duration literal if followed by ms/sec
            if (accept(TokKind::KwMS)) {
                ast::Expr e; e.node = ast::Expr::DurLit{ v, ast::DurUnit::MS }; return e;
            }
            if (accept(TokKind::KwSEC)) {
                ast::Expr e; e.node = ast::Expr::DurLit{ v, ast::DurUnit::SEC }; return e;
            }
            ast::Expr e; e.node = ast::Expr::IntLit{ v }; return e;
        }
        if (cur_.kind == TokKind::Ident) {
            ast::Expr e; e.node = ast::Expr::Ident{ cur_.text }; bump(); return e;
        }
        if (cur_.kind == TokKind::KwIsFailed) {
            bump();
            if (!expect(TokKind::LParen, "'('")) return std::nullopt;
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected name in isfailed()"; return std::nullopt; }
            std::string q = cur_.text; bump();
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            ast::Expr e; e.node = ast::Expr::IsFailed{ std::move(q) }; return e;
        }
        if (accept(TokKind::LParen)) {
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            return e;
        }
        std::cerr << "Expected expression\n";
        return std::nullopt;
    }

} // namespace innesce::front
#pragma once
#include "frontend/lexer.hpp"
#include "frontend/ast.hpp"
#include <optional>

namespace innesce::front {

    class Parser {
    public:
        explicit Parser(std::string_view src);
        std::optional<ast::Unit> parse_unit();

    private:
        Lexer lex_;
        Token cur_;

        void bump();
        bool accept(TokKind k);
        bool expect(TokKind k, const char* what);

        std::optional<ast::EnumDecl> parse_enum_decl();
        std::optional<ast::Function> parse_function();
        std::optional<ast::Stmt> parse_stmt();
        std::optional<ast::Stmt> parse_match();
        std::optional<ast::Expr> parse_expr();
        std::optional<ast::Expr> parse_add();
        std::optional<ast::Expr> parse_mul();
        std::optional<ast::Expr> parse_unary();
        std::optional<ast::Expr> parse_primary();

        std::vector<ast::Stmt> parse_case_body();
        std::optional<ast::Type> parse_type_name();
    };

} // namespace innesce::front
#include "frontend/sema.hpp"
#include <unordered_map>
#include <variant>
#include <set>
#include <algorithm>

namespace innesce::front {

    using Type = innesce::ast::Type;
    using DurUnit = innesce::ast::DurUnit;

    static bool is_i32(const Type& t) { return t.kind == Type::I32; }
    static bool is_dur(const Type& t, DurUnit* u = nullptr) { if (t.kind == Type::DUR) { if (u)*u = t.dur; return true; } return false; }
    static bool is_enum(const Type& t) { return t.kind == Type::ENUM; }

    static bool check_expr(const innesce::ast::Expr& e,
        const std::unordered_map<std::string, Type>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::string& err,
        Type* outTy = nullptr)
    {
        using E = innesce::ast::Expr;
        if (std::holds_alternative<E::IntLit>(e.node)) {
            if (outTy) outTy->kind = Type::I32; return true;
        }
        if (std::holds_alternative<E::DurLit>(e.node)) {
            if (outTy) { outTy->kind = Type::DUR; outTy->dur = std::get<E::DurLit>(e.node).unit; } return true;
        }
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& I = std::get<E::Ident>(e.node);
            auto it = locals.find(I.name);
            if (it != locals.end()) { if (outTy) *outTy = it->second; return true; }
            // enum variant?
            for (auto& [ename, vars] : enums) {
                if (vars.count(I.name)) { if (outTy) { outTy->kind = Type::ENUM; outTy->enum_name = ename; } return true; }
            }
            err = "use of undeclared identifier: " + I.name; return false;
        }
        if (std::holds_alternative<E::IsFailed>(e.node)) {
            if (outTy) outTy->kind = Type::I32; return true;
        }
        if (std::holds_alternative<E::Unary>(e.node)) {
            const auto& U = std::get<E::Unary>(e.node);
            Type t; if (!check_expr(*U.rhs, locals, enums, err, &t)) return false;
            if (!is_i32(t)) { err = "unary '-' requires i32"; return false; }
            if (outTy) outTy->kind = Type::I32; return true;
        }
        const auto& B = std::get<E::Binary>(e.node);
        Type lt, rt;
        if (!check_expr(*B.lhs, locals, enums, err, &lt)) return false;
        if (!check_expr(*B.rhs, locals, enums, err, &rt)) return false;
        // arithmetic typing
        if (B.op == '+' || B.op == '-') {
            if (is_i32(lt) && is_i32(rt)) { if (outTy) outTy->kind = Type::I32; return true; }
            DurUnit ul, ur;
            if (is_dur(lt, &ul) && is_dur(rt, &ur) && ul == ur) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = ul; } return true; }
            err = "duration addition/subtraction requires same units"; return false;
        }
        if (B.op == '*') {
            DurUnit u;
            if (is_dur(lt, &u) && is_i32(rt)) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = u; } return true; }
            if (is_i32(lt) && is_dur(rt, &u)) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = u; } return true; }
            if (is_i32(lt) && is_i32(rt)) { if (outTy) outTy->kind = Type::I32; return true; }
            err = "invalid duration multiplication"; return false;
        }
        if (B.op == '/') {
            DurUnit u;
            if (is_dur(lt, &u) && is_i32(rt)) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = u; } return true; }
            if (is_i32(lt) && is_i32(rt)) { if (outTy) outTy->kind = Type::I32; return true; }
            err = "invalid duration division"; return false;
        }
        err = "unknown binary operator"; return false;
    }

    static bool check_block(const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Type> locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::string& err,
        bool in_quarantine,
        std::unordered_map<std::string, bool>& declared_quarantines,
        const std::vector<std::string>& fn_gates) {
        using S = innesce::ast::Stmt;
        for (auto& st : body) {
            if (std::holds_alternative<S::Let>(st.node)) {
                const auto& L = std::get<S::Let>(st.node);
                Type t;
                if (!check_expr(L.init, locals, enums, err, &t)) return false;
                // type match
                if (L.type.kind != t.kind) { err = "type mismatch in let"; return false; }
                if (L.type.kind == Type::ENUM && L.type.enum_name != t.enum_name) { err = "enum type mismatch in let"; return false; }
                if (L.type.kind == Type::DUR && L.type.dur != t.dur) { err = "duration unit mismatch in let"; return false; }
                locals[L.name] = L.type;
            }
            else if (std::holds_alternative<S::Return>(st.node)) {
                const auto& R = std::get<S::Return>(st.node);
                Type t; if (!check_expr(R.value, locals, enums, err, &t)) return false;
            }
            else if (std::holds_alternative<S::If>(st.node)) {
                const auto& I = std::get<S::If>(st.node);
                Type t; if (!check_expr(I.cond, locals, enums, err, &t)) return false;
                if (!is_i32(t)) { err = "if condition must be i32"; return false; }
                if (!check_block(I.then_body, locals, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
                if (!check_block(I.else_body, locals, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
            }
            else if (std::holds_alternative<S::Match>(st.node)) {
                const auto& M = std::get<S::Match>(st.node);
                Type t; if (!check_expr(M.scrutinee, locals, enums, err, &t)) return false;
                std::set<std::string> enum_variants;
                bool has_default = false;
                if (t.kind == Type::ENUM) {
                    auto it = enums.find(t.enum_name);
                    if (it == enums.end()) { err = "unknown enum in match"; return false; }
                    for (auto& kv : it->second) enum_variants.insert(kv.first);
                }
                std::set<std::string> seen;
                for (auto& C : M.cases) {
                    if (C.is_default) { has_default = true; }
                    else {
                        if (seen.count(C.label)) { err = "duplicate case label: " + C.label; return false; }
                        seen.insert(C.label);
                        if (!enum_variants.empty() && !enum_variants.count(C.label)) { err = "label not in enum " + t.enum_name + ": " + C.label; return false; }
                    }
                    if (!check_block(C.body, locals, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
                }
                if (!has_default && !enum_variants.empty()) {
                    for (auto& v : enum_variants) if (!seen.count(v)) { err = "non-exhaustive match, missing: " + v; return false; }
                }
            }
            else if (std::holds_alternative<S::Sleep>(st.node)) {
                const auto& SL = std::get<S::Sleep>(st.node);
                Type t; if (!check_expr(SL.amount, locals, enums, err, &t)) return false;
                if (t.kind != Type::DUR) { err = "sleep expects duration value (ms or sec)"; return false; }
                // gate check
                if (std::find(fn_gates.begin(), fn_gates.end(), "time") == fn_gates.end()) {
                    err = "missing gate 'time' for sleep"; return false;
                }
                // annotate is_ms via const_cast (since sema owns AST here)
                const_cast<innesce::ast::Stmt::Sleep&>(SL).is_ms = (t.dur == DurUnit::MS);
            }
            else if (std::holds_alternative<S::Fail>(st.node)) {
                if (!in_quarantine) { err = "fail; outside of quarantine"; return false; }
            }
            else if (std::holds_alternative<S::Quarantine>(st.node)) {
                const auto& Q = std::get<S::Quarantine>(st.node);
                declared_quarantines[Q.name] = true;
                if (!check_block(Q.body, locals, enums, err, true, declared_quarantines, fn_gates)) return false;
            }
            else if (std::holds_alternative<S::Asm>(st.node)) {
                // no checks
            }
        }
        return true;
    }

    SemaResult Sema::check(const ast::Unit& u) {
        // Load enums
        enums_.clear();
        for (auto& e : u.enums) {
            std::unordered_map<std::string, int> m;
            for (int i = 0; i < (int)e.variants.size(); ++i) m[e.variants[i]] = i;
            enums_[e.name] = std::move(m);
        }

        bool has_main = false;
        for (auto& f : u.functions) {
            if (f.name == "main") has_main = true;
            std::unordered_map<std::string, Type> locals;
            std::string err;
            std::unordered_map<std::string, bool> declared;
            if (!check_block(f.body, locals, enums_, err, false, declared, f.gates)) return { err };
        }
        if (!has_main) return { "missing 'main' function" };
        return {};
    }

} // namespace innesce::front
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

        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums() const { return enums_; }
    private:
        std::unordered_map<std::string, std::unordered_map<std::string, int>> enums_;
    };

} // namespace innesce::front
#pragma once
#include <string_view>

namespace innesce {

    // Compile-time/no-op checkpoint markers; can be wired to profiling later.
    inline void checkpoint(std::string_view) noexcept {
        // no-op (placeholders for future instrumentation)
    }

} // namespace innesce
#pragma once
#include <cstdint>
#include <concepts>
#include <compare>
#include <type_traits>

namespace innesce {

    // Strong unit duration (integral count of base ticks), similar to std::chrono but minimal.
    // We intentionally do NOT mix units implicitly; conversions are explicit.
    template <typename UnitTag>
    struct Duration {
        using rep = std::int64_t;
        rep value{ 0 };

        constexpr Duration() = default;
        constexpr explicit Duration(rep v) : value(v) {}

        // Comparisons
        constexpr auto operator<=>(const Duration&) const = default;

        // Arithmetic (same-unit only)
        constexpr Duration operator+(Duration other) const { return Duration{ value + other.value }; }
        constexpr Duration operator-(Duration other) const { return Duration{ value - other.value }; }
        constexpr Duration& operator+=(Duration other) { value += other.value; return *this; }
        constexpr Duration& operator-=(Duration other) { value -= other.value; return *this; }

        // Scale by scalar
        template <std::integral I>
        constexpr Duration operator*(I k) const { return Duration{ value * static_cast<rep>(k) }; }

        template <std::integral I>
        constexpr Duration operator/(I k) const { return Duration{ value / static_cast<rep>(k) }; }
    };

    // Unit tags
    struct ns_tag {}; struct us_tag {}; struct ms_tag {}; struct sec_tag {}; struct min_tag {}; struct hr_tag {};

    using ns = Duration<ns_tag>;
    using us = Duration<us_tag>;
    using ms = Duration<ms_tag>;
    using sec = Duration<sec_tag>;
    using min = Duration<min_tag>;
    using hr = Duration<hr_tag>;

    // Explicit converters (compile-time factors)
    constexpr us to_us(ns x) { return us{ x.value / 1000 }; }
    constexpr ms to_ms(us x) { return ms{ x.value / 1000 }; }
    constexpr sec to_sec(ms x) { return sec{ x.value / 1000 }; }
    constexpr ms  to_ms(sec x) { return ms{ x.value * 1000 }; }
    constexpr us  to_us(ms x) { return us{ x.value * 1000 }; }
    constexpr ns  to_ns(us x) { return ns{ x.value * 1000 }; }

    // Literals helpers
    constexpr ns  operator""_ns(unsigned long long v) { return ns{ static_cast<ns::rep>(v) }; }
    constexpr us  operator""_us(unsigned long long v) { return us{ static_cast<us::rep>(v) * 1000 }; }
    constexpr ms  operator""_ms(unsigned long long v) { return ms{ static_cast<ms::rep>(v) * 1000 * 1000 }; }
    constexpr sec operator""_sec(unsigned long long v) { return sec{ static_cast<sec::rep>(v) }; }

} // namespace innesce
#include "core/lanes.hpp"
// Implementation currently header-only for simplicity; TU exists for future expansion.
#pragma once
#include <vector>
#include <thread>
#include <functional>
#include <barrier>
#include <atomic>
#include <cstddef>
#include <cassert>

namespace innesce {

    // Deterministic lanes: fixed number of worker threads; tasks are partitioned by index.
    class Lanes {
    public:
        explicit Lanes(std::size_t count)
            : count_(count)
        {
            assert(count_ > 0);
        }

        std::size_t size() const noexcept { return count_; }

        // Parallel for over [begin, end) with step 'chunk'.
        // Each lane operates on indices i where (i / chunk) % lane_count == lane_id.
        void parallel_for(std::size_t begin, std::size_t end, std::size_t chunk,
            const std::function<void(std::size_t, std::size_t)>& body) const
        {
            const std::size_t lanes = count_;
            std::vector<std::jthread> threads;
            threads.reserve(lanes);
            for (std::size_t lane = 0; lane < lanes; ++lane) {
                threads.emplace_back([=]() {
                    for (std::size_t i = begin; i < end; i += chunk) {
                        std::size_t slice = (i - begin) / chunk;
                        if ((slice % lanes) == lane) {
                            std::size_t hi = i + chunk;
                            if (hi > end) hi = end;
                            body(i, hi);
                        }
                    }
                    });
            }
            // join on destruction of jthreads
        }

        // Packetize into frames; barrier after each frame.
        void packetize_frames(std::size_t begin, std::size_t end, std::size_t frame,
            const std::function<void(std::size_t, std::size_t)>& body) const
        {
            const std::size_t lanes = count_;
            std::atomic<std::size_t> next{ begin };
            std::barrier sync(static_cast<std::ptrdiff_t>(lanes));
            std::vector<std::jthread> threads;
            threads.reserve(lanes);
            for (std::size_t lane = 0; lane < lanes; ++lane) {
                threads.emplace_back([&]() {
                    while (true) {
                        std::size_t i = next.fetch_add(frame);
                        if (i >= end) break;
                        std::size_t hi = i + frame;
                        if (hi > end) hi = end;
                        body(i, hi);
                        sync.arrive_and_wait();
                    }
                    });
            }
        }

    private:
        std::size_t count_;
    };

} // namespace innesce
#pragma once
#include <string>
#include <vector>
#include <utility>
#include <optional>

namespace innesce {

    struct Quarantine {
        bool failed{ false };
        std::vector<std::string> notes;

        void note(std::string msg) {
            notes.emplace_back(std::move(msg));
        }

        // Attempt wrapper: F must return std::optional<T>. If empty => failure recorded.
        template <typename F>
        auto attempt(F&& f) -> decltype(f()) {
            auto r = f();
            if (!r.has_value()) {
                failed = true;
                note("operation failed");
            }
            return r;
        }
    };

} // namespace innesce
#pragma once
#include <cstdint>

namespace innesce {

    enum class Truth : std::uint8_t { False = 0, True = 1, Unknown = 2, Both = 3 };

    // 4-valued truth operations (Kleene-like; tweak as desired)
    constexpr Truth t_not(Truth a) {
        switch (a) {
        case Truth::False:   return Truth::True;
        case Truth::True:    return Truth::False;
        case Truth::Unknown: return Truth::Unknown;
        case Truth::Both:    return Truth::Unknown; // design choice: ¬Both = Unknown
        }
        return Truth::Unknown;
    }

    constexpr Truth t_and(Truth a, Truth b) {
        // truth table encoded; symmetric
        if (a == Truth::False || b == Truth::False) return Truth::False;
        if (a == Truth::True && b == Truth::True)  return Truth::True;
        if (a == Truth::Both && b == Truth::True)  return Truth::Both;
        if (b == Truth::Both && a == Truth::True)  return Truth::Both;
        if (a == Truth::Both && b == Truth::Both)  return Truth::Both;
        // any combo with Unknown that isn't already decided:
        return Truth::Unknown;
    }

    constexpr Truth t_or(Truth a, Truth b) {
        if (a == Truth::True || b == Truth::True) return Truth::True;
        if (a == Truth::False && b == Truth::False) return Truth::False;
        if (a == Truth::Both || b == Truth::Both) return Truth::Both;
        return Truth::Unknown;
    }

} // namespace innesce

#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <variant>

namespace innesce::ast {

    enum class DurUnit { MS, SEC };

    struct Type {
        enum Kind { I32, ENUM, DUR } kind{ I32 };
        std::string enum_name; // if kind==ENUM
        DurUnit dur{};         // if kind==DUR
    };

    struct Expr {
        struct IntLit { int value; };
        struct Ident { std::string name; };
        struct Unary { char op; std::unique_ptr<Expr> rhs; };
        struct Binary { char op; std::unique_ptr<Expr> lhs, rhs; };
        struct IsFailed { std::string name; };
        struct DurLit { int value; DurUnit unit; };
        struct Cast { std::unique_ptr<Expr> inner; Type target; };
        struct Call { std::string name; std::vector<Expr> args; };

        std::variant<IntLit, Ident, Unary, Binary, IsFailed, DurLit, Cast, Call> node;
    };

    struct Stmt {
        struct Let { std::string name; Type type; Expr init; };
        struct Return { Expr value; };
        struct If { Expr cond; std::vector<Stmt> then_body; std::vector<Stmt> else_body; };
        struct Match {
            Expr scrutinee;
            struct Case { std::string label; std::vector<Stmt> body; bool is_default{ false }; };
            std::vector<Case> cases;
        };
        struct Sleep { Expr amount; bool is_ms{ true }; };
        struct Fail {};
        struct Quarantine { std::string name; std::vector<Stmt> body; };

        struct Asm {
            std::string body;
            bool intel{ true };
            struct Operand { bool is_output{ false }; std::string constraint; std::string name; bool is_immediate{ false }; int imm_val{ 0 }; };
            std::vector<Operand> ins;
            std::vector<std::string> clobbers;
        };

        std::variant<Let, Return, If, Match, Sleep, Fail, Quarantine, Asm> node;
    };

    struct EnumDecl { std::string name; std::vector<std::string> variants; };

    struct Function {
        std::string name;
        Type ret;
        std::vector<std::string> gates; // e.g., "time", "fs.read"
        bool hot{ false };
        std::vector<Stmt> body;
    };

    struct Unit { std::vector<EnumDecl> enums; std::vector<Function> functions; };

} // namespace innesce::ast

#include "frontend/lexer.hpp"
#include <cctype>

namespace innesce::front {

    Lexer::Lexer(std::string_view src) : s_(src) {}

    char Lexer::peek() const { return i_ < s_.size() ? s_[i_] : '\0'; }
    char Lexer::get() {
        if (i_ >= s_.size()) return '\0';
        char c = s_[i_++];
        if (c == '\n') { line_++; col_ = 1; }
        else { col_++; }
        return c;
    }

    void Lexer::skip_ws_and_comments() {
        while (true) {
            char c = peek();
            if (c == '\0') break;
            if (std::isspace(static_cast<unsigned char>(c))) { get(); continue; }
            if (c == '-' && i_ + 1 < s_.size() && s_[i_ + 1] == '-') {
                while (c != '\n' && c != '\0') c = get();
                continue;
            }
            break;
        }
    }

    Token Lexer::make(TokKind k, std::string t) {
        Token x{ k, std::move(t), 0, line_, col_ };
        return x;
    }

    Token Lexer::next() {
        skip_ws_and_comments();
        char c = peek();
        if (c == '\0') return make(TokKind::End, "");

        // punctuation/operators
        if (c == '(') { get(); return make(TokKind::LParen, "("); }
        if (c == ')') { get(); return make(TokKind::RParen, ")"); }
        if (c == '{') { get(); return make(TokKind::LBrace, "{"); }
        if (c == '}') { get(); return make(TokKind::RBrace, "}"); }
        if (c == '[') { get(); return make(TokKind::LBracket, "["); }
        if (c == ']') { get(); return make(TokKind::RBracket, "]"); }
        if (c == '+') { get(); return make(TokKind::Plus, "+"); }
        if (c == '-') {
            if (i_ + 1 < s_.size() && s_[i_ + 1] == '>') { get(); get(); return make(TokKind::Arrow, "->"); }
            get(); return make(TokKind::Minus, "-");
        }
        if (c == '*') { get(); return make(TokKind::Star, "*"); }
        if (c == '/') { get(); return make(TokKind::Slash, "/"); }
        if (c == ':') {
            get();
            if (peek() == '=') { get(); return make(TokKind::Assign, ":="); }
            return make(TokKind::Colon, ":");
        }
        if (c == ';') { get(); return make(TokKind::Semicolon, ";"); }
        if (c == ',') { get(); return make(TokKind::Comma, ","); }
        if (c == '=') {
            if (i_ + 1 < s_.size() && s_[i_ + 1] == '>') { get(); get(); return make(TokKind::FatArrow, "=>"); }
        }

        // number
        if (std::isdigit(static_cast<unsigned char>(c))) {
            int v = 0;
            while (std::isdigit(static_cast<unsigned char>(peek()))) {
                v = v * 10 + (get() - '0');
            }
            Token t = make(TokKind::Int, "");
            t.int_val = v;
            return t;
        }

        // identifier/keyword
        if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
            std::string id;
            while (std::isalnum(static_cast<unsigned char>(peek())) || peek() == '_') {
                id.push_back(get());
            }
            if (id == "fn") return make(TokKind::KwFn, id);
            if (id == "is") return make(TokKind::KwIs, id);
            if (id == "end") return make(TokKind::KwEnd, id);
            if (id == "return") return make(TokKind::KwReturn, id);
            if (id == "let") return make(TokKind::KwLet, id);
            if (id == "i32") return make(TokKind::KwI32, id);
            if (id == "if") return make(TokKind::KwIf, id);
            if (id == "then") return make(TokKind::KwThen, id);
            if (id == "else") return make(TokKind::KwElse, id);
            if (id == "type") return make(TokKind::KwType, id);
            if (id == "enum") return make(TokKind::KwEnum, id);
            if (id == "match") return make(TokKind::KwMatch, id);
            if (id == "case") return make(TokKind::KwCase, id);
            if (id == "default") return make(TokKind::KwDefault, id);
            if (id == "sleep") return make(TokKind::KwSleep, id);
            if (id == "quarantine") return make(TokKind::KwQuarantine, id);
            if (id == "fail") return make(TokKind::KwFail, id);
            if (id == "isfailed") return make(TokKind::KwIsFailed, id);
            if (id == "ms") return make(TokKind::KwMS, id);
            if (id == "sec") return make(TokKind::KwSEC, id);
            if (id == "asm") return make(TokKind::KwASM, id);
            if (id == "with") return make(TokKind::KwWith, id);
            if (id == "Hot") return make(TokKind::KwHot, id);
            if (id == "as") return make(TokKind::KwAs, id);
            Token t = make(TokKind::Ident, id);
            t.text = id;
            return t;
        }

        // unknown char -> skip
        get();
        return next();
    }

} // namespace innesce::front

#pragma once
#include <string>
#include <string_view>

namespace innesce::front {

    enum class TokKind {
        End,
        Ident,
        Int,
        // keywords
        KwFn, KwIs, KwEnd, KwReturn, KwLet,
        KwI32, KwIf, KwThen, KwElse,
        KwType, KwEnum, KwMatch, KwCase, KwDefault,
        KwSleep, KwQuarantine, KwFail, KwIsFailed,
        KwMS, KwSEC, KwASM, KwWith, KwHot, KwAs,
        // punctuation
        LParen, RParen, LBrace, RBrace, LBracket, RBracket,
        Colon, Semicolon, Comma,
        Arrow, Assign, FatArrow,
        // ops
        Plus, Minus, Star, Slash,
    };

    struct Token {
        TokKind kind;
        std::string text;
        int int_val{ 0 };
        int line{ 1 }, col{ 1 };
    };

    struct Lexer {
        explicit Lexer(std::string_view src);
        Token next();
    private:
        std::string_view s_;
        size_t i_{ 0 };
        int line_{ 1 }, col_{ 1 };
        char peek() const;
        char get();
        void skip_ws_and_comments();
        Token make(TokKind k, std::string t);
    };

} // namespace innesce::front

#include "frontend/parser.hpp"
#include <iostream>
#include <memory>

namespace innesce::front {

    Parser::Parser(std::string_view src) : lex_(src) { bump(); }

    void Parser::bump() { cur_ = lex_.next(); }
    bool Parser::accept(TokKind k) { if (cur_.kind == k) { bump(); return true; } return false; }
    bool Parser::expect(TokKind k, const char* what) {
        if (!accept(k)) { std::cerr << "Expected " << what << "\n"; return false; }
        return true;
    }

    std::optional<ast::Type> Parser::parse_type_name() {
        ast::Type t;
        if (accept(TokKind::KwI32)) { t.kind = ast::Type::I32; return t; }
        if (accept(TokKind::KwMS)) { t.kind = ast::Type::DUR; t.dur = ast::DurUnit::MS; return t; }
        if (accept(TokKind::KwSEC)) { t.kind = ast::Type::DUR; t.dur = ast::DurUnit::SEC; return t; }
        if (cur_.kind == TokKind::Ident) { t.kind = ast::Type::ENUM; t.enum_name = cur_.text; bump(); return t; }
        return std::nullopt;
    }

    std::optional<ast::Unit> Parser::parse_unit() {
        ast::Unit u;
        while (cur_.kind != TokKind::End) {
            if (cur_.kind == TokKind::KwType) {
                auto e = parse_enum_decl();
                if (!e) return std::nullopt;
                u.enums.push_back(std::move(*e));
                continue;
            }
            auto fn = parse_function();
            if (!fn) return std::nullopt;
            u.functions.push_back(std::move(*fn));
        }
        return u;
    }

    std::optional<ast::EnumDecl> Parser::parse_enum_decl() {
        if (!expect(TokKind::KwType, "'type'")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected enum name\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        if (!expect(TokKind::KwEnum, "'enum'")) return std::nullopt;
        if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
        ast::EnumDecl d; d.name = std::move(name);
        bool first = true;
        while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
            if (!first) expect(TokKind::Comma, "','");
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected variant name\n"; return std::nullopt; }
            d.variants.push_back(cur_.text);
            bump();
            first = false;
        }
        if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
        accept(TokKind::Semicolon);
        return d;
    }

    std::optional<ast::Function> Parser::parse_function() {
        if (!expect(TokKind::KwFn, "'fn'")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected function name\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::LParen, "'('")) return std::nullopt;
        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
        if (!expect(TokKind::Arrow, "'->'")) return std::nullopt;
        auto rty = parse_type_name(); if (!rty) { std::cerr << "Expected return type\n"; return std::nullopt; }

        // optional: with [ items ]
        std::vector<std::string> gates; bool hot = false;
        if (accept(TokKind::KwWith)) {
            if (!expect(TokKind::LBracket, "'['")) return std::nullopt;
            bool first = true;
            while (cur_.kind != TokKind::RBracket && cur_.kind != TokKind::End) {
                if (!first) expect(TokKind::Comma, "','");
                if (accept(TokKind::KwHot)) { hot = true; }
                else if (cur_.kind == TokKind::Ident) { gates.push_back(cur_.text); bump(); }
                else { std::cerr << "Expected gate name or Hot\n"; return std::nullopt; }
                first = false;
            }
            if (!expect(TokKind::RBracket, "']'")) return std::nullopt;
        }

        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;

        ast::Function f; f.name = std::move(name); f.ret = *rty; f.gates = std::move(gates); f.hot = hot;
        while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt();
            if (!st) return std::nullopt;
            f.body.push_back(std::move(*st));
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        return f;
    }

    std::optional<ast::Stmt> Parser::parse_asm_block() {
        if (!expect(TokKind::KwASM, "'asm'")) return std::nullopt;
        if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
        ast::Stmt::Asm A;
        while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
            if (cur_.kind == TokKind::Ident && cur_.text == "intel") { bump(); A.intel = true; continue; }
            if (cur_.kind == TokKind::Ident && cur_.text == "att") { bump(); A.intel = false; continue; }
            if (cur_.kind == TokKind::Ident && cur_.text == "ins") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected constraint ident after ins\n"; return std::nullopt; }
                    std::string cons = cur_.text; bump();
                    if (!expect(TokKind::LParen, "'('")) return std::nullopt;
                    if (cur_.kind == TokKind::Ident) {
                        std::string name = cur_.text; bump();
                        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                        A.ins.push_back({ false, cons, name, false, 0 });
                    }
                    else if (cur_.kind == TokKind::Int) {
                        int v = cur_.int_val; bump();
                        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                        A.ins.push_back({ false, cons, "", true, v });
                    }
                    else {
                        std::cerr << "Expected ident or int in ins()\n"; return std::nullopt;
                    }
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after ins entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "clobbers") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected clobber ident\n"; return std::nullopt; }
                    A.clobbers.push_back(cur_.text); bump();
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after clobbers entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "body") {
                bump();
                if (!expect(TokKind::Colon, "':'")) return std::nullopt;
                while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
                    if (cur_.kind == TokKind::Semicolon) { A.body.push_back('\n'); bump(); continue; }
                    if (cur_.kind == TokKind::Ident && (cur_.text == "ins" || cur_.text == "clobbers" || cur_.text == "intel" || cur_.text == "att"))
                        break;
                    if (!cur_.text.empty()) { A.body += cur_.text; A.body.push_back(' '); }
                    bump();
                }
                continue;
            }
            if (!cur_.text.empty()) { A.body += cur_.text; A.body.push_back(' '); }
            bump();
        }
        if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
        if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
        ast::Stmt st; st.node = std::move(A);
        return st;
    }

    std::optional<ast::Stmt> Parser::parse_match() {
        if (!expect(TokKind::KwMatch, "'match'")) return std::nullopt;
        auto scrut = parse_expr(); if (!scrut) return std::nullopt;
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        ast::Stmt::Match m; m.scrutinee = std::move(*scrut);
        while (cur_.kind == TokKind::KwCase) {
            bump();
            bool is_default = false;
            std::string label;
            if (accept(TokKind::KwDefault)) {
                is_default = true;
            }
            else {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected case label\n"; return std::nullopt; }
                label = cur_.text; bump();
            }
            if (!expect(TokKind::FatArrow, "'=>'")) return std::nullopt;
            auto body = parse_case_body();
            m.cases.push_back(ast::Stmt::Match::Case{ std::move(label), std::move(body), is_default });
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        ast::Stmt st; st.node = std::move(m);
        return st;
    }

    std::vector<ast::Stmt> Parser::parse_case_body() {
        std::vector<ast::Stmt> body;
        while (cur_.kind != TokKind::KwCase && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt();
            if (!st) break;
            body.push_back(std::move(*st));
        }
        return body;
    }

    std::optional<ast::Stmt> Parser::parse_stmt() {
        if (cur_.kind == TokKind::KwLet) {
            bump();
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected identifier after let\n"; return std::nullopt; }
            std::string name = cur_.text; bump();
            if (!expect(TokKind::Colon, "':'")) return std::nullopt;
            auto ty = parse_type_name(); if (!ty) { std::cerr << "Expected type name\n"; return std::nullopt; }
            if (!expect(TokKind::Assign, "':='")) return std::nullopt;
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Let{ std::move(name), *ty, std::move(*e) };
            return st;
        }
        if (cur_.kind == TokKind::KwReturn) {
            bump();
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Return{ std::move(*e) };
            return st;
        }
        if (cur_.kind == TokKind::KwIf) {
            bump();
            auto cond = parse_expr(); if (!cond) return std::nullopt;
            if (!expect(TokKind::KwThen, "'then'")) return std::nullopt;
            std::vector<ast::Stmt> then_body;
            while (cur_.kind != TokKind::KwElse && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                auto st = parse_stmt(); if (!st) return std::nullopt; then_body.push_back(std::move(*st));
            }
            std::vector<ast::Stmt> else_body;
            if (accept(TokKind::KwElse)) {
                while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                    auto st = parse_stmt(); if (!st) return std::nullopt; else_body.push_back(std::move(*st));
                }
            }
            if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::If{ std::move(*cond), std::move(then_body), std::move(else_body) };
            return st;
        }
        if (cur_.kind == TokKind::KwMatch) return parse_match();
        if (cur_.kind == TokKind::KwSleep) {
            bump();
            if (!expect(TokKind::LParen, "'('")) return std::nullopt;
            auto amt = parse_expr(); if (!amt) return std::nullopt;
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Sleep{ std::move(*amt), true };
            return st;
        }
        if (cur_.kind == TokKind::KwFail) {
            bump();
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Fail{};
            return st;
        }
        if (cur_.kind == TokKind::KwQuarantine) {
            bump();
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected quarantine name"; return std::nullopt; }
            std::string name = cur_.text; bump();
            if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
            std::vector<ast::Stmt> body;
            while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                auto st = parse_stmt(); if (!st) return std::nullopt; body.push_back(std::move(*st));
            }
            if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Quarantine{ std::move(name), std::move(body) };
            return st;
        }
        if (cur_.kind == TokKind::KwASM) {
            return parse_asm_block();
        }
        std::cerr << "Unknown statement\n";
        return std::nullopt;
    }

    std::optional<ast::Expr> Parser::parse_expr() {
        auto e = parse_add(); if (!e) return std::nullopt;
        while (accept(TokKind::KwAs)) {
            auto ty = parse_type_name(); if (!ty) { std::cerr << "Expected type after 'as'\n"; return std::nullopt; }
            ast::Expr ce; ce.node = ast::Expr::Cast{ std::make_unique<ast::Expr>(std::move(*e)), *ty };
            e = std::move(ce);
        }
        return e;
    }

    std::optional<ast::Expr> Parser::parse_add() {
        auto lhs = parse_mul(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Plus || cur_.kind == TokKind::Minus) {
            char op = (cur_.kind == TokKind::Plus) ? '+' : '-';
            bump();
            auto rhs = parse_mul(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Binary{ op, std::make_unique<innesce::ast::Expr>(std::move(*lhs)), std::make_unique<innesce::ast::Expr>(std::move(*rhs)) };
            lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_mul() {
        auto lhs = parse_unary(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Star || cur_.kind == TokKind::Slash) {
            char op = (cur_.kind == TokKind::Star) ? '*' : '/';
            bump();
            auto rhs = parse_unary(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Binary{ op, std::make_unique<innesce::ast::Expr>(std::move(*lhs)), std::make_unique<innesce::ast::Expr>(std::move(*rhs)) };
            lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_unary() {
        if (cur_.kind == TokKind::Minus) {
            bump();
            auto rhs = parse_unary(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Unary{ '-', std::make_unique<innesce::ast::Expr>(std::move(*rhs)) };
            return e;
        }
        return parse_postfix();
    }

    std::optional<ast::Expr> Parser::parse_postfix() {
        auto p = parse_primary(); if (!p) return std::nullopt;
        while (accept(TokKind::LParen)) {
            std::vector<ast::Expr> args;
            if (!accept(TokKind::RParen)) {
                while (true) {
                    auto a = parse_expr(); if (!a) return std::nullopt;
                    args.push_back(std::move(*a));
                    if (accept(TokKind::RParen)) break;
                    if (!expect(TokKind::Comma, "','")) return std::nullopt;
                }
            }
            if (!std::holds_alternative<ast::Expr::Ident>(p->node)) {
                std::cerr << "call target must be identifier\n"; return std::nullopt;
            }
            std::string name = std::get<ast::Expr::Ident>(p->node).name;
            ast::Expr call; call.node = ast::Expr::Call{ std::move(name), std::move(args) };
            p = std::move(call);
        }
        return p;
    }

    std::optional<ast::Expr> Parser::parse_primary() {
        if (cur_.kind == TokKind::Int) {
            int v = cur_.int_val; bump();
            if (accept(TokKind::KwMS)) { ast::Expr e; e.node = ast::Expr::DurLit{ v, ast::DurUnit::MS }; return e; }
            if (accept(TokKind::KwSEC)) { ast::Expr e; e.node = ast::Expr::DurLit{ v, ast::DurUnit::SEC }; return e; }
            ast::Expr e; e.node = ast::Expr::IntLit{ v }; return e;
        }
        if (cur_.kind == TokKind::Ident) { ast::Expr e; e.node = ast::Expr::Ident{ cur_.text }; bump(); return e; }
        if (cur_.kind == TokKind::KwIsFailed) {
            bump();
            if (!expect(TokKind::LParen, "'('")) return std::nullopt;
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected name in isfailed()"; return std::nullopt; }
            std::string q = cur_.text; bump();
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            ast::Expr e; e.node = ast::Expr::IsFailed{ std::move(q) }; return e;
        }
        if (accept(TokKind::LParen)) {
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            return e;
        }
        std::cerr << "Expected expression\n";
        return std::nullopt;
    }

} // namespace innesce::front

#pragma once
#include "frontend/lexer.hpp"
#include "frontend/ast.hpp"
#include <optional>

namespace innesce::front {

    class Parser {
    public:
        explicit Parser(std::string_view src);
        std::optional<ast::Unit> parse_unit();

    private:
        Lexer lex_;
        Token cur_;

        void bump();
        bool accept(TokKind k);
        bool expect(TokKind k, const char* what);

        std::optional<ast::EnumDecl> parse_enum_decl();
        std::optional<ast::Function> parse_function();
        std::optional<ast::Stmt> parse_stmt();
        std::optional<ast::Stmt> parse_match();
        std::optional<ast::Stmt> parse_asm_block();
        std::optional<ast::Expr> parse_expr();
        std::optional<ast::Expr> parse_add();
        std::optional<ast::Expr> parse_mul();
        std::optional<ast::Expr> parse_unary();
        std::optional<ast::Expr> parse_postfix();
        std::optional<ast::Expr> parse_primary();

        std::vector<ast::Stmt> parse_case_body();
        std::optional<ast::Type> parse_type_name();
    };

} // namespace innesce::front

#include "frontend/sema.hpp"
#include <unordered_map>
#include <variant>
#include <set>
#include <algorithm>

namespace innesce::front {

    using Type = innesce::ast::Type;
    using DurUnit = innesce::ast::DurUnit;

    static bool is_i32(const Type& t) { return t.kind == Type::I32; }
    static bool is_dur(const Type& t, DurUnit* u = nullptr) { if (t.kind == Type::DUR) { if (u)*u = t.dur; return true; } return false; }

    static bool has_gate(const std::vector<std::string>& gates, const std::string& g) {
        return std::find(gates.begin(), gates.end(), g) != gates.end();
    }

    static bool check_expr(const innesce::ast::Expr& e,
        const std::unordered_map<std::string, Type>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        const std::vector<std::string>& fn_gates,
        std::string& err,
        Type* outTy = nullptr)
    {
        using E = innesce::ast::Expr;
        if (std::holds_alternative<E::IntLit>(e.node)) {
            if (outTy) outTy->kind = Type::I32; return true;
        }
        if (std::holds_alternative<E::DurLit>(e.node)) {
            if (outTy) { outTy->kind = Type::DUR; outTy->dur = std::get<E::DurLit>(e.node).unit; } return true;
        }
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& I = std::get<E::Ident>(e.node);
            auto it = locals.find(I.name);
            if (it != locals.end()) { if (outTy) *outTy = it->second; return true; }
            for (auto& [ename, vars] : enums) {
                if (vars.count(I.name)) { if (outTy) { outTy->kind = Type::ENUM; outTy->enum_name = ename; } return true; }
            }
            err = "use of undeclared identifier: " + I.name; return false;
        }
        if (std::holds_alternative<E::IsFailed>(e.node)) {
            if (outTy) outTy->kind = Type::I32; return true;
        }
        if (std::holds_alternative<E::Unary>(e.node)) {
            const auto& U = std::get<E::Unary>(e.node);
            Type t; if (!check_expr(*U.rhs, locals, enums, fn_gates, err, &t)) return false;
            if (!is_i32(t)) { err = "unary '-' requires i32"; return false; }
            if (outTy) outTy->kind = Type::I32; return true;
        }
        if (std::holds_alternative<E::Cast>(e.node)) {
            const auto& C = std::get<E::Cast>(e.node);
            Type it; if (!check_expr(*C.inner, locals, enums, fn_gates, err, &it)) return false;
            if (is_dur(it) && C.target.kind == Type::DUR) {
                if (outTy) { *outTy = C.target; } return true;
            }
            err = "invalid cast: only duration unit conversions are supported"; return false;
        }
        if (std::holds_alternative<E::Call>(e.node)) {
            const auto& C = std::get<E::Call>(e.node);
            if (C.name == "fs_read_i32") {
                if (!has_gate(fn_gates, "fs.read")) { err = "missing gate 'fs.read' to call fs_read_i32"; return false; }
                if (outTy) outTy->kind = Type::I32; return true;
            }
            else if (C.name == "fs_write_i32") {
                if (!has_gate(fn_gates, "fs.write")) { err = "missing gate 'fs.write' to call fs_write_i32"; return false; }
                if (outTy) outTy->kind = Type::I32; return true;
            }
            else if (C.name == "net_ping_i32") {
                if (!has_gate(fn_gates, "net")) { err = "missing gate 'net' to call net_ping_i32"; return false; }
                if (outTy) outTy->kind = Type::I32; return true;
            }
            else {
                err = "unknown function: " + C.name; return false;
            }
        }
        const auto& B = std::get<E::Binary>(e.node);
        Type lt, rt;
        if (!check_expr(*B.lhs, locals, enums, fn_gates, err, &lt)) return false;
        if (!check_expr(*B.rhs, locals, enums, fn_gates, err, &rt)) return false;
        if (B.op == '+' || B.op == '-') {
            if (is_i32(lt) && is_i32(rt)) { if (outTy) outTy->kind = Type::I32; return true; }
            DurUnit ul, ur;
            if (is_dur(lt, &ul) && is_dur(rt, &ur) && ul == ur) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = ul; } return true; }
            err = "duration addition/subtraction requires same units"; return false;
        }
        if (B.op == '*') {
            DurUnit u;
            if (is_dur(lt, &u) && is_i32(rt)) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = u; } return true; }
            if (is_i32(lt) && is_dur(rt, &u)) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = u; } return true; }
            if (is_i32(lt) && is_i32(rt)) { if (outTy) outTy->kind = Type::I32; return true; }
            err = "invalid duration multiplication"; return false;
        }
        if (B.op == '/') {
            DurUnit u;
            if (is_dur(lt, &u) && is_i32(rt)) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = u; } return true; }
            if (is_i32(lt) && is_i32(rt)) { if (outTy) outTy->kind = Type::I32; return true; }
            err = "invalid duration division"; return false;
        }
        err = "unknown binary operator"; return false;
    }

    static bool check_block(const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Type> locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::string& err,
        bool in_quarantine,
        std::unordered_map<std::string, bool>& declared_quarantines,
        const std::vector<std::string>& fn_gates) {
        using S = innesce::ast::Stmt;
        for (auto& st : body) {
            if (std::holds_alternative<S::Let>(st.node)) {
                const auto& L = std::get<S::Let>(st.node);
                Type t;
                if (!check_expr(L.init, locals, enums, fn_gates, err, &t)) return false;
                if (L.type.kind != t.kind) { err = "type mismatch in let"; return false; }
                if (L.type.kind == Type::ENUM && L.type.enum_name != t.enum_name) { err = "enum type mismatch in let"; return false; }
                if (L.type.kind == Type::DUR && L.type.dur != t.dur) { err = "duration unit mismatch in let"; return false; }
                locals[L.name] = L.type;
            }
            else if (std::holds_alternative<S::Return>(st.node)) {
                const auto& R = std::get<S::Return>(st.node);
                Type t; if (!check_expr(R.value, locals, enums, fn_gates, err, &t)) return false;
            }
            else if (std::holds_alternative<S::If>(st.node)) {
                const auto& I = std::get<S::If>(st.node);
                Type t; if (!check_expr(I.cond, locals, enums, fn_gates, err, &t)) return false;
                if (!is_i32(t)) { err = "if condition must be i32"; return false; }
                if (!check_block(I.then_body, locals, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
                if (!check_block(I.else_body, locals, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
            }
            else if (std::holds_alternative<S::Match>(st.node)) {
                // Skip exhaustive details in this minimal skeleton
            }
            else if (std::holds_alternative<S::Sleep>(st.node)) {
                const auto& SL = std::get<S::Sleep>(st.node);
                Type t; if (!check_expr(SL.amount, locals, enums, fn_gates, err, &t)) return false;
                if (t.kind != Type::DUR) { err = "sleep expects duration value (ms or sec)"; return false; }
                if (!has_gate(fn_gates, "time")) { err = "missing gate 'time' for sleep"; return false; }
            }
            else if (std::holds_alternative<S::Fail>(st.node)) {
                if (!in_quarantine) { err = "fail; outside of quarantine"; return false; }
            }
            else if (std::holds_alternative<S::Quarantine>(st.node)) {
                const auto& Q = std::get<S::Quarantine>(st.node);
                declared_quarantines[Q.name] = true;
                if (!check_block(Q.body, locals, enums, err, true, declared_quarantines, fn_gates)) return false;
            }
            else if (std::holds_alternative<S::Asm>(st.node)) {
                // no checks
            }
        }
        return true;
    }

    SemaResult Sema::check(const ast::Unit& u) {
        enums_.clear();
        for (auto& e : u.enums) {
            std::unordered_map<std::string, int> m;
            for (int i = 0; i < (int)e.variants.size(); ++i) m[e.variants[i]] = i;
            enums_[e.name] = std::move(m);
        }

        bool has_main = false;
        for (auto& f : u.functions) {
            if (f.name == "main") has_main = true;
            std::unordered_map<std::string, Type> locals;
            std::string err;
            std::unordered_map<std::string, bool> declared;
            if (!check_block(f.body, locals, enums_, err, false, declared, f.gates)) return { err };
        }
        if (!has_main) return { "missing 'main' function" };
        return {};
    }

} // namespace innesce::front

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

        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums() const { return enums_; }
    private:
        std::unordered_map<std::string, std::unordered_map<std::string, int>> enums_;
    };

} // namespace innesce::front

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

#include "backend/llvm/codegen.hpp"
#include "frontend/ast.hpp"
#include "frontend/sema.hpp"
#include <llvm/ADT/Triple.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/raw_ostream.h>
#include <unordered_map>
#include <variant>

using namespace ::llvm;

namespace innesce::backend {

    // Forward decl
    static llvm::Type* toLlvmTy(LLVMContext& C, const innesce::ast::Type& t) {
        (void)t;
        return Type::getInt32Ty(C); // i32 for all for now (durations too)
    }

    static innesce::ast::Type get_expr_type(const innesce::ast::Expr& e,
        const std::unordered_map<std::string, innesce::ast::Type>& localsTy,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums) {
        using E = innesce::ast::Expr;
        innesce::ast::Type t;
        if (std::holds_alternative<E::IntLit>(e.node)) { t.kind = innesce::ast::Type::I32; return t; }
        if (std::holds_alternative<E::DurLit>(e.node)) { t.kind = innesce::ast::Type::DUR; t.dur = std::get<E::DurLit>(e.node).unit; return t; }
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& id = std::get<E::Ident>(e.node).name;
            auto it = localsTy.find(id);
            if (it != localsTy.end()) return it->second;
            // enum variant name?
            for (auto& [ename, vars] : enums) { (void)ename; if (vars.count(id)) { t.kind = innesce::ast::Type::I32; return t; } }
            t.kind = innesce::ast::Type::I32; return t;
        }
        if (std::holds_alternative<E::Unary>(e.node)) { t.kind = innesce::ast::Type::I32; return t; }
        if (std::holds_alternative<E::Binary>(e.node)) {
            auto& B = std::get<E::Binary>(e.node);
            auto lt = get_expr_type(*B.lhs, localsTy, enums);
            auto rt = get_expr_type(*B.rhs, localsTy, enums);
            if (B.op == '+' || B.op == '-') {
                if (lt.kind == innesce::ast::Type::DUR && rt.kind == innesce::ast::Type::DUR && lt.dur == rt.dur) return lt;
                t.kind = innesce::ast::Type::I32; return t;
            }
            if (B.op == '*' || B.op == '/') {
                if (lt.kind == innesce::ast::Type::DUR && rt.kind == innesce::ast::Type::I32) return lt;
                if (lt.kind == innesce::ast::Type::I32 && rt.kind == innesce::ast::Type::DUR) return rt;
                t.kind = innesce::ast::Type::I32; return t;
            }
            t.kind = innesce::ast::Type::I32; return t;
        }
        if (std::holds_alternative<E::IsFailed>(e.node)) { t.kind = innesce::ast::Type::I32; return t; }
        if (std::holds_alternative<E::Cast>(e.node)) {
            const auto& Cst = std::get<E::Cast>(e.node);
            return Cst.target;
        }
        if (std::holds_alternative<E::Call>(e.node)) { t.kind = innesce::ast::Type::I32; return t; }
        t.kind = innesce::ast::Type::I32; return t;
    }

    static Value* emit_expr(IRBuilder<>& B,
        const innesce::ast::Expr& e,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, Value*>& qflags,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        const std::unordered_map<std::string, innesce::ast::Type>& localsTy);

    static bool emit_block(IRBuilder<>& B,
        Function* F,
        const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Value*>& locals,
        std::unordered_map<std::string, Value*>& qflags,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::unordered_map<std::string, innesce::ast::Type>& localsTy);

    static Value* emit_expr(IRBuilder<>& B,
        const innesce::ast::Expr& e,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, Value*>& qflags,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        const std::unordered_map<std::string, innesce::ast::Type>& localsTy) {
        using E = innesce::ast::Expr;
        auto& Ctx = B.getContext();
        if (std::holds_alternative<E::IntLit>(e.node)) {
            int v = std::get<E::IntLit>(e.node).value; return B.getInt32(v);
        }
        if (std::holds_alternative<E::DurLit>(e.node)) {
            int v = std::get<E::DurLit>(e.node).value; return B.getInt32(v);
        }
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& nm = std::get<E::Ident>(e.node).name;
            auto it = locals.find(nm);
            if (it != locals.end()) return B.CreateLoad(B.getInt32Ty(), it->second);
            // enum variant constant
            for (auto& [ename, vars] : enums) {
                (void)ename;
                auto vit = vars.find(nm); if (vit != vars.end()) return B.getInt32(vit->second);
            }
            return nullptr;
        }
        if (std::holds_alternative<E::IsFailed>(e.node)) {
            const auto& IS = std::get<E::IsFailed>(e.node);
            auto it = qflags.find(IS.name); if (it == qflags.end()) return nullptr;
            auto flag = B.CreateLoad(B.getInt1Ty(), it->second);
            return B.CreateZExt(flag, B.getInt32Ty());
        }
        if (std::holds_alternative<E::Unary>(e.node)) {
            const auto& U = std::get<E::Unary>(e.node);
            Value* R = emit_expr(B, *U.rhs, locals, qflags, enums, localsTy); if (!R) return nullptr;
            if (U.op == '-') return B.CreateNeg(R);
            return R;
        }
        if (std::holds_alternative<E::Binary>(e.node)) {
            const auto& BN = std::get<E::Binary>(e.node);
            Value* L = emit_expr(B, *BN.lhs, locals, qflags, enums, localsTy); if (!L) return nullptr;
            Value* R = emit_expr(B, *BN.rhs, locals, qflags, enums, localsTy); if (!R) return nullptr;
            switch (BN.op) {
            case '+': return B.CreateAdd(L, R);
            case '-': return B.CreateSub(L, R);
            case '*': return B.CreateMul(L, R);
            case '/': return B.CreateSDiv(L, R);
            }
            return nullptr;
        }
        if (std::holds_alternative<E::Cast>(e.node)) {
            const auto& Cst = std::get<E::Cast>(e.node);
            Value* inner = emit_expr(B, *Cst.inner, locals, qflags, enums, localsTy); if (!inner) return nullptr;
            auto srcTy = get_expr_type(*Cst.inner, localsTy, enums);
            if (srcTy.kind == innesce::ast::Type::DUR && Cst.target.kind == innesce::ast::Type::DUR) {
                if (srcTy.dur == innesce::ast::DurUnit::MS && Cst.target.dur == innesce::ast::DurUnit::SEC) {
                    return B.CreateSDiv(inner, B.getInt32(1000));
                }
                else if (srcTy.dur == innesce::ast::DurUnit::SEC && Cst.target.dur == innesce::ast::DurUnit::MS) {
                    return B.CreateMul(inner, B.getInt32(1000));
                }
                else {
                    return inner; // same unit
                }
            }
            return inner;
        }
        if (std::holds_alternative<E::Call>(e.node)) {
            const auto& C = std::get<E::Call>(e.node);
            Module* M = B.GetInsertBlock()->getModule();
            std::vector<Type*> argtys;
            std::vector<Value*> args;
            for (auto& a : C.args) {
                Value* v = emit_expr(B, a, locals, qflags, enums, localsTy); if (!v) return nullptr;
                if (v->getType() != B.getInt32Ty()) v = B.CreateTruncOrBitCast(v, B.getInt32Ty());
                argtys.push_back(B.getInt32Ty()); args.push_back(v);
            }
            FunctionType* FT = FunctionType::get(B.getInt32Ty(), argtys, false);
            FunctionCallee cal = M->getOrInsertFunction(C.name, FT);
            return B.CreateCall(cal, args);
        }
        return nullptr;
    }

    static bool emit_match(IRBuilder<>& B,
        Function* F,
        const innesce::ast::Stmt::Match& M,
        std::unordered_map<std::string, Value*>& locals,
        std::unordered_map<std::string, Value*>& qflags,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::unordered_map<std::string, innesce::ast::Type>& localsTy) {
        Value* scr = emit_expr(B, M.scrutinee, locals, qflags, enums, localsTy); if (!scr) return false;
        BasicBlock* endBB = BasicBlock::Create(B.getContext(), "match.end", F);
        SwitchInst* swi = B.CreateSwitch(scr, endBB, M.cases.size());
        // build blocks
        std::vector<BasicBlock*> caseBBs;
        for (auto& C : M.cases) {
            BasicBlock* bb = BasicBlock::Create(B.getContext(), "case", F);
            swi->addCase(B.getInt32(C.is_default ? 0 : 0), bb); // default patched later
            caseBBs.push_back(bb);
        }
        // patch cases
        for (size_t i = 0; i < M.cases.size(); ++i) {
            auto& C = M.cases[i];
            if (!C.is_default) {
                // find enum value
                int val = 0;
                for (auto& [ename, vars] : enums) {
                    auto it = vars.find(C.label);
                    if (it != vars.end()) { val = it->second; break; }
                }
                swi->setSuccessorValue(i, B.getInt32(val));
            }
            else {
                swi->setDefaultDest(caseBBs[i]);
            }
        }
        // emit bodies
        for (size_t i = 0; i < M.cases.size(); ++i) {
            B.SetInsertPoint(caseBBs[i]);
            bool ret = emit_block(B, F, M.cases[i].body, locals, qflags, enums, localsTy);
            if (!ret) { /* fallthrough */ }
            if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(endBB);
        }
        B.SetInsertPoint(endBB);
        return true;
    }

    static bool emit_block(IRBuilder<>& B,
        Function* F,
        const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Value*>& locals,
        std::unordered_map<std::string, Value*>& qflags,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::unordered_map<std::string, innesce::ast::Type>& localsTy) {
        using S = innesce::ast::Stmt;
        for (auto& st : body) {
            if (std::holds_alternative<S::Let>(st.node)) {
                const auto& L = std::get<S::Let>(st.node);
                Value* init = emit_expr(B, L.init, locals, qflags, enums, localsTy); if (!init) return false;
                AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, L.name);
                B.CreateStore(init, a);
                locals[L.name] = a; localsTy[L.name] = L.type;
            }
            else if (std::holds_alternative<S::Return>(st.node)) {
                const auto& R = std::get<S::Return>(st.node);
                Value* v = emit_expr(B, R.value, locals, qflags, enums, localsTy); if (!v) return false;
                B.CreateRet(v);
                return true;
            }
            else if (std::holds_alternative<S::If>(st.node)) {
                const auto& I = std::get<S::If>(st.node);
                Value* c = emit_expr(B, I.cond, locals, qflags, enums, localsTy); if (!c) return false;
                c = B.CreateICmpNE(c, B.getInt32(0));
                BasicBlock* thenBB = BasicBlock::Create(B.getContext(), "then", F);
                BasicBlock* elseBB = BasicBlock::Create(B.getContext(), "else", F);
                BasicBlock* contBB = BasicBlock::Create(B.getContext(), "endif", F);
                B.CreateCondBr(c, thenBB, elseBB);
                // then
                B.SetInsertPoint(thenBB);
                emit_block(B, F, I.then_body, locals, qflags, enums, localsTy);
                if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(contBB);
                // else
                B.SetInsertPoint(elseBB);
                emit_block(B, F, I.else_body, locals, qflags, enums, localsTy);
                if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(contBB);
                // cont
                B.SetInsertPoint(contBB);
            }
            else if (std::holds_alternative<S::Match>(st.node)) {
                if (!emit_match(B, F, std::get<S::Match>(st.node), locals, qflags, enums, localsTy)) return false;
            }
            else if (std::holds_alternative<S::Sleep>(st.node)) {
                const auto& SL = std::get<S::Sleep>(st.node);
                Value* amt = emit_expr(B, SL.amount, locals, qflags, enums, localsTy); if (!amt) return false;
                Module* M = B.GetInsertBlock()->getModule();
                auto i64 = Type::getInt64Ty(B.getContext());
                auto FT = FunctionType::get(Type::getVoidTy(B.getContext()), { i64 }, false);
                const char* fname = SL.is_ms ? "inn_sleep_ms" : "inn_sleep_sec";
                Function* fs = cast<Function>(M->getOrInsertFunction(fname, FT).getCallee());
                Value* amt64 = B.CreateSExt(amt, i64);
                B.CreateCall(fs, { amt64 });
            }
            else if (std::holds_alternative<S::Fail>(st.node)) {
                if (qflags.empty()) return false;
                auto it = qflags.end(); --it;
                B.CreateStore(B.getInt1(true), it->second);
            }
            else if (std::holds_alternative<S::Quarantine>(st.node)) {
                const auto& Q = std::get<S::Quarantine>(st.node);
                AllocaInst* flag = B.CreateAlloca(B.getInt1Ty(), nullptr, (Q.name + "_failed").c_str());
                B.CreateStore(B.getInt1(false), flag);
                qflags[Q.name] = flag;
                emit_block(B, F, Q.body, locals, qflags, enums, localsTy);
            }
            else if (std::holds_alternative<S::Asm>(st.node)) {
                const auto& A = std::get<S::Asm>(st.node);
                std::vector<Type*> outtys;
                std::vector<Type*> argtys;
                std::vector<Value*> args;
                // Build locals load for inputs
                for (auto& op : A.ins) {
                    Value* v = nullptr;
                    if (op.is_immediate) v = B.getInt32(op.imm_val);
                    else {
                        auto it = locals.find(op.name);
                        if (it == locals.end()) return false;
                        v = B.CreateLoad(B.getInt32Ty(), it->second);
                    }
                    if (v->getType() != B.getInt32Ty()) v = B.CreateTruncOrBitCast(v, B.getInt32Ty());
                    argtys.push_back(B.getInt32Ty()); args.push_back(v);
                }
                // Outputs: assume all i32
                for (auto& op : A.outs) { (void)op; outtys.push_back(B.getInt32Ty()); }
                Type* retTy = Type::getVoidTy(B.getContext());
                if (outtys.size() == 1) retTy = outtys[0];
                else if (!outtys.empty()) retTy = StructType::get(B.getContext(), outtys);
                FunctionType* FT = FunctionType::get(retTy, argtys, false);
                // Constraints: outputs first, then inputs, then clobbers
                std::string cons;
                for (size_t i = 0; i < A.outs.size(); ++i) { cons += "=" + A.outs[i].constraint; if (i + 1 < A.outs.size()) cons += ","; }
                if (!A.outs.empty() && !A.ins.empty()) cons += ",";
                for (size_t i = 0; i < A.ins.size(); ++i) { cons += A.ins[i].constraint; if (i + 1 < A.ins.size()) cons += ","; }
                for (auto& c : A.clobbers) { cons += ",~{" + c + "}"; }
                auto IA = InlineAsm::get(FT, A.body, cons, /*sideeffect*/ true, /*alignstack*/ false,
                    A.intel ? InlineAsm::AD_Intel : InlineAsm::AD_ATT);
                CallInst* call = B.CreateCall(IA, args);
                // store outputs back to locals
                if (outtys.size() == 1) {
                    auto& op = A.outs[0];
                    auto it = locals.find(op.name); if (it == locals.end()) return false;
                    B.CreateStore(call, it->second);
                }
                else if (outtys.size() > 1) {
                    for (size_t i = 0; i < outtys.size(); ++i) {
                        auto& op = A.outs[i];
                        auto it = locals.find(op.name); if (it == locals.end()) return false;
                        Value* v = B.CreateExtractValue(call, { unsigned(i) });
                        B.CreateStore(v, it->second);
                    }
                }
            }
        }
        return true;
    }

    bool compile_to_object(const innesce::ast::Unit& unit,
        const innesce::front::Sema& sema,
        const std::string& out_obj_path,
        std::string& err) {
        LLVMInitializeNativeTarget();
        LLVMInitializeNativeAsmPrinter();
        LLVMInitializeNativeAsmParser();

        LLVMContext Ctx;
        auto mod = std::make_unique<Module>("inn", Ctx);
        IRBuilder<> B(Ctx);

        // Map enums from sema
        const auto& enums = sema.enums();

        // Create functions
        for (auto& f : unit.functions) {
            FunctionType* fty = FunctionType::get(Type::getInt32Ty(Ctx), {}, false);
            Function* F = Function::Create(fty, Function::ExternalLinkage, f.name, mod.get());
            if (f.hot) F->addFnAttr(Attribute::Hot);
            BasicBlock* entry = BasicBlock::Create(Ctx, "entry", F);
            B.SetInsertPoint(entry);
            std::unordered_map<std::string, Value*> locals;
            std::unordered_map<std::string, Value*> qflags;
            std::unordered_map<std::string, innesce::ast::Type> localsTy;
            emit_block(B, F, f.body, locals, qflags, enums, localsTy);
            if (!entry->getTerminator()) {
                // default return 0
                B.CreateRet(B.getInt32(0));
            }
        }

        // verify
        std::string verr;
        raw_string_ostream os(verr);
        if (verifyModule(*mod, &os)) { err = os.str(); return false; }

        // target machine
        auto targetTriple = sys::getDefaultTargetTriple();
        std::string errStr;
        auto target = TargetRegistry::lookupTarget(targetTriple, errStr);
        if (!target) { err = errStr; return false; }

        TargetOptions opt;
        auto RM = std::optional<Reloc::Model>();
        auto tm = std::unique_ptr<TargetMachine>(target->createTargetMachine(targetTriple, "generic", "", opt, RM));

        mod->setDataLayout(tm->createDataLayout());
        mod->setTargetTriple(targetTriple);

        std::error_code EC;
        raw_fd_ostream dest(out_obj_path, EC, sys::fs::OF_None);
        if (EC) { err = "Could not open output file: " + EC.message(); return false; }

        legacy::PassManager pass;
        if (tm->addPassesToEmitFile(pass, dest, nullptr, CGFT_ObjectFile)) { err = "TargetMachine can't emit an object file"; return false; }
        pass.run(*mod);
        dest.flush();
        return true;
    }

} // namespace innesce::backend

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
    if (argc < 2) {
        std::cerr << "usage: innescec <file.inn> [-o out.o]\n";
        return 2;
    }
    std::string srcpath = argv[1];
    std::string outobj;
    for (int i = 2; i < argc; i++) {
        std::string a = argv[i];
        if (a == "-o" && i + 1 < argc) { outobj = argv[++i]; }
    }
    std::ifstream in(srcpath);
    if (!in) { std::cerr << "cannot open " << srcpath << "\n"; return 2; }
    std::ostringstream ss; ss << in.rdbuf();
    innesce::front::Parser p(ss.str());
    auto unit = p.parse_unit();
    if (!unit) { std::cerr << "Parse error.\n"; return 3; }
    innesce::front::Sema s;
    auto res = s.check(*unit);
    if (!res.ok()) { std::cerr << "Sema error: " << res.error << "\n"; return 4; }

#ifdef INNSCE_ENABLE_LLVM
    if (!outobj.empty()) {
        std::string err;
        if (!innesce::backend::compile_to_object(*unit, s, outobj, err)) {
            std::cerr << "Codegen error: " << err << "\n"; return 5;
        }
        std::cout << "Wrote " << outobj << "\n";
        return 0;
    }
#endif

    std::cout << "OK\n";
    return 0;
}

#include <cstdint>
#include <cstdio>
extern "C" int fs_read_i32() { return 42; }      // demo stub
extern "C" int fs_write_i32() { return 1; }      // success
extern "C" int fs_open_i32() { return 3; }       // fake fd

#include <cstdint>
extern "C" int net_ping_i32() { return 7; }        // demo stub
extern "C" int net_tcp_i32() { return 200; }       // fake code

#include <random>
extern "C" int rand_i32() {
    static thread_local std::mt19937 rng{ 12345u };
    return static_cast<int>(rng());
}

#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <variant>

namespace innesce::ast {

    enum class DurUnit { MS, SEC };

    struct Type {
        enum Kind { I32, ENUM, DUR } kind{ I32 };
        std::string enum_name; // if kind==ENUM
        DurUnit dur{};         // if kind==DUR
    };

    struct Expr {
        struct IntLit { int value; };
        struct Ident { std::string name; };
        struct Unary { char op; std::unique_ptr<Expr> rhs; };
        struct Binary { char op; std::unique_ptr<Expr> lhs, rhs; };
        struct IsFailed { std::string name; };
        struct DurLit { int value; DurUnit unit; };
        struct Cast { std::unique_ptr<Expr> inner; Type target; };
        struct Call { std::string name; std::vector<Expr> args; };

        std::variant<IntLit, Ident, Unary, Binary, IsFailed, DurLit, Cast, Call> node;
    };

    struct Stmt {
        struct Let { std::string name; Type type; Expr init; };
        struct Return { Expr value; };
        struct If { Expr cond; std::vector<Stmt> then_body; std::vector<Stmt> else_body; };
        struct Match {
            Expr scrutinee;
            struct Case { std::string label; std::vector<Stmt> body; bool is_default{ false }; };
            std::vector<Case> cases;
        };
        struct Sleep { Expr amount; bool is_ms{ true }; };
        struct Fail {};
        struct Quarantine { std::string name; std::vector<Stmt> body; };

        struct Asm {
            std::string body;
            bool intel{ true };
            struct Operand { bool is_output{ false }; std::string constraint; std::string name; bool is_immediate{ false }; int imm_val{ 0 }; };
            std::vector<Operand> outs;
            std::vector<Operand> ins;
            std::vector<std::string> clobbers;
        };

        std::variant<Let, Return, If, Match, Sleep, Fail, Quarantine, Asm> node;
    };

    struct EnumDecl { std::string name; std::vector<std::string> variants; };

    struct Function {
        std::string name;
        Type ret;
        std::vector<std::string> gates; // e.g., "time", "fs.read"
        bool hot{ false };
        std::vector<Stmt> body;
    };

    struct Unit { std::vector<EnumDecl> enums; std::vector<Function> functions; };

} // namespace innesce::ast

#include "frontend/lexer.hpp"
#include <cctype>

namespace innesce::front {

    Lexer::Lexer(std::string_view src) : s_(src) {}

    char Lexer::peek() const { return i_ < s_.size() ? s_[i_] : '\0'; }
    char Lexer::get() {
        if (i_ >= s_.size()) return '\0';
        char c = s_[i_++];
        if (c == '\n') { line_++; col_ = 1; }
        else { col_++; }
        return c;
    }

    void Lexer::skip_ws_and_comments() {
        while (true) {
            char c = peek();
            if (c == '\0') break;
            if (std::isspace(static_cast<unsigned char>(c))) { get(); continue; }
            if (c == '-' && i_ + 1 < s_.size() && s_[i_ + 1] == '-') {
                while (c != '\n' && c != '\0') c = get();
                continue;
            }
            break;
        }
    }

    Token Lexer::make(TokKind k, std::string t) {
        Token x{ k, std::move(t), 0, line_, col_ };
        return x;
    }

    Token Lexer::next() {
        skip_ws_and_comments();
        char c = peek();
        if (c == '\0') return make(TokKind::End, "");

        // punctuation/operators
        if (c == '(') { get(); return make(TokKind::LParen, "("); }
        if (c == ')') { get(); return make(TokKind::RParen, ")"); }
        if (c == '{') { get(); return make(TokKind::LBrace, "{"); }
        if (c == '}') { get(); return make(TokKind::RBrace, "}"); }
        if (c == '[') { get(); return make(TokKind::LBracket, "["); }
        if (c == ']') { get(); return make(TokKind::RBracket, "]"); }
        if (c == '+') { get(); return make(TokKind::Plus, "+"); }
        if (c == '-') {
            if (i_ + 1 < s_.size() && s_[i_ + 1] == '>') { get(); get(); return make(TokKind::Arrow, "->"); }
            get(); return make(TokKind::Minus, "-");
        }
        if (c == '*') { get(); return make(TokKind::Star, "*"); }
        if (c == '/') { get(); return make(TokKind::Slash, "/"); }
        if (c == ':') {
            get();
            if (peek() == '=') { get(); return make(TokKind::Assign, ":="); }
            return make(TokKind::Colon, ":");
        }
        if (c == ';') { get(); return make(TokKind::Semicolon, ";"); }
        if (c == ',') { get(); return make(TokKind::Comma, ","); }
        if (c == '=') {
            if (i_ + 1 < s_.size() && s_[i_ + 1] == '>') { get(); get(); return make(TokKind::FatArrow, "=>"); }
        }

        // number
        if (std::isdigit(static_cast<unsigned char>(c))) {
            int v = 0;
            while (std::isdigit(static_cast<unsigned char>(peek()))) {
                v = v * 10 + (get() - '0');
            }
            Token t = make(TokKind::Int, "");
            t.int_val = v;
            return t;
        }

        // identifier/keyword
        if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
            std::string id;
            while (std::isalnum(static_cast<unsigned char>(peek())) || peek() == '_') {
                id.push_back(get());
            }
            if (id == "fn") return make(TokKind::KwFn, id);
            if (id == "is") return make(TokKind::KwIs, id);
            if (id == "end") return make(TokKind::KwEnd, id);
            if (id == "return") return make(TokKind::KwReturn, id);
            if (id == "let") return make(TokKind::KwLet, id);
            if (id == "i32") return make(TokKind::KwI32, id);
            if (id == "if") return make(TokKind::KwIf, id);
            if (id == "then") return make(TokKind::KwThen, id);
            if (id == "else") return make(TokKind::KwElse, id);
            if (id == "type") return make(TokKind::KwType, id);
            if (id == "enum") return make(TokKind::KwEnum, id);
            if (id == "match") return make(TokKind::KwMatch, id);
            if (id == "case") return make(TokKind::KwCase, id);
            if (id == "default") return make(TokKind::KwDefault, id);
            if (id == "sleep") return make(TokKind::KwSleep, id);
            if (id == "quarantine") return make(TokKind::KwQuarantine, id);
            if (id == "fail") return make(TokKind::KwFail, id);
            if (id == "isfailed") return make(TokKind::KwIsFailed, id);
            if (id == "ms") return make(TokKind::KwMS, id);
            if (id == "sec") return make(TokKind::KwSEC, id);
            if (id == "asm") return make(TokKind::KwASM, id);
            if (id == "with") return make(TokKind::KwWith, id);
            if (id == "Hot") return make(TokKind::KwHot, id);
            if (id == "as") return make(TokKind::KwAs, id);
            Token t = make(TokKind::Ident, id);
            t.text = id;
            return t;
        }

        // unknown char -> skip
        get();
        return next();
    }

} // namespace innesce::front

#pragma once
#include <string>
#include <string_view>

namespace innesce::front {

    enum class TokKind {
        End,
        Ident,
        Int,
        // keywords
        KwFn, KwIs, KwEnd, KwReturn, KwLet,
        KwI32, KwIf, KwThen, KwElse,
        KwType, KwEnum, KwMatch, KwCase, KwDefault,
        KwSleep, KwQuarantine, KwFail, KwIsFailed,
        KwMS, KwSEC, KwASM, KwWith, KwHot, KwAs,
        // punctuation
        LParen, RParen, LBrace, RBrace, LBracket, RBracket,
        Colon, Semicolon, Comma,
        Arrow, Assign, FatArrow,
        // ops
        Plus, Minus, Star, Slash,
    };

    struct Token {
        TokKind kind;
        std::string text;
        int int_val{ 0 };
        int line{ 1 }, col{ 1 };
    };

    struct Lexer {
        explicit Lexer(std::string_view src);
        Token next();
    private:
        std::string_view s_;
        size_t i_{ 0 };
        int line_{ 1 }, col_{ 1 };
        char peek() const;
        char get();
        void skip_ws_and_comments();
        Token make(TokKind k, std::string t);
    };

} // namespace innesce::front

#include "frontend/parser.hpp"
#include <iostream>
#include <memory>

namespace innesce::front {

    Parser::Parser(std::string_view src) : lex_(src) { bump(); }

    void Parser::bump() { cur_ = lex_.next(); }
    bool Parser::accept(TokKind k) { if (cur_.kind == k) { bump(); return true; } return false; }
    bool Parser::expect(TokKind k, const char* what) {
        if (!accept(k)) { std::cerr << "Expected " << what << "\n"; return false; }
        return true;
    }

    std::optional<ast::Type> Parser::parse_type_name() {
        ast::Type t;
        if (accept(TokKind::KwI32)) { t.kind = ast::Type::I32; return t; }
        if (accept(TokKind::KwMS)) { t.kind = ast::Type::DUR; t.dur = ast::DurUnit::MS; return t; }
        if (accept(TokKind::KwSEC)) { t.kind = ast::Type::DUR; t.dur = ast::DurUnit::SEC; return t; }
        if (cur_.kind == TokKind::Ident) { t.kind = ast::Type::ENUM; t.enum_name = cur_.text; bump(); return t; }
        return std::nullopt;
    }

    std::optional<ast::Unit> Parser::parse_unit() {
        ast::Unit u;
        while (cur_.kind != TokKind::End) {
            if (cur_.kind == TokKind::KwType) {
                auto e = parse_enum_decl();
                if (!e) return std::nullopt;
                u.enums.push_back(std::move(*e));
                continue;
            }
            auto fn = parse_function();
            if (!fn) return std::nullopt;
            u.functions.push_back(std::move(*fn));
        }
        return u;
    }

    std::optional<ast::EnumDecl> Parser::parse_enum_decl() {
        if (!expect(TokKind::KwType, "'type'")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected enum name\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        if (!expect(TokKind::KwEnum, "'enum'")) return std::nullopt;
        if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
        ast::EnumDecl d; d.name = std::move(name);
        bool first = true;
        while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
            if (!first) expect(TokKind::Comma, "','");
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected variant name\n"; return std::nullopt; }
            d.variants.push_back(cur_.text);
            bump();
            first = false;
        }
        if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
        accept(TokKind::Semicolon);
        return d;
    }

    std::optional<ast::Function> Parser::parse_function() {
        if (!expect(TokKind::KwFn, "'fn'")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected function name\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::LParen, "'('")) return std::nullopt;
        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
        if (!expect(TokKind::Arrow, "'->'")) return std::nullopt;
        auto rty = parse_type_name(); if (!rty) { std::cerr << "Expected return type\n"; return std::nullopt; }

        // optional: with [ items ]
        std::vector<std::string> gates; bool hot = false;
        if (accept(TokKind::KwWith)) {
            if (!expect(TokKind::LBracket, "'['")) return std::nullopt;
            bool first = true;
            while (cur_.kind != TokKind::RBracket && cur_.kind != TokKind::End) {
                if (!first) expect(TokKind::Comma, "','");
                if (accept(TokKind::KwHot)) { hot = true; }
                else if (cur_.kind == TokKind::Ident) { gates.push_back(cur_.text); bump(); }
                else { std::cerr << "Expected gate name or Hot\n"; return std::nullopt; }
                first = false;
            }
            if (!expect(TokKind::RBracket, "']'")) return std::nullopt;
        }

        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;

        ast::Function f; f.name = std::move(name); f.ret = *rty; f.gates = std::move(gates); f.hot = hot;
        while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt();
            if (!st) return std::nullopt;
            f.body.push_back(std::move(*st));
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        return f;
    }

    std::optional<ast::Stmt> Parser::parse_asm_block() {
        if (!expect(TokKind::KwASM, "'asm'")) return std::nullopt;
        if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
        ast::Stmt::Asm A;
        while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
            if (cur_.kind == TokKind::Ident && cur_.text == "intel") { bump(); A.intel = true; continue; }
            if (cur_.kind == TokKind::Ident && cur_.text == "att") { bump(); A.intel = false; continue; }

            if (cur_.kind == TokKind::Ident && cur_.text == "outs") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected constraint ident after outs\n"; return std::nullopt; }
                    std::string cons = cur_.text; bump();
                    if (!expect(TokKind::LParen, "'('")) return std::nullopt;
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected variable name in outs()\n"; return std::nullopt; }
                    std::string name = cur_.text; bump();
                    if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                    A.outs.push_back({ true, cons, name, false, 0 });
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after outs entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "ins") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected constraint ident after ins\n"; return std::nullopt; }
                    std::string cons = cur_.text; bump();
                    if (!expect(TokKind::LParen, "'('")) return std::nullopt;
                    if (cur_.kind == TokKind::Ident) {
                        std::string name = cur_.text; bump();
                        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                        A.ins.push_back({ false, cons, name, false, 0 });
                    }
                    else if (cur_.kind == TokKind::Int) {
                        int v = cur_.int_val; bump();
                        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                        A.ins.push_back({ false, cons, "", true, v });
                    }
                    else {
                        std::cerr << "Expected ident or int in ins()\n"; return std::nullopt;
                    }
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after ins entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "clobbers") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected clobber ident\n"; return std::nullopt; }
                    A.clobbers.push_back(cur_.text); bump();
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after clobbers entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "body") {
                bump();
                if (!expect(TokKind::Colon, "':'")) return std::nullopt;
                while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
                    if (cur_.kind == TokKind::Semicolon) { A.body.push_back('\n'); bump(); continue; }
                    if (cur_.kind == TokKind::Ident && (cur_.text == "ins" || cur_.text == "clobbers" || cur_.text == "intel" || cur_.text == "att"))
                        break;
                    if (!cur_.text.empty()) { A.body += cur_.text; A.body.push_back(' '); }
                    bump();
                }
                continue;
            }
            if (!cur_.text.empty()) { A.body += cur_.text; A.body.push_back(' '); }
            bump();
        }
        if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
        if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
        ast::Stmt st; st.node = std::move(A);
        return st;
    }

    std::optional<ast::Stmt> Parser::parse_match() {
        if (!expect(TokKind::KwMatch, "'match'")) return std::nullopt;
        auto scrut = parse_expr(); if (!scrut) return std::nullopt;
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        ast::Stmt::Match m; m.scrutinee = std::move(*scrut);
        while (cur_.kind == TokKind::KwCase) {
            bump();
            bool is_default = false;
            std::string label;
            if (accept(TokKind::KwDefault)) {
                is_default = true;
            }
            else {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected case label\n"; return std::nullopt; }
                label = cur_.text; bump();
            }
            if (!expect(TokKind::FatArrow, "'=>'")) return std::nullopt;
            auto body = parse_case_body();
            m.cases.push_back(ast::Stmt::Match::Case{ std::move(label), std::move(body), is_default });
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        ast::Stmt st; st.node = std::move(m);
        return st;
    }

    std::vector<ast::Stmt> Parser::parse_case_body() {
        std::vector<ast::Stmt> body;
        while (cur_.kind != TokKind::KwCase && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt();
            if (!st) break;
            body.push_back(std::move(*st));
        }
        return body;
    }

    std::optional<ast::Stmt> Parser::parse_stmt() {
        if (cur_.kind == TokKind::KwLet) {
            bump();
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected identifier after let\n"; return std::nullopt; }
            std::string name = cur_.text; bump();
            if (!expect(TokKind::Colon, "':'")) return std::nullopt;
            auto ty = parse_type_name(); if (!ty) { std::cerr << "Expected type name\n"; return std::nullopt; }
            if (!expect(TokKind::Assign, "':='")) return std::nullopt;
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Let{ std::move(name), *ty, std::move(*e) };
            return st;
        }
        if (cur_.kind == TokKind::KwReturn) {
            bump();
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Return{ std::move(*e) };
            return st;
        }
        if (cur_.kind == TokKind::KwIf) {
            bump();
            auto cond = parse_expr(); if (!cond) return std::nullopt;
            if (!expect(TokKind::KwThen, "'then'")) return std::nullopt;
            std::vector<ast::Stmt> then_body;
            while (cur_.kind != TokKind::KwElse && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                auto st = parse_stmt(); if (!st) return std::nullopt; then_body.push_back(std::move(*st));
            }
            std::vector<ast::Stmt> else_body;
            if (accept(TokKind::KwElse)) {
                while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                    auto st = parse_stmt(); if (!st) return std::nullopt; else_body.push_back(std::move(*st));
                }
            }
            if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::If{ std::move(*cond), std::move(then_body), std::move(else_body) };
            return st;
        }
        if (cur_.kind == TokKind::KwMatch) return parse_match();
        if (cur_.kind == TokKind::KwSleep) {
            bump();
            if (!expect(TokKind::LParen, "'('")) return std::nullopt;
            auto amt = parse_expr(); if (!amt) return std::nullopt;
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Sleep{ std::move(*amt), true };
            return st;
        }
        if (cur_.kind == TokKind::KwFail) {
            bump();
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Fail{};
            return st;
        }
        if (cur_.kind == TokKind::KwQuarantine) {
            bump();
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected quarantine name"; return std::nullopt; }
            std::string name = cur_.text; bump();
            if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
            std::vector<ast::Stmt> body;
            while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                auto st = parse_stmt(); if (!st) return std::nullopt; body.push_back(std::move(*st));
            }
            if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Quarantine{ std::move(name), std::move(body) };
            return st;
        }
        if (cur_.kind == TokKind::KwASM) {
            return parse_asm_block();
        }
        std::cerr << "Unknown statement\n";
        return std::nullopt;
    }

    std::optional<ast::Expr> Parser::parse_expr() {
        auto e = parse_add(); if (!e) return std::nullopt;
        while (accept(TokKind::KwAs)) {
            auto ty = parse_type_name(); if (!ty) { std::cerr << "Expected type after 'as'\n"; return std::nullopt; }
            ast::Expr ce; ce.node = ast::Expr::Cast{ std::make_unique<ast::Expr>(std::move(*e)), *ty };
            e = std::move(ce);
        }
        return e;
    }

    std::optional<ast::Expr> Parser::parse_add() {
        auto lhs = parse_mul(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Plus || cur_.kind == TokKind::Minus) {
            char op = (cur_.kind == TokKind::Plus) ? '+' : '-';
            bump();
            auto rhs = parse_mul(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Binary{ op, std::make_unique<innesce::ast::Expr>(std::move(*lhs)), std::make_unique<innesce::ast::Expr>(std::move(*rhs)) };
            lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_mul() {
        auto lhs = parse_unary(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Star || cur_.kind == TokKind::Slash) {
            char op = (cur_.kind == TokKind::Star) ? '*' : '/';
            bump();
            auto rhs = parse_unary(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Binary{ op, std::make_unique<innesce::ast::Expr>(std::move(*lhs)), std::make_unique<innesce::ast::Expr>(std::move(*rhs)) };
            lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_unary() {
        if (cur_.kind == TokKind::Minus) {
            bump();
            auto rhs = parse_unary(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Unary{ '-', std::make_unique<innesce::ast::Expr>(std::move(*rhs)) };
            return e;
        }
        return parse_postfix();
    }

    std::optional<ast::Expr> Parser::parse_postfix() {
        auto p = parse_primary(); if (!p) return std::nullopt;
        while (accept(TokKind::LParen)) {
            std::vector<ast::Expr> args;
            if (!accept(TokKind::RParen)) {
                while (true) {
                    auto a = parse_expr(); if (!a) return std::nullopt;
                    args.push_back(std::move(*a));
                    if (accept(TokKind::RParen)) break;
                    if (!expect(TokKind::Comma, "','")) return std::nullopt;
                }
            }
            if (!std::holds_alternative<ast::Expr::Ident>(p->node)) {
                std::cerr << "call target must be identifier\n"; return std::nullopt;
            }
            std::string name = std::get<ast::Expr::Ident>(p->node).name;
            ast::Expr call; call.node = ast::Expr::Call{ std::move(name), std::move(args) };
            p = std::move(call);
        }
        return p;
    }

    std::optional<ast::Expr> Parser::parse_primary() {
        if (cur_.kind == TokKind::Int) {
            int v = cur_.int_val; bump();
            if (accept(TokKind::KwMS)) { ast::Expr e; e.node = ast::Expr::DurLit{ v, ast::DurUnit::MS }; return e; }
            if (accept(TokKind::KwSEC)) { ast::Expr e; e.node = ast::Expr::DurLit{ v, ast::DurUnit::SEC }; return e; }
            ast::Expr e; e.node = ast::Expr::IntLit{ v }; return e;
        }
        if (cur_.kind == TokKind::Ident) { ast::Expr e; e.node = ast::Expr::Ident{ cur_.text }; bump(); return e; }
        if (cur_.kind == TokKind::KwIsFailed) {
            bump();
            if (!expect(TokKind::LParen, "'('")) return std::nullopt;
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected name in isfailed()"; return std::nullopt; }
            std::string q = cur_.text; bump();
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            ast::Expr e; e.node = ast::Expr::IsFailed{ std::move(q) }; return e;
        }
        if (accept(TokKind::LParen)) {
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            return e;
        }
        std::cerr << "Expected expression\n";
        return std::nullopt;
    }

} // namespace innesce::front

#pragma once
#include "frontend/lexer.hpp"
#include "frontend/ast.hpp"
#include <optional>

namespace innesce::front {

    class Parser {
    public:
        explicit Parser(std::string_view src);
        std::optional<ast::Unit> parse_unit();

    private:
        Lexer lex_;
        Token cur_;

        void bump();
        bool accept(TokKind k);
        bool expect(TokKind k, const char* what);

        std::optional<ast::EnumDecl> parse_enum_decl();
        std::optional<ast::Function> parse_function();
        std::optional<ast::Stmt> parse_stmt();
        std::optional<ast::Stmt> parse_match();
        std::optional<ast::Stmt> parse_asm_block();
        std::optional<ast::Expr> parse_expr();
        std::optional<ast::Expr> parse_add();
        std::optional<ast::Expr> parse_mul();
        std::optional<ast::Expr> parse_unary();
        std::optional<ast::Expr> parse_postfix();
        std::optional<ast::Expr> parse_primary();

        std::vector<ast::Stmt> parse_case_body();
        std::optional<ast::Type> parse_type_name();
    };

} // namespace innesce::front

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

        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums() const { return enums_; }
    private:
        std::unordered_map<std::string, std::unordered_map<std::string, int>> enums_;
    };

} // namespace innesce::front

#include "backend/llvm/codegen.hpp"
#include "frontend/ast.hpp"
#include "frontend/sema.hpp"
#include <llvm/ADT/Triple.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/raw_ostream.h>
#include <unordered_map>
#include <variant>

using namespace ::llvm;

namespace innesce::backend {

    static Value* make_cstring(IRBuilder<>& B, Module* M, std::string_view s) {
        auto& C = B.getContext();
        auto arrTy = ArrayType::get(Type::getInt8Ty(C), s.size() + 1);
        auto gv = new GlobalVariable(*M, arrTy, /*isConst*/true, GlobalValue::PrivateLinkage, nullptr, ".str");
        gv->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
        gv->setAlignment(Align(1));
        std::vector<Constant*> chars;
        for (char ch : s) chars.push_back(ConstantInt::get(Type::getInt8Ty(C), (uint8_t)ch));
        chars.push_back(ConstantInt::get(Type::getInt8Ty(C), 0));
        gv->setInitializer(ConstantArray::get(arrTy, chars));
        Value* zero = ConstantInt::get(Type::getInt32Ty(C), 0);
        Value* gep = ConstantExpr::getInBoundsGetElementPtr(arrTy, gv, { zero, zero });
        return ConstantExpr::getBitCast(gep, Type::getInt8PtrTy(C));
    }

    static innesce::ast::Type get_expr_type(const innesce::ast::Expr& e,
        const std::unordered_map<std::string, innesce::ast::Type>& localsTy,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums) {
        using E = innesce::ast::Expr;
        innesce::ast::Type t;
        if (std::holds_alternative<E::IntLit>(e.node)) { t.kind = innesce::ast::Type::I32; return t; }
        if (std::holds_alternative<E::StringLit>(e.node)) { t.kind = innesce::ast::Type::STR; return t; }
        if (std::holds_alternative<E::DurLit>(e.node)) { t.kind = innesce::ast::Type::DUR; t.dur = std::get<E::DurLit>(e.node).unit; return t; }
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& id = std::get<E::Ident>(e.node).name;
            auto it = localsTy.find(id);
            if (it != localsTy.end()) return it->second;
            for (auto& [ename, vars] : enums) { (void)ename; if (vars.count(id)) { t.kind = innesce::ast::Type::I32; return t; } }
            t.kind = innesce::ast::Type::I32; return t;
        }
        if (std::holds_alternative<E::Unary>(e.node)) { t.kind = innesce::ast::Type::I32; return t; }
        if (std::holds_alternative<E::Binary>(e.node)) { t.kind = innesce::ast::Type::I32; return t; }
        if (std::holds_alternative<E::IsFailed>(e.node)) { t.kind = innesce::ast::Type::I32; return t; }
        if (std::holds_alternative<E::Cast>(e.node)) { return std::get<E::Cast>(e.node).target; }
        if (std::holds_alternative<E::Call>(e.node)) { t.kind = innesce::ast::Type::I32; return t; }
        if (std::holds_alternative<E::AsmExpr>(e.node)) {
            const auto& A = std::get<E::AsmExpr>(e.node);
            if (A.outs.size() <= 1) { t.kind = innesce::ast::Type::I32; return t; }
            t.kind = innesce::ast::Type::TUPLE; t.tuple_elems.assign(A.outs.size(), innesce::ast::Type{ .kind = innesce::ast::Type::I32 });
            return t;
        }
        t.kind = innesce::ast::Type::I32; return t;
    }

    static Value* emit_expr(IRBuilder<>& B,
        const innesce::ast::Expr& e,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, Value*>& qflags,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        const std::unordered_map<std::string, innesce::ast::Type>& localsTy);

    static bool emit_block(IRBuilder<>& B,
        Function* F,
        const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Value*>& locals,
        std::unordered_map<std::string, Value*>& qflags,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::unordered_map<std::string, innesce::ast::Type>& localsTy);

    static Value* emit_expr(IRBuilder<>& B,
        const innesce::ast::Expr& e,
        std::unordered_map<std::string, Value*>& locals,
        const std::unordered_map<std::string, Value*>& qflags,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        const std::unordered_map<std::string, innesce::ast::Type>& localsTy) {
        using E = innesce::ast::Expr;
        if (std::holds_alternative<E::IntLit>(e.node)) {
            int v = std::get<E::IntLit>(e.node).value; return B.getInt32(v);
        }
        if (std::holds_alternative<E::StringLit>(e.node)) {
            Module* M = B.GetInsertBlock()->getModule();
            return make_cstring(B, M, std::get<E::StringLit>(e.node).value);
        }
        if (std::holds_alternative<E::DurLit>(e.node)) {
            int v = std::get<E::DurLit>(e.node).value; return B.getInt32(v);
        }
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& nm = std::get<E::Ident>(e.node).name;
            auto it = locals.find(nm);
            if (it != locals.end()) return B.CreateLoad(B.getInt32Ty(), it->second);
            for (auto& [ename, vars] : enums) {
                (void)ename;
                auto vit = vars.find(nm); if (vit != vars.end()) return B.getInt32(vit->second);
            }
            return nullptr;
        }
        if (std::holds_alternative<E::IsFailed>(e.node)) {
            const auto& IS = std::get<E::IsFailed>(e.node);
            auto it = qflags.find(IS.name); if (it == qflags.end()) return nullptr;
            auto flag = B.CreateLoad(B.getInt1Ty(), it->second);
            return B.CreateZExt(flag, B.getInt32Ty());
        }
        if (std::holds_alternative<E::Unary>(e.node)) {
            const auto& U = std::get<E::Unary>(e.node);
            Value* R = emit_expr(B, *U.rhs, locals, qflags, enums, localsTy); if (!R) return nullptr;
            if (U.op == '-') return B.CreateNeg(R);
            return R;
        }
        if (std::holds_alternative<E::Binary>(e.node)) {
            const auto& BN = std::get<E::Binary>(e.node);
            Value* L = emit_expr(B, *BN.lhs, locals, qflags, enums, localsTy); if (!L) return nullptr;
            Value* R = emit_expr(B, *BN.rhs, locals, qflags, enums, localsTy); if (!R) return nullptr;
            switch (BN.op) {
            case '+': return B.CreateAdd(L, R);
            case '-': return B.CreateSub(L, R);
            case '*': return B.CreateMul(L, R);
            case '/': return B.CreateSDiv(L, R);
            }
            return nullptr;
        }
        if (std::holds_alternative<E::Cast>(e.node)) {
            const auto& Cst = std::get<E::Cast>(e.node);
            if (std::holds_alternative<E::DurLit>(Cst.inner->node)) {
                auto dl = std::get<E::DurLit>(Cst.inner->node);
                int v = dl.value;
                if (dl.unit == innesce::ast::DurUnit::MS && Cst.target.kind == innesce::ast::Type::DUR && Cst.target.dur == innesce::ast::DurUnit::SEC) {
                    return B.getInt32(v / 1000);
                }
                else if (dl.unit == innesce::ast::DurUnit::SEC && Cst.target.kind == innesce::ast::Type::DUR && Cst.target.dur == innesce::ast::DurUnit::MS) {
                    return B.getInt32(v * 1000);
                }
                else {
                    return B.getInt32(v);
                }
            }
            Value* inner = emit_expr(B, *Cst.inner, locals, qflags, enums, localsTy); if (!inner) return nullptr;
            auto srcTy = get_expr_type(*Cst.inner, localsTy, enums);
            if (srcTy.kind == innesce::ast::Type::DUR && Cst.target.kind == innesce::ast::Type::DUR) {
                if (srcTy.dur == innesce::ast::DurUnit::MS && Cst.target.dur == innesce::ast::DurUnit::SEC) {
                    return B.CreateSDiv(inner, B.getInt32(1000));
                }
                else if (srcTy.dur == innesce::ast::DurUnit::SEC && Cst.target.dur == innesce::ast::DurUnit::MS) {
                    return B.CreateMul(inner, B.getInt32(1000));
                }
                else {
                    return inner;
                }
            }
            return inner;
        }
        if (std::holds_alternative<E::Call>(e.node)) {
            const auto& C = std::get<E::Call>(e.node);
            Module* M = B.GetInsertBlock()->getModule();
            std::vector<Type*> argtys;
            std::vector<Value*> args;
            auto i32 = B.getInt32Ty();
            auto i8p = Type::getInt8PtrTy(B.getContext());
            auto marshal = [&](const innesce::ast::Expr& a)->Value* {
                return emit_expr(B, a, locals, qflags, enums, localsTy);
                };
            if (C.name == "fs_open") {
                Value* v = marshal(C.args[0]); if (v->getType() != i8p) v = B.CreateBitCast(v, i8p);
                FunctionType* FT = FunctionType::get(i32, { i8p }, false);
                return B.CreateCall(M->getOrInsertFunction("fs_open_str", FT), { v });
            }
            else if (C.name == "net_tcp") {
                Value* host = marshal(C.args[0]); if (host->getType() != i8p) host = B.CreateBitCast(host, i8p);
                Value* port = marshal(C.args[1]); if (port->getType() != i32) port = B.CreateTruncOrBitCast(port, i32);
                FunctionType* FT = FunctionType::get(i32, { i8p, i32 }, false);
                return B.CreateCall(M->getOrInsertFunction("net_tcp_str_i32", FT), { host, port });
            }
            else if (C.name == "rand_range") {
                Value* lo = marshal(C.args[0]); if (lo->getType() != i32) lo = B.CreateTruncOrBitCast(lo, i32);
                Value* hi = marshal(C.args[1]); if (hi->getType() != i32) hi = B.CreateTruncOrBitCast(hi, i32);
                FunctionType* FT = FunctionType::get(i32, { i32, i32 }, false);
                return B.CreateCall(M->getOrInsertFunction("rand_range_i32", FT), { lo, hi });
            }
            else {
                for (auto& a : C.args) {
                    Value* v = marshal(a);
                    if (v->getType() != i32) v = B.CreateTruncOrBitCast(v, i32);
                    argtys.push_back(i32); args.push_back(v);
                }
                FunctionType* FT = FunctionType::get(i32, argtys, false);
                FunctionCallee cal = M->getOrInsertFunction(C.name, FT);
                return B.CreateCall(cal, args);
            }
        }
        if (std::holds_alternative<E::AsmExpr>(e.node)) {
            const auto& A = std::get<E::AsmExpr>(e.node);
            std::vector<Type*> outtys;
            std::vector<Type*> argtys;
            std::vector<Value*> args;
            for (auto& op : A.ins) {
                Value* v = nullptr;
                if (op.is_immediate) v = B.getInt32(op.imm_val);
                else {
                    auto it = locals.find(op.name);
                    if (it == locals.end()) return nullptr;
                    v = B.CreateLoad(B.getInt32Ty(), it->second);
                }
                if (v->getType() != B.getInt32Ty()) v = B.CreateTruncOrBitCast(v, B.getInt32Ty());
                argtys.push_back(B.getInt32Ty()); args.push_back(v);
            }
            for (auto& op : A.outs) outtys.push_back(B.getInt32Ty());
            Type* retTy = Type::getVoidTy(B.getContext());
            if (outtys.size() == 1) retTy = outtys[0];
            else if (!outtys.empty()) retTy = StructType::get(B.getContext(), outtys);
            std::string cons;
            for (size_t i = 0; i < A.outs.size(); ++i) { cons += "=" + A.outs[i].constraint; if (i + 1 < A.outs.size()) cons += ","; }
            if (!A.outs.empty() && !A.ins.empty()) cons += ",";
            for (size_t i = 0; i < A.ins.size(); ++i) { cons += A.ins[i].constraint; if (i + 1 < A.ins.size()) cons += ","; }
            for (auto& c : A.clobbers) { cons += ",~{" + c + "}"; }
            auto IA = InlineAsm::get(FunctionType::get(retTy, argtys, false), A.body, cons, true, false, A.intel ? InlineAsm::AD_Intel : InlineAsm::AD_ATT);
            CallInst* call = B.CreateCall(IA, args);
            if (outtys.size() == 1) return call;
            if (!outtys.empty()) return call;
            return B.getInt32(0);
        }
        return nullptr;
    }

    static bool emit_match(IRBuilder<>& B,
        Function* F,
        const innesce::ast::Stmt::Match& M,
        std::unordered_map<std::string, Value*>& locals,
        std::unordered_map<std::string, Value*>& qflags,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::unordered_map<std::string, innesce::ast::Type>& localsTy) {
        auto scrTy = get_expr_type(M.scrutinee, localsTy, enums);
        Value* scr = emit_expr(B, M.scrutinee, locals, qflags, enums, localsTy); if (!scr) return false;

        BasicBlock* endBB = BasicBlock::Create(B.getContext(), "match.end", F);

        if (scrTy.kind == innesce::ast::Type::I32 || scrTy.kind == innesce::ast::Type::ENUM) {
            SwitchInst* swi = B.CreateSwitch(scr, endBB, M.cases.size());
            std::vector<BasicBlock*> bodyBBs; bodyBBs.reserve(M.cases.size());
            int defaultIdx = -1;
            for (size_t i = 0; i < M.cases.size(); ++i) {
                auto& C = M.cases[i];
                BasicBlock* bb = BasicBlock::Create(B.getContext(), "case", F);
                bodyBBs.push_back(bb);
                if (C.is_default) { defaultIdx = int(i); continue; }
                int val = 0;
                if (!C.is_tuple) {
                    for (auto& [ename, vars] : enums) {
                        auto it = vars.find(C.label);
                        if (it != vars.end()) { val = it->second; break; }
                    }
                }
                swi->addCase(B.getInt32(val), bb);
            }
            if (defaultIdx >= 0) swi->setDefaultDest(bodyBBs[defaultIdx]);
            for (size_t i = 0; i < M.cases.size(); ++i) {
                B.SetInsertPoint(bodyBBs[i]);
                emit_block(B, F, M.cases[i].body, locals, qflags, enums, localsTy);
                if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(endBB);
            }
            B.SetInsertPoint(endBB);
            return true;
        }

        // Tuple: create temp alloca to extract elements multiple times
        AllocaInst* tupAlloca = B.CreateAlloca(scr->getType(), nullptr, "tup.scr");
        B.CreateStore(scr, tupAlloca);

        auto extract = [&](unsigned idx)->Value* {
            Value* v = B.CreateLoad(tupAlloca->getAllocatedType(), tupAlloca);
            return B.CreateExtractValue(v, { idx });
            };

        BasicBlock* curCont = endBB;
        for (int i = int(M.cases.size()) - 1; i >= 0; --i) {
            auto& C = M.cases[size_t(i)];
            BasicBlock* bodyBB = BasicBlock::Create(B.getContext(), "case.body", F);
            B.SetInsertPoint(bodyBB);
            emit_block(B, F, C.body, locals, qflags, enums, localsTy);
            if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(endBB);

            BasicBlock* testBB = BasicBlock::Create(B.getContext(), "case.test", F);
            B.SetInsertPoint(testBB);
            Value* cond = nullptr;
            if (C.is_default) {
                cond = B.getInt1(true);
            }
            else if (C.is_tuple) {
                for (unsigned idx = 0; idx < C.tpat.size(); ++idx) {
                    auto el = extract(idx);
                    Value* c = nullptr;
                    if (std::holds_alternative<innesce::ast::Pattern::Wild>(C.tpat[idx].node)) {
                        c = B.getInt1(true);
                    }
                    else if (std::holds_alternative<innesce::ast::Pattern::Int>(C.tpat[idx].node)) {
                        int v = std::get<innesce::ast::Pattern::Int>(C.tpat[idx].node).value;
                        c = B.CreateICmpEQ(el, B.getInt32(v));
                    }
                    else if (std::holds_alternative<innesce::ast::Pattern::Dur>(C.tpat[idx].node)) {
                        int v = std::get<innesce::ast::Pattern::Dur>(C.tpat[idx].node).value;
                        c = B.CreateICmpEQ(el, B.getInt32(v));
                    }
                    cond = cond ? B.CreateAnd(cond, c) : c;
                }
            }
            else {
                cond = B.getInt1(false);
            }
            B.CreateCondBr(cond, bodyBB, curCont);
            curCont = testBB;
        }
        B.SetInsertPoint(curCont);
        B.CreateBr(endBB);
        B.SetInsertPoint(endBB);
        return true;
    }

    static bool emit_block(IRBuilder<>& B,
        Function* F,
        const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Value*>& locals,
        std::unordered_map<std::string, Value*>& qflags,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::unordered_map<std::string, innesce::ast::Type>& localsTy) {
        using S = innesce::ast::Stmt;
        for (auto& st : body) {
            if (std::holds_alternative<S::Let>(st.node)) {
                const auto& L = std::get<S::Let>(st.node);
                Value* init = emit_expr(B, L.init, locals, qflags, enums, localsTy); if (!init) return false;
                AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, L.name);
                B.CreateStore(init, a);
                locals[L.name] = a; localsTy[L.name] = L.type;
            }
            else if (std::holds_alternative<S::LetTuple>(st.node)) {
                const auto& LT = std::get<S::LetTuple>(st.node);
                Value* init = emit_expr(B, LT.init, locals, qflags, enums, localsTy); if (!init) return false;
                if (LT.names.size() == 1) {
                    AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, LT.names[0]);
                    B.CreateStore(init, a);
                    locals[LT.names[0]] = a; localsTy[LT.names[0]] = LT.types[0];
                }
                else {
                    for (size_t i = 0; i < LT.names.size(); ++i) {
                        Value* v = B.CreateExtractValue(init, { unsigned(i) });
                        AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, LT.names[i]);
                        B.CreateStore(v, a);
                        locals[LT.names[i]] = a; localsTy[LT.names[i]] = LT.types[i];
                    }
                }
            }
            else if (std::holds_alternative<S::Return>(st.node)) {
                const auto& R = std::get<S::Return>(st.node);
                Value* v = emit_expr(B, R.value, locals, qflags, enums, localsTy); if (!v) return false;
                B.CreateRet(v);
                return true;
            }
            else if (std::holds_alternative<S::If>(st.node)) {
                const auto& I = std::get<S::If>(st.node);
                Value* c = emit_expr(B, I.cond, locals, qflags, enums, localsTy); if (!c) return false;
                c = B.CreateICmpNE(c, B.getInt32(0));
                BasicBlock* thenBB = BasicBlock::Create(B.getContext(), "then", F);
                BasicBlock* elseBB = BasicBlock::Create(B.getContext(), "else", F);
                BasicBlock* contBB = BasicBlock::Create(B.getContext(), "endif", F);
                B.CreateCondBr(c, thenBB, elseBB);
                B.SetInsertPoint(thenBB);
                emit_block(B, F, I.then_body, locals, qflags, enums, localsTy);
                if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(contBB);
                B.SetInsertPoint(elseBB);
                emit_block(B, F, I.else_body, locals, qflags, enums, localsTy);
                if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(contBB);
                B.SetInsertPoint(contBB);
            }
            else if (std::holds_alternative<S::Match>(st.node)) {
                if (!emit_match(B, F, std::get<S::Match>(st.node), locals, qflags, enums, localsTy)) return false;
            }
            else if (std::holds_alternative<S::Sleep>(st.node)) {
                const auto& SL = std::get<S::Sleep>(st.node);
                Value* amt = emit_expr(B, SL.amount, locals, qflags, enums, localsTy); if (!amt) return false;
                auto i64 = Type::getInt64Ty(B.getContext());
                auto FT = FunctionType::get(Type::getVoidTy(B.getContext()), { i64 }, false);
                const char* fname = SL.is_ms ? "inn_sleep_ms" : "inn_sleep_sec";
                Function* fs = cast<Function>(B.GetInsertBlock()->getModule()->getOrInsertFunction(fname, FT).getCallee());
                Value* amt64 = B.CreateSExt(amt, i64);
                B.CreateCall(fs, { amt64 });
            }
            else if (std::holds_alternative<S::Fail>(st.node)) {
                if (qflags.empty()) return false;
                auto it = qflags.end(); --it;
                B.CreateStore(B.getInt1(true), it->second);
            }
            else if (std::holds_alternative<S::Quarantine>(st.node)) {
                const auto& Q = std::get<S::Quarantine>(st.node);
                AllocaInst* flag = B.CreateAlloca(B.getInt1Ty(), nullptr, (Q.name + std::string("_failed")).c_str());
                B.CreateStore(B.getInt1(false), flag);
                qflags[Q.name] = flag;
                emit_block(B, F, Q.body, locals, qflags, enums, localsTy);
            }
            else if (std::holds_alternative<S::Asm>(st.node)) {
                const auto& A = std::get<S::Asm>(st.node);
                std::vector<Type*> outtys;
                std::vector<Type*> argtys;
                std::vector<Value*> args;
                for (auto& op : A.ins) {
                    Value* v = nullptr;
                    if (op.is_immediate) v = B.getInt32(op.imm_val);
                    else {
                        auto it = locals.find(op.name);
                        if (it == locals.end()) return false;
                        v = B.CreateLoad(B.getInt32Ty(), it->second);
                    }
                    if (v->getType() != B.getInt32Ty()) v = B.CreateTruncOrBitCast(v, B.getInt32Ty());
                    argtys.push_back(B.getInt32Ty()); args.push_back(v);
                }
                for (auto& op : A.outs) outtys.push_back(B.getInt32Ty());
                Type* retTy = Type::getVoidTy(B.getContext());
                if (outtys.size() == 1) retTy = outtys[0];
                else if (!outtys.empty()) retTy = StructType::get(B.getContext(), outtys);
                std::string cons;
                for (size_t i = 0; i < A.outs.size(); ++i) { cons += "=" + A.outs[i].constraint; if (i + 1 < A.outs.size()) cons += ","; }
                if (!A.outs.empty() && !A.ins.empty()) cons += ",";
                for (size_t i = 0; i < A.ins.size(); ++i) { cons += A.ins[i].constraint; if (i + 1 < A.ins.size()) cons += ","; }
                for (auto& c : A.clobbers) { cons += ",~{" + c + "}"; }
                auto IA = InlineAsm::get(FunctionType::get(retTy, argtys, false), A.body, cons, true, false, A.intel ? InlineAsm::AD_Intel : InlineAsm::AD_ATT);
                CallInst* call = B.CreateCall(IA, args);
                if (outtys.size() == 1) {
                    auto& op = A.outs[0];
                    auto it = locals.find(op.name); if (it == locals.end()) return false;
                    B.CreateStore(call, it->second);
                }
                else if (outtys.size() > 1) {
                    for (size_t i = 0; i < outtys.size(); ++i) {
                        auto& op = A.outs[i];
                        auto it = locals.find(op.name); if (it == locals.end()) return false;
                        Value* v = B.CreateExtractValue(call, { unsigned(i) });
                        B.CreateStore(v, it->second);
                    }
                }
            }
        }
        return true;
    }

    bool compile_to_object(const innesce::ast::Unit& unit,
        const innesce::front::Sema& sema,
        const std::string& out_obj_path,
        std::string& err) {
        LLVMInitializeNativeTarget();
        LLVMInitializeNativeAsmPrinter();
        LLVMInitializeNativeAsmParser();

        LLVMContext Ctx;
        auto mod = std::make_unique<Module>("inn", Ctx);
        IRBuilder<> B(Ctx);

        const auto& enums = sema.enums();

        for (auto& f : unit.functions) {
            FunctionType* fty = FunctionType::get(Type::getInt32Ty(Ctx), {}, false);
            Function* F = Function::Create(fty, Function::ExternalLinkage, f.name, mod.get());
            if (f.hot) F->addFnAttr(Attribute::Hot);
            BasicBlock* entry = BasicBlock::Create(Ctx, "entry", F);
            B.SetInsertPoint(entry);
            std::unordered_map<std::string, Value*> locals;
            std::unordered_map<std::string, Value*> qflags;
            std::unordered_map<std::string, innesce::ast::Type> localsTy;
            emit_block(B, F, f.body, locals, qflags, enums, localsTy);
            if (!entry->getTerminator()) {
                B.CreateRet(B.getInt32(0));
            }
        }

        std::string verr;
        raw_string_ostream os(verr);
        if (verifyModule(*mod, &os)) { err = os.str(); return false; }

        auto targetTriple = sys::getDefaultTargetTriple();
        std::string errStr;
        auto target = TargetRegistry::lookupTarget(targetTriple, errStr);
        if (!target) { err = errStr; return false; }

        TargetOptions opt;
        auto RM = std::optional<Reloc::Model>();
        auto tm = std::unique_ptr<TargetMachine>(target->createTargetMachine(targetTriple, "generic", "", opt, RM));

        mod->setDataLayout(tm->createDataLayout());
        mod->setTargetTriple(targetTriple);

        std::error_code EC;
        raw_fd_ostream dest(out_obj_path, EC, sys::fs::OF_None);
        if (EC) { err = "Could not open output file: " + EC.message(); return false; }

        legacy::PassManager pass;
        if (tm->addPassesToEmitFile(pass, dest, nullptr, CGFT_ObjectFile)) { err = "TargetMachine can't emit an object file"; return false; }
        pass.run(*mod);
        dest.flush();
        return true;
    }

} // namespace innesce::backend

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
    if (argc < 2) {
        std::cerr << "usage: innescec <file.inn> [-o out.o]\n";
        return 2;
    }
    std::string srcpath = argv[1];
    std::string outobj;
    for (int i = 2; i < argc; i++) {
        std::string a = argv[i];
        if (a == "-o" && i + 1 < argc) { outobj = argv[++i]; }
    }
    std::ifstream in(srcpath);
    if (!in) { std::cerr << "cannot open " << srcpath << "\n"; return 2; }
    std::ostringstream ss; ss << in.rdbuf();
    innesce::front::Parser p(ss.str());
    auto unit = p.parse_unit();
    if (!unit) { std::cerr << "Parse error.\n"; return 3; }
    innesce::front::Sema s;
    auto res = s.check(*unit);
    if (!res.ok()) { std::cerr << "Sema error: " << res.error << "\n"; return 4; }

#ifdef INNSCE_ENABLE_LLVM
    if (!outobj.empty()) {
        std::string err;
        if (!innesce::backend::compile_to_object(*unit, s, outobj, err)) {
            std::cerr << "Codegen error: " << err << "\n"; return 5;
        }
        std::cout << "Wrote " << outobj << "\n";
        return 0;
    }
#endif

    std::cout << "OK\n";
    return 0;
}

#include <cstdint>
#include <cstdio>
extern "C" int fs_read_i32() { return 42; }
extern "C" int fs_write_i32() { return 1; }
extern "C" int fs_open_i32() { return 3; }

#include <cstdint>
extern "C" int fs_open_str(const char* path) {
    int n = 0; if (path) while (path[n]) ++n;
    return n > 0 ? n : -1;
}

#include <cstdint>
extern "C" int net_ping_i32() { return 7; }
extern "C" int net_tcp_i32() { return 200; }

#include <cstdint>
extern "C" int net_tcp_str_i32(const char* host, int port) {
    int n = 0; if (host) while (host[n]) ++n;
    return port + n;
}

#include <random>
extern "C" int rand_i32() {
    static thread_local std::mt19937 rng{ 12345u };
    return static_cast<int>(rng());
}

#include <random>
extern "C" int rand_range_i32(int lo, int hi) {
    if (hi < lo) { int t = lo; lo = hi; hi = t; }
    static thread_local std::mt19937 rng{ 67890u };
    std::uniform_int_distribution<int> dist(lo, hi);
    return dist(rng);
}

#include <thread>
#include <chrono>
extern "C" void inn_sleep_ms(long long n) { std::this_thread::sleep_for(std::chrono::milliseconds(n)); }
extern "C" void inn_sleep_sec(long long n) { std::this_thread::sleep_for(std::chrono::seconds(n)); }

#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <variant>

namespace innesce::ast {

    enum class DurUnit { MS, SEC };

    struct Type {
        enum Kind { I32, ENUM, DUR, STR, TUPLE } kind{ I32 };
        std::string enum_name; // ENUM
        DurUnit dur{};         // DUR
        std::vector<Type> tuple_elems; // TUPLE
    };

    struct Expr {
        struct IntLit { int value; };
        struct StringLit { std::string value; };
        struct Ident { std::string name; };
        struct Unary { char op; std::unique_ptr<Expr> rhs; };
        struct Binary { char op; std::unique_ptr<Expr> lhs, rhs; };
        struct IsFailed { std::string name; };
        struct DurLit { int value; DurUnit unit; };
        struct Cast { std::unique_ptr<Expr> inner; Type target; };
        struct Call { std::string name; std::vector<Expr> args; };

        struct AsmExpr {
            std::string body;
            bool intel{ true };
            struct Operand { bool is_output{ false }; std::string constraint; bool is_immediate{ false }; int imm_val{ 0 }; std::string name; };
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
            struct Case { bool is_default{ false }; bool is_tuple{ false }; std::string label; std::vector<Pattern> tpat; std::vector<Stmt> body; };
            std::vector<Case> cases;
        };
        struct Sleep { Expr amount; bool is_ms{ true }; };
        struct Fail {};
        struct Quarantine { std::string name; std::vector<Stmt> body; };

        struct Asm {
            std::string body;
            bool intel{ true };
            struct Operand { bool is_output{ false }; std::string constraint; std::string name; bool is_immediate{ false }; int imm_val{ 0 }; };
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
        bool hot{ false };
        std::vector<Stmt> body;
    };

    struct Unit { std::vector<EnumDecl> enums; std::vector<Function> functions; };

} // namespace innesce::ast

#include "frontend/lexer.hpp"
#include <cctype>

namespace innesce::front {

    Lexer::Lexer(std::string_view src) : s_(src) {}

    char Lexer::peek() const { return i_ < s_.size() ? s_[i_] : '\0'; }
    char Lexer::get() {
        if (i_ >= s_.size()) return '\0';
        char c = s_[i_++];
        if (c == '\n') { line_++; col_ = 1; }
        else { col_++; }
        return c;
    }

    void Lexer::skip_ws_and_comments() {
        while (true) {
            char c = peek();
            if (c == '\0') break;
            if (std::isspace(static_cast<unsigned char>(c))) { get(); continue; }
            if (c == '-' && i_ + 1 < s_.size() && s_[i_ + 1] == '-') {
                while (c != '\n' && c != '\0') c = get();
                continue;
            }
            break;
        }
    }

    Token Lexer::make(TokKind k, std::string t) {
        Token x{ k, std::move(t), 0, line_, col_ };
        return x;
    }

    Token Lexer::next() {
        skip_ws_and_comments();
        char c = peek();
        if (c == '\0') return make(TokKind::End, "");

        // punctuation/operators
        if (c == '(') { get(); return make(TokKind::LParen, "("); }
        if (c == ')') { get(); return make(TokKind::RParen, ")"); }
        if (c == '{') { get(); return make(TokKind::LBrace, "{"); }
        if (c == '}') { get(); return make(TokKind::RBrace, "}"); }
        if (c == '[') { get(); return make(TokKind::LBracket, "["); }
        if (c == ']') { get(); return make(TokKind::RBracket, "]"); }
        if (c == '+') { get(); return make(TokKind::Plus, "+"); }
        if (c == '-') {
            if (i_ + 1 < s_.size() && s_[i_ + 1] == '>') { get(); get(); return make(TokKind::Arrow, "->"); }
            get(); return make(TokKind::Minus, "-");
        }
        if (c == '*') { get(); return make(TokKind::Star, "*"); }
        if (c == '/') { get(); return make(TokKind::Slash, "/"); }
        if (c == ':') {
            get();
            if (peek() == '=') { get(); return make(TokKind::Assign, ":="); }
            return make(TokKind::Colon, ":");
        }
        if (c == ';') { get(); return make(TokKind::Semicolon, ";"); }
        if (c == ',') { get(); return make(TokKind::Comma, ","); }
        if (c == '=') {
            if (i_ + 1 < s_.size() && s_[i_ + 1] == '>') { get(); get(); return make(TokKind::FatArrow, "=>"); }
        }

        // number
        if (std::isdigit(static_cast<unsigned char>(c))) {
            int v = 0;
            while (std::isdigit(static_cast<unsigned char>(peek()))) {
                v = v * 10 + (get() - '0');
            }
            Token t = make(TokKind::Int, "");
            t.int_val = v;
            return t;
        }

        // string literal
        if (c == '\"') {
            get();
            std::string val;
            while (true) {
                char ch = get();
                if (ch == '\0') break;
                if (ch == '\"') break;
                if (ch == '\\') {
                    char esc = get();
                    switch (esc) {
                    case 'n': val.push_back('\n'); break;
                    case 't': val.push_back('\t'); break;
                    case '\\': val.push_back('\\'); break;
                    case '"': val.push_back('"'); break;
                    default: val.push_back(esc); break;
                    }
                }
                else {
                    val.push_back(ch);
                }
            }
            Token t = make(TokKind::String, val);
            t.text = val;
            return t;
        }

        // identifier/keyword
        if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
            std::string id;
            while (std::isalnum(static_cast<unsigned char>(peek())) || peek() == '_') {
                id.push_back(get());
            }
            if (id == "fn") return make(TokKind::KwFn, id);
            if (id == "is") return make(TokKind::KwIs, id);
            if (id == "end") return make(TokKind::KwEnd, id);
            if (id == "return") return make(TokKind::KwReturn, id);
            if (id == "let") return make(TokKind::KwLet, id);
            if (id == "i32") return make(TokKind::KwI32, id);
            if (id == "str") return make(TokKind::KwStr, id);
            if (id == "if") return make(TokKind::KwIf, id);
            if (id == "then") return make(TokKind::KwThen, id);
            if (id == "else") return make(TokKind::KwElse, id);
            if (id == "type") return make(TokKind::KwType, id);
            if (id == "enum") return make(TokKind::KwEnum, id);
            if (id == "match") return make(TokKind::KwMatch, id);
            if (id == "case") return make(TokKind::KwCase, id);
            if (id == "default") return make(TokKind::KwDefault, id);
            if (id == "sleep") return make(TokKind::KwSleep, id);
            if (id == "quarantine") return make(TokKind::KwQuarantine, id);
            if (id == "fail") return make(TokKind::KwFail, id);
            if (id == "isfailed") return make(TokKind::KwIsFailed, id);
            if (id == "ms") return make(TokKind::KwMS, id);
            if (id == "sec") return make(TokKind::KwSEC, id);
            if (id == "asm") return make(TokKind::KwASM, id);
            if (id == "with") return make(TokKind::KwWith, id);
            if (id == "Hot") return make(TokKind::KwHot, id);
            if (id == "as") return make(TokKind::KwAs, id);
            Token t = make(TokKind::Ident, id);
            t.text = id;
            return t;
        }

        get();
        return next();
    }

} // namespace innesce::front

#pragma once
#include <string>
#include <string_view>

namespace innesce::front {

    enum class TokKind {
        End,
        Ident,
        Int,
        String,
        // keywords
        KwFn, KwIs, KwEnd, KwReturn, KwLet,
        KwI32, KwStr, KwIf, KwThen, KwElse,
        KwType, KwEnum, KwMatch, KwCase, KwDefault,
        KwSleep, KwQuarantine, KwFail, KwIsFailed,
        KwMS, KwSEC, KwASM, KwWith, KwHot, KwAs,
        // punctuation
        LParen, RParen, LBrace, RBrace, LBracket, RBracket,
        Colon, Semicolon, Comma,
        Arrow, Assign, FatArrow,
        // ops
        Plus, Minus, Star, Slash,
    };

    struct Token {
        TokKind kind;
        std::string text;
        int int_val{ 0 };
        int line{ 1 }, col{ 1 };
    };

    struct Lexer {
        explicit Lexer(std::string_view src);
        Token next();
    private:
        std::string_view s_;
        size_t i_{ 0 };
        int line_{ 1 }, col_{ 1 };
        char peek() const;
        char get();
        void skip_ws_and_comments();
        Token make(TokKind k, std::string t);
    };

} // namespace innesce::front

#include "frontend/parser.hpp"
#include <iostream>
#include <memory>

namespace innesce::front {

    Parser::Parser(std::string_view src) : lex_(src) { bump(); }

    void Parser::bump() { cur_ = lex_.next(); }
    bool Parser::accept(TokKind k) { if (cur_.kind == k) { bump(); return true; } return false; }
    bool Parser::expect(TokKind k, const char* what) {
        if (!accept(k)) { std::cerr << "Expected " << what << "\n"; return false; }
        return true;
    }

    std::optional<ast::Type> Parser::parse_type_name() {
        ast::Type t;
        if (accept(TokKind::KwI32)) { t.kind = ast::Type::I32; return t; }
        if (accept(TokKind::KwStr)) { t.kind = ast::Type::STR; return t; }
        if (accept(TokKind::KwMS)) { t.kind = ast::Type::DUR; t.dur = ast::DurUnit::MS; return t; }
        if (accept(TokKind::KwSEC)) { t.kind = ast::Type::DUR; t.dur = ast::DurUnit::SEC; return t; }
        if (accept(TokKind::LParen)) {
            std::vector<ast::Type> elems;
            if (!accept(TokKind::RParen)) {
                while (true) {
                    auto et = parse_type_name(); if (!et) return std::nullopt;
                    elems.push_back(*et);
                    if (accept(TokKind::RParen)) break;
                    if (!expect(TokKind::Comma, "','")) return std::nullopt;
                }
            }
            t.kind = ast::Type::TUPLE;
            t.tuple_elems = std::move(elems);
            return t;
        }
        if (cur_.kind == TokKind::Ident) { t.kind = ast::Type::ENUM; t.enum_name = cur_.text; bump(); return t; }
        return std::nullopt;
    }

    std::optional<ast::Unit> Parser::parse_unit() {
        ast::Unit u;
        while (cur_.kind != TokKind::End) {
            if (cur_.kind == TokKind::KwType) {
                auto e = parse_enum_decl();
                if (!e) return std::nullopt;
                u.enums.push_back(std::move(*e));
                continue;
            }
            auto fn = parse_function();
            if (!fn) return std::nullopt;
            u.functions.push_back(std::move(*fn));
        }
        return u;
    }

    std::optional<ast::EnumDecl> Parser::parse_enum_decl() {
        if (!expect(TokKind::KwType, "'type'")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected enum name\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        if (!expect(TokKind::KwEnum, "'enum'")) return std::nullopt;
        if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
        ast::EnumDecl d; d.name = std::move(name);
        bool first = true;
        while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
            if (!first) expect(TokKind::Comma, "','");
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected variant name\n"; return std::nullopt; }
            d.variants.push_back(cur_.text);
            bump();
            first = false;
        }
        if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
        accept(TokKind::Semicolon);
        return d;
    }

    std::optional<ast::Function> Parser::parse_function() {
        if (!expect(TokKind::KwFn, "'fn'")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected function name\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::LParen, "'('")) return std::nullopt;
        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
        if (!expect(TokKind::Arrow, "'->'")) return std::nullopt;
        auto rty = parse_type_name(); if (!rty) { std::cerr << "Expected return type\n"; return std::nullopt; }

        std::vector<std::string> gates; bool hot = false;
        if (accept(TokKind::KwWith)) {
            if (!expect(TokKind::LBracket, "'['")) return std::nullopt;
            bool first = true;
            while (cur_.kind != TokKind::RBracket && cur_.kind != TokKind::End) {
                if (!first) expect(TokKind::Comma, "','");
                if (accept(TokKind::KwHot)) { hot = true; }
                else if (cur_.kind == TokKind::Ident) { gates.push_back(cur_.text); bump(); }
                else { std::cerr << "Expected gate name or Hot\n"; return std::nullopt; }
                first = false;
            }
            if (!expect(TokKind::RBracket, "']'")) return std::nullopt;
        }

        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;

        ast::Function f; f.name = std::move(name); f.ret = *rty; f.gates = std::move(gates); f.hot = hot;
        while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt();
            if (!st) return std::nullopt;
            f.body.push_back(std::move(*st));
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        return f;
    }

    std::optional<ast::Stmt> Parser::parse_asm_block() {
        if (!expect(TokKind::KwASM, "'asm'")) return std::nullopt;
        if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
        ast::Stmt::Asm A;
        while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
            if (cur_.kind == TokKind::Ident && cur_.text == "intel") { bump(); A.intel = true; continue; }
            if (cur_.kind == TokKind::Ident && cur_.text == "att") { bump(); A.intel = false; continue; }
            if (cur_.kind == TokKind::Ident && cur_.text == "outs") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected constraint ident after outs\n"; return std::nullopt; }
                    std::string cons = cur_.text; bump();
                    if (!expect(TokKind::LParen, "'('")) return std::nullopt;
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected variable name in outs()\n"; return std::nullopt; }
                    std::string name = cur_.text; bump();
                    if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                    A.outs.push_back({ true, cons, name, false, 0 });
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after outs entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "ins") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected constraint ident after ins\n"; return std::nullopt; }
                    std::string cons = cur_.text; bump();
                    if (!expect(TokKind::LParen, "'('")) return std::nullopt;
                    if (cur_.kind == TokKind::Ident) {
                        std::string name = cur_.text; bump();
                        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                        A.ins.push_back({ false, cons, name, false, 0 });
                    }
                    else if (cur_.kind == TokKind::Int) {
                        int v = cur_.int_val; bump();
                        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                        A.ins.push_back({ false, cons, "", true, v });
                    }
                    else {
                        std::cerr << "Expected ident or int in ins()\n"; return std::nullopt;
                    }
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after ins entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "clobbers") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected clobber ident\n"; return std::nullopt; }
                    A.clobbers.push_back(cur_.text); bump();
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after clobbers entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "body") {
                bump();
                if (!expect(TokKind::Colon, "':'")) return std::nullopt;
                while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
                    if (cur_.kind == TokKind::Semicolon) { A.body.push_back('\n'); bump(); continue; }
                    if (cur_.kind == TokKind::Ident && (cur_.text == "outs" || cur_.text == "ins" || cur_.text == "clobbers" || cur_.text == "intel" || cur_.text == "att"))
                        break;
                    if (!cur_.text.empty()) { A.body += cur_.text; A.body.push_back(' '); }
                    bump();
                }
                continue;
            }
            if (!cur_.text.empty()) { A.body += cur_.text; A.body.push_back(' '); }
            bump();
        }
        if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
        if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
        ast::Stmt st; st.node = std::move(A);
        return st;
    }

    std::optional<ast::Expr> Parser::parse_asm_expr() {
        if (!expect(TokKind::KwASM, "'asm'")) return std::nullopt;
        if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
        ast::Expr A; ast::Expr::AsmExpr AX;
        while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
            if (cur_.kind == TokKind::Ident && cur_.text == "intel") { bump(); AX.intel = true; continue; }
            if (cur_.kind == TokKind::Ident && cur_.text == "att") { bump(); AX.intel = false; continue; }
            if (cur_.kind == TokKind::Ident && cur_.text == "outs") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected constraint ident after outs\n"; return std::nullopt; }
                    std::string cons = cur_.text; bump();
                    if (!expect(TokKind::LParen, "'('")) return std::nullopt;
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected name in outs() (use '_' for placeholder)\n"; return std::nullopt; }
                    std::string name = cur_.text; bump();
                    if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                    AX.outs.push_back({ true, cons, false, 0, name });
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after outs entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "ins") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected constraint ident after ins\n"; return std::nullopt; }
                    std::string cons = cur_.text; bump();
                    if (!expect(TokKind::LParen, "'('")) return std::nullopt;
                    if (cur_.kind == TokKind::Ident) {
                        std::string name = cur_.text; bump();
                        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                        AX.ins.push_back({ false, cons, false, 0, name });
                    }
                    else if (cur_.kind == TokKind::Int) {
                        int v = cur_.int_val; bump();
                        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                        AX.ins.push_back({ false, cons, true, v, "" });
                    }
                    else {
                        std::cerr << "Expected ident or int in ins()\n"; return std::nullopt;
                    }
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after ins entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "clobbers") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected clobber ident\n"; return std::nullopt; }
                    AX.clobbers.push_back(cur_.text); bump();
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after clobbers entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "body") {
                bump();
                if (!expect(TokKind::Colon, "':'")) return std::nullopt;
                while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
                    if (cur_.kind == TokKind::Semicolon) { AX.body.push_back('\n'); bump(); continue; }
                    if (cur_.kind == TokKind::Ident && (cur_.text == "outs" || cur_.text == "ins" || cur_.text == "clobbers" || cur_.text == "intel" || cur_.text == "att"))
                        break;
                    if (!cur_.text.empty()) { AX.body += cur_.text; AX.body.push_back(' '); }
                    bump();
                }
                continue;
            }
            if (!cur_.text.empty()) { AX.body += cur_.text; AX.body.push_back(' '); }
            bump();
        }
        if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
        A.node = std::move(AX);
        return A;
    }

    std::optional<ast::Stmt> Parser::parse_match() {
        if (!expect(TokKind::KwMatch, "'match'")) return std::nullopt;
        auto scrut = parse_expr(); if (!scrut) return std::nullopt;
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        ast::Stmt::Match m; m.scrutinee = std::move(*scrut);
        while (cur_.kind == TokKind::KwCase) {
            bump();
            ast::Stmt::Match::Case c;
            if (accept(TokKind::KwDefault)) {
                c.is_default = true;
            }
            else if (accept(TokKind::LParen)) {
                c.is_tuple = true;
                if (!accept(TokKind::RParen)) {
                    while (true) {
                        if (accept(TokKind::Comma)) { std::cerr << "missing element in tuple pattern\n"; return std::nullopt; }
                        ast::Pattern p;
                        if (cur_.kind == TokKind::Ident && cur_.text == "_") {
                            p.node = ast::Pattern::Wild{}; bump();
                        }
                        else if (cur_.kind == TokKind::Int) {
                            int v = cur_.int_val; bump();
                            if (accept(TokKind::KwMS)) { p.node = ast::Pattern::Dur{ v, ast::DurUnit::MS }; }
                            else if (accept(TokKind::KwSEC)) { p.node = ast::Pattern::Dur{ v, ast::DurUnit::SEC }; }
                            else { p.node = ast::Pattern::Int{ v }; }
                        }
                        else {
                            std::cerr << "unsupported tuple pattern element\n"; return std::nullopt;
                        }
                        c.tpat.push_back(std::move(p));
                        if (accept(TokKind::RParen)) break;
                        if (!expect(TokKind::Comma, "','")) return std::nullopt;
                    }
                }
            }
            else {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected case label or tuple pattern\n"; return std::nullopt; }
                c.label = cur_.text; bump();
            }
            if (!expect(TokKind::FatArrow, "'=>'")) return std::nullopt;
            c.body = parse_case_body();
            m.cases.push_back(std::move(c));
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        ast::Stmt st; st.node = std::move(m);
        return st;
    }

    std::vector<ast::Stmt> Parser::parse_case_body() {
        std::vector<ast::Stmt> body;
        while (cur_.kind != TokKind::KwCase && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt();
            if (!st) break;
            body.push_back(std::move(*st));
        }
        return body;
    }

    std::optional<ast::Stmt> Parser::parse_stmt() {
        if (cur_.kind == TokKind::KwLet) {
            bump();
            if (accept(TokKind::LParen)) {
                // tuple destructuring
                std::vector<std::string> names;
                if (!accept(TokKind::RParen)) {
                    while (true) {
                        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected name in tuple pattern\n"; return std::nullopt; }
                        names.push_back(cur_.text); bump();
                        if (accept(TokKind::RParen)) break;
                        if (!expect(TokKind::Comma, "','")) return std::nullopt;
                    }
                }
                if (!expect(TokKind::Colon, "':'")) return std::nullopt;
                if (!accept(TokKind::LParen)) { std::cerr << "Expected '(' for tuple types\n"; return std::nullopt; }
                std::vector<ast::Type> tys;
                if (!accept(TokKind::RParen)) {
                    while (true) {
                        auto t = parse_type_name(); if (!t) return std::nullopt;
                        tys.push_back(*t);
                        if (accept(TokKind::RParen)) break;
                        if (!expect(TokKind::Comma, "','")) return std::nullopt;
                    }
                }
                if (!expect(TokKind::Assign, "':='")) return std::nullopt;
                auto e = parse_expr(); if (!e) return std::nullopt;
                if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
                ast::Stmt st; st.node = ast::Stmt::LetTuple{ std::move(names), std::move(tys), std::move(*e) };
                return st;
            }
            else {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected identifier after let\n"; return std::nullopt; }
                std::string name = cur_.text; bump();
                if (!expect(TokKind::Colon, "':'")) return std::nullopt;
                auto ty = parse_type_name(); if (!ty) { std::cerr << "Expected type name\n"; return std::nullopt; }
                if (!expect(TokKind::Assign, "':='")) return std::nullopt;
                auto e = parse_expr(); if (!e) return std::nullopt;
                if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
                ast::Stmt st; st.node = ast::Stmt::Let{ std::move(name), *ty, std::move(*e) };
                return st;
            }
        }
        if (cur_.kind == TokKind::KwReturn) {
            bump();
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Return{ std::move(*e) };
            return st;
        }
        if (cur_.kind == TokKind::KwIf) {
            bump();
            auto cond = parse_expr(); if (!cond) return std::nullopt;
            if (!expect(TokKind::KwThen, "'then'")) return std::nullopt;
            std::vector<ast::Stmt> then_body;
            while (cur_.kind != TokKind::KwElse && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                auto st = parse_stmt(); if (!st) return std::nullopt; then_body.push_back(std::move(*st));
            }
            std::vector<ast::Stmt> else_body;
            if (accept(TokKind::KwElse)) {
                while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                    auto st = parse_stmt(); if (!st) return std::nullopt; else_body.push_back(std::move(*st));
                }
            }
            if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::If{ std::move(*cond), std::move(then_body), std::move(else_body) };
            return st;
        }
        if (cur_.kind == TokKind::KwMatch) return parse_match();
        if (cur_.kind == TokKind::KwSleep) {
            bump();
            if (!expect(TokKind::LParen, "'('")) return std::nullopt;
            auto amt = parse_expr(); if (!amt) return std::nullopt;
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Sleep{ std::move(*amt), true };
            return st;
        }
        if (cur_.kind == TokKind::KwFail) {
            bump();
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Fail{};
            return st;
        }
        if (cur_.kind == TokKind::KwQuarantine) {
            bump();
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected quarantine name"; return std::nullopt; }
            std::string name = cur_.text; bump();
            if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
            std::vector<ast::Stmt> body;
            while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
                auto st = parse_stmt(); if (!st) return std::nullopt; body.push_back(std::move(*st));
            }
            if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Quarantine{ std::move(name), std::move(body) };
            return st;
        }
        if (cur_.kind == TokKind::KwASM) {
            return parse_asm_block();
        }
        std::cerr << "Unknown statement\n";
        return std::nullopt;
    }

    std::optional<ast::Expr> Parser::parse_expr() {
        auto e = parse_add(); if (!e) return std::nullopt;
        while (accept(TokKind::KwAs)) {
            auto ty = parse_type_name(); if (!ty) { std::cerr << "Expected type after 'as'\n"; return std::nullopt; }
            ast::Expr ce; ce.node = ast::Expr::Cast{ std::make_unique<ast::Expr>(std::move(*e)), *ty };
            e = std::move(ce);
        }
        return e;
    }

    std::optional<ast::Expr> Parser::parse_add() {
        auto lhs = parse_mul(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Plus || cur_.kind == TokKind::Minus) {
            char op = (cur_.kind == TokKind::Plus) ? '+' : '-';
            bump();
            auto rhs = parse_mul(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Binary{ op, std::make_unique<innesce::ast::Expr>(std::move(*lhs)), std::make_unique<innesce::ast::Expr>(std::move(*rhs)) };
            lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_mul() {
        auto lhs = parse_unary(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Star || cur_.kind == TokKind::Slash) {
            char op = (cur_.kind == TokKind::Star) ? '*' : '/';
            bump();
            auto rhs = parse_unary(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Binary{ op, std::make_unique<innesce::ast::Expr>(std::move(*lhs)), std::make_unique<innesce::ast::Expr>(std::move(*rhs)) };
            lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_unary() {
        if (cur_.kind == TokKind::Minus) {
            bump();
            auto rhs = parse_unary(); if (!rhs) return std::nullopt;
            ast::Expr e;
            e.node = ast::Expr::Unary{ '-', std::make_unique<innesce::ast::Expr>(std::move(*rhs)) };
            return e;
        }
        return parse_postfix();
    }

    std::optional<ast::Expr> Parser::parse_postfix() {
        auto p = parse_primary(); if (!p) return std::nullopt;
        while (accept(TokKind::LParen)) {
            std::vector<ast::Expr> args;
            if (!accept(TokKind::RParen)) {
                while (true) {
                    auto a = parse_expr(); if (!a) return std::nullopt;
                    args.push_back(std::move(*a));
                    if (accept(TokKind::RParen)) break;
                    if (!expect(TokKind::Comma, "','")) return std::nullopt;
                }
            }
            if (!std::holds_alternative<ast::Expr::Ident>(p->node)) {
                std::cerr << "call target must be identifier\n"; return std::nullopt;
            }
            std::string name = std::get<ast::Expr::Ident>(p->node).name;
            ast::Expr call; call.node = ast::Expr::Call{ std::move(name), std::move(args) };
            p = std::move(call);
        }
        return p;
    }

    std::optional<ast::Expr> Parser::parse_primary() {
        if (cur_.kind == TokKind::Int) {
            int v = cur_.int_val; bump();
            if (accept(TokKind::KwMS)) { ast::Expr e; e.node = ast::Expr::DurLit{ v, ast::DurUnit::MS }; return e; }
            if (accept(TokKind::KwSEC)) { ast::Expr e; e.node = ast::Expr::DurLit{ v, ast::DurUnit::SEC }; return e; }
            ast::Expr e; e.node = ast::Expr::IntLit{ v }; return e;
        }
        if (cur_.kind == TokKind::String) { ast::Expr e; e.node = ast::Expr::StringLit{ cur_.text }; bump(); return e; }
        if (cur_.kind == TokKind::Ident) { ast::Expr e; e.node = ast::Expr::Ident{ cur_.text }; bump(); return e; }
        if (cur_.kind == TokKind::KwASM) { return parse_asm_expr(); }
        if (cur_.kind == TokKind::KwIsFailed) {
            bump();
            if (!expect(TokKind::LParen, "'('")) return std::nullopt;
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected name in isfailed()"; return std::nullopt; }
            std::string q = cur_.text; bump();
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            ast::Expr e; e.node = ast::Expr::IsFailed{ std::move(q) }; return e;
        }
        if (accept(TokKind::LParen)) {
            auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::RParen, "')'")) return std::nullopt;
            return e;
        }
        std::cerr << "Expected expression\n";
        return std::nullopt;
    }

} // namespace innesce::front

#pragma once
#include "frontend/lexer.hpp"
#include "frontend/ast.hpp"
#include <optional>

namespace innesce::front {

    class Parser {
    public:
        explicit Parser(std::string_view src);
        std::optional<ast::Unit> parse_unit();

    private:
        Lexer lex_;
        Token cur_;

        void bump();
        bool accept(TokKind k);
        bool expect(TokKind k, const char* what);

        std::optional<ast::EnumDecl> parse_enum_decl();
        std::optional<ast::Function> parse_function();
        std::optional<ast::Stmt> parse_stmt();
        std::optional<ast::Stmt> parse_match();
        std::optional<ast::Stmt> parse_asm_block();
        std::optional<ast::Expr> parse_asm_expr();
        std::optional<ast::Expr> parse_expr();
        std::optional<ast::Expr> parse_add();
        std::optional<ast::Expr> parse_mul();
        std::optional<ast::Expr> parse_unary();
        std::optional<ast::Expr> parse_postfix();
        std::optional<ast::Expr> parse_primary();

        std::vector<ast::Stmt> parse_case_body();
        std::optional<ast::Type> parse_type_name();
    };

} // namespace innesce::front

#include "frontend/sema.hpp"
#include <unordered_map>
#include <variant>
#include <set>
#include <algorithm>
#include <iostream>

namespace innesce::front {

    using Type = innesce::ast::Type;
    using DurUnit = innesce::ast::DurUnit;

    static bool is_i32(const Type& t) { return t.kind == Type::I32; }
    static bool is_str(const Type& t) { return t.kind == Type::STR; }
    static bool is_tuple(const Type& t) { return t.kind == Type::TUPLE; }
    static bool is_dur(const Type& t, DurUnit* u = nullptr) { if (t.kind == Type::DUR) { if (u)*u = t.dur; return true; } return false; }
    static bool is_enum(const Type& t) { return t.kind == Type::ENUM; }

    static bool same_type(const Type& a, const Type& b) {
        if (a.kind != b.kind) return false;
        if (a.kind == Type::ENUM) return a.enum_name == b.enum_name;
        if (a.kind == Type::DUR) return a.dur == b.dur;
        if (a.kind == Type::TUPLE) {
            if (a.tuple_elems.size() != b.tuple_elems.size()) return false;
            for (size_t i = 0; i < a.tuple_elems.size(); ++i) if (!same_type(a.tuple_elems[i], b.tuple_elems[i])) return false;
        }
        return true;
    }

    static bool has_gate(const std::vector<std::string>& gates, const std::string& g) {
        return std::find(gates.begin(), gates.end(), g) != gates.end();
    }

    static bool check_expr(const innesce::ast::Expr& e,
        const std::unordered_map<std::string, Type>& locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        const std::vector<std::string>& fn_gates,
        std::string& err,
        Type* outTy = nullptr)
    {
        using E = innesce::ast::Expr;
        if (std::holds_alternative<E::IntLit>(e.node)) {
            if (outTy) outTy->kind = Type::I32; return true;
        }
        if (std::holds_alternative<E::StringLit>(e.node)) {
            if (outTy) outTy->kind = Type::STR; return true;
        }
        if (std::holds_alternative<E::DurLit>(e.node)) {
            if (outTy) { outTy->kind = Type::DUR; outTy->dur = std::get<E::DurLit>(e.node).unit; } return true;
        }
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& I = std::get<E::Ident>(e.node);
            auto it = locals.find(I.name);
            if (it != locals.end()) { if (outTy) *outTy = it->second; return true; }
            for (auto& [ename, vars] : enums) {
                if (vars.count(I.name)) { if (outTy) { outTy->kind = Type::ENUM; outTy->enum_name = ename; } return true; }
            }
            err = "use of undeclared identifier: " + I.name; return false;
        }
        if (std::holds_alternative<E::IsFailed>(e.node)) {
            if (outTy) outTy->kind = Type::I32; return true;
        }
        if (std::holds_alternative<E::Unary>(e.node)) {
            const auto& U = std::get<E::Unary>(e.node);
            Type t; if (!check_expr(*U.rhs, locals, enums, fn_gates, err, &t)) return false;
            if (!is_i32(t)) { err = "unary '-' requires i32"; return false; }
            if (outTy) outTy->kind = Type::I32; return true;
        }
        if (std::holds_alternative<E::Cast>(e.node)) {
            const auto& C = std::get<E::Cast>(e.node);
            Type it; if (!check_expr(*C.inner, locals, enums, fn_gates, err, &it)) return false;
            if (is_dur(it) && C.target.kind == Type::DUR) { if (outTy) { *outTy = C.target; } return true; }
            err = "invalid cast: only duration unit conversions are supported"; return false;
        }
        if (std::holds_alternative<E::AsmExpr>(e.node)) {
            const auto& A = std::get<E::AsmExpr>(e.node);
            if (A.outs.empty()) { if (outTy) outTy->kind = Type::I32; return true; }
            if (A.outs.size() == 1) { if (outTy) outTy->kind = Type::I32; return true; }
            if (outTy) { outTy->kind = Type::TUPLE; outTy->tuple_elems.assign(A.outs.size(), Type{ .kind = Type::I32 }); }
            return true;
        }
        if (std::holds_alternative<E::Call>(e.node)) {
            const auto& C = std::get<E::Call>(e.node);
            auto argc = (int)C.args.size();
            if (C.name == "fs_read_i32") {
                if (!has_gate(fn_gates, "fs.read")) { err = "missing gate 'fs.read' to call fs_read_i32"; return false; }
                if (argc != 0) { err = "fs_read_i32() takes 0 args"; return false; }
                if (outTy) outTy->kind = Type::I32; return true;
            }
            else if (C.name == "fs_open_i32") {
                if (!has_gate(fn_gates, "fs.open")) { err = "missing gate 'fs.open' to call fs_open_i32"; return false; }
                if (argc != 0) { err = "fs_open_i32() takes 0 args"; return false; }
                if (outTy) outTy->kind = Type::I32; return true;
            }
            else if (C.name == "fs_open") {
                if (!has_gate(fn_gates, "fs.open")) { err = "missing gate 'fs.open' to call fs_open"; return false; }
                if (argc != 1) { err = "fs_open(path) takes 1 arg"; return false; }
                Type t0; if (!check_expr(C.args[0], locals, enums, fn_gates, err, &t0)) return false;
                if (!is_str(t0)) { err = "fs_open(path) expects str"; return false; }
                if (outTy) outTy->kind = Type::I32; return true;
            }
            else if (C.name == "fs_write_i32") {
                if (!has_gate(fn_gates, "fs.write")) { err = "missing gate 'fs.write' to call fs_write_i32"; return false; }
                if (argc != 0) { err = "fs_write_i32() takes 0 args"; return false; }
                if (outTy) outTy->kind = Type::I32; return true;
            }
            else if (C.name == "net_ping_i32") {
                if (!has_gate(fn_gates, "net")) { err = "missing gate 'net' to call net_ping_i32"; return false; }
                if (argc != 0) { err = "net_ping_i32() takes 0 args"; return false; }
                if (outTy) outTy->kind = Type::I32; return true;
            }
            else if (C.name == "net_tcp_i32") {
                if (!has_gate(fn_gates, "net.tcp")) { err = "missing gate 'net.tcp' to call net_tcp_i32"; return false; }
                if (argc != 0) { err = "net_tcp_i32() takes 0 args"; return false; }
                if (outTy) outTy->kind = Type::I32; return true;
            }
            else if (C.name == "net_tcp") {
                if (!has_gate(fn_gates, "net.tcp")) { err = "missing gate 'net.tcp' to call net_tcp"; return false; }
                if (argc != 2) { err = "net_tcp(host, port) takes 2 args"; return false; }
                Type t0, t1; if (!check_expr(C.args[0], locals, enums, fn_gates, err, &t0)) return false;
                if (!check_expr(C.args[1], locals, enums, fn_gates, err, &t1)) return false;
                if (!is_str(t0) || !is_i32(t1)) { err = "net_tcp(host, port) expects (str, i32)"; return false; }
                if (outTy) outTy->kind = Type::I32; return true;
            }
            else if (C.name == "rand_i32") {
                if (!has_gate(fn_gates, "rand")) { err = "missing gate 'rand' to call rand_i32"; return false; }
                if (argc != 0) { err = "rand_i32() takes 0 args"; return false; }
                if (outTy) outTy->kind = Type::I32; return true;
            }
            else if (C.name == "rand_range") {
                if (!has_gate(fn_gates, "rand")) { err = "missing gate 'rand' to call rand_range"; return false; }
                if (argc != 2) { err = "rand_range(lo, hi) takes 2 args"; return false; }
                Type t0, t1; if (!check_expr(C.args[0], locals, enums, fn_gates, err, &t0)) return false;
                if (!check_expr(C.args[1], locals, enums, fn_gates, err, &t1)) return false;
                if (!is_i32(t0) || !is_i32(t1)) { err = "rand_range(lo, hi) expects (i32, i32)"; return false; }
                if (outTy) outTy->kind = Type::I32; return true;
            }
            else {
                err = "unknown function: " + C.name; return false;
            }
        }
        const auto& B = std::get<E::Binary>(e.node);
        Type lt, rt;
        if (!check_expr(*B.lhs, locals, enums, fn_gates, err, &lt)) return false;
        if (!check_expr(*B.rhs, locals, enums, fn_gates, err, &rt)) return false;
        if (B.op == '+' || B.op == '-') {
            if (is_i32(lt) && is_i32(rt)) { if (outTy) outTy->kind = Type::I32; return true; }
            DurUnit ul, ur;
            if (is_dur(lt, &ul) && is_dur(rt, &ur) && ul == ur) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = ul; } return true; }
            err = "duration addition/subtraction requires same units"; return false;
        }
        if (B.op == '*') {
            DurUnit u;
            if (is_dur(lt, &u) && is_i32(rt)) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = u; } return true; }
            if (is_i32(lt) && is_dur(rt, &u)) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = u; } return true; }
            if (is_i32(lt) && is_i32(rt)) { if (outTy) outTy->kind = Type::I32; return true; }
            err = "invalid duration multiplication"; return false;
        }
        if (B.op == '/') {
            DurUnit u;
            if (is_dur(lt, &u) && is_i32(rt)) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = u; } return true; }
            if (is_i32(lt) && is_i32(rt)) { if (outTy) outTy->kind = Type::I32; return true; }
            err = "invalid duration division"; return false;
        }
        err = "unknown binary operator"; return false;
    }

    static bool check_block(const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Type> locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::string& err,
        bool in_quarantine,
        std::unordered_map<std::string, bool>& declared_quarantines,
        const std::vector<std::string>& fn_gates) {
        using S = innesce::ast::Stmt;
        for (auto& st : body) {
            if (std::holds_alternative<S::Let>(st.node)) {
                const auto& L = std::get<S::Let>(st.node);
                Type t;
                if (!check_expr(L.init, locals, enums, fn_gates, err, &t)) return false;
                if (!same_type(L.type, t)) { err = "type mismatch in let"; return false; }
                locals[L.name] = L.type;
            }
            else if (std::holds_alternative<S::LetTuple>(st.node)) {
                const auto& LT = std::get<S::LetTuple>(st.node);
                Type t;
                if (!check_expr(LT.init, locals, enums, fn_gates, err, &t)) return false;
                if (!is_tuple(t) || t.tuple_elems.size() != LT.names.size()) { err = "tuple arity mismatch in let"; return false; }
                if (LT.types.size() != LT.names.size()) { err = "tuple type list mismatch"; return false; }
                for (size_t i = 0; i < LT.names.size(); ++i) {
                    if (!same_type(LT.types[i], t.tuple_elems[i])) {
                        bool ok = false;
                        if (t.tuple_elems[i].kind == Type::I32 && LT.types[i].kind == Type::DUR) ok = true;
                        if (!ok) { err = "tuple element type mismatch"; return false; }
                    }
                    locals[LT.names[i]] = LT.types[i];
                }
            }
            else if (std::holds_alternative<S::Return>(st.node)) {
                const auto& R = std::get<S::Return>(st.node);
                Type t; if (!check_expr(R.value, locals, enums, fn_gates, err, &t)) return false;
            }
            else if (std::holds_alternative<S::If>(st.node)) {
                const auto& I = std::get<S::If>(st.node);
                Type t; if (!check_expr(I.cond, locals, enums, fn_gates, err, &t)) return false;
                if (!is_i32(t)) { err = "if condition must be i32"; return false; }
                if (!check_block(I.then_body, locals, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
                if (!check_block(I.else_body, locals, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
            }
            else if (std::holds_alternative<S::Match>(st.node)) {
                const auto& M = std::get<S::Match>(st.node);
                Type scrTy; if (!check_expr(M.scrutinee, locals, enums, fn_gates, err, &scrTy)) return false;
                for (auto& C : M.cases) {
                    if (C.is_default) continue;
                    if (C.is_tuple) {
                        if (scrTy.kind != Type::TUPLE) { err = "match tuple pattern on non-tuple"; return false; }
                        if (C.tpat.size() != scrTy.tuple_elems.size()) { err = "tuple pattern arity mismatch"; return false; }
                        for (size_t i = 0; i < C.tpat.size(); ++i) {
                            const auto& et = scrTy.tuple_elems[i];
                            if (std::holds_alternative<innesce::ast::Pattern::Wild>(C.tpat[i].node)) continue;
                            if (std::holds_alternative<innesce::ast::Pattern::Int>(C.tpat[i].node)) {
                                if (!is_i32(et)) { err = "int pattern requires i32 element"; return false; }
                            }
                            else if (std::holds_alternative<innesce::ast::Pattern::Dur>(C.tpat[i].node)) {
                                if (!is_dur(et)) { err = "duration pattern requires duration element"; return false; }
                            }
                        }
                    }
                    else {
                        if (!is_enum(scrTy)) { err = "match label requires enum scrutinee"; return false; }
                    }
                }
                for (auto& C : M.cases) {
                    if (!check_block(C.body, locals, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
                }
            }
            else if (std::holds_alternative<S::Sleep>(st.node)) {
                const auto& SL = std::get<S::Sleep>(st.node);
                Type t; if (!check_expr(SL.amount, locals, enums, fn_gates, err, &t)) return false;
                if (t.kind != Type::DUR) { err = "sleep expects duration value (ms or sec)"; return false; }
                if (!has_gate(fn_gates, "time")) { err = "missing gate 'time' for sleep"; return false; }
            }
            else if (std::holds_alternative<S::Fail>(st.node)) {
                if (!in_quarantine) { err = "fail; outside of quarantine"; return false; }
            }
            else if (std::holds_alternative<S::Quarantine>(st.node)) {
                const auto& Q = std::get<S::Quarantine>(st.node);
                declared_quarantines[Q.name] = true;
                if (!check_block(Q.body, locals, enums, err, true, declared_quarantines, fn_gates)) return false;
            }
            else if (std::holds_alternative<S::Asm>(st.node)) {
                // ok
            }
        }
        return true;
    }

    SemaResult Sema::check(const ast::Unit& u) {
        enums_.clear();
        for (auto& e : u.enums) {
            std::unordered_map<std::string, int> m;
            for (int i = 0; i < (int)e.variants.size(); ++i) m[e.variants[i]] = i;
            enums_[e.name] = std::move(m);
        }

        bool has_main = false;
        for (auto& f : u.functions) {
            if (f.name == "main") has_main = true;
            std::unordered_map<std::string, Type> locals;
            std::string err;
            std::unordered_map<std::string, bool> declared;
            if (!check_block(f.body, locals, enums_, err, false, declared, f.gates)) return { err };
        }
        if (!has_main) return { "missing 'main' function" };
        return {};
    }

} // namespace innesce::front

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

        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums() const { return enums_; }
    private:
        std::unordered_map<std::string, std::unordered_map<std::string, int>> enums_;
    };

} // namespace innesce::front

#include "backend/llvm/codegen.hpp"
#include "frontend/ast.hpp"
#include "frontend/sema.hpp"
#include <llvm/ADT/Triple.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/raw_ostream.h>
#include <unordered_map>
#include <variant>

using namespace ::llvm;

namespace innesce::backend {

    static Value* make_cstring(IRBuilder<>& B, Module* M, std::string_view s) {
        auto& C = B.getContext();
        auto arrTy = ArrayType::get(Type::getInt8Ty(C), s.size() + 1);
        auto gv = new GlobalVariable(*M, arrTy, true, GlobalValue::PrivateLinkage, nullptr, ".str");
        gv->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
        gv->setAlignment(Align(1));
        std::vector<Constant*> chars;
        for (char ch : s) chars.push_back(ConstantInt::get(Type::getInt8Ty(C), (uint8_t)ch));
        chars.push_back(ConstantInt::get(Type::getInt8Ty(C), 0));
        gv->setInitializer(ConstantArray::get(arrTy, chars));
        Value* zero = ConstantInt::get(Type::getInt32Ty(C), 0);
        Value* gep = ConstantExpr::getInBoundsGetElementPtr(arrTy, gv, { zero, zero });
        return ConstantExpr::getBitCast(gep, Type::getInt8PtrTy(C));
    }

    static innesce::ast::Type get_expr_type_simple(const innesce::ast::Expr& e) {
        using E = innesce::ast::Expr;
        innesce::ast::Type t;
        if (std::holds_alternative<E::DurLit>(e.node)) { t.kind = innesce::ast::Type::DUR; t.dur = std::get<E::DurLit>(e.node).unit; return t; }
        if (std::holds_alternative<E::AsmExpr>(e.node)) {
            const auto& A = std::get<E::AsmExpr>(e.node);
            if (A.outs.size() <= 1) { t.kind = innesce::ast::Type::I32; return t; }
            t.kind = innesce::ast::Type::TUPLE; t.tuple_elems.assign(A.outs.size(), innesce::ast::Type{ .kind = innesce::ast::Type::I32 });
            return t;
        }
        t.kind = innesce::ast::Type::I32; return t;
    }

    static Value* emit_expr(IRBuilder<>& B,
        const innesce::ast::Expr& e,
        std::unordered_map<std::string, Value*>& locals,
        std::unordered_map<std::string, Value*>& qflags,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::unordered_map<std::string, innesce::ast::Type>& localsTy);

    static bool emit_block(IRBuilder<>& B,
        Function* F,
        const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Value*>& locals,
        std::unordered_map<std::string, Value*>& qflags,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::unordered_map<std::string, innesce::ast::Type>& localsTy);

    static Value* emit_match_expr(IRBuilder<>& B,
        Function* F,
        const innesce::ast::Expr::MatchExpr& M,
        std::unordered_map<std::string, Value*>& locals,
        std::unordered_map<std::string, Value*>& qflags,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::unordered_map<std::string, innesce::ast::Type>& localsTy) {
        using P = innesce::ast::Expr::Pattern;
        auto scrT = get_expr_type_simple(M.scrutinee);
        Value* scr = emit_expr(B, M.scrutinee, locals, qflags, enums, localsTy); if (!scr) return nullptr;

        BasicBlock* endBB = BasicBlock::Create(B.getContext(), "match.end", F);
        PHINode* phi = nullptr;

        // Prepare tuple extraction if needed
        AllocaInst* tupAlloca = nullptr;
        auto i32 = B.getInt32Ty();
        if (scrT.kind == innesce::ast::Type::TUPLE) {
            tupAlloca = B.CreateAlloca(scr->getType(), nullptr, "tup.scr");
            B.CreateStore(scr, tupAlloca);
        }

        BasicBlock* curCont = endBB;
        std::vector<std::pair<BasicBlock*, Value*>> incoming;

        for (int i = (int)M.cases.size() - 1; i >= 0; --i) {
            const auto& C = M.cases[(size_t)i];
            // Body block
            BasicBlock* bodyBB = BasicBlock::Create(B.getContext(), "case.body", F);
            B.SetInsertPoint(bodyBB);
            // Bind locals into stack slots just-in-time
            std::unordered_map<std::string, Value*> savedLocals = locals;
            std::unordered_map<std::string, innesce::ast::Type> savedLocalsTy = localsTy;
            auto bind_extract = [&](unsigned idx, std::string name) {
                Value* vload = nullptr;
                if (tupAlloca) {
                    Value* tupv = B.CreateLoad(tupAlloca->getAllocatedType(), tupAlloca);
                    vload = B.CreateExtractValue(tupv, { idx });
                }
                else {
                    vload = B.getInt32(0);
                }
                AllocaInst* a = B.CreateAlloca(i32, nullptr, name);
                B.CreateStore(vload, a);
                locals[name] = a;
                localsTy[name] = (scrT.kind == innesce::ast::Type::TUPLE) ? scrT.tuple_elems[idx] : innesce::ast::Type{ .kind = innesce::ast::Type::I32 };
                };
            if (C.is_tuple) {
                for (unsigned idx = 0; idx < C.tpat.size(); ++idx) {
                    if (std::holds_alternative<P::Bind>(C.tpat[idx].node)) {
                        bind_extract(idx, std::get<P::Bind>(C.tpat[idx].node).name);
                    }
                }
            }
            Value* val = emit_expr(B, *C.value, locals, qflags, enums, localsTy);
            locals = savedLocals; localsTy = savedLocalsTy;
            if (!val) return nullptr;
            if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(endBB);
            BasicBlock* doneBody = B.GetInsertBlock();
            incoming.push_back({ doneBody, val });

            // Test block
            BasicBlock* testBB = BasicBlock::Create(B.getContext(), "case.test", F);
            B.SetInsertPoint(testBB);
            Value* cond = nullptr;
            if (C.is_default) {
                cond = B.getInt1(true);
            }
            else if (C.is_tuple) {
                for (unsigned idx = 0; idx < C.tpat.size(); ++idx) {
                    Value* el = nullptr;
                    if (tupAlloca) {
                        Value* tupv = B.CreateLoad(tupAlloca->getAllocatedType(), tupAlloca);
                        el = B.CreateExtractValue(tupv, { idx });
                    }
                    else {
                        el = scr;
                    }
                    Value* c = nullptr;
                    if (std::holds_alternative<P::Wild>(C.tpat[idx].node)) c = B.getInt1(true);
                    else if (std::holds_alternative<P::Int>(C.tpat[idx].node)) {
                        int v = std::get<P::Int>(C.tpat[idx].node).value;
                        c = B.CreateICmpEQ(el, B.getInt32(v));
                    }
                    else if (std::holds_alternative<P::Dur>(C.tpat[idx].node)) {
                        int v = std::get<P::Dur>(C.tpat[idx].node).value;
                        c = B.CreateICmpEQ(el, B.getInt32(v));
                    }
                    else if (std::holds_alternative<P::Bind>(C.tpat[idx].node)) {
                        c = B.getInt1(true);
                    }
                    cond = cond ? B.CreateAnd(cond, c) : c;
                }
            }
            else {
                cond = B.getInt1(false); // enum paths not implemented for expr form here
            }
            // Guard
            if (C.guard) {
                Value* gv = emit_expr(B, *C.guard, locals, qflags, enums, localsTy);
                gv = B.CreateICmpNE(gv, B.getInt32(0));
                cond = cond ? B.CreateAnd(cond, gv) : gv;
            }
            B.CreateCondBr(cond, bodyBB, curCont);
            curCont = testBB;
        }
        // entry into first test
        B.SetInsertPoint(curCont);
        B.CreateBr(endBB);
        B.SetInsertPoint(endBB);

        // PHI node with unified i32 (durations are represented as i32 as well)
        if (!incoming.empty()) {
            phi = B.CreatePHI(B.getInt32Ty(), incoming.size(), "match.val");
            for (auto& inc : incoming) phi->addIncoming(inc.second, inc.first);
            return phi;
        }
        return B.getInt32(0);
    }

    static Value* emit_expr(IRBuilder<>& B,
        const innesce::ast::Expr& e,
        std::unordered_map<std::string, Value*>& locals,
        std::unordered_map<std::string, Value*>& qflags,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::unordered_map<std::string, innesce::ast::Type>& localsTy) {
        using E = innesce::ast::Expr;
        if (std::holds_alternative<E::IntLit>(e.node)) { return B.getInt32(std::get<E::IntLit>(e.node).value); }
        if (std::holds_alternative<E::StringLit>(e.node)) { return make_cstring(B, B.GetInsertBlock()->getModule(), std::get<E::StringLit>(e.node).value); }
        if (std::holds_alternative<E::DurLit>(e.node)) { return B.getInt32(std::get<E::DurLit>(e.node).value); }
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& nm = std::get<E::Ident>(e.node).name;
            auto it = locals.find(nm); if (it != locals.end()) return B.CreateLoad(B.getInt32Ty(), it->second);
            for (auto& [ename, vars] : enums) { (void)ename; auto vit = vars.find(nm); if (vit != vars.end()) return B.getInt32(vit->second); }
            return nullptr;
        }
        if (std::holds_alternative<E::IsFailed>(e.node)) {
            const auto& IS = std::get<E::IsFailed>(e.node);
            auto it = qflags.find(IS.name); if (it == qflags.end()) return nullptr;
            auto flag = B.CreateLoad(B.getInt1Ty(), it->second);
            return B.CreateZExt(flag, B.getInt32Ty());
        }
        if (std::holds_alternative<E::Unary>(e.node)) {
            const auto& U = std::get<E::Unary>(e.node);
            Value* R = emit_expr(B, *U.rhs, locals, qflags, enums, localsTy); if (!R) return nullptr;
            if (U.op == '-') return B.CreateNeg(R);
            return R;
        }
        if (std::holds_alternative<E::Binary>(e.node)) {
            const auto& BN = std::get<E::Binary>(e.node);
            Value* L = emit_expr(B, *BN.lhs, locals, qflags, enums, localsTy); if (!L) return nullptr;
            Value* R = emit_expr(B, *BN.rhs, locals, qflags, enums, localsTy); if (!R) return nullptr;
        switch (BN.op) { case '+': return B.CreateAdd(L, R); case '-': return B.CreateSub(L, R); case '*': return B.CreateMul(L, R); case '/': return B.CreateSDiv(L, R); }
                                 return nullptr;
        }
        if (std::holds_alternative<E::Cast>(e.node)) {
            const auto& Cst = std::get<E::Cast>(e.node);
            if (std::holds_alternative<E::DurLit>(Cst.inner->node)) {
                auto dl = std::get<E::DurLit>(Cst.inner->node);
                int v = dl.value;
                if (dl.unit == innesce::ast::DurUnit::MS && Cst.target.kind == innesce::ast::Type::DUR && Cst.target.dur == innesce::ast::DurUnit::SEC) return B.getInt32(v / 1000);
                if (dl.unit == innesce::ast::DurUnit::SEC && Cst.target.kind == innesce::ast::Type::DUR && Cst.target.dur == innesce::ast::DurUnit::MS) return B.getInt32(v * 1000);
                return B.getInt32(v);
            }
            Value* inner = emit_expr(B, *Cst.inner, locals, qflags, enums, localsTy); if (!inner) return nullptr;
            // runtime unit conversion if needed
            return inner;
        }
        if (std::holds_alternative<E::Call>(e.node)) {
            const auto& C = std::get<E::Call>(e.node);
            Module* M = B.GetInsertBlock()->getModule();
            auto i32 = B.getInt32Ty(); auto i8p = Type::getInt8PtrTy(B.getContext());
            auto marshal = [&](const innesce::ast::Expr& a)->Value* { return emit_expr(B, a, locals, qflags, enums, localsTy); };
            if (C.name == "fs_open") {
                Value* v = marshal(C.args[0]); if (v->getType() != i8p) v = B.CreateBitCast(v, i8p);
                FunctionType* FT = FunctionType::get(i32, { i8p }, false);
                return B.CreateCall(M->getOrInsertFunction("fs_open_str", FT), { v });
            }
            else if (C.name == "net_tcp") {
                Value* host = marshal(C.args[0]); if (host->getType() != i8p) host = B.CreateBitCast(host, i8p);
                Value* port = marshal(C.args[1]); if (port->getType() != i32) port = B.CreateTruncOrBitCast(port, i32);
                FunctionType* FT = FunctionType::get(i32, { i8p, i32 }, false);
                return B.CreateCall(M->getOrInsertFunction("net_tcp_str_i32", FT), { host, port });
            }
            else if (C.name == "rand_range") {
                Value* lo = marshal(C.args[0]); if (lo->getType() != i32) lo = B.CreateTruncOrBitCast(lo, i32);
                Value* hi = marshal(C.args[1]); if (hi->getType() != i32) hi = B.CreateTruncOrBitCast(hi, i32);
                FunctionType* FT = FunctionType::get(i32, { i32, i32 }, false);
                return B.CreateCall(M->getOrInsertFunction("rand_range_i32", FT), { lo, hi });
            }
            else {
                std::vector<Type*> argtys; std::vector<Value*> args;
                for (auto& a : C.args) { Value* v = marshal(a); if (v->getType() != i32) v = B.CreateTruncOrBitCast(v, i32); argtys.push_back(i32); args.push_back(v); }
                FunctionType* FT = FunctionType::get(i32, argtys, false);
                FunctionCallee cal = M->getOrInsertFunction(C.name, FT);
                return B.CreateCall(cal, args);
            }
        }
        if (std::holds_alternative<E::AsmExpr>(e.node)) {
            const auto& A = std::get<E::AsmExpr>(e.node);
            std::vector<Type*> outtys; std::vector<Type*> argtys; std::vector<Value*> args;
            for (auto& op : A.ins) {
                Value* v = nullptr; if (op.is_immediate) v = B.getInt32(op.imm_val);
                else { auto it = locals.find(op.name); if (it == locals.end()) return nullptr; v = B.CreateLoad(B.getInt32Ty(), it->second); }
                if (v->getType() != B.getInt32Ty()) v = B.CreateTruncOrBitCast(v, B.getInt32Ty());
                argtys.push_back(B.getInt32Ty()); args.push_back(v);
            }
            for (auto& op : A.outs) outtys.push_back(B.getInt32Ty());
            Type* retTy = Type::getVoidTy(B.getContext());
            if (outtys.size() == 1) retTy = outtys[0];
            else if (!outtys.empty()) retTy = StructType::get(B.getContext(), outtys);
            std::string cons;
            for (size_t i = 0; i < A.outs.size(); ++i) { cons += "=" + A.outs[i].constraint; if (i + 1 < A.outs.size()) cons += ","; }
            if (!A.outs.empty() && !A.ins.empty()) cons += ",";
            for (size_t i = 0; i < A.ins.size(); ++i) { cons += A.ins[i].constraint; if (i + 1 < A.ins.size()) cons += ","; }
            for (auto& c : A.clobbers) { cons += ",~{" + c + "}"; }
            auto IA = InlineAsm::get(FunctionType::get(retTy, argtys, false), A.body, cons, true, false, A.intel ? InlineAsm::AD_Intel : InlineAsm::AD_ATT);
            CallInst* call = B.CreateCall(IA, args);
            if (outtys.size() == 1) return call;
            if (!outtys.empty()) return call;
            return B.getInt32(0);
        }
        if (std::holds_alternative<E::MatchExpr>(e.node)) {
            return emit_match_expr(B, F, std::get<E::MatchExpr>(e.node), locals, qflags, enums, localsTy);
        }
        return nullptr;
    }

    static bool emit_block(IRBuilder<>& B,
        Function* F,
        const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Value*>& locals,
        std::unordered_map<std::string, Value*>& qflags,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::unordered_map<std::string, innesce::ast::Type>& localsTy) {
        using S = innesce::ast::Stmt;
        for (auto& st : body) {
            if (std::holds_alternative<S::Let>(st.node)) {
                const auto& L = std::get<S::Let>(st.node);
                Value* init = emit_expr(B, L.init, locals, qflags, enums, localsTy); if (!init) return false;
                AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, L.name);
                B.CreateStore(init, a);
                locals[L.name] = a; localsTy[L.name] = L.type;
            }
            else if (std::holds_alternative<S::LetTuple>(st.node)) {
                const auto& LT = std::get<S::LetTuple>(st.node);
                Value* init = emit_expr(B, LT.init, locals, qflags, enums, localsTy); if (!init) return false;
                if (LT.names.size() == 1) {
                    AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, LT.names[0]); B.CreateStore(init, a);
                    locals[LT.names[0]] = a; localsTy[LT.names[0]] = LT.types[0];
                }
                else {
                    for (size_t i = 0; i < LT.names.size(); ++i) {
                        Value* v = B.CreateExtractValue(init, { unsigned(i) });
                        AllocaInst* a = B.CreateAlloca(B.getInt32Ty(), nullptr, LT.names[i]);
                        B.CreateStore(v, a);
                        locals[LT.names[i]] = a; localsTy[LT.names[i]] = LT.types[i];
                    }
                }
            }
            else if (std::holds_alternative<S::Return>(st.node)) {
                const auto& R = std::get<S::Return>(st.node);
                Value* v = emit_expr(B, R.value, locals, qflags, enums, localsTy); if (!v) return false;
                B.CreateRet(v); return true;
            }
            else if (std::holds_alternative<S::If>(st.node)) {
                const auto& I = std::get<S::If>(st.node);
                Value* c = emit_expr(B, I.cond, locals, qflags, enums, localsTy); if (!c) return false;
                c = B.CreateICmpNE(c, B.getInt32(0));
                BasicBlock* thenBB = BasicBlock::Create(B.getContext(), "then", F);
                BasicBlock* elseBB = BasicBlock::Create(B.getContext(), "else", F);
                BasicBlock* contBB = BasicBlock::Create(B.getContext(), "endif", F);
                B.CreateCondBr(c, thenBB, elseBB);
                B.SetInsertPoint(thenBB); emit_block(B, F, I.then_body, locals, qflags, enums, localsTy); if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(contBB);
                B.SetInsertPoint(elseBB); emit_block(B, F, I.else_body, locals, qflags, enums, localsTy); if (!B.GetInsertBlock()->getTerminator()) B.CreateBr(contBB);
                B.SetInsertPoint(contBB);
            }
            else if (std::holds_alternative<S::Match>(st.node)) {
                // Lower as a side-effect-only chain (no value)
                const auto& M = std::get<S::Match>(st.node);
                // Reuse expression lowering with dummy value by emitting a constant in each body via return to end block
                // For brevity, we just create bodies and tests similarly to expr form but discard the result.
                // We'll call emit_match_expr to generate value and then ignore it.
                using E = innesce::ast::Expr;
                E::MatchExpr mx; mx.scrutinee = M.scrutinee;
                for (auto& c : M.cases) {
                    E::MatchExpr::Case ec;
                    ec.is_default = c.is_default; ec.is_tuple = c.is_tuple; ec.label = c.label; ec.tpat = c.tpat; ec.guard = c.guard;
                    // Wrap the body as value: if it has a trailing 'return x;' we can't easily introspect.
                    // Emit a constant 0 for each case body; but to execute the user's statements, we need a separate lowering.
                    // To keep consistency, we'll inline a trivial 0 value and then actually emit the body before jumping end.
                    // Handled by duplication isn't trivial here; for simplicity, ignore stmt form in this minimal demo.
                    ec.value = std::make_unique<E>(E{ E::IntLit{0} });
                    mx.cases.push_back(std::move(ec));
                }
                (void)emit_match_expr(B, F, mx, locals, qflags, enums, localsTy);
            }
            else if (std::holds_alternative<S::Sleep>(st.node)) {
                const auto& SL = std::get<S::Sleep>(st.node);
                Value* amt = emit_expr(B, SL.amount, locals, qflags, enums, localsTy); if (!amt) return false;
                auto i64 = Type::getInt64Ty(B.getContext());
                auto FT = FunctionType::get(Type::getVoidTy(B.getContext()), { i64 }, false);
                const char* fname = SL.is_ms ? "inn_sleep_ms" : "inn_sleep_sec";
                Function* fs = cast<Function>(B.GetInsertBlock()->getModule()->getOrInsertFunction(fname, FT).getCallee());
                Value* amt64 = B.CreateSExt(amt, i64);
                B.CreateCall(fs, { amt64 });
            }
            else if (std::holds_alternative<S::Fail>(st.node)) {
                if (qflags.empty()) return false;
                auto it = qflags.end(); --it;
                B.CreateStore(B.getInt1(true), it->second);
            }
            else if (std::holds_alternative<S::Quarantine>(st.node)) {
                const auto& Q = std::get<S::Quarantine>(st.node);
                AllocaInst* flag = B.CreateAlloca(B.getInt1Ty(), nullptr, (Q.name + std::string("_failed")).c_str());
                B.CreateStore(B.getInt1(false), flag);
                qflags[Q.name] = flag;
                emit_block(B, F, Q.body, locals, qflags, enums, localsTy);
            }
            else if (std::holds_alternative<S::Asm>(st.node)) {
                // no-op for demo
            }
        }
        return true;
    }

    bool compile_to_object(const innesce::ast::Unit& unit,
        const innesce::front::Sema& sema,
        const std::string& out_obj_path,
        std::string& err) {
        LLVMInitializeNativeTarget();
        LLVMInitializeNativeAsmPrinter();
        LLVMInitializeNativeAsmParser();

        LLVMContext Ctx;
        auto mod = std::make_unique<Module>("inn", Ctx);
        IRBuilder<> B(Ctx);

        const auto& enums = sema.enums();

        for (auto& f : unit.functions) {
            FunctionType* fty = FunctionType::get(Type::getInt32Ty(Ctx), {}, false);
            Function* F = Function::Create(fty, Function::ExternalLinkage, f.name, mod.get());
            if (f.hot) F->addFnAttr(Attribute::Hot);
            BasicBlock* entry = BasicBlock::Create(Ctx, "entry", F);
            B.SetInsertPoint(entry);
            std::unordered_map<std::string, Value*> locals;
            std::unordered_map<std::string, Value*> qflags;
            std::unordered_map<std::string, innesce::ast::Type> localsTy;
            emit_block(B, F, f.body, locals, qflags, enums, localsTy);
            if (!entry->getTerminator()) B.CreateRet(B.getInt32(0));
        }

        std::string verr; raw_string_ostream os(verr);
        if (verifyModule(*mod, &os)) { err = os.str(); return false; }

        auto targetTriple = sys::getDefaultTargetTriple();
        std::string errStr;
        auto target = TargetRegistry::lookupTarget(targetTriple, errStr);
        if (!target) { err = errStr; return false; }

        TargetOptions opt; auto RM = std::optional<Reloc::Model>();
        auto tm = std::unique_ptr<TargetMachine>(target->createTargetMachine(targetTriple, "generic", "", opt, RM));

        mod->setDataLayout(tm->createDataLayout()); mod->setTargetTriple(targetTriple);

        std::error_code EC; raw_fd_ostream dest(out_obj_path, EC, sys::fs::OF_None);
        if (EC) { err = "Could not open output file: " + EC.message(); return false; }

        legacy::PassManager pass;
        if (tm->addPassesToEmitFile(pass, dest, nullptr, CGFT_ObjectFile)) { err = "TargetMachine can't emit an object file"; return false; }
        pass.run(*mod); dest.flush(); return true;
    }

} // namespace innesce::backend

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
    for (int i = 2; i < argc; i++) { std::string a = argv[i]; if (a == "-o" && i + 1 < argc) outobj = argv[++i]; }
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
    for (int i = 2; i < argc; i++) { std::string a = argv[i]; if (a == "-o" && i + 1 < argc) outobj = argv[++i]; }
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
    for (int i = 2; i < argc; i++) { std::string a = argv[i]; if (a == "-o" && i + 1 < argc) outobj = argv[++i]; }
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
extern "C" int fs_read_i32() { return 42; } extern "C" int fs_write_i32() { return 1; } extern "C" int fs_open_i32() { return 3; }
extern "C" int fs_open_str(const char* p) { int n = 0; if (p) while (p[n]) ++n; return n ? n : -1; }
extern "C" int net_ping_i32() { return 7; } extern "C" int net_tcp_i32() { return 200; }
extern "C" int net_tcp_str_i32(const char* h, int port) { int n = 0; if (h) while (h[n]) ++n; return port + n; }
extern "C" int rand_i32() { return 4; }
extern "C" int rand_range_i32(int lo, int hi) { return lo <= hi ? lo : hi; }
extern "C" void inn_sleep_ms(long long) {}
extern "C" void inn_sleep_sec(long long) {}

#pragma once
#include <string>
#include <vector>
#include <memory>
#include <optional>
#include <variant>

namespace innesce::ast {

    enum class DurUnit { MS, SEC };

    struct Type {
        enum Kind { I32, ENUM, DUR, STR, TUPLE } kind{ I32 };
        std::string enum_name; // ENUM
        DurUnit dur{};         // DUR
        std::vector<Type> tuple_elems; // TUPLE
    };

    struct Expr {
        struct IntLit { int value; };
        struct StringLit { std::string value; };
        struct Ident { std::string name; };
        struct Unary { char op; std::unique_ptr<Expr> rhs; };
        struct Binary { char op; std::unique_ptr<Expr> lhs, rhs; };
        struct IsFailed { std::string name; };
        struct DurLit { int value; DurUnit unit; };
        struct Cast { std::unique_ptr<Expr> inner; Type target; };
        struct Call { std::string name; std::vector<Expr> args; };

        struct AsmExpr {
            std::string body;
            bool intel{ true };
            struct Operand { bool is_output{ false }; std::string constraint; bool is_immediate{ false }; int imm_val{ 0 }; std::string name; };
            std::vector<Operand> outs;
            std::vector<Operand> ins;
            std::vector<std::string> clobbers;
        };

        struct Pattern {
            struct Wild {};
            struct Int { int value; };
            struct Dur { int value; DurUnit unit; };
            struct Bind { std::string name; };
            using Node = std::variant<Wild, Int, Dur, Bind>;
            Node node;
        };

        struct MatchExpr {
            Expr scrutinee;
            struct Case { bool is_default{ false }; bool is_tuple{ false }; std::string label; std::vector<Pattern> tpat; std::optional<Expr> guard; std::unique_ptr<Expr> value; };
            std::vector<Case> cases;
        };

        std::variant<IntLit, StringLit, Ident, Unary, Binary, IsFailed, DurLit, Cast, Call, AsmExpr, MatchExpr> node;
    };

    struct Stmt {
        struct Let { std::string name; Type type; Expr init; };
        struct LetTuple { std::vector<std::string> names; std::vector<Type> types; Expr init; };
        struct Return { Expr value; };
        struct If { Expr cond; std::vector<Stmt> then_body; std::vector<Stmt> else_body; };
        struct Match {
            Expr scrutinee;
            struct Case { bool is_default{ false }; bool is_tuple{ false }; std::string label; std::vector<Expr::Pattern> tpat; std::optional<Expr> guard; std::vector<Stmt> body; };
            std::vector<Case> cases;
        };
        struct Sleep { Expr amount; bool is_ms{ true }; };
        struct Fail {};
        struct Quarantine { std::string name; std::vector<Stmt> body; };

        struct Asm {
            std::string body;
            bool intel{ true };
            struct Operand { bool is_output{ false }; std::string constraint; std::string name; bool is_immediate{ false }; int imm_val{ 0 }; };
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
        bool hot{ false };
        std::vector<Stmt> body;
    };

    struct Unit { std::vector<EnumDecl> enums; std::vector<Function> functions; };

} // namespace innesce::ast

#include "frontend/lexer.hpp"
#include <cctype>

namespace innesce::front {

    Lexer::Lexer(std::string_view src) : s_(src) {}

    char Lexer::peek() const { return i_ < s_.size() ? s_[i_] : '\0'; }
    char Lexer::get() {
        if (i_ >= s_.size()) return '\0';
        char c = s_[i_++];
        if (c == '\n') { line_++; col_ = 1; }
        else { col_++; }
        return c;
    }

    void Lexer::skip_ws_and_comments() {
        while (true) {
            char c = peek();
            if (c == '\0') break;
            if (std::isspace(static_cast<unsigned char>(c))) { get(); continue; }
            if (c == '-' && i_ + 1 < s_.size() && s_[i_ + 1] == '-') {
                while (c != '\n' && c != '\0') c = get();
                continue;
            }
            break;
        }
    }

    Token Lexer::make(TokKind k, std::string t) { return Token{ k, std::move(t), 0, line_, col_ }; }

    Token Lexer::next() {
        skip_ws_and_comments();
        char c = peek();
        if (c == '\0') return make(TokKind::End, "");

        if (c == '(') { get(); return make(TokKind::LParen, "("); }
        if (c == ')') { get(); return make(TokKind::RParen, ")"); }
        if (c == '{') { get(); return make(TokKind::LBrace, "{"); }
        if (c == '}') { get(); return make(TokKind::RBrace, "}"); }
        if (c == '[') { get(); return make(TokKind::LBracket, "["); }
        if (c == ']') { get(); return make(TokKind::RBracket, "]"); }
        if (c == '+') { get(); return make(TokKind::Plus, "+"); }
        if (c == '-') { if (i_ + 1 < s_.size() && s_[i_ + 1] == '>') { get(); get(); return make(TokKind::Arrow, "->"); } get(); return make(TokKind::Minus, "-"); }
        if (c == '*') { get(); return make(TokKind::Star, "*"); }
        if (c == '/') { get(); return make(TokKind::Slash, "/"); }
        if (c == ':') { get(); if (peek() == '=') { get(); return make(TokKind::Assign, ":="); } return make(TokKind::Colon, ":"); }
        if (c == ';') { get(); return make(TokKind::Semicolon, ";"); }
        if (c == ',') { get(); return make(TokKind::Comma, ","); }
        if (c == '=') { if (i_ + 1 < s_.size() && s_[i_ + 1] == '>') { get(); get(); return make(TokKind::FatArrow, "=>"); } }

        if (std::isdigit(static_cast<unsigned char>(c))) {
            int v = 0; while (std::isdigit(static_cast<unsigned char>(peek()))) v = v * 10 + (get() - '0');
            Token t = make(TokKind::Int, ""); t.int_val = v; return t;
        }
        if (c == '"') {
            get(); std::string val;
            while (true) { char ch = get(); if (ch == '\0' || ch == '"') break; val.push_back(ch); }
            Token t = make(TokKind::String, val); t.text = val; return t;
        }
        if (std::isalpha(static_cast<unsigned char>(c)) || c == '_') {
            std::string id; while (std::isalnum(static_cast<unsigned char>(peek())) || peek() == '_') id.push_back(get());
            if (id == "fn") return make(TokKind::KwFn, id);
            if (id == "is") return make(TokKind::KwIs, id);
            if (id == "end") return make(TokKind::KwEnd, id);
            if (id == "return") return make(TokKind::KwReturn, id);
            if (id == "let") return make(TokKind::KwLet, id);
            if (id == "i32") return make(TokKind::KwI32, id);
            if (id == "str") return make(TokKind::KwStr, id);
            if (id == "if") return make(TokKind::KwIf, id);
            if (id == "then") return make(TokKind::KwThen, id);
            if (id == "else") return make(TokKind::KwElse, id);
            if (id == "type") return make(TokKind::KwType, id);
            if (id == "enum") return make(TokKind::KwEnum, id);
            if (id == "match") return make(TokKind::KwMatch, id);
            if (id == "case") return make(TokKind::KwCase, id);
            if (id == "default") return make(TokKind::KwDefault, id);
            if (id == "sleep") return make(TokKind::KwSleep, id);
            if (id == "quarantine") return make(TokKind::KwQuarantine, id);
            if (id == "fail") return make(TokKind::KwFail, id);
            if (id == "isfailed") return make(TokKind::KwIsFailed, id);
            if (id == "ms") return make(TokKind::KwMS, id);
            if (id == "sec") return make(TokKind::KwSEC, id);
            if (id == "asm") return make(TokKind::KwASM, id);
            if (id == "with") return make(TokKind::KwWith, id);
            if (id == "Hot") return make(TokKind::KwHot, id);
            if (id == "as") return make(TokKind::KwAs, id);
            if (id == "when") return make(TokKind::KwWhen, id);
            Token t = make(TokKind::Ident, id); t.text = id; return t;
        }
        get(); return next();
    }

} // namespace innesce::front

#pragma once
#include <string>

namespace innesce::front {

    enum class TokKind {
        End, Ident, Int, String,
        KwFn, KwIs, KwEnd, KwReturn, KwLet,
        KwI32, KwStr, KwIf, KwThen, KwElse,
        KwType, KwEnum, KwMatch, KwCase, KwDefault,
        KwSleep, KwQuarantine, KwFail, KwIsFailed,
        KwMS, KwSEC, KwASM, KwWith, KwHot, KwAs, KwWhen,
        LParen, RParen, LBrace, RBrace, LBracket, RBracket,
        Colon, Semicolon, Comma,
        Arrow, Assign, FatArrow,
        Plus, Minus, Star, Slash,
    };

    struct Token {
        TokKind kind;
        std::string text;
        int int_val{ 0 };
        int line{ 1 }, col{ 1 };
    };

    struct Lexer {
        explicit Lexer(std::string_view src);
        Token next();
    private:
        std::string s_;
        size_t i_{ 0 };
        int line_{ 1 }, col_{ 1 };
        char peek() const;
        char get();
        void skip_ws_and_comments();
        Token make(TokKind k, std::string t);
    };

} // namespace innesce::front

#include "frontend/parser.hpp"
#include <iostream>
#include <memory>

namespace innesce::front {

    Parser::Parser(std::string_view src) : lex_(src) { bump(); }
    void Parser::bump() { cur_ = lex_.next(); }
    bool Parser::accept(TokKind k) { if (cur_.kind == k) { bump(); return true; } return false; }
    bool Parser::expect(TokKind k, const char* what) { if (!accept(k)) { std::cerr << "Expected " << what << "\n"; return false; } return true; }

    std::optional<ast::Type> Parser::parse_type_name() {
        ast::Type t;
        if (accept(TokKind::KwI32)) { t.kind = ast::Type::I32; return t; }
        if (accept(TokKind::KwStr)) { t.kind = ast::Type::STR; return t; }
        if (accept(TokKind::KwMS)) { t.kind = ast::Type::DUR; t.dur = ast::DurUnit::MS; return t; }
        if (accept(TokKind::KwSEC)) { t.kind = ast::Type::DUR; t.dur = ast::DurUnit::SEC; return t; }
        if (accept(TokKind::LParen)) {
            std::vector<ast::Type> elems;
            if (!accept(TokKind::RParen)) {
                while (true) {
                    auto et = parse_type_name(); if (!et) return std::nullopt;
                    elems.push_back(*et);
                    if (accept(TokKind::RParen)) break;
                    if (!expect(TokKind::Comma, "','")) return std::nullopt;
                }
            }
            t.kind = ast::Type::TUPLE; t.tuple_elems = std::move(elems); return t;
        }
        if (cur_.kind == TokKind::Ident) { t.kind = ast::Type::ENUM; t.enum_name = cur_.text; bump(); return t; }
        return std::nullopt;
    }

    std::optional<ast::Unit> Parser::parse_unit() {
        ast::Unit u;
        while (cur_.kind != TokKind::End) {
            if (cur_.kind == TokKind::KwType) {
                auto e = parse_enum_decl(); if (!e) return std::nullopt; u.enums.push_back(std::move(*e)); continue;
            }
            auto fn = parse_function(); if (!fn) return std::nullopt; u.functions.push_back(std::move(*fn));
        }
        return u;
    }

    std::optional<ast::EnumDecl> Parser::parse_enum_decl() {
        if (!expect(TokKind::KwType, "'type'")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected enum name\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        if (!expect(TokKind::KwEnum, "'enum'")) return std::nullopt;
        if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
        ast::EnumDecl d; d.name = std::move(name);
        bool first = true;
        while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
            if (!first) expect(TokKind::Comma, "','");
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected variant name\n"; return std::nullopt; }
            d.variants.push_back(cur_.text); bump();
            first = false;
        }
        if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
        accept(TokKind::Semicolon);
        return d;
    }

    std::optional<ast::Function> Parser::parse_function() {
        if (!expect(TokKind::KwFn, "'fn'")) return std::nullopt;
        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected function name\n"; return std::nullopt; }
        std::string name = cur_.text; bump();
        if (!expect(TokKind::LParen, "'('")) return std::nullopt;
        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
        if (!expect(TokKind::Arrow, "'->'")) return std::nullopt;
        auto rty = parse_type_name(); if (!rty) { std::cerr << "Expected return type\n"; return std::nullopt; }

        std::vector<std::string> gates; bool hot = false;
        if (accept(TokKind::KwWith)) {
            if (!expect(TokKind::LBracket, "'['")) return std::nullopt;
            bool first = true;
            while (cur_.kind != TokKind::RBracket && cur_.kind != TokKind::End) {
                if (!first) expect(TokKind::Comma, "','");
                if (accept(TokKind::KwHot)) { hot = true; }
                else if (cur_.kind == TokKind::Ident) { gates.push_back(cur_.text); bump(); }
                else { std::cerr << "Expected gate name or Hot\n"; return std::nullopt; }
                first = false;
            }
            if (!expect(TokKind::RBracket, "']'")) return std::nullopt;
        }

        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;

        ast::Function f; f.name = std::move(name); f.ret = *rty; f.gates = std::move(gates); f.hot = hot;
        while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt(); if (!st) return std::nullopt; f.body.push_back(std::move(*st));
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        return f;
    }

    std::optional<ast::Expr::Pattern> Parser::parse_tuple_pat_elem() {
        ast::Expr::Pattern p;
        if (cur_.kind == TokKind::Ident && cur_.text == "_") { p.node = ast::Expr::Pattern::Wild{}; bump(); return p; }
        if (cur_.kind == TokKind::Int) {
            int v = cur_.int_val; bump();
            if (accept(TokKind::KwMS)) { p.node = ast::Expr::Pattern::Dur{ v, ast::DurUnit::MS }; return p; }
            if (accept(TokKind::KwSEC)) { p.node = ast::Expr::Pattern::Dur{ v, ast::DurUnit::SEC }; return p; }
            p.node = ast::Expr::Pattern::Int{ v }; return p;
        }
        if (cur_.kind == TokKind::Ident) {
            std::string name = cur_.text; bump();
            p.node = ast::Expr::Pattern::Bind{ std::move(name) }; return p;
        }
        return std::nullopt;
    }

    std::optional<ast::Stmt> Parser::parse_match_stmt() {
        if (!expect(TokKind::KwMatch, "'match'")) return std::nullopt;
        auto scrut = parse_expr(); if (!scrut) return std::nullopt;
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        ast::Stmt::Match m; m.scrutinee = std::move(*scrut);
        while (cur_.kind == TokKind::KwCase) {
            bump();
            ast::Stmt::Match::Case c;
            if (accept(TokKind::KwDefault)) {
                c.is_default = true;
            }
            else if (accept(TokKind::LParen)) {
                c.is_tuple = true;
                if (!accept(TokKind::RParen)) {
                    while (true) {
                        auto pe = parse_tuple_pat_elem(); if (!pe) return std::nullopt;
                        c.tpat.push_back(std::move(*pe));
                        if (accept(TokKind::RParen)) break;
                        if (!expect(TokKind::Comma, "','")) return std::nullopt;
                    }
                }
            }
            else {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected case label or tuple pattern\n"; return std::nullopt; }
                c.label = cur_.text; bump();
            }
            if (accept(TokKind::KwWhen)) {
                auto g = parse_expr(); if (!g) return std::nullopt; c.guard = std::move(*g);
            }
            if (!expect(TokKind::FatArrow, "'=>'")) return std::nullopt;
            c.body = parse_case_body();
            m.cases.push_back(std::move(c));
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        ast::Stmt st; st.node = std::move(m); return st;
    }

    std::vector<ast::Stmt> Parser::parse_case_body() {
        std::vector<ast::Stmt> body;
        while (cur_.kind != TokKind::KwCase && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) {
            auto st = parse_stmt(); if (!st) break; body.push_back(std::move(*st));
        }
        return body;
    }

    std::optional<ast::Expr> Parser::parse_case_value() {
        // A single expression until end-of-line case separator (we rely on 'case'/'end'/'default'/'case' boundaries)
        return parse_expr();
    }

    std::optional<ast::Stmt> Parser::parse_stmt() {
        if (cur_.kind == TokKind::KwLet) {
            bump();
            if (accept(TokKind::LParen)) {
                std::vector<std::string> names;
                if (!accept(TokKind::RParen)) {
                    while (true) {
                        if (cur_.kind != TokKind::Ident) { std::cerr << "Expected name in tuple pattern\n"; return std::nullopt; }
                        names.push_back(cur_.text); bump();
                        if (accept(TokKind::RParen)) break;
                        if (!expect(TokKind::Comma, "','")) return std::nullopt;
                    }
                }
                if (!expect(TokKind::Colon, "':'")) return std::nullopt;
                if (!accept(TokKind::LParen)) { std::cerr << "Expected '(' for tuple types\n"; return std::nullopt; }
                std::vector<ast::Type> tys;
                if (!accept(TokKind::RParen)) {
                    while (true) {
                        auto t = parse_type_name(); if (!t) return std::nullopt;
                        tys.push_back(*t);
                        if (accept(TokKind::RParen)) break;
                        if (!expect(TokKind::Comma, "','")) return std::nullopt;
                    }
                }
                if (!expect(TokKind::Assign, "':='")) return std::nullopt;
                auto e = parse_expr(); if (!e) return std::nullopt;
                if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
                ast::Stmt st; st.node = ast::Stmt::LetTuple{ std::move(names), std::move(tys), std::move(*e) };
                return st;
            }
            else {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected identifier after let\n"; return std::nullopt; }
                std::string name = cur_.text; bump();
                if (!expect(TokKind::Colon, "':'")) return std::nullopt;
                auto ty = parse_type_name(); if (!ty) { std::cerr << "Expected type name\n"; return std::nullopt; }
                if (!expect(TokKind::Assign, "':='")) return std::nullopt;
                auto e = parse_expr(); if (!e) return std::nullopt;
                if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
                ast::Stmt st; st.node = ast::Stmt::Let{ std::move(name), *ty, std::move(*e) };
                return st;
            }
        }
        if (cur_.kind == TokKind::KwReturn) {
            bump(); auto e = parse_expr(); if (!e) return std::nullopt;
            if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Return{ std::move(*e) }; return st;
        }
        if (cur_.kind == TokKind::KwIf) {
            bump(); auto cond = parse_expr(); if (!cond) return std::nullopt;
            if (!expect(TokKind::KwThen, "'then'")) return std::nullopt;
            std::vector<ast::Stmt> then_body;
            while (cur_.kind != TokKind::KwElse && cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) { auto st = parse_stmt(); if (!st) return std::nullopt; then_body.push_back(std::move(*st)); }
            std::vector<ast::Stmt> else_body;
            if (accept(TokKind::KwElse)) { while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) { auto st = parse_stmt(); if (!st) return std::nullopt; else_body.push_back(std::move(*st)); } }
            if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::If{ std::move(*cond), std::move(then_body), std::move(else_body) }; return st;
        }
        if (cur_.kind == TokKind::KwMatch) return parse_match_stmt();
        if (cur_.kind == TokKind::KwSleep) { bump(); if (!expect(TokKind::LParen, "'('")) return std::nullopt; auto amt = parse_expr(); if (!amt) return std::nullopt; if (!expect(TokKind::RParen, "')'")) return std::nullopt; if (!expect(TokKind::Semicolon, "';'")) return std::nullopt; ast::Stmt st; st.node = ast::Stmt::Sleep{ std::move(*amt), true }; return st; }
        if (cur_.kind == TokKind::KwFail) { bump(); if (!expect(TokKind::Semicolon, "';'")) return std::nullopt; ast::Stmt st; st.node = ast::Stmt::Fail{}; return st; }
        if (cur_.kind == TokKind::KwQuarantine) {
            bump();
            if (cur_.kind != TokKind::Ident) { std::cerr << "Expected quarantine name"; return std::nullopt; }
            std::string name = cur_.text; bump();
            if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
            std::vector<ast::Stmt> body;
            while (cur_.kind != TokKind::KwEnd && cur_.kind != TokKind::End) { auto st = parse_stmt(); if (!st) return std::nullopt; body.push_back(std::move(*st)); }
            if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
            ast::Stmt st; st.node = ast::Stmt::Quarantine{ std::move(name), std::move(body) }; return st;
        }
        if (cur_.kind == TokKind::KwASM) return parse_asm_block();
        std::cerr << "Unknown statement\n"; return std::nullopt;
    }

    std::optional<ast::Expr> Parser::parse_expr() {
        auto e = parse_add(); if (!e) return std::nullopt;
        while (accept(TokKind::KwAs)) {
            auto ty = parse_type_name(); if (!ty) { std::cerr << "Expected type after 'as'\n"; return std::nullopt; }
            ast::Expr ce; ce.node = ast::Expr::Cast{ std::make_unique<ast::Expr>(std::move(*e)), *ty }; e = std::move(ce);
        }
        return e;
    }

    std::optional<ast::Expr> Parser::parse_add() {
        auto lhs = parse_mul(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Plus || cur_.kind == TokKind::Minus) {
            char op = (cur_.kind == TokKind::Plus) ? '+' : '-'; bump();
            auto rhs = parse_mul(); if (!rhs) return std::nullopt;
            ast::Expr e; e.node = ast::Expr::Binary{ op, std::make_unique<ast::Expr>(std::move(*lhs)), std::make_unique<ast::Expr>(std::move(*rhs)) }; lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_mul() {
        auto lhs = parse_unary(); if (!lhs) return std::nullopt;
        while (cur_.kind == TokKind::Star || cur_.kind == TokKind::Slash) {
            char op = (cur_.kind == TokKind::Star) ? '*' : '/'; bump();
            auto rhs = parse_unary(); if (!rhs) return std::nullopt;
            ast::Expr e; e.node = ast::Expr::Binary{ op, std::make_unique<ast::Expr>(std::move(*lhs)), std::make_unique<ast::Expr>(std::move(*rhs)) }; lhs = std::move(e);
        }
        return lhs;
    }

    std::optional<ast::Expr> Parser::parse_unary() {
        if (cur_.kind == TokKind::Minus) { bump(); auto rhs = parse_unary(); if (!rhs) return std::nullopt; ast::Expr e; e.node = ast::Expr::Unary{ '-', std::make_unique<ast::Expr>(std::move(*rhs)) }; return e; }
        return parse_postfix();
    }

    std::optional<ast::Expr> Parser::parse_postfix() {
        auto p = parse_primary(); if (!p) return std::nullopt;
        while (accept(TokKind::LParen)) {
            std::vector<ast::Expr> args;
            if (!accept(TokKind::RParen)) {
                while (true) { auto a = parse_expr(); if (!a) return std::nullopt; args.push_back(std::move(*a)); if (accept(TokKind::RParen)) break; if (!expect(TokKind::Comma, "','")) return std::nullopt; }
            }
            if (!std::holds_alternative<ast::Expr::Ident>(p->node)) { std::cerr << "call target must be identifier\n"; return std::nullopt; }
            std::string name = std::get<ast::Expr::Ident>(p->node).name;
            ast::Expr call; call.node = ast::Expr::Call{ std::move(name), std::move(args) }; p = std::move(call);
        }
        return p;
    }

    std::optional<ast::Expr> Parser::parse_primary() {
        if (cur_.kind == TokKind::Int) { int v = cur_.int_val; bump(); if (accept(TokKind::KwMS)) { ast::Expr e; e.node = ast::Expr::DurLit{ v, ast::DurUnit::MS }; return e; } if (accept(TokKind::KwSEC)) { ast::Expr e; e.node = ast::Expr::DurLit{ v, ast::DurUnit::SEC }; return e; } ast::Expr e; e.node = ast::Expr::IntLit{ v }; return e; }
        if (cur_.kind == TokKind::String) { ast::Expr e; e.node = ast::Expr::StringLit{ cur_.text }; bump(); return e; }
        if (cur_.kind == TokKind::Ident) { ast::Expr e; e.node = ast::Expr::Ident{ cur_.text }; bump(); return e; }
        if (cur_.kind == TokKind::KwASM) { return parse_asm_expr(); }
        if (cur_.kind == TokKind::KwIsFailed) { bump(); if (!expect(TokKind::LParen, "'('")) return std::nullopt; if (cur_.kind != TokKind::Ident) return std::nullopt; std::string q = cur_.text; bump(); if (!expect(TokKind::RParen, "')'")) return std::nullopt; ast::Expr e; e.node = ast::Expr::IsFailed{ std::move(q) }; return e; }
        if (cur_.kind == TokKind::KwMatch) { return parse_match_expr(); }
        if (accept(TokKind::LParen)) { auto e = parse_expr(); if (!e) return std::nullopt; if (!expect(TokKind::RParen, "')'")) return std::nullopt; return e; }
        std::cerr << "Expected expression\n"; return std::nullopt;
    }

    std::optional<ast::Expr> Parser::parse_match_expr() {
        if (!expect(TokKind::KwMatch, "'match'")) return std::nullopt;
        auto scrut = parse_expr(); if (!scrut) return std::nullopt;
        if (!expect(TokKind::KwIs, "'is'")) return std::nullopt;
        ast::Expr M; ast::Expr::MatchExpr mx; mx.scrutinee = std::move(*scrut);
        while (cur_.kind == TokKind::KwCase) {
            bump();
            ast::Expr::MatchExpr::Case c;
            if (accept(TokKind::KwDefault)) {
                c.is_default = true;
            }
            else if (accept(TokKind::LParen)) {
                c.is_tuple = true;
                if (!accept(TokKind::RParen)) {
                    while (true) {
                        auto pe = parse_tuple_pat_elem(); if (!pe) return std::nullopt;
                        c.tpat.push_back(std::move(*pe));
                        if (accept(TokKind::RParen)) break;
                        if (!expect(TokKind::Comma, "','")) return std::nullopt;
                    }
                }
            }
            else {
                if (cur_.kind != TokKind::Ident) { std::cerr << "Expected case label or tuple pattern\n"; return std::nullopt; }
                c.label = cur_.text; bump();
            }
            if (accept(TokKind::KwWhen)) {
                auto g = parse_expr(); if (!g) return std::nullopt; c.guard = std::move(*g);
            }
            if (!expect(TokKind::FatArrow, "'=>'")) return std::nullopt;
            auto val = parse_case_value(); if (!val) return std::nullopt;
            c.value = std::make_unique<ast::Expr>(std::move(*val));
            mx.cases.push_back(std::move(c));
        }
        if (!expect(TokKind::KwEnd, "'end'")) return std::nullopt;
        M.node = std::move(mx);
        return M;
    }

    std::optional<ast::Stmt> Parser::parse_asm_block() {
        if (!expect(TokKind::KwASM, "'asm'")) return std::nullopt;
        if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
        ast::Stmt::Asm A;
        while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
            if (cur_.kind == TokKind::Ident && cur_.text == "intel") { bump(); A.intel = true; continue; }
            if (cur_.kind == TokKind::Ident && cur_.text == "att") { bump(); A.intel = false; continue; }
            if (cur_.kind == TokKind::Ident && cur_.text == "outs") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected constraint ident after outs\n"; return std::nullopt; }
                    std::string cons = cur_.text; bump();
                    if (!expect(TokKind::LParen, "'('")) return std::nullopt;
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected variable name in outs()\n"; return std::nullopt; }
                    std::string name = cur_.text; bump();
                    if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                    A.outs.push_back({ true, cons, name, false, 0 });
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after outs entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "ins") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected constraint ident after ins\n"; return std::nullopt; }
                    std::string cons = cur_.text; bump();
                    if (!expect(TokKind::LParen, "'('")) return std::nullopt;
                    if (cur_.kind == TokKind::Ident) {
                        std::string name = cur_.text; bump();
                        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                        A.ins.push_back({ false, cons, name, false, 0 });
                    }
                    else if (cur_.kind == TokKind::Int) {
                        int v = cur_.int_val; bump();
                        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                        A.ins.push_back({ false, cons, "", true, v });
                    }
                    else { std::cerr << "Expected ident or int in ins()\n"; return std::nullopt; }
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after ins entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "clobbers") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected clobber ident\n"; return std::nullopt; }
                    A.clobbers.push_back(cur_.text); bump();
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after clobbers entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "body") {
                bump();
                if (!expect(TokKind::Colon, "':'")) return std::nullopt;
                while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
                    if (cur_.kind == TokKind::Semicolon) { A.body.push_back('\n'); bump(); continue; }
                    if (cur_.kind == TokKind::Ident && (cur_.text == "outs" || cur_.text == "ins" || cur_.text == "clobbers" || cur_.text == "intel" || cur_.text == "att")) break;
                    if (!cur_.text.empty()) { A.body += cur_.text; A.body.push_back(' '); }
                    bump();
                }
                continue;
            }
            if (!cur_.text.empty()) { A.body += cur_.text; A.body.push_back(' '); }
            bump();
        }
        if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
        if (!expect(TokKind::Semicolon, "';'")) return std::nullopt;
        ast::Stmt st; st.node = std::move(A); return st;
    }

    std::optional<ast::Expr> Parser::parse_asm_expr() {
        if (!expect(TokKind::KwASM, "'asm'")) return std::nullopt;
        if (!expect(TokKind::LBrace, "'{'")) return std::nullopt;
        ast::Expr A; ast::Expr::AsmExpr AX;
        while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
            if (cur_.kind == TokKind::Ident && cur_.text == "intel") { bump(); AX.intel = true; continue; }
            if (cur_.kind == TokKind::Ident && cur_.text == "att") { bump(); AX.intel = false; continue; }
            if (cur_.kind == TokKind::Ident && cur_.text == "outs") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected constraint ident after outs\n"; return std::nullopt; }
                    std::string cons = cur_.text; bump();
                    if (!expect(TokKind::LParen, "'('")) return std::nullopt;
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected name in outs() (use '_' for placeholder)\n"; return std::nullopt; }
                    std::string name = cur_.text; bump();
                    if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                    AX.outs.push_back({ true, cons, false, 0, name });
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after outs entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "ins") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected constraint ident after ins\n"; return std::nullopt; }
                    std::string cons = cur_.text; bump();
                    if (!expect(TokKind::LParen, "'('")) return std::nullopt;
                    if (cur_.kind == TokKind::Ident) {
                        std::string name = cur_.text; bump();
                        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                        AX.ins.push_back({ false, cons, false, 0, name });
                    }
                    else if (cur_.kind == TokKind::Int) {
                        int v = cur_.int_val; bump();
                        if (!expect(TokKind::RParen, "')'")) return std::nullopt;
                        AX.ins.push_back({ false, cons, true, v, "" });
                    }
                    else { std::cerr << "Expected ident or int in ins()\n"; return std::nullopt; }
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after ins entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "clobbers") {
                bump();
                while (true) {
                    if (cur_.kind != TokKind::Ident) { std::cerr << "Expected clobber ident\n"; return std::nullopt; }
                    AX.clobbers.push_back(cur_.text); bump();
                    if (accept(TokKind::Comma)) continue;
                    if (accept(TokKind::Semicolon)) break;
                    std::cerr << "Expected ',' or ';' after clobbers entry\n"; return std::nullopt;
                }
                continue;
            }
            if (cur_.kind == TokKind::Ident && cur_.text == "body") {
                bump();
                if (!expect(TokKind::Colon, "':'")) return std::nullopt;
                while (cur_.kind != TokKind::RBrace && cur_.kind != TokKind::End) {
                    if (cur_.kind == TokKind::Semicolon) { AX.body.push_back('\n'); bump(); continue; }
                    if (cur_.kind == TokKind::Ident && (cur_.text == "outs" || cur_.text == "ins" || cur_.text == "clobbers" || cur_.text == "intel" || cur_.text == "att")) break;
                    if (!cur_.text.empty()) { AX.body += cur_.text; AX.body.push_back(' '); }
                    bump();
                }
                continue;
            }
            if (!cur_.text.empty()) { AX.body += cur_.text; AX.body.push_back(' '); }
            bump();
        }
        if (!expect(TokKind::RBrace, "'}'")) return std::nullopt;
        A.node = std::move(AX);
        return A;
    }

} // namespace innesce::front

#pragma once
#include "frontend/lexer.hpp"
#include "frontend/ast.hpp"
#include <optional>

namespace innesce::front {

    class Parser {
    public:
        explicit Parser(std::string_view src);
        std::optional<ast::Unit> parse_unit();

    private:
        Lexer lex_;
        Token cur_;

        void bump();
        bool accept(TokKind k);
        bool expect(TokKind k, const char* what);

        std::optional<ast::EnumDecl> parse_enum_decl();
        std::optional<ast::Function> parse_function();
        std::optional<ast::Stmt> parse_stmt();
        std::optional<ast::Stmt> parse_match_stmt();
        std::optional<ast::Expr> parse_match_expr();
        std::optional<ast::Stmt> parse_asm_block();
        std::optional<ast::Expr> parse_asm_expr();
        std::optional<ast::Expr> parse_expr();
        std::optional<ast::Expr> parse_add();
        std::optional<ast::Expr> parse_mul();
        std::optional<ast::Expr> parse_unary();
        std::optional<ast::Expr> parse_postfix();
        std::optional<ast::Expr> parse_primary();

        std::vector<ast::Stmt> parse_case_body();
        std::optional<ast::Expr> parse_case_value();
        std::optional<ast::Expr::Pattern> parse_tuple_pat_elem();
        std::optional<ast::Type> parse_type_name();
    };

} // namespace innesce::front

#include "frontend/sema.hpp"
#include <unordered_map>
#include <variant>
#include <set>
#include <algorithm>
#include <iostream>

namespace innesce::front {

    using Type = innesce::ast::Type;
    using DurUnit = innesce::ast::DurUnit;

    static bool is_i32(const Type& t) { return t.kind == Type::I32; }
    static bool is_str(const Type& t) { return t.kind == Type::STR; }
    static bool is_tuple(const Type& t) { return t.kind == Type::TUPLE; }
    static bool is_dur(const Type& t, DurUnit* u = nullptr) { if (t.kind == Type::DUR) { if (u)*u = t.dur; return true; } return false; }
    static bool is_enum(const Type& t) { return t.kind == Type::ENUM; }

    static bool same_type(const Type& a, const Type& b) {
        if (a.kind != b.kind) return false;
        if (a.kind == Type::ENUM) return a.enum_name == b.enum_name;
        if (a.kind == Type::DUR) return a.dur == b.dur;
        if (a.kind == Type::TUPLE) {
            if (a.tuple_elems.size() != b.tuple_elems.size()) return false;
            for (size_t i = 0; i < a.tuple_elems.size(); ++i) if (!same_type(a.tuple_elems[i], b.tuple_elems[i])) return false;
        }
        return true;
    }

    static bool has_gate(const std::vector<std::string>& gates, const std::string& g) {
        return std::find(gates.begin(), gates.end(), g) != gates.end();
    }

    static bool check_expr(const innesce::ast::Expr& e,
        std::unordered_map<std::string, Type> locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        const std::vector<std::string>& fn_gates,
        std::string& err,
        Type* outTy = nullptr);

    static void bind_tuple_pattern_locals(const std::vector<innesce::ast::Expr::Pattern>& pat,
        const Type& scrTy,
        std::unordered_map<std::string, Type>& locals,
        std::string& err) {
        if (scrTy.kind != Type::TUPLE) { err = "bind on non-tuple"; return; }
        for (size_t i = 0; i < pat.size(); ++i) {
            if (std::holds_alternative<innesce::ast::Expr::Pattern::Bind>(pat[i].node)) {
                auto name = std::get<innesce::ast::Expr::Pattern::Bind>(pat[i].node).name;
                locals[name] = scrTy.tuple_elems[i];
            }
        }
    }

    static bool check_expr(const innesce::ast::Expr& e,
        std::unordered_map<std::string, Type> locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        const std::vector<std::string>& fn_gates,
        std::string& err,
        Type* outTy) {
        using E = innesce::ast::Expr;
        if (std::holds_alternative<E::IntLit>(e.node)) { if (outTy) outTy->kind = Type::I32; return true; }
        if (std::holds_alternative<E::StringLit>(e.node)) { if (outTy) outTy->kind = Type::STR; return true; }
        if (std::holds_alternative<E::DurLit>(e.node)) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = std::get<E::DurLit>(e.node).unit; } return true; }
        if (std::holds_alternative<E::Ident>(e.node)) {
            const auto& I = std::get<E::Ident>(e.node);
            auto it = locals.find(I.name);
            if (it != locals.end()) { if (outTy) *outTy = it->second; return true; }
            for (auto& [ename, vars] : enums) if (vars.count(I.name)) { if (outTy) { outTy->kind = Type::ENUM; outTy->enum_name = ename; } return true; }
            err = "use of undeclared identifier: " + I.name; return false;
        }
        if (std::holds_alternative<E::IsFailed>(e.node)) { if (outTy) outTy->kind = Type::I32; return true; }
        if (std::holds_alternative<E::Unary>(e.node)) {
            const auto& U = std::get<E::Unary>(e.node);
            Type t; if (!check_expr(*U.rhs, locals, enums, fn_gates, err, &t)) return false;
            if (!is_i32(t)) { err = "unary '-' requires i32"; return false; }
            if (outTy) outTy->kind = Type::I32; return true;
        }
        if (std::holds_alternative<E::Cast>(e.node)) {
            const auto& C = std::get<E::Cast>(e.node);
            Type it; if (!check_expr(*C.inner, locals, enums, fn_gates, err, &it)) return false;
            if (is_dur(it) && C.target.kind == Type::DUR) { if (outTy) { *outTy = C.target; } return true; }
            err = "invalid cast: only duration unit conversions are supported"; return false;
        }
        if (std::holds_alternative<E::AsmExpr>(e.node)) {
            const auto& A = std::get<E::AsmExpr>(e.node);
            if (A.outs.empty()) { if (outTy) outTy->kind = Type::I32; return true; }
            if (A.outs.size() == 1) { if (outTy) outTy->kind = Type::I32; return true; }
            if (outTy) { outTy->kind = Type::TUPLE; outTy->tuple_elems.assign(A.outs.size(), Type{ .kind = Type::I32 }); }
            return true;
        }
        if (std::holds_alternative<E::MatchExpr>(e.node)) {
            const auto& M = std::get<E::MatchExpr>(e.node);
            Type scr; if (!check_expr(M.scrutinee, locals, enums, fn_gates, err, &scr)) return false;
            Type rty; bool rty_set = false;
            for (auto& C : M.cases) {
                auto l2 = locals;
                if (C.is_tuple) {
                    if (scr.kind != Type::TUPLE) { err = "tuple match on non-tuple expression"; return false; }
                    if (C.tpat.size() != scr.tuple_elems.size()) { err = "tuple pattern arity mismatch"; return false; }
                    // validate pattern types and bind names
                    for (size_t i = 0; i < C.tpat.size(); ++i) {
                        const auto& et = scr.tuple_elems[i];
                        if (std::holds_alternative<E::Pattern::Wild>(C.tpat[i].node)) {}
                        else if (std::holds_alternative<E::Pattern::Int>(C.tpat[i].node)) { if (et.kind != Type::I32) { err = "int literal pattern on non-i32"; return false; } }
                        else if (std::holds_alternative<E::Pattern::Dur>(C.tpat[i].node)) { if (et.kind != Type::DUR) { err = "duration literal pattern on non-duration"; return false; } }
                        else if (std::holds_alternative<E::Pattern::Bind>(C.tpat[i].node)) { /* bind ok */ }
                    }
                    bind_tuple_pattern_locals(C.tpat, scr, l2, err);
                }
                if (C.guard) {
                    Type gt; if (!check_expr(*C.guard, l2, enums, fn_gates, err, &gt)) return false;
                    if (!is_i32(gt)) { err = "guard must be i32 (truthy)"; return false; }
                }
                Type vt; if (!check_expr(*C.value, l2, enums, fn_gates, err, &vt)) return false;
                if (!rty_set) { rty = vt; rty_set = true; }
                else if (!same_type(rty, vt)) { err = "match expression case types differ"; return false; }
            }
            if (outTy) *outTy = rty_set ? rty : Type{ .kind = Type::I32 };
            return true;
        }
        if (std::holds_alternative<E::Call>(e.node)) {
            const auto& C = std::get<E::Call>(e.node);
            int argc = (int)C.args.size();
            if (C.name == "fs_open") {
                if (!has_gate(fn_gates, "fs.open")) { err = "missing gate 'fs.open' to call fs_open"; return false; }
                if (argc != 1) { err = "fs_open(path) takes 1 arg"; return false; }
                Type t0; if (!check_expr(C.args[0], locals, enums, fn_gates, err, &t0)) return false;
                if (!is_str(t0)) { err = "fs_open(path) expects str"; return false; }
                if (outTy) outTy->kind = Type::I32; return true;
            }
            else if (C.name == "net_tcp") {
                if (!has_gate(fn_gates, "net.tcp")) { err = "missing gate 'net.tcp' to call net_tcp"; return false; }
                if (argc != 2) { err = "net_tcp(host, port) takes 2 args"; return false; }
                Type t0, t1; if (!check_expr(C.args[0], locals, enums, fn_gates, err, &t0)) return false; if (!check_expr(C.args[1], locals, enums, fn_gates, err, &t1)) return false;
                if (!is_str(t0) || !is_i32(t1)) { err = "net_tcp(host, port) expects (str, i32)"; return false; }
                if (outTy) outTy->kind = Type::I32; return true;
            }
            else if (C.name == "rand_range") {
                if (!has_gate(fn_gates, "rand")) { err = "missing gate 'rand' to call rand_range"; return false; }
                if (argc != 2) { err = "rand_range(lo, hi) takes 2 args"; return false; }
                Type t0, t1; if (!check_expr(C.args[0], locals, enums, fn_gates, err, &t0)) return false; if (!check_expr(C.args[1], locals, enums, fn_gates, err, &t1)) return false;
                if (!is_i32(t0) || !is_i32(t1)) { err = "rand_range(lo, hi) expects (i32, i32)"; return false; }
                if (outTy) outTy->kind = Type::I32; return true;
            }
            else { if (outTy) outTy->kind = Type::I32; return true; }
        }
        const auto& B = std::get<E::Binary>(e.node);
        Type lt, rt;
        if (!check_expr(*B.lhs, locals, enums, fn_gates, err, &lt)) return false;
        if (!check_expr(*B.rhs, locals, enums, fn_gates, err, &rt)) return false;
        if (B.op == '+' || B.op == '-') {
            if (is_i32(lt) && is_i32(rt)) { if (outTy) outTy->kind = Type::I32; return true; }
            DurUnit ul, ur; if (is_dur(lt, &ul) && is_dur(rt, &ur) && ul == ur) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = ul; } return true; }
            err = "duration addition/subtraction requires same units"; return false;
        }
        if (B.op == '*') {
            DurUnit u; if (is_dur(lt, &u) && is_i32(rt)) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = u; } return true; }
            if (is_i32(lt) && is_dur(rt, &u)) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = u; } return true; }
            if (is_i32(lt) && is_i32(rt)) { if (outTy) outTy->kind = Type::I32; return true; }
            err = "invalid duration multiplication"; return false;
        }
        if (B.op == '/') {
            DurUnit u; if (is_dur(lt, &u) && is_i32(rt)) { if (outTy) { outTy->kind = Type::DUR; outTy->dur = u; } return true; }
            if (is_i32(lt) && is_i32(rt)) { if (outTy) outTy->kind = Type::I32; return true; }
            err = "invalid duration division"; return false;
        }
        err = "unknown binary operator"; return false;
    }

    static bool check_block(const std::vector<innesce::ast::Stmt>& body,
        std::unordered_map<std::string, Type> locals,
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums,
        std::string& err,
        bool in_quarantine,
        std::unordered_map<std::string, bool>& declared_quarantines,
        const std::vector<std::string>& fn_gates) {
        using S = innesce::ast::Stmt;
        for (auto& st : body) {
            if (std::holds_alternative<S::Let>(st.node)) {
                const auto& L = std::get<S::Let>(st.node);
                Type t; if (!check_expr(L.init, locals, enums, fn_gates, err, &t)) return false;
                if (!same_type(L.type, t)) { err = "type mismatch in let"; return false; }
                locals[L.name] = L.type;
            }
            else if (std::holds_alternative<S::LetTuple>(st.node)) {
                const auto& LT = std::get<S::LetTuple>(st.node);
                Type t; if (!check_expr(LT.init, locals, enums, fn_gates, err, &t)) return false;
                if (!is_tuple(t) || t.tuple_elems.size() != LT.names.size()) { err = "tuple arity mismatch in let"; return false; }
                if (LT.types.size() != LT.names.size()) { err = "tuple type list mismatch"; return false; }
                for (size_t i = 0; i < LT.names.size(); ++i) {
                    if (!same_type(LT.types[i], t.tuple_elems[i])) {
                        bool ok = false; if (t.tuple_elems[i].kind == Type::I32 && LT.types[i].kind == Type::DUR) ok = true;
                        if (!ok) { err = "tuple element type mismatch"; return false; }
                    }
                    locals[LT.names[i]] = LT.types[i];
                }
            }
            else if (std::holds_alternative<S::Return>(st.node)) {
                const auto& R = std::get<S::Return>(st.node);
                Type t; if (!check_expr(R.value, locals, enums, fn_gates, err, &t)) return false;
            }
            else if (std::holds_alternative<S::If>(st.node)) {
                const auto& I = std::get<S::If>(st.node);
                Type t; if (!check_expr(I.cond, locals, enums, fn_gates, err, &t)) return false;
                if (!is_i32(t)) { err = "if condition must be i32"; return false; }
                if (!check_block(I.then_body, locals, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
                if (!check_block(I.else_body, locals, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
            }
            else if (std::holds_alternative<S::Match>(st.node)) {
                const auto& M = std::get<S::Match>(st.node);
                Type scrTy; if (!check_expr(M.scrutinee, locals, enums, fn_gates, err, &scrTy)) return false;
                for (auto& C : M.cases) {
                    auto l2 = locals;
                    if (C.is_default) { /* ok */ }
                    else if (C.is_tuple) {
                        if (scrTy.kind != Type::TUPLE) { err = "match tuple pattern on non-tuple"; return false; }
                        if (C.tpat.size() != scrTy.tuple_elems.size()) { err = "tuple pattern arity mismatch"; return false; }
                        for (size_t i = 0; i < C.tpat.size(); ++i) {
                            const auto& et = scrTy.tuple_elems[i];
                            if (std::holds_alternative<innesce::ast::Expr::Pattern::Wild>(C.tpat[i].node)) {}
                            else if (std::holds_alternative<innesce::ast::Expr::Pattern::Int>(C.tpat[i].node)) { if (et.kind != Type::I32) { err = "int pattern requires i32 element"; return false; } }
                            else if (std::holds_alternative<innesce::ast::Expr::Pattern::Dur>(C.tpat[i].node)) { if (et.kind != Type::DUR) { err = "duration pattern requires duration element"; return false; } }
                            else if (std::holds_alternative<innesce::ast::Expr::Pattern::Bind>(C.tpat[i].node)) { /* bind ok */ }
                        }
                        bind_tuple_pattern_locals(C.tpat, scrTy, l2, err);
                    }
                    else {
                        if (!is_enum(scrTy)) { err = "match label requires enum scrutinee"; return false; }
                    }
                    if (C.guard) {
                        Type gt; if (!check_expr(*C.guard, l2, enums, fn_gates, err, &gt)) return false;
                        if (!is_i32(gt)) { err = "guard must be i32"; return false; }
                    }
                    if (!check_block(C.body, l2, enums, err, in_quarantine, declared_quarantines, fn_gates)) return false;
                }
            }
            else if (std::holds_alternative<S::Sleep>(st.node)) {
                const auto& SL = std::get<S::Sleep>(st.node);
                Type t; if (!check_expr(SL.amount, locals, enums, fn_gates, err, &t)) return false;
                if (t.kind != Type::DUR) { err = "sleep expects duration value (ms or sec)"; return false; }
                if (!has_gate(fn_gates, "time")) { err = "missing gate 'time' for sleep"; return false; }
            }
            else if (std::holds_alternative<S::Fail>(st.node)) {
                if (!in_quarantine) { err = "fail; outside of quarantine"; return false; }
            }
            else if (std::holds_alternative<S::Quarantine>(st.node)) {
                const auto& Q = std::get<S::Quarantine>(st.node);
                std::unordered_map<std::string, bool> declared;
                if (!check_block(Q.body, locals, enums, err, true, declared, fn_gates)) return false;
            }
            else if (std::holds_alternative<S::Asm>(st.node)) {
                // ok
            }
        }
        return true;
    }

    SemaResult Sema::check(const ast::Unit& u) {
        enums_.clear();
        for (auto& e : u.enums) {
            std::unordered_map<std::string, int> m; for (int i = 0; i < (int)e.variants.size(); ++i) m[e.variants[i]] = i;
            enums_[e.name] = std::move(m);
        }
        bool has_main = false;
        for (auto& f : u.functions) {
            if (f.name == "main") has_main = true;
            std::unordered_map<std::string, Type> locals;
            std::string err;
            std::unordered_map<std::string, bool> declared;
            if (!check_block(f.body, locals, enums_, err, false, declared, f.gates)) return { err };
        }
        if (!has_main) return { "missing 'main' function" };
        return {};
    }

} // namespace innesce::front

#pragma once
#include "frontend/ast.hpp"
#include <string>
#include <unordered_map>
#include <optional>

namespace innesce::front {

    struct SemaResult { std::string error; bool ok() const { return error.empty(); } };

    class Sema {
    public:
        Sema() = default;
        SemaResult check(const ast::Unit& u);
        const std::unordered_map<std::string, std::unordered_map<std::string, int>>& enums() const { return enums_; }
    private:
        std::unordered_map<std::string, std::unordered_map<std::string, int>> enums_;
    };

} // namespace innesce::front
#include <iostream>
int main() { std::cout << "OK\n"; }
