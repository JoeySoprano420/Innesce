# Innesce (N‑S) — Starter Skeleton (CMake + C++23 + optional LLVM)

This is a minimal, **deterministic AOT** skeleton you can extend into the full Innesce compiler.

## Layout
```
innesce-starter/
  CMakeLists.txt
  cmake/FindLLVMHelper.cmake
  src/
    core/                # No-LLVM utilities used by tests
      durations.hpp
      truth.hpp
      quarantine.hpp
      lanes.hpp
      lanes.cpp
      checkpoint.hpp
    backend/llvm/        # LLVM stub
      ir_builder.hpp
      ir_builder.cpp
  cli/
    innescec.cpp         # CLI driver (prints version banner)
  tests/
    CMakeLists.txt
    test_common.hpp
    test_durations.cpp
    test_truth.cpp
    test_quarantine.cpp
    test_lanes.cpp
```

## Build (no LLVM needed for core + tests)
```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
ctest --test-dir build --output-on-failure
```

## Build with LLVM backend enabled
Make sure LLVM 17+ is installed and `LLVM_DIR` points at its CMake config folder (e.g. `/usr/lib/llvm-17/lib/cmake/llvm`).

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DINNSCE_ENABLE_LLVM=ON -DLLVM_DIR=/path/to/llvm/cmake
cmake --build build -j
```

On Windows (MSVC), use the “x64 Native Tools” prompt and pass `-G "Ninja"` (recommended) or use the default MSBuild generator:
```pwsh
cmake -S . -B build -G "Ninja" -DCMAKE_BUILD_TYPE=Release -DINNSCE_ENABLE_LLVM=ON -DLLVM_DIR="C:/Program Files/LLVM/lib/cmake/llvm"
cmake --build build -j
ctest --test-dir build --output-on-failure
```

## What’s here
- **durations**: strong unit types (`ns, us, ms, sec`) with explicit conversion and type-safe ops.
- **truth**: 4‑valued truth lattice with constexpr `t_and`, `t_or`, `t_not` folding.
- **quarantine**: containment utility to record recoverable failures instead of crashing.
- **lanes**: deterministic, fixed worker lanes + `parallel_for` and simple frame packetizer.
- **LLVM stub**: `IrBuilder` placeholder to start wiring AST→LLVM IR later.

All components are written in portable C++23 with no exceptions in hot paths.


## Try the tiny front-end + codegen
```bash
# Configure with LLVM enabled (set LLVM_DIR to your install)
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DINNSCE_ENABLE_LLVM=ON -DLLVM_DIR=/path/to/llvm/lib/cmake/llvm
cmake --build build -j

# Compile the sample into an object file
./build/innescec sample/hello.inn -o hello.o

# Link to an executable (Linux/macOS)
clang hello.o -o hello
./hello ; echo $?
```
Expected exit code is `40` for the default sample (returns `x`).

**Subset supported right now**
- Single `main()` function returning `i32`
- `let <name>: i32 := <int|ident>;`
- `return <int|ident>;`
