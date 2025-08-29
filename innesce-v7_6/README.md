
# Innesce v7.6 — multi-output asm (expr) + tuple destructuring + strings + richer stdlib + const-fold casts

This build includes **everything from 7.5** and adds:
- `asm { ... }` usable as an **expression** that returns 1+ values.
- **Named tuple destructuring** in `let (a, b): (i32, i32) := <expr>;` form.
- A first-class **`str`** type and **string literals** (`"hello"`).
- **Extended stdlib (gated)**:
  - `fs_open(path: str) -> i32`  (gate: `fs.open`)
  - `net_tcp(host: str, port: i32) -> i32`  (gate: `net.tcp`)
  - `rand_range(lo: i32, hi: i32) -> i32`  (gate: `rand`)
  - Back-compat: `fs_read_i32`, `fs_write_i32`, `fs_open_i32`, `net_ping_i32`, `net_tcp_i32`, `rand_i32`
- **Duration casts constant-folded** at AOT: `(1500 ms as sec)` becomes `1`.

## Build
```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DINNSCE_ENABLE_LLVM=ON -DLLVM_DIR=/path/to/llvm/lib/cmake/llvm
cmake --build build -j
```

## Compile to obj & link
```bash
./build/innescec sample/asm_destructure.inn -o asm_destructure.o
clang asm_destructure.o -o asm_destructure
./asm_destructure ; echo $?
```

## Samples
- `sample/asm_destructure.inn` — asm as expr + tuple destructuring
- `sample/duration_casts.inn` — unit casts (+ constant fold example)
- `sample/stdlib_args.inn` — fs.open / net.tcp / rand.range
- `sample/asm_block.inn` — legacy statement-form asm
- `sample/perm_stdlib.inn` — all gates together

## Notes
- Tuple values are currently only **i32 elements**; extendable later.
- In asm-expr form, `outs ={eax}(_), ={ebx}(_)` declares return arity/order. Placeholders `_` are allowed; names are ignored since values are not stored until destructured.
- Statement-form asm still writes directly to declared locals via `outs ={reg}(var)`.
