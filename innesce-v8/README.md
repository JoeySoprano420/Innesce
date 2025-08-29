
# Innesce v8 — Mixed-type tuples + tuple `match`

## What’s new
- **Mixed-type tuples**: tuple elements can be `i32` or duration types (`ms`/`sec`). In destructuring, `asm`-returned tuples unify to your annotated types (durations are represented as `i32` under the hood).
- **Match on tuples**: write tuple patterns with `_`, integer literals, and duration literals:
  ```inn
  match pair is
    case (5, 1500 ms) => ...
    case (_, 100 ms)  => ...
    default => ...
  end
  ```

## Still here from v7.x
- `asm { ... }` as **expression** (multi-output) and as **statement**.
- `str` type + string literals.
- Gated stdlib: `fs_open(path: str) -> i32`, `net_tcp(host: str, port: i32) -> i32`, `rand_range(lo: i32, hi: i32) -> i32` (+ legacy *_i32 variants).
- Duration unit casts with **constant-folding**: `(1500 ms as sec)` becomes `1`.

## Build
```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release -DINNSCE_ENABLE_LLVM=ON -DLLVM_DIR=/path/to/llvm/lib/cmake/llvm
cmake --build build -j
```

## Example
```bash
./build/innescec sample/match_tuple.inn -o match_tuple.o
clang match_tuple.o -o match_tuple
./match_tuple ; echo $?
```
