
# Innesce v7 (grammar + Sema skeleton)

This minimal skeleton focuses on **grammar**, **type-checking**, **gates**, **duration unit conversions**, and **asm blocks**. It compiles a CLI that parses `.inn` and runs semantic checks.

> LLVM lowering is omitted in this cut to keep it self-contained. If you want the v6 LLVM-backed lowering re-wired into this v7 surface, say the word and I'll layer it back in.

## Build
```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
```

## Run
```bash
./build/innescec sample/asm_block.inn
# -> OK
```

## Language bits in this build

### First-class durations + `as` conversions
- Types: `ms`, `sec`
- Literals: `100 ms`, `2 sec`
- Arithmetic: `+`/`-` requires same units; `*`/`/` with `i32` keeps unit.
- Conversions: `(expr as ms)` / `(expr as sec)` — only **duration↔duration** casts are allowed.

### `sleep(<duration>)` (requires gate `time`)
```inn
fn main() -> i32 with [time] is
  sleep(200 ms);
  return 0;
end
```

### Gates permissions scaffold
- Declare on the function: `with [time, fs.read, fs.write, net, Hot]`
- Enforced in Sema for:
  - `sleep` → `time`
  - `fs_read_i32()` → `fs.read`
  - `fs_write_i32()` → `fs.write`
  - `net_ping_i32()` → `net`
- `Hot` is recorded (no effect in this skeleton).

### Inline `asm { ... }` blocks (multi-line + operands)
```inn
asm {
  intel;                 -- or `att;`
  ins r(v), i(7);        -- inputs: constraint + (var) or (imm)
  clobbers cc, memory;   -- optional
  body: mov eax, eax; cpuid; add eax, eax;
};
```
> Parser stores: body (string), inputs, clobbers, dialect. (Codegen not present in this skeleton.)

### Enum + match (grammar present; minimal sema here)

## Samples
- `sample/duration_casts.inn`
- `sample/asm_block.inn`
- `sample/perm_stdlib.inn`

## Roadmap hooks
- Reattach LLVM IR codegen (v6) with:
  - `asm` inline via LLVM InlineAsm (inputs wired; outputs next)
  - Duration casts lower to `*1000` / `/1000`
  - Gates guard calls to `fs_*`/`net_*` externs
