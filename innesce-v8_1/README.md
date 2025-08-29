
# Innesce v8.1 — Tuple binds, guards, and `match` as an expression

## New in v8.1
- **Tuple pattern binds**: bind elements by name inside patterns  
  `case (x, 100 ms) => ...` introduces `x` for that case body.
- **Guards**: add `when <expr>` after the pattern to further filter a case. The guard expression must be `i32`-truthy (non-zero).
- **`match` as an expression**: evaluate to a value (currently lowered as `i32` under the hood, durations are represented as `i32`). All case values must have the **same type**.

### Syntax
```inn
let v: i32 := match expr is
  case (x, 1000 ms) when (x + 1) -> i32 => x + 7
  case (_,  500 ms)                     => 2
  default                               => 0
end;
```

### Notes
- Binds are scoped to the **case body/value** where they appear.
- Tuple patterns support `_`, integer literals, duration literals, and `name` binds.
- Enum `match` remains supported in statement form.
- Guards are checked **after** the pattern matches.

See samples in `sample/`:
- `match_expr_binds_guards.inn`
- `tuple_bind_only.inn`
```

Build is the same as v8.0.
