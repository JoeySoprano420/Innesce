
# Innesce v9 — Value-returning `match` bodies, multi-condition guards, enum payloads

Syntax highlights (see samples):

- `let res: i32 := match r is ... yield ... end;`
- Guards: `when x > 0 && x < 10`
- Enums with payloads: `type Result is enum { Ok(i32, ms), Err(i32) }`
- Constructors: `Result.Ok(7, 500 ms)`
- Pattern payloads: `case Ok(x, 500 ms) => ...`

See `sample/*.inn`.
