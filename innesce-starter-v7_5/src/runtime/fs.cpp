
#include <cstdint>
#include <cstdio>
extern "C" int fs_read_i32() { return 42; }      // demo stub
extern "C" int fs_write_i32() { return 1; }      // success
extern "C" int fs_open_i32() { return 3; }       // fake fd
