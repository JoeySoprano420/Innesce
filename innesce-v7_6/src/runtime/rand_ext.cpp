
#include <random>
extern "C" int rand_range_i32(int lo, int hi) {
    if (hi < lo) { int t = lo; lo = hi; hi = t; }
    static thread_local std::mt19937 rng{67890u};
    std::uniform_int_distribution<int> dist(lo, hi);
    return dist(rng);
}
