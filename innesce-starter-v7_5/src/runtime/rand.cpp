
#include <random>
extern "C" int rand_i32() {
    static thread_local std::mt19937 rng{12345u};
    return static_cast<int>(rng());
}
