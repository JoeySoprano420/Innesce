#pragma once
#include <iostream>
#include <string>
#include <cstdlib>

inline void assert_true(bool cond, const char* msg) {
    if (!cond) {
        std::cerr << "ASSERT_TRUE failed: " << msg << "\n";
        std::exit(1);
    }
}
template <typename T, typename U>
inline void assert_eq(const T& a, const U& b, const char* msg) {
    if (!(a == b)) {
        std::cerr << "ASSERT_EQ failed: " << msg << "\n";
        std::exit(1);
    }
}
