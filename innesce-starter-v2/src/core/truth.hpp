#pragma once
#include <cstdint>

namespace innesce {

enum class Truth : std::uint8_t { False=0, True=1, Unknown=2, Both=3 };

// 4-valued truth operations (Kleene-like; tweak as desired)
constexpr Truth t_not(Truth a) {
    switch (a) {
        case Truth::False:   return Truth::True;
        case Truth::True:    return Truth::False;
        case Truth::Unknown: return Truth::Unknown;
        case Truth::Both:    return Truth::Unknown; // design choice: ¬Both = Unknown
    }
    return Truth::Unknown;
}

constexpr Truth t_and(Truth a, Truth b) {
    // truth table encoded; symmetric
    if (a == Truth::False || b == Truth::False) return Truth::False;
    if (a == Truth::True  && b == Truth::True)  return Truth::True;
    if (a == Truth::Both  && b == Truth::True)  return Truth::Both;
    if (b == Truth::Both  && a == Truth::True)  return Truth::Both;
    if (a == Truth::Both  && b == Truth::Both)  return Truth::Both;
    // any combo with Unknown that isn't already decided:
    return Truth::Unknown;
}

constexpr Truth t_or(Truth a, Truth b) {
    if (a == Truth::True || b == Truth::True) return Truth::True;
    if (a == Truth::False && b == Truth::False) return Truth::False;
    if (a == Truth::Both || b == Truth::Both) return Truth::Both;
    return Truth::Unknown;
}

} // namespace innesce
