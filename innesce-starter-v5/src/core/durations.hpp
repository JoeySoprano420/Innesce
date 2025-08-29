#pragma once
#include <cstdint>
#include <concepts>
#include <compare>
#include <type_traits>

namespace innesce {

// Strong unit duration (integral count of base ticks), similar to std::chrono but minimal.
// We intentionally do NOT mix units implicitly; conversions are explicit.
template <typename UnitTag>
struct Duration {
    using rep = std::int64_t;
    rep value{0};

    constexpr Duration() = default;
    constexpr explicit Duration(rep v) : value(v) {}

    // Comparisons
    constexpr auto operator<=>(const Duration&) const = default;

    // Arithmetic (same-unit only)
    constexpr Duration operator+(Duration other) const { return Duration{value + other.value}; }
    constexpr Duration operator-(Duration other) const { return Duration{value - other.value}; }
    constexpr Duration& operator+=(Duration other) { value += other.value; return *this; }
    constexpr Duration& operator-=(Duration other) { value -= other.value; return *this; }

    // Scale by scalar
    template <std::integral I>
    constexpr Duration operator*(I k) const { return Duration{value * static_cast<rep>(k)}; }

    template <std::integral I>
    constexpr Duration operator/(I k) const { return Duration{value / static_cast<rep>(k)}; }
};

// Unit tags
struct ns_tag{}; struct us_tag{}; struct ms_tag{}; struct sec_tag{}; struct min_tag{}; struct hr_tag{};

using ns  = Duration<ns_tag>;
using us  = Duration<us_tag>;
using ms  = Duration<ms_tag>;
using sec = Duration<sec_tag>;
using min = Duration<min_tag>;
using hr  = Duration<hr_tag>;

// Explicit converters (compile-time factors)
constexpr us to_us(ns x)  { return us{ x.value / 1000 }; }
constexpr ms to_ms(us x)  { return ms{ x.value / 1000 }; }
constexpr sec to_sec(ms x){ return sec{ x.value / 1000 }; }
constexpr ms  to_ms(sec x){ return ms{ x.value * 1000 }; }
constexpr us  to_us(ms x) { return us{ x.value * 1000 }; }
constexpr ns  to_ns(us x) { return ns{ x.value * 1000 }; }

// Literals helpers
constexpr ns  operator""_ns(unsigned long long v)  { return ns{ static_cast<ns::rep>(v) }; }
constexpr us  operator""_us(unsigned long long v)  { return us{ static_cast<us::rep>(v) * 1000 }; }
constexpr ms  operator""_ms(unsigned long long v)  { return ms{ static_cast<ms::rep>(v) * 1000 * 1000 }; }
constexpr sec operator""_sec(unsigned long long v) { return sec{ static_cast<sec::rep>(v) }; }

} // namespace innesce
