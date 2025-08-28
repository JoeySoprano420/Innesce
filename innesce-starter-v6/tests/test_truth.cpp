#include "tests/test_common.hpp"
#include "core/truth.hpp"
using namespace innesce;

int main() {
    assert_eq(t_and(Truth::True, Truth::True), Truth::True, "T and T = T");
    assert_eq(t_or(Truth::False, Truth::False), Truth::False, "F or F = F");
    assert_eq(t_not(Truth::True), Truth::False, "not T = F");
    // Unknown/ Both paths
    auto x = t_and(Truth::Unknown, Truth::True);
    assert_eq(x, Truth::Unknown, "U and T = U");
    auto y = t_or(Truth::Both, Truth::False);
    assert_eq(y, Truth::Both, "B or F = B");
    return 0;
}
