#include "tests/test_common.hpp"
#include "core/durations.hpp"
using namespace innesce;

int main() {
    auto a = 1000_us;
    auto b = to_ms(a);
    assert_eq(b.value, ms::rep(1), "1000us -> 1ms");
    auto c = to_sec(2000_ms);
    assert_eq(c.value, sec::rep(2), "2000ms -> 2sec");
    auto d = 5_sec;
    auto e = to_ms(d);
    assert_eq(e.value, ms::rep(5000), "5sec -> 5000ms");
    return 0;
}
