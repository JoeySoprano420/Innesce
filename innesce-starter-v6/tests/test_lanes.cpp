#include "tests/test_common.hpp"
#include "core/lanes.hpp"
#include <atomic>
#include <vector>
#include <numeric>

using namespace innesce;

int main() {
    const std::size_t N = 1000;
    std::vector<int> v(N);
    std::iota(v.begin(), v.end(), 1);
    std::atomic<long long> sum{0};

    Lanes lanes(4);
    lanes.parallel_for(0, N, 16, [&](std::size_t lo, std::size_t hi){
        long long local = 0;
        for (std::size_t i = lo; i < hi; ++i) local += v[i];
        sum.fetch_add(local);
    });
    // Expected sum: arithmetic series 1..1000
    long long expected = (N * (N + 1)) / 2;
    assert_eq(sum.load(), expected, "parallel sum correctness");
    return 0;
}
