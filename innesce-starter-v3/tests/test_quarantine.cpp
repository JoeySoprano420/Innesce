#include "tests/test_common.hpp"
#include "core/quarantine.hpp"
#include <optional>
#include <string>

using namespace innesce;

static std::optional<int> ok()    { return 42; }
static std::optional<int> fail()  { return std::nullopt; }

int main() {
    Quarantine q{};
    auto r1 = q.attempt(ok);
    assert_true(r1.has_value() && *r1 == 42, "ok returns 42");
    auto r2 = q.attempt(fail);
    assert_true(!r2.has_value(), "fail returns nullopt");
    assert_true(q.failed, "quarantine marked failed");
    assert_true(!q.notes.empty(), "note recorded");
    return 0;
}
