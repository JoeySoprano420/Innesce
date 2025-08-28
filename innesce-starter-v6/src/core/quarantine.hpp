#pragma once
#include <string>
#include <vector>
#include <utility>
#include <optional>

namespace innesce {

struct Quarantine {
    bool failed{false};
    std::vector<std::string> notes;

    void note(std::string msg) {
        notes.emplace_back(std::move(msg));
    }

    // Attempt wrapper: F must return std::optional<T>. If empty => failure recorded.
    template <typename F>
    auto attempt(F&& f) -> decltype(f()) {
        auto r = f();
        if (!r.has_value()) {
            failed = true;
            note("operation failed");
        }
        return r;
    }
};

} // namespace innesce
