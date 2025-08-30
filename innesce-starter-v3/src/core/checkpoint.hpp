#pragma once
#include <string_view>

namespace innesce {

// Compile-time/no-op checkpoint markers; can be wired to profiling later.
inline void checkpoint(std::string_view) noexcept {
    // no-op (placeholders for future instrumentation)
}

} // namespace innesce
