#pragma once
#include <vector>
#include <thread>
#include <functional>
#include <barrier>
#include <atomic>
#include <cstddef>
#include <cassert>

namespace innesce {

// Deterministic lanes: fixed number of worker threads; tasks are partitioned by index.
class Lanes {
public:
    explicit Lanes(std::size_t count)
        : count_(count)
    {
        assert(count_ > 0);
    }

    std::size_t size() const noexcept { return count_; }

    // Parallel for over [begin, end) with step 'chunk'.
    // Each lane operates on indices i where (i / chunk) % lane_count == lane_id.
    void parallel_for(std::size_t begin, std::size_t end, std::size_t chunk,
                      const std::function<void(std::size_t, std::size_t)>& body) const
    {
        const std::size_t lanes = count_;
        std::vector<std::jthread> threads;
        threads.reserve(lanes);
        for (std::size_t lane = 0; lane < lanes; ++lane) {
            threads.emplace_back([=]() {
                for (std::size_t i = begin; i < end; i += chunk) {
                    std::size_t slice = (i - begin) / chunk;
                    if ((slice % lanes) == lane) {
                        std::size_t hi = i + chunk;
                        if (hi > end) hi = end;
                        body(i, hi);
                    }
                }
            });
        }
        // join on destruction of jthreads
    }

    // Packetize into frames; barrier after each frame.
    void packetize_frames(std::size_t begin, std::size_t end, std::size_t frame,
                          const std::function<void(std::size_t, std::size_t)>& body) const
    {
        const std::size_t lanes = count_;
        std::atomic<std::size_t> next{begin};
        std::barrier sync(static_cast<std::ptrdiff_t>(lanes));
        std::vector<std::jthread> threads;
        threads.reserve(lanes);
        for (std::size_t lane = 0; lane < lanes; ++lane) {
            threads.emplace_back([&]() {
                while (true) {
                    std::size_t i = next.fetch_add(frame);
                    if (i >= end) break;
                    std::size_t hi = i + frame;
                    if (hi > end) hi = end;
                    body(i, hi);
                    sync.arrive_and_wait();
                }
            });
        }
    }

private:
    std::size_t count_;
};

} // namespace innesce
