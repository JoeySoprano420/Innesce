
#include <thread>
#include <chrono>
extern "C" void inn_sleep_ms(long long n) { std::this_thread::sleep_for(std::chrono::milliseconds(n)); }
extern "C" void inn_sleep_sec(long long n) { std::this_thread::sleep_for(std::chrono::seconds(n)); }
