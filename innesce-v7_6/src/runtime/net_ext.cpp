
#include <cstdint>
extern "C" int net_tcp_str_i32(const char* host, int port) {
    int n=0; if (host) while (host[n]) ++n;
    return port + n;
}
