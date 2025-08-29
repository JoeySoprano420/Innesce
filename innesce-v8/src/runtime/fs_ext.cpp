
#include <cstdint>
extern "C" int fs_open_str(const char* path) {
    int n=0; if (path) while (path[n]) ++n;
    return n > 0 ? n : -1;
}
