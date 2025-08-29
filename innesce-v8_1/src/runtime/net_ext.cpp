extern "C" int net_tcp_str_i32(const char* h, int port){int n=0; if(h) while(h[n]) ++n; return port+n;}
