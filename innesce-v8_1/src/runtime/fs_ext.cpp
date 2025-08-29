extern "C" int fs_open_str(const char* p){int n=0; if(p) while(p[n]) ++n; return n? n: -1;}
