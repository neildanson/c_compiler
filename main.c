int lotsofargs(int l, int o) {
    return l + 1;
}

int main(void) {
    int ret = 0;
    for (int i = 0; i < 10000000; i = i + 1) {
        ret = lotsofargs(1);
    }
    return ret == 150000000;
}