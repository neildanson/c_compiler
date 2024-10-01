int main(void) {
    int k = 0;
    for (int i = 0; i < 1000000; i = i + 1) {
        k = k + i;
    }
    return k;
}