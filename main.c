int main(void) {
    int k = 1;

    for (int i = 0; i < 10; i = i + 1) {
        if (k > 5) {
            k = k + 3;
            if (k % 2 == 0) {
                continue;
            }
        }
        k = k + 2;
    }
    return k;
}