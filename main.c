int main(void) {
    int k = 1;

    do {
        int i = 1;
        while (i < 100) {
            i = i + 1;
            if (i == 50) {
                break;
            } else {
                continue;
            }
        }
        k = k + 1;
    } while (k < 50000);
    return k;
}