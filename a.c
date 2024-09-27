int main(void) {
    int k = 1;
    do {
        if (k % 10 == 0) {
            break;
        }
        k = k + 1;
    } while (k < 50);
    return k;
}
