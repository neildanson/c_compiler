int putchar(int c);

int main(void) {
    int i = 0;
    int k = 1;
    for (int i = 100; i > 0; i = i - 1) {
        int i = 1;
        k = k + 1;
    }

    //k is incorrect => equals 2, not 101
    return k ;/*k == 101 && i == 0 && j == 0*/
}
