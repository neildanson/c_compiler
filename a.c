int putchar(int c);
int helloworld(int x) {
    putchar(72);
    putchar(101);
    putchar(108);
    putchar(108);
    putchar(111);
    putchar(44);
    putchar(32);
    putchar(87);
    putchar(111);
    putchar(114);
    putchar(108);
    putchar(100);
    putchar(33);
    putchar(10);
    putchar(0);
    return x;
}
int main(void) {
    for (int i = 0; i < 100; i = i + 1) {
        helloworld(66);
    }
    return 0;
}
