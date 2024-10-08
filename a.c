int putchar(int c);
int helloworld(void) {
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
    return 66;
}
int main(void) {
    for (int i = 0; i < 10; i = i + 1) {
        helloworld();
    }
    return 0;
}
