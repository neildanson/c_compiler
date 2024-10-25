int putchar(int c);

int print_fizz(void) {
    putchar(70);
    putchar(73);
    putchar(90);
    putchar(90);
    putchar(10);
    return 0;
}

int print_buzz(void) {
    putchar(66);
    putchar(85);
    putchar(90);
    putchar(90);
    putchar(10);
    return 0;
}

int print_number(int i) {

}

int main(void) {
    print_fizz();
    print_buzz();
    return 0;
}