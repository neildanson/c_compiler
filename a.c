int foo(void)
int foo(void) {
    return 8;
}
int main(void) {
    int foo = 3;
    int bar = 4;
    if (foo + bar > 0) {
        bar = foo(1);
    }
    return foo + bar;
}
