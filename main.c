int static foo(void) {
    return 3;
}

int static bar = 4;

int main(void) {
    //movl bar(%rip), %r10d
    return foo() + bar;
}