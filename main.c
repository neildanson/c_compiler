int i = 100L;

int sixsixsix(void) {
    return 666L;
}

int main(void) {
    int j = (int)sixsixsix();
    return i + j;
}