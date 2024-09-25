int main(void) {
    int a = 2;
    {
        int b = 47;
        a = b;
    }
    return a;
}
