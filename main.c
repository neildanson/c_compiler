int main(void) {
    /* "Unsigned double" is not a valid type specifier */
    double d = 10.0;
    int i = d;
    return (double)(int)d;
}