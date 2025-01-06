/* Test that we follow the calling convention for a double return type */


int main(void) {
    double retval = 1234.e75;
    return retval < 1234.e75;
}