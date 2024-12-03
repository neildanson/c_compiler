/* The order in which multiple casts are applied matters */

// start with a global variable so we can't optimize away casts in Part III
unsigned int ui = 4294967200u; // 2^32 - 96

int main(void) {


    /* In this case we
     * 1. convert ui to a signed int by computing ui - 2^32, producing -96
     * 2. signed-extend the result, which preserves the value of -96
     * Note that if we cast ui directly to a signed long, its value wouldn't change
     */
    if ((long) (signed) ui != -96l)
        return 1;

    return 0;
}