/* Test basic arithmetic operations on unsigned integers
 * None of these operations wrap around; that's tested separately in arithmetic_wraparound
 */

unsigned long ul_a;
unsigned long ul_b;


int remaind(void) {
    // ul_a = 100
    // ul_b = 18446744073709551605

    /* ul_b % ul_a is 5.
     * If you interpreted these as signed values, ul_b would be -11
     * and ul_b % ul_a would also be -11.
     */

    return (ul_b % ul_a == 5ul);
}

int main(void) {
    ul_a = 100ul;
    ul_b = 18446744073709551605ul;
    if (!remaind()) {
        return 7;
    }


    return 0;
}