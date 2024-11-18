/* Test out different, equivalent ways to declare the same identifier  */

int main(void) {
    /* Several different ways to declare local long variables */

    /* make sure we can use long type specifier in for loop initializer
     * i is 2^40 so this loop should have 41 iterations
    */
   int sum = 0;
    for (long i = 1099511627776l; i > 0; i = i / 2) {
        sum = sum + 1;
    }

    if (sum != 41) {
        return 5;
    }
    return 0;
}