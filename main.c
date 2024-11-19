/* Make sure we can handle adding, subtracting,
 * and multiplying by constants that are outside
 * the range of int, but inside the range of unsigned int;
 * this tests several assembly rewrite rules.
 */


/* Make x a global variable so this test doesn't rely on
 * correct argument passing for longs but won't get optimized away in part III
 */
long x = 5l;

int multiply_by_large(void) {
    // x = 5
    x = x * 4294967290l;
    return (x == 21474836450l);
}

int main(void) {
    if (!multiply_by_large()) {
        return 3;
    }

    return 0;
}