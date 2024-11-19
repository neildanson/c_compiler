/* Test that we correctly find the common type in binary expressions */

long l;
int i;

int conditional(void) {
    // l = 8589934592l, i.e. 2^33
    // i = 10;

    /* When a conditional expression includes both int and long branches,
     * make sure the int type is promoted to a long, rather than the long being
     * converted to an int
     */
    long result = 1 ? l : i;
    return (result == 8589934592l);
}

int main(void) {
    // Conditional
    l = 8589934592l; // 2^33
    i = 10;
    if (!conditional()) {
        return 4;
    }

    return 0;
}