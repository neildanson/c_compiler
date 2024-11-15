
/* Comparisons where both operands are constants */
int compare_constants(long l) {
    /* Note that if we considered only the lower 32 bits of
     * each number (or cast them to ints), 255 would be larger,
     * because 8589934593l == 2^33 + 1.
     * This exercises the rewrite rule for cmp with two constant operands
     */
    return 8589934593l > l;
}


int main(void) {

    if (!compare_constants(256L)) {
        return 1;
    }

    return 0;
}
