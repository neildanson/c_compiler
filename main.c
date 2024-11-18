int main(void) {
    /* Initialize and then update a mix of long and int variables,
     * to check that we allocate enough stack space for each of them,
     * and writing to one doesn't clobber another */

    long c = -8589934592l; // also outside the range of int
    
    if (c != -8589934592l) {
        return 3;
    }
    
    return 0;
}