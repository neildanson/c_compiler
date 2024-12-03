/* Test that we correctly perform conversions "as if by assignment", including:
 * - actual assignment expressions
 * - initializers for automatic variables
 * - return statements
 * Implicit conversions of function arguments are in a separate test case, convert_function_arguments.c
 */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wconstant-conversion"
#else
#pragma GCC diagnostic ignored "-Woverflow"
#endif
#endif
long return_extended_int(int i) {
    return i;
}


int main(void) {
    long result = return_extended_int(-10);
    if (result != -10) {
        return 2;
    }

    return 0;
}