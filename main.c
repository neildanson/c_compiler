#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wsign-compare"
#endif
/* Test that we correctly find the common type of different integers */

int int_gt_uint(int i, unsigned int u) {
    // common type is unsigned int
    return i > u;
}

int main(void) {

    // converting -100 from int to unsigned int gives us 2^32 - 100,
    // so -100 > 100u
    return int_gt_uint(-100, 100u);
}