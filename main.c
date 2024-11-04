int my_function(long a, long int b, int long c) {
    return a + b + c;
}

int main(void) {
    if (my_function(1,  2, 3) != 6) {
        return 4;
    }

    return 0;
}