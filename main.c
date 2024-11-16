int return_truncated_long(long l) {
    return l;
}

int main(void) {
    int result = return_truncated_long(4294967298l);
    if (result != 2) {
        return 1;
    }

    return 0;
}