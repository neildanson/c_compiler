int simple(int param1, int param2) {
    if (param1 > 0) {
        return param2;
    }
    return param1;
}
int main(void) {
    int x = 3;
    return simple(x, 2);
}
