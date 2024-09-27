int main(void) {
    int k = 1;

    for (int i = 0; i < 100000000; i = i + 1) {
        k = k + 1;
    }
    
    return k;
}