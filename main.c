/* you can redeclare a function multiple times,
 * but only define it once
 */
extern int sum(int a, int b);

int sum(int i, int j) {
    static int k = 0;
    return i + j;
}

int sum(int x, int y);
