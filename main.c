unsigned long call_strange_function_siganature(unsigned long int a)
{
    return a + 1;
}

int main(void)
{
    return (int)call_strange_function_siganature(5L);
}