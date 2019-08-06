public static class Test
{
    [CompileTime]
    public static int Add(int a, int b)
    {
        return a + b;
    }

    public static int GetNum()
    {
        return Add(3, 5);
    }
}
