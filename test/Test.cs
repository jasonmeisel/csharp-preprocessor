using System;
using System.IO;

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

    [CompileTime]
    public static int RandomInt()
    {
        return new Random().Next();
    }

    public static readonly int UniqueId = RandomInt();

    [CompileTime]
    public static string GetFileText(string path)
    {
        return File.ReadAllText(path);
    }

    public static readonly string GitIgnoreContents = GetFileText(".gitignore");
}
