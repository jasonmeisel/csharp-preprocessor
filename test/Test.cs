using System;
using System.IO;

public static class Test
{
    [CompileTime]
    public static int Add(int a, int b)
    {
        return a + b;
    }

    public static int Num => Add(3, 5);

    [CompileTime]
    public static int RandomInt()
    {
        return new Random().Next();
    }

    public static int UniqueId => RandomInt();

    [CompileTime]
    public static string GetFileText(string path)
    {
        return File.ReadAllText(path);
    }

    public static string GitIgnoreContents => GetFileText(".gitignore");

    [CompileTime]
    static int s_nextNumber = 0;

    [CompileTime]
    public static int GetNextNumber()
    {
        return s_nextNumber++;
    }

    public static int Num0 => GetNextNumber();
    public static int Num1 => GetNextNumber();
    public static int Num2 => GetNextNumber();
    public static int Num3 => GetNextNumber();
}
