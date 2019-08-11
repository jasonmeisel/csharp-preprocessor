using System;
using System.IO;

public static class Test
{
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

    [CompileTime] public static T Add<[DuckType] T>(T a, T b) => a + b;

    public static int TestDuckTypeAddInts => Add(1, 2);
    public static float TestDuckTypeAddFloats => Add(1.2f, 3.4f);

    public class Adder<[DuckType] T>
    {
        public T Total { get; private set; } = new T();

        public void Add<[DuckType] T2>(T2 value) => Total += value;
    }

    public static float TestDuckTypeStruct()
    {
        var adder = new Adder<float>();
        adder.Add(3);
        adder.Add(1.5f);
        return adder.Total;
    }
}
