using System;

public class CompileTimeAttribute : Attribute
{

}

public class DuckTypeAttribute : Attribute
{
    public DuckTypeAttribute(params string[] typeParameters)
    {
    }
}
