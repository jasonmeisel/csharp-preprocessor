using System;
using System.IO;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;

class Program
{
    static void Main(string[] args)
    {
        var references = new[]
        {
            MetadataReference.CreateFromFile(typeof(object).Assembly.Location),
            MetadataReference.CreateFromFile(Path.Combine(
                Path.GetDirectoryName(typeof(object).Assembly.Location),
                "System.Runtime.dll")),
            MetadataReference.CreateFromFile("C:\\projects\\IL2Workshop\\WorkshopStub\\bin\\Debug\\netcoreapp3.0\\WorkshopStub.dll"),
        };

        var options = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary, optimizationLevel: OptimizationLevel.Release);
        var syntaxTree = SyntaxFactory.ParseSyntaxTree(source);
        var compilation = CSharpCompilation.Create(
            "AsmBuild",
            new[] { syntaxTree },
            references,
            options);
        var semanticModel = compilation.GetSemanticModel(syntaxTree);

        var substituter = new MethodSubstituter(semanticModel, m_generatedMethodToWorkshopCode);
        var newRoot = substituter.Visit(syntaxTree.GetRoot());
        var newTree = SyntaxFactory.SyntaxTree(newRoot);
        var generatedClassTree = substituter.GetGeneratedClass();

        compilation = compilation.RemoveAllSyntaxTrees();
        compilation = compilation.AddSyntaxTrees(newTree, generatedClassTree);

        Console.WriteLine(generatedClassTree.ToString());
    }
}
