using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

public static class Extensions
{
    public static string ListToString<T>(this IEnumerable<T> list, string separator = ", ")
    {
        return string.Join(separator, list);
    }
    
    // https://stackoverflow.com/questions/27105909/get-fully-qualified-metadata-name-in-roslyn
    public static string GetFullMetadataName(this ISymbol s)
    {
        if (s == null || IsRootNamespace(s))
        {
            return string.Empty;
        }

        var sb = new StringBuilder(s.MetadataName);
        var last = s;

        s = s.ContainingSymbol;

        while (!IsRootNamespace(s))
        {
            if (s is ITypeSymbol && last is ITypeSymbol)
            {
                sb.Insert(0, '+');
            }
            else
            {
                sb.Insert(0, '.');
            }

            sb.Insert(0, s.OriginalDefinition.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
            //sb.Insert(0, s.MetadataName);
            s = s.ContainingSymbol;
        }

        return sb.ToString();
    }

    private static bool IsRootNamespace(ISymbol symbol)
    {
        return symbol is INamespaceSymbol s && s.IsGlobalNamespace;
    }
}

class Rewriter : CSharpSyntaxRewriter
{
    SemanticModel m_semanticModel;
    Assembly m_assembly;

    public Rewriter(SemanticModel semanticModel, Assembly asm)
    {
        m_semanticModel = semanticModel;
        m_assembly = asm;
    }

    object CompileAndRun(string returnType, string source)
    {
        var syntaxTree = SyntaxFactory.ParseSyntaxTree($"public static class Runner {{ public static {returnType} Run() {{ return {source}; }} }}");
        var options = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary);
        var compilation = CSharpCompilation.Create("InMemoryAssembly", options: options).
            AddReferences(Program.MetadataReferences.Concat(new[] { MetadataReference.CreateFromFile(m_assembly.Location) })).
            AddSyntaxTrees(syntaxTree);

        var stream = new MemoryStream();
        var emitResult = compilation.Emit(stream);

        stream.Seek(0, SeekOrigin.Begin);
        var assembly = Assembly.Load(stream.ToArray());
        var value = assembly.GetType("Runner").GetMethod("Run").Invoke(null, new object[0]);
        // TODO: unload assembly
        return value;
    }

    IMethodSymbol GetMethodSemantics(InvocationExpressionSyntax invocation)
    {
        switch (invocation.Expression)
        {
            case MemberAccessExpressionSyntax memberAccess:
                return (IMethodSymbol)m_semanticModel.GetSymbolInfo(memberAccess.Name).Symbol;
            default:
                var symbolInfo = m_semanticModel.GetSymbolInfo(invocation.Expression);
                return symbolInfo.Symbol as IMethodSymbol;
        }
    }

    public override SyntaxNode VisitInvocationExpression(InvocationExpressionSyntax node)
    {
        var methodSemantics = GetMethodSemantics(node);
        if (HasCompileTimeAttribute(methodSemantics))
        {
            var returnType = methodSemantics.ReturnType.ToString();
            var fullInvocation = SyntaxFactory.InvocationExpression(
                SyntaxFactory.ParseExpression(methodSemantics.GetFullMetadataName()),
                node.ArgumentList);
            var value = CompileAndRun(returnType, fullInvocation.ToString());
            var valueStr = value.ToString();
            return SyntaxFactory.ParseExpression(value is string ? $"\"{Regex.Escape(valueStr)}\"" : valueStr);
        }
        return base.VisitInvocationExpression(node);
    }

    private static bool HasCompileTimeAttribute(IMethodSymbol methodSemantics)
    {
        return methodSemantics.GetAttributes().Any(attr => attr.AttributeClass.Name == "CompileTimeAttribute");
    }

    public override SyntaxNode VisitMethodDeclaration(MethodDeclarationSyntax node)
    {
        var symbol = m_semanticModel.GetDeclaredSymbol(node);
        if (HasCompileTimeAttribute(symbol))
        {
            return null;
        }
        return base.VisitMethodDeclaration(node);
    }
}

class Program
{
    static void Main(string[] args)
    {
        var thread = new Thread(Run);
        thread.Start();
        thread.Join();
    }

    public static MetadataReference[] MetadataReferences { get; private set; }

    static async void Run()
    {
        var source = await File.ReadAllTextAsync("test/Test.cs");

        var references = MetadataReferences = AppDomain.CurrentDomain.GetAssemblies().
            Select(ass => MetadataReference.CreateFromFile(ass.Location)).
            Concat(new[]
            {
                MetadataReference.CreateFromFile("lib/bin/Debug/netcoreapp3.0/lib.dll"),
            }).
            ToArray();

        var options = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary, optimizationLevel: OptimizationLevel.Release);
        var syntaxTree = SyntaxFactory.ParseSyntaxTree(source);
        var compilation = CSharpCompilation.Create(
            "AsmBuild",
            new[] { syntaxTree },
            references,
            options);

        var result = compilation.Emit("AsmBuild.dll");
        if (!result.Success)
        {
            foreach (var diag in result.Diagnostics)
                Console.WriteLine(diag);
            throw new System.Exception("Fail!");
        }

        var asm = Assembly.LoadFrom("AsmBuild.dll");

        var semanticModel = compilation.GetSemanticModel(syntaxTree);

        var substituter = new Rewriter(semanticModel, asm);
        var newRoot = substituter.Visit(syntaxTree.GetRoot());
        var newTree = SyntaxFactory.SyntaxTree(newRoot);
        Console.WriteLine(newTree);
        // var generatedClassTree = substituter.GetGeneratedClass();

        // compilation = compilation.RemoveAllSyntaxTrees();
        // compilation = compilation.AddSyntaxTrees(newTree, generatedClassTree);

        // Console.WriteLine(generatedClassTree.ToString());
    }
}
