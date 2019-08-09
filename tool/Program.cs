﻿using System;
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

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

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

    public static AttributeData GetAttribute(this ISymbol symbol, string attributeTypeName)
    {
        return symbol.GetAttributes().FirstOrDefault(attr => attr.AttributeClass.Name == attributeTypeName);
    }

    public static IMethodSymbol GetMethodSemantics(this SemanticModel semanticModel, InvocationExpressionSyntax invocation)
    {
        switch (invocation.Expression)
        {
            case MemberAccessExpressionSyntax memberAccess:
                return (IMethodSymbol)semanticModel.GetSymbolInfo(memberAccess.Name).Symbol;
            default:
                var symbolInfo = semanticModel.GetSymbolInfo(invocation.Expression);
                return symbolInfo.Symbol as IMethodSymbol;
        }
    }
}

class DuckTypeRewriter : CSharpSyntaxRewriter
{
    SemanticModel m_semanticModel;

    public Dictionary<string, List<MethodDeclarationSyntax>> NewMethods { get; } =
        new Dictionary<string, List<MethodDeclarationSyntax>>();

    public DuckTypeRewriter(SemanticModel semanticModel)
    {
        m_semanticModel = semanticModel;
    }

    public override SyntaxNode VisitInvocationExpression(InvocationExpressionSyntax node)
    {
        var methodSymbol = m_semanticModel.GetMethodSemantics(node);
        var attribute = methodSymbol.GetAttribute("DuckTypeAttribute");
        if (attribute != null)
        {
            var methodReference = methodSymbol.DeclaringSyntaxReferences.First();
            if (methodReference.GetSyntax() is MethodDeclarationSyntax methodNode)
            {
                var newMethodName = $"GENERATED_{methodSymbol.Name}_{methodSymbol.TypeArguments.ListToString("_")}";

                var duckTypes = attribute.ConstructorArguments.First().Values.Select(constant => constant.Value).Cast<string>().Distinct();
                var newTypeParams = methodNode.TypeParameterList.Parameters.Where(p => !duckTypes.Contains(p.Identifier.ToString()));

                var newAttrLists = methodNode.AttributeLists.
                    Select(list => AttributeList(SeparatedList(list.Attributes.Where(attr =>
                        attr.Name.ToString() != "DuckTypeAttribute" && attr.Name.ToString() != "DuckType")))).
                    Where(list => list.Attributes.Any());

                var newMethodNode = methodNode.WithIdentifier(ParseToken(newMethodName)).
                    WithTypeParameterList(newTypeParams.Any() ? TypeParameterList(SeparatedList(newTypeParams)) : null).
                    WithAttributeLists(List(newAttrLists));

                var duckToInfer = methodSymbol.TypeParameters.Zip(methodSymbol.TypeArguments).
                    ToDictionary(kv => kv.First.ToString(), kv => kv.Second.ToString());
                newMethodNode = newMethodNode.ReplaceNodes(
                    newMethodNode.DescendantNodes().OfType<TypeSyntax>(),
                    (_, n) => duckToInfer.TryGetValue(n.ToString(), out var infer) ? ParseTypeName(infer).WithTriviaFrom(n) : n);

                var methodNodeStr = methodNode.ToString();
                NewMethods.TryAdd(methodNodeStr, new List<MethodDeclarationSyntax>());
                NewMethods[methodNodeStr].Add(newMethodNode);

                return node.WithExpression(ParseExpression(newMethodName));
            }
        }
        return base.VisitInvocationExpression(node);
    }
}

class CompileTimeRewriter : CSharpSyntaxRewriter
{
    SemanticModel m_semanticModel;
    Assembly m_assembly;

    public CompileTimeRewriter(SemanticModel semanticModel, Assembly asm)
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

    public override SyntaxNode VisitInvocationExpression(InvocationExpressionSyntax node)
    {
        var symbol = m_semanticModel.GetMethodSemantics(node);
        if (symbol.GetAttribute("CompileTimeAttribute") != null)
        {
            var returnType = symbol.ReturnType.ToString();
            var fullInvocation = SyntaxFactory.InvocationExpression(
                SyntaxFactory.ParseExpression(symbol.GetFullMetadataName()),
                node.ArgumentList);
            var value = CompileAndRun(returnType, fullInvocation.ToString());
            var valueStr = value.ToString();
            return SyntaxFactory.ParseExpression(ObjectToLiteral(value, valueStr));
        }
        return base.VisitInvocationExpression(node);
    }

    static string ObjectToLiteral(object value, string valueStr)
    {
        switch (value)
        {
            case string str:
                return $"\"{Regex.Escape(valueStr)}\"";
            case float f:
                return $"{valueStr}f";
        }
        return valueStr;
    }

    public override SyntaxNode Visit(SyntaxNode node)
    {
        switch (node)
        {
            case MemberDeclarationSyntax member when m_semanticModel.GetDeclaredSymbol(member)?.GetAttribute("CompileTimeAttribute") != null:
            case FieldDeclarationSyntax field when field.Declaration.Variables.Any(v => m_semanticModel.GetDeclaredSymbol(v)?.GetAttribute("CompileTimeAttribute") != null):
                return null;
        }
        return base.Visit(node);
    }
}

class Program
{
    static void Main(string[] args)
    {
        // var thread = new Thread(Run);
        // thread.Start();
        // thread.Join();
        Run();
    }

    public static MetadataReference[] MetadataReferences { get; private set; }

    static void Run()
    {
        var source = File.ReadAllText("test/Test.cs");

        var references = MetadataReferences = AppDomain.CurrentDomain.GetAssemblies().
            Select(ass => MetadataReference.CreateFromFile(ass.Location)).
            Concat(new[]
            {
                MetadataReference.CreateFromFile("lib/bin/Debug/netcoreapp3.0/lib.dll"),
            }).
            ToArray();

        var options = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary, optimizationLevel: OptimizationLevel.Release);
        var syntaxTree = SyntaxFactory.ParseSyntaxTree(source);
        var compilation = CompileTree(references, options, syntaxTree);
        var semanticModel = compilation.GetSemanticModel(syntaxTree);
        syntaxTree = ExecuteDuckTypes(syntaxTree, semanticModel);

        compilation = CompileTree(references, options, syntaxTree);
        semanticModel = compilation.GetSemanticModel(syntaxTree);
        var assembly = Assembly.LoadFrom("AsmBuild.dll");
        syntaxTree = RunRewriter(syntaxTree, new CompileTimeRewriter(semanticModel, assembly));

        Console.WriteLine(syntaxTree);

        // var generatedClassTree = substituter.GetGeneratedClass();

        // compilation = compilation.RemoveAllSyntaxTrees();
        // compilation = compilation.AddSyntaxTrees(newTree, generatedClassTree);

        // Console.WriteLine(generatedClassTree.ToString());
    }

    static CSharpCompilation CompileTree(MetadataReference[] references, CSharpCompilationOptions options, SyntaxTree syntaxTree)
    {
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
        }

        return compilation;
    }

    static SyntaxTree ExecuteDuckTypes(SyntaxTree syntaxTree, SemanticModel semanticModel)
    {
        var rewriter = new DuckTypeRewriter(semanticModel);
        syntaxTree = RunRewriter(syntaxTree, rewriter);

        var root = syntaxTree.GetRoot();
        var allMethodDecls = syntaxTree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>();
        foreach (var method in allMethodDecls)
        {
            if (rewriter.NewMethods.TryGetValue(method.ToString(), out var newMethods))
            {
                root = root.ReplaceNode(method, newMethods);
            }
        }
        return syntaxTree.WithRootAndOptions(root, syntaxTree.Options);
    }

    static SyntaxTree RunRewriter(SyntaxTree syntaxTree, CSharpSyntaxRewriter rewriter)
    {
        var newRoot = rewriter.Visit(syntaxTree.GetRoot());
        return SyntaxFactory.SyntaxTree(newRoot);
    }
}
