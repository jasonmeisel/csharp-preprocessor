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

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

public static class Utilities
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

    public static IEnumerable<SyntaxNode> DepthFirstDescendentNodes(this SyntaxNode node)
    {
        foreach (var child in node.ChildNodes())
            foreach (var grandchild in child.DepthFirstDescendentNodes())
                yield return grandchild;
        yield return node;
    }

    public static string GenerateNewCode(AttributeData attribute, string name, System.Collections.Immutable.ImmutableArray<ITypeParameterSymbol> typeParameters, System.Collections.Immutable.ImmutableArray<ITypeSymbol> typeArguments, TypeParameterListSyntax typeParameterList, SyntaxList<AttributeListSyntax> attributeLists, Func<CSharpSyntaxNode, string, IEnumerable<TypeParameterSyntax>, IEnumerable<AttributeListSyntax>, CSharpSyntaxNode> withDeclarationInfo, CSharpSyntaxNode ogNode, Dictionary<string, List<CSharpSyntaxNode>> newNodes)
    {
        var newName = $"GENERATED_{name}_{typeArguments.ListToString("_")}";

        var duckTypes = attribute.ConstructorArguments.First().Values.Select(constant => constant.Value).Cast<string>().Distinct();
        var newTypeParams = typeParameterList.Parameters.Where(p => !duckTypes.Contains(p.Identifier.ToString()));

        var newAttrLists = attributeLists.
            Select(list => AttributeList(SeparatedList(list.Attributes.Where(attr =>
                attr.Name.ToString() != "DuckTypeAttribute" && attr.Name.ToString() != "DuckType")))).
            Where(list => list.Attributes.Any());

        var newNode = withDeclarationInfo(ogNode, newName, newTypeParams, newAttrLists);

        var duckToInfer = typeParameters.Zip(typeArguments).
            ToDictionary(kv => kv.First.ToString(), kv => kv.Second.ToString());
        newNode = newNode.ReplaceNodes(
            newNode.DescendantNodes().OfType<TypeSyntax>(),
            (_, n) => duckToInfer.TryGetValue(n.ToString(), out var infer) ? ParseTypeName(infer).WithTriviaFrom(n) : n);

        var ogNodeStr = ogNode.ToString();
        newNodes.TryAdd(ogNodeStr, new List<CSharpSyntaxNode>());
        newNodes[ogNodeStr].Add(newNode);
        return newName;
    }
}

abstract class DuckTypeRewriter : CSharpSyntaxRewriter
{
    protected SemanticModel m_semanticModel;

    public Dictionary<string, List<CSharpSyntaxNode>> NewNodes { get; } =
        new Dictionary<string, List<CSharpSyntaxNode>>();

    public DuckTypeRewriter(SemanticModel semanticModel)
    {
        m_semanticModel = semanticModel;
    }
}

class DuckTypeMethodRewriter : DuckTypeRewriter
{
    public DuckTypeMethodRewriter(SemanticModel semanticModel) : base(semanticModel)
    {
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
                var newName = Utilities.GenerateNewCode(attribute, methodSymbol.Name, methodSymbol.TypeParameters, methodSymbol.TypeArguments, methodNode.TypeParameterList, methodNode.AttributeLists, WithMethodDeclarationInfo, methodNode, NewNodes);
                var newExpression = ParseExpression(newName);
                if (node.Expression is MemberAccessExpressionSyntax memberAccess)
                {
                    newExpression = memberAccess.WithName(IdentifierName(newName));
                }
                return node.WithExpression(newExpression);
            }
        }
        return base.VisitInvocationExpression(node);
    }

    static MethodDeclarationSyntax WithMethodDeclarationInfo(CSharpSyntaxNode methodNode, string newMethodName, IEnumerable<TypeParameterSyntax> newTypeParams, IEnumerable<AttributeListSyntax> newAttrLists)
    {
        return ((MethodDeclarationSyntax)methodNode).WithIdentifier(ParseToken(newMethodName)).
            WithTypeParameterList(newTypeParams.Any() ? TypeParameterList(SeparatedList(newTypeParams)) : null).
            WithAttributeLists(List(newAttrLists));
    }
}

class DuckTypeTypeRewriter : DuckTypeRewriter
{
    public DuckTypeTypeRewriter(SemanticModel semanticModel) : base(semanticModel)
    {
    }

    static TypeDeclarationSyntax WithTypeDeclarationInfo(CSharpSyntaxNode methodNode, string newMethodName, IEnumerable<TypeParameterSyntax> newTypeParams, IEnumerable<AttributeListSyntax> newAttrLists)
    {
        return ((TypeDeclarationSyntax)methodNode).WithIdentifier(ParseToken(newMethodName)).
            WithTypeParameterList(newTypeParams.Any() ? TypeParameterList(SeparatedList(newTypeParams)) : null).
            WithAttributeLists(List(newAttrLists));
    }

    public override SyntaxNode VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
    {
        var typeSymbol = m_semanticModel.GetSymbolInfo(node.Type);
        if (typeSymbol.Symbol is INamedTypeSymbol namedTypeSymbol)
        {
            var attribute = namedTypeSymbol.GetAttribute("DuckTypeAttribute");
            if (attribute != null)
            {
                var typeDeclaration = typeSymbol.Symbol.DeclaringSyntaxReferences.First();
                if (typeDeclaration.GetSyntax() is TypeDeclarationSyntax typeDeclarationNode)
                {
                    var newName = Utilities.GenerateNewCode(attribute, namedTypeSymbol.Name, namedTypeSymbol.TypeParameters, namedTypeSymbol.TypeArguments, typeDeclarationNode.TypeParameterList, typeDeclarationNode.AttributeLists, WithTypeDeclarationInfo, typeDeclarationNode, NewNodes);
                    return node.WithType(ParseTypeName(newName));
                }
            }
        }
        return base.VisitObjectCreationExpression(node);
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
        syntaxTree = ExecuteDuckTypes(syntaxTree, new DuckTypeMethodRewriter(semanticModel));
        
        compilation = CompileTree(references, options, syntaxTree);
        semanticModel = compilation.GetSemanticModel(syntaxTree);
        syntaxTree = ExecuteDuckTypes(syntaxTree, new DuckTypeTypeRewriter(semanticModel));
        
        Console.WriteLine(syntaxTree);

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

    static SyntaxTree ExecuteDuckTypes(SyntaxTree syntaxTree, DuckTypeRewriter rewriter)
    {
        var root = syntaxTree.GetRoot();
        root = rewriter.Visit(root);

        var allDecls = root.DescendantNodes().Where(n => n is MethodDeclarationSyntax || n is TypeDeclarationSyntax);

        root = root.TrackNodes(allDecls);
        foreach (var decl in allDecls)
        {
            if (rewriter.NewNodes.TryGetValue(decl.ToString(), out var newDecls))
            {
                root = root.ReplaceNode(root.GetCurrentNode(decl), newDecls);
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
