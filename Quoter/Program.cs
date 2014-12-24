using System;
using Microsoft.CodeAnalysis.CSharp;
using CodeQuoter;

namespace QuoterHost
{
    class Program
    {
        static void Main(string[] args)
        {
            var sourceText = "class C{}";
            var sourceNode = CSharpSyntaxTree.ParseText(sourceText).GetRoot() as CSharpSyntaxNode;
            var quoter = new CodeQuoter.CodeQuoter( );

            var generatedCode = quoter.Quote(sourceNode);

            Console.WriteLine(generatedCode);
        }
    }
}
