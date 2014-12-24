//using System;
//using System.Collections;
//using System.Collections.Generic;
//using System.Linq;
//using System.Reflection;
//using System.Text;
//using Microsoft.CodeAnalysis;
//using Microsoft.CodeAnalysis.CSharp;
//using Microsoft.CodeAnalysis.CSharp.Syntax;
//using Exts;
//using CodeQuoter;

//namespace CodeQuoter
//{ 
///// <summary>
///// A tool that for a given C# program constructs Roslyn API calls to create a syntax tree that
///// describes this program. As opposed to SyntaxTree.ParseText() that creates the syntax tree object
///// graph in runtime, Quoter returns the C# source code that will construct such syntax tree object
///// graph when compiled and executed.
///// </summary>
///// <example>
///// new Quoter().Quote("class C{}") returns:
///// 
///// Syntax.CompilationUnit()
///// .WithMembers(
/////     Syntax.List&lt;MemberDeclarationSyntax&gt;
/////         Syntax.ClassDeclaration(
/////             Syntax.Identifier(
/////                 @"C"))
/////         .WithKeyword(
/////             Syntax.Token(
/////                 SyntaxKind.ClassKeyword,
/////                 Syntax.TriviaList(
/////                     Syntax.Space)))
/////         .WithOpenBraceToken(
/////             Syntax.Token(
/////                 SyntaxKind.OpenBraceToken))
/////         .WithCloseBraceToken(
/////             Syntax.Token(
/////                 SyntaxKind.CloseBraceToken))))
///// .WithEndOfFileToken(
/////     Syntax.Token(
/////         SyntaxKind.EndOfFileToken))
///// </example>
//public class CodeQuoter
//{
//    public bool OpenParenthesisOnNewLine      { get; private set; }
//    public bool ClosingParenthesisOnNewLine   { get; private set; }
//    public bool UseDefaultFormatting          { get; private set; }
//    public bool RemoveRedundantModifyingCalls { get; private set; }

//    public CodeQuoter (bool useDefaultFormatting = true, bool removeRedundantModifyingCalls = true )
//    {
//        UseDefaultFormatting          = useDefaultFormatting;
//        RemoveRedundantModifyingCalls = removeRedundantModifyingCalls;
//    }
//    /// <summary>
//    /// Given the input C# program <paramref name="sourceText"/> returns the C# source code of
//    /// Roslyn API calls that recreate the syntax tree for the input program.
//    /// </summary>
//    /// <param name="sourceText">A C# program (one compilation unit)</param>
//    /// <returns>A C# expression that describes calls to the Roslyn syntax API necessary to recreate
//    /// the syntax tree for the source program.</returns>
//    public string Quote ( string sourceText )
//    {
//        var sourceTree = CSharpSyntaxTree.ParseText(sourceText);
//        return Quote( sourceTree.GetRoot( ) );
//    }



//}