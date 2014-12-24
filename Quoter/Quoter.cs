using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Exts;

/// <summary>
/// A tool that for a given C# program constructs Roslyn API calls to create a syntax tree that
/// describes this program. As opposed to SyntaxTree.ParseText() that creates the syntax tree object
/// graph in runtime, Quoter returns the C# source code that will construct such syntax tree object
/// graph when compiled and executed.
/// </summary>
/// <example>
/// new Quoter().Quote("class C{}") returns:
/// 
/// Syntax.CompilationUnit()
/// .WithMembers(
///     Syntax.List&lt;MemberDeclarationSyntax&gt;
///         Syntax.ClassDeclaration(
///             Syntax.Identifier(
///                 @"C"))
///         .WithKeyword(
///             Syntax.Token(
///                 SyntaxKind.ClassKeyword,
///                 Syntax.TriviaList(
///                     Syntax.Space)))
///         .WithOpenBraceToken(
///             Syntax.Token(
///                 SyntaxKind.OpenBraceToken))
///         .WithCloseBraceToken(
///             Syntax.Token(
///                 SyntaxKind.CloseBraceToken))))
/// .WithEndOfFileToken(
///     Syntax.Token(
///         SyntaxKind.EndOfFileToken))
/// </example>
public class Quoter
{
    public bool OpenParenthesisOnNewLine      { get; private set; }
    public bool ClosingParenthesisOnNewLine   { get; private set; }
    public bool UseDefaultFormatting          { get; private set; }
    public bool RemoveRedundantModifyingCalls { get; private set; }

    public Quoter (bool useDefaultFormatting = true, bool removeRedundantModifyingCalls = true )
    {
        UseDefaultFormatting          = useDefaultFormatting;
        RemoveRedundantModifyingCalls = removeRedundantModifyingCalls;
    }
    /// <summary>
    /// Given the input C# program <paramref name="sourceText"/> returns the C# source code of
    /// Roslyn API calls that recreate the syntax tree for the input program.
    /// </summary>
    /// <param name="sourceText">A C# program (one compilation unit)</param>
    /// <returns>A C# expression that describes calls to the Roslyn syntax API necessary to recreate
    /// the syntax tree for the source program.</returns>
    public string Quote ( string sourceText )
    {
        var sourceTree = CSharpSyntaxTree.ParseText(sourceText);
        return Quote( sourceTree.GetRoot( ) );
    }

    /// <summary>
    /// Given the input C# syntax node <paramref name="node"/> returns the C# source code of
    /// Roslyn API calls that recreate the syntax node.
    /// </summary>
    /// <param name="sourceText">A C# syntax node</param>
    /// <returns>A C# expression that describes calls to the Roslyn syntax API necessary to recreate
    /// the input syntax node.</returns>
    public string Quote ( SyntaxNode node )
    {
        ApiCall rootApiCall = Quote(node, name: null);
        if ( UseDefaultFormatting )  rootApiCall.Add( new MethodCall( ".NormalizeWhitespace") );
        return Print(rootApiCall);
    }


    /// <summary>
    /// Recursive method that "quotes" a SyntaxNode, SyntaxToken, SyntaxTrivia or other objects.
    /// </summary>
    /// <returns>A description of Roslyn API calls necessary to recreate the input object.</returns>
    private ApiCall Quote ( object treeElement, string name = null )
    {
        if ( treeElement is SyntaxTrivia )         return QuoteTrivia( (SyntaxTrivia)treeElement );
        if ( treeElement is SyntaxToken  )         return QuoteToken( (SyntaxToken)treeElement, name );
        if ( treeElement is SyntaxNodeOrToken )
        {
            SyntaxNodeOrToken syntaxNodeOrToken = (SyntaxNodeOrToken)treeElement;
            return syntaxNodeOrToken.IsNode ? QuoteNode( syntaxNodeOrToken.AsNode( ), name ) : QuoteToken( syntaxNodeOrToken.AsToken( ), name );
        }
        return QuoteNode( (SyntaxNode)treeElement, name );
    }

    /// <summary>
    /// The main recursive method that given a SyntaxNode recursively quotes the entire subtree.
    /// </summary>
    private ApiCall QuoteNode ( SyntaxNode node, string name )
    {
        APIList quotedPropertyValues = QuotePropertyValues(node);
        MethodInfo           factoryMethod = PickFactoryMethodToCreateNode(node);
        var factoryMethodCall = new MethodCall(factoryMethod.DeclaringType.Name + "." + factoryMethod.Name);
        var         codeBlock = new ApiCall(name, factoryMethodCall);
        APIList.AddFactoryMethodArguments( factoryMethod, factoryMethodCall, quotedPropertyValues );
        ApiCall.AddModifyingCalls( node, codeBlock, quotedPropertyValues );
        return codeBlock;
    }

    /// <summary>
    /// Inspects the property values of the <paramref name="node"/> object using Reflection and
    /// creates API call descriptions for the property values recursively. Properties that are not
    /// essential to the shape of the syntax tree (such as Span) are ignored.
    /// </summary>
    private APIList QuotePropertyValues ( SyntaxNode node )
    {
        var result =  APIList.Create();
        var properties = node.GetType().GetProperties(BindingFlags.Public | BindingFlags.Instance);

        // Filter out non-essential properties listed in nonStructuralProperties
        result.Add( properties.Where( propertyInfo => !nonStructuralProperties.Contains( propertyInfo.Name ) )
                                   .Select( propertyInfo => QuotePropertyValue( node, propertyInfo ) )
                                   .Where( apiCall => apiCall != null ).ToArray() );

        // HACK: factory methods for the following node types accept back the first "kind" parameter
        // that we filter out above. Add an artificial "property value" that can be later used to
        // satisfy the first parameter of type SyntaxKind.
        if ( node is AccessorDeclarationSyntax      ||  node is BinaryExpressionSyntax            ||
             node is ClassOrStructConstraintSyntax  ||  node is CheckedExpressionSyntax           ||
             node is CheckedStatementSyntax         ||  node is ConstructorInitializerSyntax      ||
             node is GotoStatementSyntax            ||  node is InitializerExpressionSyntax       ||
             node is LiteralExpressionSyntax        ||  node is MemberAccessExpressionSyntax      ||
             node is OrderingSyntax                 ||  node is PostfixUnaryExpressionSyntax      ||
             node is PrefixUnaryExpressionSyntax    ||  node is DocumentationCommentTriviaSyntax  ||
             node is SwitchLabelSyntax              ||  node is YieldStatementSyntax )
        {
            result.Add( new ApiCall( "Kind", "SyntaxKind." + node.CSharpKind( ).ToString( ) ) );
        }

        return result;
    }

    /// <summary>
    /// Quote the value of the property <paramref name="property"/> of object <paramref
    /// name="node"/>
    /// </summary>
    private ApiCall QuotePropertyValue ( SyntaxNode node, PropertyInfo property )
    {
        var value = property.GetValue(node, null);
        var pType = property.PropertyType;
        if ( pType == typeof(SyntaxToken) )      return QuoteToken( (SyntaxToken)value, property.Name );
        if ( pType == typeof(SyntaxTokenList) )  return QuoteList( (IEnumerable)value, property.Name );
        if ( pType.IsGenericType &&
              ( pType.GetGenericTypeDefinition() == typeof(SyntaxList<>) ||
                pType.GetGenericTypeDefinition() == typeof(SeparatedSyntaxList<>) ) )   return QuoteList( (IEnumerable)value, property.Name );
        if ( value is SyntaxNode )   return QuoteNode( (SyntaxNode)value, property.Name );
        if ( value is string )       return new ApiCall( property.Name, "\"" + Escape( value.ToString( ) ) + "\"" );
        if ( value is bool )         return new ApiCall( property.Name, value.ToString( ).ToLowerInvariant( ) );
        return null;
    }

    private ApiCall QuoteList ( IEnumerable syntaxList, string name )
    {
        IEnumerable<object> sourceList = syntaxList.Cast<object>();
        string    mName = "SyntaxFactory.List";
        string listType = null;
        var pType = syntaxList.GetType();
        if ( pType.IsGenericType )
        {
            var mType = pType.GetGenericArguments()[0].Name;
            listType = mType;

            if ( pType.GetGenericTypeDefinition() == typeof(SeparatedSyntaxList<>) )
            {
                listType   = "SyntaxNodeOrToken";
                mName      = "SyntaxFactory.SeparatedList";
                sourceList = ( (SyntaxNodeOrTokenList)
                syntaxList.GetType( ).GetMethod( "GetWithSeparators" ).Invoke(syntaxList,null)) .Cast<object>().ToArray( );
            }
            mName += "<" + mType + ">";
        }
        if ( pType.Name == "SyntaxTokenList"  )  mName = "SyntaxFactory.TokenList";
        if ( pType.Name == "SyntaxTriviaList" )  mName = "SyntaxFactory.TriviaList";
        var elements = ArgList.Create( sourceList.Select(o => Quote(o)).Where(cb => cb != null).ToArray());
        if ( elements.Count == 0 ) return null;
        if ( elements.Count == 1 ) { mName = mName.Replace( ".List", ".SingletonList" ).Replace( ".SeparatedList", ".SingletonSeparatedList" ); }
        else                       { elements = ArgList.Create( new ApiCall("methodName", "new " + listType + "[]",  elements, useCurliesInsteadOfParentheses: true ) ); }
        var codeBlock = new ApiCall(name, mName, elements);
        return codeBlock;
    }

    private ApiCall QuoteToken ( SyntaxToken value, string name )
    {
        if ( value == default(SyntaxToken) || value.IsKind(SyntaxKind.None) )  {  return null;  }
        var      args = ArgList.Create();
        string  mName = "SyntaxFactory.Token";
        string escapedTokenValueText = "@\"" + Escape(value.ToString()) + "\"";
        object   head = GetLeadingTrivia (value);
        object   tail = GetTrailingTrivia(value);
        object actualValue;
        if ( head != null || tail != null )  { head = head ?? GetEmptyTrivia( "LeadingTrivia"  ); tail = tail ?? GetEmptyTrivia( "TrailingTrivia" );  }
        if ( value.IsKind( SyntaxKind.IdentifierToken ) )
        {
            mName = "SyntaxFactory.Identifier";
            if ( value.IsMissing )  {  mName = "SyntaxFactory.MissingToken"; }
            if ( value.IsMissing )  {  actualValue = value.CSharpKind( );    }
            else                    {  actualValue = escapedTokenValueText;  }
            args.AddIfNotNull(  head );
            args.Add( actualValue );
            args.AddIfNotNull( tail );
        }
        else if ( value.IsKindAny(SyntaxKind.XmlTextLiteralToken,
                                  SyntaxKind.XmlTextLiteralNewLineToken,
                                  SyntaxKind.XmlEntityLiteralToken) )
        {
            mName = "SyntaxFactory.XmlTextLiteral";
            if ( value.IsKind(SyntaxKind.XmlTextLiteralNewLineToken) )   {  mName = "SyntaxFactory.XmlTextNewLine"; }
            else if ( value.IsKind( SyntaxKind.XmlEntityLiteralToken) )  {  mName = "SyntaxFactory.XmlEntity";      }
            args.Add( head ?? GetEmptyTrivia( "LeadingTrivia" ), escapedTokenValueText, escapedTokenValueText, tail ?? GetEmptyTrivia( "TrailingTrivia" ) );
        }
        else if ( ( value.Parent is LiteralExpressionSyntax ||
            value.IsKindAny(SyntaxKind.StringLiteralToken,SyntaxKind.NumericLiteralToken )) &&
            value.IsKindNoneOf( SyntaxKind.TrueKeyword , SyntaxKind.FalseKeyword ,
                                SyntaxKind.NullKeyword , SyntaxKind.ArgListKeyword) )
        {
            mName = "SyntaxFactory.Literal";
            args.Add( head ?? GetEmptyTrivia( "LeadingTrivia" ) , escapedTokenValueText );
            string escapedValue = value.ToString();
            if ( value.IsKind(SyntaxKind.StringLiteralToken ))  escapedValue = escapedTokenValueText;
            args.Add( escapedValue , tail ?? GetEmptyTrivia( "TrailingTrivia" ) );
        }
        else
        {
            if ( value.IsMissing )  {  mName = "SyntaxFactory.MissingToken"; }
            if ( value.IsKind(SyntaxKind.BadToken ))
            {
                mName = "SyntaxFactory.BadToken";
                 head = head ?? GetEmptyTrivia( "LeadingTrivia" );
                 tail = tail ?? GetEmptyTrivia( "TrailingTrivia" );
            }
            object tokenValue = value.CSharpKind();
            if ( value.IsKind(SyntaxKind.BadToken) )  {  tokenValue = escapedTokenValueText;  }
            args.AddIfNotNull( head );
            args.Add( tokenValue );
            args.AddIfNotNull( tail );
        }
        return new ApiCall( name, mName, args );
    }

    private ApiCall QuoteTrivia ( SyntaxTrivia syntaxTrivia )
    {
        string factoryMethodName = "SyntaxFactory.Trivia";
        string text = syntaxTrivia.ToString();
        if ( syntaxTrivia.FullSpan.Length == 0 || ( syntaxTrivia.IsKind(SyntaxKind.WhitespaceTrivia) && UseDefaultFormatting ) )   return null;

        FieldInfo field = null;
        if ( triviaFactoryFields.TryGetValue( syntaxTrivia.ToString( ), out field ) &&
            ( (SyntaxTrivia)field.GetValue( null ) ).CSharpKind( ) == syntaxTrivia.CSharpKind( ) )
        {
            return UseDefaultFormatting ? null : new ApiCall( null, "SyntaxFactory." + field.Name );
        }

        if ( !string.IsNullOrEmpty( text )  &&  string.IsNullOrWhiteSpace( text )  &&  syntaxTrivia.IsKind(SyntaxKind.WhitespaceTrivia) )
        {
            if ( UseDefaultFormatting )   return null;        
            factoryMethodName = "SyntaxFactory.Whitespace";
        }

        if ( syntaxTrivia.IsTriviaAny( SyntaxKind.SingleLineCommentTrivia ,
                                       SyntaxKind.MultiLineCommentTrivia) )        { factoryMethodName = "SyntaxFactory.Comment"; }
        if ( syntaxTrivia.IsKind( SyntaxKind.PreprocessingMessageTrivia ))         { factoryMethodName = "SyntaxFactory.PreprocessingMessage"; }
        if ( syntaxTrivia.IsKind( SyntaxKind.DisabledTextTrivia ))                 { factoryMethodName = "SyntaxFactory.DisabledText"; }
        if ( syntaxTrivia.IsKind( SyntaxKind.DocumentationCommentExteriorTrivia )) { factoryMethodName = "SyntaxFactory.DocumentationCommentExterior"; }

        object argument = "@\"" + Escape(syntaxTrivia.ToString()) + "\"";

        if ( syntaxTrivia.HasStructure )  { argument = QuoteNode( syntaxTrivia.GetStructure( ), "Structure" );  }

        return new ApiCall( null, factoryMethodName, ArgList.Create( argument ) );
    }



    private object GetLeadingTrivia  ( SyntaxToken value )  { return !value.HasLeadingTrivia  ? null : QuoteList(value.LeadingTrivia,  "LeadingTrivia");  }
    private object GetTrailingTrivia ( SyntaxToken value )  { return !value.HasTrailingTrivia ? null : QuoteList(value.TrailingTrivia, "TrailingTrivia"); }
    private object GetEmptyTrivia ( string parentPropertyName )  { return new ApiCall( parentPropertyName, "SyntaxFactory.TriviaList", arguments: null ); }

    /// <summary>
    /// Escapes strings to be included within "" using C# escaping rules
    /// </summary>
    private string Escape ( string text, bool escapeVerbatim = true )
    {
        StringBuilder sb = new StringBuilder();
        for ( int i = 0; i < text.Length; i++ )
        {
            string toAppend = text[i].ToString();
            if ( text [ i ] == '"' )                           { toAppend = escapeVerbatim ? "\"\"" : toAppend = "\\\"";  }
            else if ( text [ i ] == '\\' && !escapeVerbatim )  { toAppend = "\\\\";  }
            sb.Append( toAppend );
        }
        return sb.ToString( );
    }



    /// <summary>
    /// Static methods on Roslyn.Compilers.CSharp.Syntax class that construct SyntaxNodes
    /// </summary>
    /// <example>Syntax.ClassDeclaration()</example>
    private static readonly Dictionary<string, List<MethodInfo>> factoryMethods = GetFactoryMethods();

    /// <summary>
    /// Five public fields on Roslyn.Compilers.CSharp.Syntax that return trivia: CarriageReturn,
    /// LineFeed, CarriageReturnLineFeed, Space and Tab.
    /// </summary>
    private static readonly Dictionary<string, FieldInfo> triviaFactoryFields = GetTriviaFactoryFields();

    /// <summary>
    /// Gets the five fields on Syntax that return ready-made trivia: CarriageReturn,
    /// CarriageReturnLineFeed, LineFeed, Space and Tab.
    /// </summary>
    private static Dictionary<string, FieldInfo> GetTriviaFactoryFields ( )
    {
       return typeof(SyntaxFactory).GetFields(BindingFlags.Public | BindingFlags.Static)
                                   .Where(fieldInfo => fieldInfo.FieldType == typeof(SyntaxTrivia))
                                   .Where(fieldInfo => !fieldInfo.Name.Contains("Elastic"))
                                   .ToDictionary(fieldInfo => ((SyntaxTrivia)fieldInfo.GetValue(null)).ToString());
    }

    /// <summary>
    /// Returns static methods on Roslyn.Compilers.CSharp.Syntax that return types derived from
    /// SyntaxNode and bucketizes them by overloads.
    /// </summary>
    private static Dictionary<string, List<MethodInfo>> GetFactoryMethods ( )
    {
        var result        = new Dictionary<string, List<MethodInfo>>();
        var staticMethods = typeof(SyntaxFactory).GetMethods( BindingFlags.Public | BindingFlags.Static);
        foreach ( var method in staticMethods )
        {
            var returnTypeName = method.ReturnType.Name;
            List<MethodInfo> bucket = null;
            if ( !result.TryGetValue( returnTypeName, out bucket ) )  { bucket = new List<MethodInfo>();  result.Add( returnTypeName, bucket ); }
            bucket.Add( method );
        }
        return result;
    }

    /// <summary>
    /// Uses Reflection to inspect static factory methods on the Roslyn.Compilers.CSharp.Syntax
    /// class and pick an overload that creates a node of the same type as the input <paramref
    /// name="node"/>
    /// </summary>
    private MethodInfo PickFactoryMethodToCreateNode ( SyntaxNode node )
    {
        string  name = node.GetType().Name;
        List<MethodInfo> candidates = null;
        if ( !factoryMethods.TryGetValue( name, out candidates ) )  { throw new NotSupportedException( name + " is not supported" ); }
        int minParameterCount = candidates.Min(m => m.GetParameters().Length);
        // HACK: for LiteralExpression pick the overload with two parameters - the overload with one
        // parameter only allows true/false/null literals
        if ( node is LiteralExpressionSyntax )
        {
            var les = ((LiteralExpressionSyntax)node);
            var res = les.IsKindNoneOf( SyntaxKind.TrueLiteralExpression, SyntaxKind.FalseLiteralExpression, SyntaxKind.NullLiteralExpression );
            if (res) {  minParameterCount = 2;  }
        }
        MethodInfo factory = null;
        if ( ( node is BaseTypeDeclarationSyntax || node is IdentifierNameSyntax ) )
        {
            Type desiredParameterType = typeof(string);
            factory = candidates.FirstOrDefault( m => m.GetParameters( ) [ 0 ].ParameterType == desiredParameterType );
            if ( factory != null )  return factory;
        }
        return candidates.First( m => m.GetParameters( ).Length == minParameterCount );
    }


    /// <summary>
    /// Flattens a tree of ApiCalls into a single string.
    /// </summary>
    private string Print ( ApiCall root )  { return  Print( root, new StringBuilder(), 0, OpenParenthesisOnNewLine, ClosingParenthesisOnNewLine ).ToString(); }

    private static string PrintWithDefaultFormatting ( ApiCall root )  {  return  Print( root, new StringBuilder(), 0, openNewLine: false, closeNewLine: false ).ToString();  }

    private static StringBuilder Print ( ApiCall codeBlock, StringBuilder sb, int depth = 0,  bool openNewLine = false,  bool closeNewLine = false )
    {
        Print( codeBlock.FactoryMethodCall, sb, depth, useCurly: codeBlock.useCurly, openNewLine: openNewLine, closeNewLine: closeNewLine );
        if ( codeBlock.InstanceMethodCalls == null ) return sb;
        foreach ( var call in codeBlock.InstanceMethodCalls )
        {
          Print( call, PrintNewLine( sb ), depth, useCurly: codeBlock.useCurly, openNewLine: openNewLine, closeNewLine: closeNewLine );
        }
        return sb;
    }

    private static StringBuilder Print ( MemberCall call, StringBuilder sb, int depth, bool  openNewLine = false, bool closeNewLine = false, bool useCurly = false )
    {
        var openParen  = useCurly ? "{" : "(";
        var closeParen = useCurly ? "}" : ")";
        Print( call.Name, sb, depth );
        MethodCall methodCall = call as MethodCall;
        if ( methodCall == null ) return sb;
        if ( methodCall.Arguments == null || !methodCall.Arguments.Any( ) )   {  Print( openParen + closeParen, sb, 0 );  return sb;  }
        sb =  openNewLine ?  Print( openParen,  PrintNewLine( sb ), depth ) :  Print( openParen, sb, 0 );
        PrintNewLine( sb );
        bool needComma = false;
        foreach ( var block in methodCall.Arguments )
        {
            if ( needComma )                 {  Print( ",", sb, 0 );  PrintNewLine( sb );  }
            if ( block is string )           {  Print( (string)block, sb, depth + 1 );     }
            else if ( block is SyntaxKind )  {  Print( "SyntaxKind." + ( (SyntaxKind)block ).ToString( ), sb, depth + 1 );  }
            else if ( block is ApiCall )     {  Print(  block as ApiCall, sb, depth + 1, openNewLine: openNewLine, closeNewLine: closeNewLine );
                                             }
            needComma = true;
        }
        return closeNewLine ?  Print( closeParen, PrintNewLine( sb ), depth ) : Print( closeParen, sb, 0 );
    }

    private static StringBuilder PrintNewLine ( StringBuilder sb )                             { return sb.AppendLine(); }
    private static StringBuilder Print        ( string line, StringBuilder   sb, int indent )  { return  PrintIndent( sb, indent ).Append( line ); }
    private static StringBuilder PrintIndent  ( StringBuilder sb, int indent )                 { return sb.Append( new string( ' ', indent * 4 ) ); }
    private static string        ProperCase   ( string str )                                   { return char.ToUpperInvariant( str [ 0 ] ) + str.Substring( 1 );  }

    /// <summary>
    /// Enumerates names of properties on SyntaxNode, SyntaxToken and SyntaxTrivia classes that do
    /// not impact the shape of the syntax tree and are not essential to reconstructing the tree.
    /// </summary>
    private static readonly string[] nonStructuralProperties =
    {
        "AllowsAnyExpression",               "Arity",  "ContainsAnnotations", "ContainsDiagnostics",
         "ContainsDirectives", "ContainsSkippedText",   "DirectiveNameToken",            "FullSpan",
           "HasLeadingTrivia",   "HasTrailingTrivia",  "HasStructuredTrivia",        "HasStructure",
                    "IsConst",         "IsDirective",            "IsElastic",             "IsFixed",
                  "IsMissing",  "IsStructuredTrivia", "IsUnboundGenericName",               "IsVar",
                       "Kind",            "Language",               "Parent",        "ParentTrivia",
                  "PlainName",                "Span",           "SyntaxTree", 
    };

    /// <summary>
    /// "Stringly typed" representation of a C# property or method invocation expression, with a
    /// string for the property or method name and a list of similarly loosely typed argument
    /// expressions. Simply speaking, this is a tree of strings.
    /// </summary>
    /// <example>
    /// Data structure to represent code (API calls) of simple hierarchical shape such as:
    /// A.B(C, D.E(F(G, H), I))
    /// </example>
    private class ApiCall
    {
        public string                          Name  { get; private set; }
        public MemberCall         FactoryMethodCall  { get; private set; }
        public List<MethodCall> InstanceMethodCalls  { get; private set; }
        public bool  useCurly  { get; private set; }

        public ApiCall ( string parentPropertyName ) { Name = parentPropertyName; }

        public ApiCall ( string parentPropertyName, string factoryMethodName )  : this(parentPropertyName)
          { FactoryMethodCall = new MemberCall(factoryMethodName); }

        public ApiCall ( string parentPropertyName,
                         string  factoryMethodName,
                        ArgList          arguments,
                           bool useCurliesInsteadOfParentheses = false )  : this(parentPropertyName, new MethodCall( factoryMethodName, arguments ))
          {  useCurly = useCurliesInsteadOfParentheses; }

        public ApiCall ( string name, MethodCall factoryMethodCall ) : this(name)  {  FactoryMethodCall = factoryMethodCall; }

        public void Add    ( MethodCall methodCall )  { InstanceMethodCalls = InstanceMethodCalls ?? new List<MethodCall>( ); InstanceMethodCalls.Add   ( methodCall ); }
        public void Remove ( MethodCall methodCall )  { if( InstanceMethodCalls != null) InstanceMethodCalls.Remove( methodCall ); }
        public override string ToString () { return Quoter.PrintWithDefaultFormatting( this ); }

        public void AddModifyingCall ( MethodCall methodCall )
        {
            // TODO: this needs scripting
            ////if (RemoveRedundantModifyingCalls)
            ////{
            ////    var before = Evaluate(apiCall, UseDefaultFormatting);
            ////    apiCall.Add(methodCall);
            ////    var after = Evaluate(apiCall, UseDefaultFormatting);
            ////    if (before == after)
            ////    {
            ////        apiCall.Remove(methodCall);
            ////    }
            ////}
            this.Add( methodCall );
        }


        /// <summary>
        /// Adds information about subsequent modifying fluent interface style calls on an object (like
        /// foo.With(...).With(...))
        /// </summary>
        public static void AddModifyingCalls ( object treeElement, ApiCall apiCall, APIList values )
        {
            var methods = treeElement.GetType().GetMethods(BindingFlags.Public | BindingFlags.Instance);
            foreach ( var value in values )
            {
                var methodName = "With" + ProperCase(value.Name);
                if ( !methods.Any( m => m.Name == methodName ) ) throw new NotSupportedException( );
                methodName = "." + methodName;
                apiCall.AddModifyingCall( new MethodCall( methodName, ArgList.Create( value ) ) );
            }
        }
    }

    private class APIList : IEnumerable<ApiCall>
    {
        private List<ApiCall> APIs  = new List<ApiCall>( );
        public int Count { get { return APIs.Count; } }
        public object this [ int index ] { get { return APIs [ index ]; } }

        private APIList ( params ApiCall[] apis ) { APIs = new List<ApiCall>( apis ); }

        public static APIList Create ( params ApiCall[] apis ) { return new APIList( apis ); }

        public void Add ( params ApiCall [ ] apis ) { APIs.AddRange( apis ); }
        public void AddIfNotNull ( ApiCall api ) { if ( api != null ) { APIs.Add( api ); } }

        IEnumerator<ApiCall> IEnumerable<ApiCall>.GetEnumerator ( ) { return this.APIs.GetEnumerator( ); }
        IEnumerator IEnumerable.GetEnumerator ( ) { return this.APIs.GetEnumerator( ); }

        internal void Remove ( ApiCall quotedCodeBlock )
        {
            if( APIs != null) APIs.Remove( quotedCodeBlock );
        }

        /// <summary>
        /// Finds a value in a list using case-insensitive search
        /// </summary>
        private ApiCall FindValue ( string parameterName )
        {
            return APIs.FirstOrDefault( v => parameterName.Equals( v.Name, StringComparison.OrdinalIgnoreCase ) );
        }

        public static void AddFactoryMethodArguments ( MethodInfo factory, MethodCall factoryMethodCall, APIList quotedValues )
        {
            foreach ( var FMP in factory.GetParameters( ) )
            {
                var pName = FMP.Name;
                var pType = FMP.ParameterType;

                ApiCall quotedCodeBlock = quotedValues.FindValue(pName );

                // special case to prefer Syntax.IdentifierName("C") to 
                // Syntax.IdentifierName(Syntax.Identifier("C"))
                if ( pName == "name" && pType == typeof(string) )
                {
                    quotedCodeBlock = quotedValues.First( a => a.Name == "Identifier" );
                    var methodCall  = quotedCodeBlock.FactoryMethodCall as MethodCall;
                    if ( methodCall != null && methodCall.Name == "SyntaxFactory.Identifier" )
                    {
                        if ( methodCall.Arguments.Count == 1 ) { factoryMethodCall.Arguments.Add( methodCall.Arguments [ 0 ] ); }
                        else { factoryMethodCall.Arguments.Add( quotedCodeBlock ); }
                        quotedValues.Remove( quotedCodeBlock );
                        continue;
                    }
                }
                // special case to prefer Syntax.ClassDeclarationSyntax(string) instead of 
                // Syntax.ClassDeclarationSyntax(SyntaxToken)
                if ( pName == "identifier" && pType == typeof(string) )
                {
                    var methodCall = quotedCodeBlock.FactoryMethodCall as MethodCall;
                    if ( methodCall != null && methodCall.Name == "SyntaxFactory.Identifier" && methodCall.Arguments.Count == 1 )
                    {
                        factoryMethodCall.Arguments.Add( methodCall.Arguments [ 0 ] );
                        quotedValues.Remove( quotedCodeBlock );
                        continue;
                    }
                }

                if ( quotedCodeBlock != null ) { factoryMethodCall.Arguments.Add( quotedCodeBlock ); quotedValues.Remove( quotedCodeBlock ); }
                else if ( !FMP.IsOptional )
                {
                    throw new InvalidOperationException(
                        string.Format(
                            "Couldn't find value for parameter '{0}' of method '{1}'. Go to QuotePropertyValues and add your node type to the exception list.",
                            pName,
                            factory ) );
                }
            }
        }

    }

    /// <summary>
    /// Simple data structure to represent a member call, primarily just the string Name.
    /// </summary>
    private class MemberCall
    {
       public string Name { get; private set; }
       public MemberCall ( string factoryMethodName )  { Name = factoryMethodName; }
       public override string ToString ()  {  return Print( this, new StringBuilder(), 0 ).ToString();  }
    }

    /// <summary>
    /// Represents a method call that has a Name and an arbitrary list of Arguments.
    /// </summary>
    private class MethodCall : MemberCall
    {
        public ArgList Arguments { get; private set; } = ArgList.Create();

        public MethodCall ( string methodName ) : base( methodName ) { }
        public MethodCall ( string methodName, ArgList argList ) : this(methodName)  { Arguments  = argList; }
    }
}

public static class Exts
{
    public static bool IsKind       ( this SyntaxNode   CSNode, SyntaxKind CSKind ) { return CSNode.CSharpKind( ) == CSKind; }
    public static bool IsKindAny    ( this SyntaxNode   CSNode, params SyntaxKind [ ] CSKinds ) { return CSKinds.Any( csk => CSNode.CSharpKind() == csk ); }
    public static bool IsKindNoneOf ( this SyntaxNode   CSNode, params SyntaxKind [ ] CSKinds ) { return CSKinds.All( csk => CSNode.CSharpKind() != csk ); }
    public static bool IsTriviaAny  ( this SyntaxTrivia CSNode, params SyntaxKind [ ] CSKinds ) { return CSKinds.Any( csk => CSNode.IsKind( csk) ); }
    public static bool IsKindAny    ( this SyntaxToken  CSNode, params SyntaxKind [ ] CSKinds ) { return CSKinds.Any( csk => CSNode.CSharpKind() == csk ); }
    public static bool IsKindNoneOf ( this SyntaxToken  CSNode, params SyntaxKind [ ] CSKinds ) { return CSKinds.All( csk => CSNode.CSharpKind() != csk ); }

}

public class ArgList : IEnumerable<object>
{
    private List<object> Args  = new List<object>( );
    public  int         Count { get { return Args.Count; } }
    public object this [ int index ]  {  get { return Args [ index ];}   }

    private ArgList ( params object [ ] Arguments ) { Args = new List<object>( Arguments ); }

    public static ArgList Create(params object[] Arguments ) {  return new ArgList( Arguments ); }

    public void          Add (params object[] Arguments) { Args.AddRange( Arguments ); }
    public void AddIfNotNull (       object      value ) { if ( value != null ) { Args.Add( value ); }  }

    IEnumerator<object> IEnumerable<object>.GetEnumerator ( ) { return this.Args.GetEnumerator(); }
    IEnumerator                 IEnumerable.GetEnumerator ( ) { return this.Args.GetEnumerator(); }

}