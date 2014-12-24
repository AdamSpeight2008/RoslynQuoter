Imports System.Reflection
Imports CodeQuoter
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic

Public Class VBQuoter
  Inherits CodeQuoter.BaseQuoter

  Dim _VBPrinter As New VBPrinter

  Public Sub New(Optional useDefaultFormatting As Boolean = True, Optional removeRedundantModifyingCalls As Boolean = True)
    MyBase.New(useDefaultFormatting, removeRedundantModifyingCalls)
  End Sub

  Public Overrides Function Quote(sourceText As String) As String
    Dim sourceTree = VisualBasicSyntaxTree.ParseText(sourceText)
    Return Quote(sourceTree.GetRoot())
  End Function

  Overloads Function Quote(node As SyntaxNode) As String
    Dim rootApiCall As ApiCall = Quote(node, name:    Nothing)
    If UseDefaultFormatting Then rootApiCall.Add(New MethodCall(".NormalizeWhitespace"))
    Return _VBPrinter.Print(rootApiCall)
  End Function

  ''' <summary>
  ''' Recursive method that "quotes" a SyntaxNode, SyntaxToken, SyntaxTrivia Or other objects.
  ''' </summary>
  ''' <returns>A description of Roslyn API calls necessary to recreate the input object.</returns>
  Private Overloads Function Quote(treeElement As Object, Optional name As String = Nothing) As ApiCall
    If (TypeOf treeElement Is SyntaxTrivia) Then Return QuoteTrivia(DirectCast(treeElement, SyntaxTrivia))
    If (TypeOf treeElement Is SyntaxToken) Then Return QuoteToken(DirectCast(treeElement, SyntaxToken), name)
    If (TypeOf treeElement Is SyntaxNodeOrToken) Then
      Dim syntaxNodeOrToken = DirectCast(treeElement, SyntaxNodeOrToken)
      Return If(syntaxNodeOrToken.IsNode, QuoteNode(syntaxNodeOrToken.AsNode(), name), QuoteToken(syntaxNodeOrToken.AsToken(), name))
    End If
    Return QuoteNode(DirectCast(treeElement, SyntaxNode), name)
  End Function


  ''' <summary>
  ''' The main recursive method that given a SyntaxNode recursively quotes the entire subtree.
  ''' </summary>
  Private Function QuoteNode(node As SyntaxNode, name As String) As ApiCall
    Dim quotedPropertyValues As APIList = QuotePropertyValues(node)
    Dim factoryMethod As MethodInfo = PickFactoryMethodToCreateNode(node)
    Dim factoryMethodCall = New MethodCall(factoryMethod.DeclaringType.Name & "." & factoryMethod.Name)
    Dim codeBlock = New ApiCall(name, factoryMethodCall)
    APIList.AddFactoryMethodArguments(factoryMethod, factoryMethodCall, quotedPropertyValues)
    ApiCall.AddModifyingCalls(node, codeBlock, quotedPropertyValues)
    Return codeBlock
  End Function


  ''' <summary>
  ''' Inspects the property values of the <paramref name="node"/> object using Reflection And
  ''' creates API call descriptions for the property values recursively. Properties that are Not
  ''' essential to the shape of the syntax tree (such as Span) are ignored.
  ''' </summary>
  Private Function QuotePropertyValues(node As SyntaxNode) As APIList
    Dim result = APIList.Create()
    Dim properties = node.GetType().GetProperties(BindingFlags.Public Or BindingFlags.Instance)
    ' Filter out non-essential properties listed in nonStructuralProperties
    result.Add(properties.Where(Function(propertyInfo) Not nonStructuralProperties.Contains(propertyInfo.Name)).Select(Function(propertyInfo) QuotePropertyValue(node, propertyInfo)).Where(Function(apiCall) apiCall IsNot Nothing).ToArray())

    ' HACK: factory methods For the following node types accept back the first "kind" parameter
    ' that we filter out above. Add an artificial "property value" that can be later used to
    ' satisfy the first parameter of type SyntaxKind.
    If TypeOf node Is  AccessorDeclarationSyntax OrElse
       TypeOf node Is Syntax.BinaryExpressionSyntax OrElse
       TypeOf node Is ClassOrStructConstraintSyntax OrElse
       TypeOf node Is CheckedExpressionSyntax OrElse
       TypeOf node Is CheckedStatementSyntax OrElse
       TypeOf node Is ConstructorInitializerSyntax OrElse
       TypeOf node Is Syntax.GotoStatementSyntax OrElse
       TypeOf node Is InitializerExpressionSyntax OrElse
       TypeOf node Is Syntax.LiteralExpressionSyntax OrElse
       TypeOf node Is Syntax.MemberAccessExpressionSyntax OrElse
       TypeOf node Is Syntax.OrderingSyntax OrElse
       TypeOf node Is PostfixUnaryExpressionSyntax OrElse
       TypeOf node Is Syntax.UnaryExpressionSyntax OrElse
       TypeOf node Is Syntax.DocumentationCommentTriviaSyntax OrElse
       TypeOf node Is Syntax.CaseClauseSyntax  OrElse
       TypeOf node Is Syntax.YieldStatementSyntax Then

      result.Add(New ApiCall("Kind", "SyntaxKind." + node.VBKind.ToString))
            End If

    Return result
  End Function
  
        ' <summary>
        ' Quote the value of the property <paramref name="property"/> of object <paramref
        ' name="node"/>
        ' </summary>
        Private Function QuotePropertyValue ( node As SyntaxNode,  [Property] As PropertyInfo) As ApiCall
        
            Dim value = [Property] .GetValue(node, nothing)
            Dim pType = [Property] .PropertyType
            If pType = GetType(SyntaxToken) Then Return QuoteToken( DirectCast(value,SyntaxToken), [Property] .Name )
            If pType = GetType(SyntaxTokenList) Then Return QuoteList( DirectCast(value, IEnumerable), [Property].Name )
    If (pType.IsGenericType AndAlso
                  (pType.GetGenericTypeDefinition = GetType(SyntaxList(Of )) OrElse
                  (pType.GetGenericTypeDefinition = GetType(SeparatedSyntaxList(Of )) Then
                Return QuoteList(DirectCast(value, IEnumerable),  [Property].Name )
      End If
            If TypeOf value Is SyntaxNode Then Return QuoteNode( DirectCast(value,SyntaxNode), [Property].Name )
            If TypeOf value Is String     Then Return New ApiCall(  [Property].Name,"")'TODO: "\"" + Escape( value.ToString( ) ) + "\"" )
            If TypeOF value Is Boolean    Then Return New ApiCall(  [Property].Name, value.ToString( ).ToLowerInvariant( ) )
            Return Nothing 
        End Function


  Function QuoteList (  syntaxList As IEnumerable, name As String ) As ApiCall
    Dim sourceList As IEnumerable(Of object) = syntaxList.Cast(Of object)
    Dim  mName = "SyntaxFactory.List"
    Dim  listType As String = Nothing
    Dim  pType = SyntaxList.GetType()
    If (pType.IsGenericType) Then
      Dim mType = pType.GetGenericArguments(0).Name
      listType = mType
      If (pType.GetGenericTypeDefinition = GetType(SeparatedSyntaxList(Of )) ) Then
                    listType = "SyntaxNodeOrToken"
                    mName = "SyntaxFactory.SeparatedList"
                    sourceList = DirectCast(syntaxList.GetType().GetMethod("GetWithSeparators").Invoke(syntaxList, Nothing),SyntaxNodeOrTokenList) ).Cast(Of Object).ToArray
                End If
                mName &= "(Of " & mType + ")"
            End If
            If (pType.Name = "SyntaxTokenList") Then mName = "SyntaxFactory.TokenList"
            If (pType.Name = "SyntaxTriviaList") Then mName = "SyntaxFactory.TriviaList"
            Dim elements = ArgList.Create(sourceList.Select(Function(o) Quote(o)).Where(Function(cb) cb IsNot Nothing).ToArray)
            If (elements.Count = 0) Then Return Nothing
            If (elements.Count = 1) Then
                mName = mName.Replace( ".List", ".SingletonList" ).Replace( ".SeparatedList", ".SingletonSeparatedList" )
            Else 
 elements = ArgList.Create( New ApiCall( "methodName", "new " + listType + "[]", elements, useCurliesInsteadOfParentheses: true ) ) 
 End If
    Dim codeBlock = New ApiCall(name, mName, elements)
    Return codeBlock
        End Function

End Class

Public Class VBPrinter
  Inherits CodeQuoter.CodePrint

  Public Overrides Function Print(node As MemberCall) As String
    Throw New NotImplementedException()
  End Function

  Public Overrides Function Print(node As ApiCall) As String
    Throw New NotImplementedException()
  End Function

  Public Overrides Function PrintWithDefaultFormatting(node As ApiCall) As String
    Throw New NotImplementedException()
  End Function
End Class
