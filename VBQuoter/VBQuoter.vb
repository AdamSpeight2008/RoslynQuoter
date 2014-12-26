Imports System.Reflection
Imports System.Text
Imports CodeQuoter
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

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
    Dim rootApiCall As ApiCall = Quote(node, name:=Nothing)
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
    Select Case node
      'Case typeblock As Syntax.TypeBlockSyntax : Quote_TypeBlock(typeblock)
      'Case statement As Syntax.StatementSyntax : Quote_Statement(statement)
      'Case expression As Syntax.ExpressionSyntax : Quote_Expression(expression)
      'Case 
      Case agg               As AggregationRangeVariableSyntax
      Case args              As ArgumentListSyntax
      Case arg As ArgumentSyntax : Quute_Argument(arg)
      Case arr               As ArrayRankSpecifierSyntax
      Case asClause          As AsClauseSyntax
      Case attrList          As AttributeListSyntax
      Case attr              As AttributeSyntax
      Case attrTarget        As AttributeTargetSyntax
      Case caseBlock         As CaseBlockSyntax
      Case caseClause        As CaseClauseSyntax
      Case catchBlock        As CatchBlockSyntax
      Case catheFilterClause As CatchFilterClauseSyntax
      Case collectionRange   As CollectionRangeVariableSyntax
      Case compUnit          As CompilationUnitSyntax
      Case constraint        As ConstraintSyntax
      Case crefRef           As CrefReferenceSyntax
      Case crefSigPart       As CrefSignaturePartSyntax
      Case crefSig           As CrefSignatureSyntax
      Case elseBlock         As ElseBlockSyntax
      Case elseIfBlock       As ElseIfBlockSyntax
      Case equlalValue       As EqualsValueSyntax
      Case exprRangeVar      As ExpressionRangeVariableSyntax
      Case expression        As ExpressionSyntax : Quote_Expression(expression)
      Case field             As FieldInitializerSyntax
      Case finallyBlockS     As FinallyBlockSyntax
      Case forStep           As ForStepClauseSyntax
      Case handlesClauseItem As HandlesClauseItemSyntax
      Case handlesClause     As HandlesClauseSyntax
      Case implementsClause  As ImplementsClauseSyntax
      Case importsAlias      As ImportAliasClauseSyntax
      Case importsClause     As ImportsClauseSyntax
      Case joinCondition     As JoinConditionSyntax
      Case modifiedID        As ModifiedIdentifierSyntax
      Case nameColon         As NameColonEqualsSyntax
      Case objInit           As ObjectCreationInitializerSyntax
      Case order             As OrderingSyntax
      Case paramList         As ParameterListSyntax
      Case param             As ParameterSyntax
      Case queryClause       As QueryClauseSyntax
      Case redimClause       As RedimClauseSyntax
      Case sinlgeLineElse    As SingleLineElseClauseSyntax
      Case statement         As StatementSyntax : Quote_Statement(statement)
      Case structuredTriva   As StructuredTriviaSyntax
      Case tArgList          As TypeArgumentListSyntax
      Case tParamConstraint  As TypeParameterConstraintClauseSyntax
      Case tParameterList    As TypeParameterListSyntax
      Case tParam            As TypeParameterSyntax
      Case vDeclare          As VariableDeclaratorSyntax
      Case vNameEquals       As VariableNameEqualsSyntax
      Case whileOrUntil      As WhileOrUntilClauseSyntax
      Case xmlDecOpt         As XmlDeclarationOptionSyntax
      Case xmlDec            As XmlDeclarationSyntax
      Case xmlPre            As XmlPrefixSyntax
      Case Else
        Throw New NotSupportedException(String.Format("SyntaxNode not supported: {0}", node))

    End Select
    'Dim quotedPropertyValues As APIList = QuotePropertyValues(node)
    'Dim factoryMethod As MethodInfo = PickFactoryMethodToCreateNode(node)
    'Dim factoryMethodCall = New MethodCall(factoryMethod.DeclaringType.Name & "." & factoryMethod.Name)
    'Dim codeBlock = New ApiCall(name, factoryMethodCall)
    'APIList.AddFactoryMethodArguments(factoryMethod, factoryMethodCall, quotedPropertyValues)
    'ApiCall.AddModifyingCalls(node, codeBlock, quotedPropertyValues)
    'Return codeBlock
  End Function


#Region "Expression"

  Private Sub Quote_Expression(expression As ExpressionSyntax)
    Select Case expression
      Case expr As AggregationSyntax
      Case expr As AwaitExpressionSyntax
      Case expr As BinaryConditionalExpressionSyntax
      Case expr As BinaryExpressionSyntax
      Case expr As CastExpressionSyntax : Qoute_CastExpression(expr)
      Case expr As CollectionInitializerSyntax
      Case expr As ConditionalAccessExpressionSyntax
      Case expr As EventContainerSyntax
      Case expr As GetTypeExpressionSyntax
      Case expr As GetXmlNamespaceExpressionSyntax
      Case expr As InstanceExpressionSyntax : Qoute_InstanceExpression(expr)
      Case expr As InvocationExpressionSyntax
      Case expr As LabelSyntax
      Case expr As LambdaExpressionSyntax : Qoute_LambdaExpression(expr)
      Case expr As LiteralExpressionSyntax
      Case expr As MemberAccessExpressionSyntax
      Case expr As MidExpressionSyntax
        '  Case expr As NameOfExpressionSyntax
      Case expr As NewExpressionSyntax : Quote_NewExpression(expr)
      Case expr As ParenthesizedExpressionSyntax
      Case expr As PredefinedCastExpressionSyntax
      Case expr As QueryExpressionSyntax
      Case expr As TernaryConditionalExpressionSyntax
      Case expr As TypeOfExpressionSyntax
      Case expr As TypeSyntax
      Case expr As UnaryExpressionSyntax
      Case expr As XmlMemberAccessExpressionSyntax
      Case expr As XmlNodeSyntax
      Case Else
        Throw New NotSupportedException(String.Format("Expression not supported: {0}", expression))
    End Select
  End Sub

  Private Sub Quute_Argument(arg As ArgumentSyntax)
    Select Case arg
      Case expr As OmittedArgumentSyntax
      Case expr As RangeArgumentSyntax
      Case expr As SimpleArgumentSyntax
      Case Else
        Throw New NotSupportedException(String.Format("Expression not supported: {0}", expression))

    End Select
  End Sub

#Region "Expression Quotes"
  Private Sub Qoute_LambdaExpression(expr As LambdaExpressionSyntax)
    Throw New NotImplementedException()
  End Sub

  Private Sub Qoute_InstanceExpression(expr As InstanceExpressionSyntax)
    Throw New NotImplementedException()
  End Sub

  Private Sub Qoute_CastExpression(expr As CastExpressionSyntax)
    Throw New NotImplementedException()
  End Sub

  Private Sub Quote_NewExpression(expr As NewExpressionSyntax)
    Throw New NotImplementedException()
  End Sub
#End Region

#End Region


#Region "Statements"

  Private Sub Quote_Statement(statement As StatementSyntax)
    Select Case statement
'      Case expr As TypeStatementSyntax : Qoute_TypeStatement(expr)
'      Case expr As InheritsOrImplementsStatementSyntax : Qoute_InheritsOrImplementsStatement(expr)
      Case expr As CaseStatementSyntax
      Case expr As CatchStatementSyntax
      Case expr As DeclarationStatementSyntax : Qoute_DeclarationStatement(expr)
      Case expr As DoStatementSyntax
      Case expr As ElseIfStatementSyntax
      Case expr As ElseStatementSyntax
      Case expr As EmptyStatementSyntax
      Case expr As ExecutableStatementSyntax : Qoute_ExecutableStatement(expr)
      Case expr As FinallyStatementSyntax
      Case expr As ForOrForEachStatementSyntax
      Case expr As IfStatementSyntax
      Case expr As LoopStatementSyntax
      Case expr As NextStatementSyntax
      Case expr As SelectStatementSyntax
      Case expr As SyncLockStatementSyntax
      Case expr As TryStatementSyntax
      Case expr As UsingStatementSyntax
      Case expr As WhileStatementSyntax
      Case expr As WithStatementSyntax
      Case Else
        Throw New NotSupportedException(String.Format("Statement not supported: {0}", statement))
    End Select
  End Sub


#Region "Statement Quoutes"

  Private Sub Qoute_InheritsOrImplementsStatement(expr As InheritsOrImplementsStatementSyntax)
    Throw New NotImplementedException()
  End Sub

  Private Sub Qoute_TypeStatement(expr As TypeStatementSyntax)
    Throw New NotImplementedException()
  End Sub

  Private Sub Qoute_DeclarationStatement(expr As DeclarationStatementSyntax)
    Throw New NotImplementedException()
  End Sub

  Private Sub Qoute_ExecutableStatement(expr As ExecutableStatementSyntax)
    Throw New NotImplementedException()
  End Sub
#End Region

#End Region

#Region "Clauses"

  

#Region "Clause Quotes"
 
#End Region

#End Region






  Private Sub Quote_TypeBlock(block As TypeBlockSyntax)
    Throw New NotImplementedException()
  End Sub

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
    If TypeOf node Is AccessorDeclarationSyntax OrElse
       TypeOf node Is Syntax.BinaryExpressionSyntax OrElse
       TypeOf node Is ClassOrStructConstraintSyntax OrElse
       TypeOf node Is CheckedExpressionSyntax OrElse
       TypeOf node Is CheckedStatementSyntax OrElse
       TypeOf node Is ConstructorInitializerSyntax OrElse
       TypeOf node Is Syntax.GoToStatementSyntax OrElse
       TypeOf node Is InitializerExpressionSyntax OrElse
       TypeOf node Is Syntax.LiteralExpressionSyntax OrElse
       TypeOf node Is Syntax.MemberAccessExpressionSyntax OrElse
       TypeOf node Is Syntax.OrderingSyntax OrElse
       TypeOf node Is PostfixUnaryExpressionSyntax OrElse
       TypeOf node Is Syntax.UnaryExpressionSyntax OrElse
       TypeOf node Is Syntax.DocumentationCommentTriviaSyntax OrElse
       TypeOf node Is Syntax.CaseClauseSyntax OrElse
       TypeOf node Is Syntax.YieldStatementSyntax Then

      result.Add(New ApiCall("Kind", "SyntaxKind." + node.VBKind.ToString))
    End If

    Return result
  End Function

  ' <summary>
  ' Quote the value of the property <paramref name="property"/> of object <paramref
  ' name="node"/>
  ' </summary>
  Private Function QuotePropertyValue(node As SyntaxNode, [Property] As PropertyInfo) As ApiCall

    Dim value = [Property].GetValue(node, Nothing)
    Dim pType = [Property].PropertyType
    If pType = GetType(SyntaxToken) Then Return QuoteToken(DirectCast(value, SyntaxToken), [Property].Name)
    If pType = GetType(SyntaxTokenList) Then Return QuoteList(DirectCast(value, IEnumerable), [Property].Name)
    If (pType.IsGenericType AndAlso
                  (pType.GetGenericTypeDefinition = GetType(SyntaxList(Of )) OrElse
                  (pType.GetGenericTypeDefinition = GetType(SeparatedSyntaxList(Of )) Then
      Return QuoteList(DirectCast(value, IEnumerable), [Property].Name)
    End If
    If TypeOf value Is SyntaxNode Then Return QuoteNode(DirectCast(value, SyntaxNode), [Property].Name)
    If TypeOf value Is String Then Return New ApiCall([Property].Name, "") 'TODO: "\"" + Escape( value.ToString( ) ) + "\"" )
    If TypeOf value Is Boolean Then Return New ApiCall([Property].Name, value.ToString().ToLowerInvariant())
    Return Nothing
  End Function


  Function QuoteList(syntaxList As IEnumerable, name As String) As ApiCall
    Dim sourceList As IEnumerable(Of Object) = syntaxList.Cast(Of Object)
    Dim mName = "SyntaxFactory.List"
    Dim listType As String = Nothing
    Dim pType = syntaxList.GetType()
    If (pType.IsGenericType) Then
      Dim mType = pType.GetGenericArguments(0).Name
      listType = mType
      If (pType.GetGenericTypeDefinition = GetType(SeparatedSyntaxList(Of ))) Then
        listType = "SyntaxNodeOrToken"
        mName = "SyntaxFactory.SeparatedList"
        sourceList = DirectCast(syntaxList.GetType().GetMethod("GetWithSeparators").Invoke(syntaxList, Nothing), SyntaxNodeOrTokenList) ).Cast(Of Object).ToArray
      End If
      mName &= "(Of " & mType + ")"
    End If
    If (pType.Name = "SyntaxTokenList") Then mName = "SyntaxFactory.TokenList"
    If (pType.Name = "SyntaxTriviaList") Then mName = "SyntaxFactory.TriviaList"
    Dim elements = ArgList.Create(sourceList.Select(Function(o) Quote(o)).Where(Function(cb) cb IsNot Nothing).ToArray)
    If (elements.Count = 0) Then Return Nothing
    If (elements.Count = 1) Then
      mName = mName.Replace(".List", ".SingletonList").Replace(".SeparatedList", ".SingletonSeparatedList")
    Else
      elements = ArgList.Create(New ApiCall("methodName", "new " + listType + "[]", elements, useCurliesInsteadOfParentheses:      true ) ) 
 End If
    Dim codeBlock = New ApiCall(name, mName, elements)
    Return codeBlock
  End Function

  Private Function QuoteToken(value As SyntaxToken, name As String) As ApiCall
    If (value = Nothing) OrElse value.IsKind(SyntaxKind.None) Then Return Nothing
    Dim args = ArgList.Create()
    Dim mName = "SyntaxFactory.Token"
    Dim escapedTokenValueText = """" & Escape(value.ToString()) + """"
    Dim head = GetLeadingTrivia(value)
    Dim tail = GetTrailingTrivia(value)
    Dim actualValue As Object
    If (head IsNot Nothing OrElse tail IsNot Nothing) Then
      head = If(head, GetEmptyTrivia("LeadingTrivia"))
      tail = If(tail, GetEmptyTrivia("TrailingTrivia"))
    End If
    Select Case value.VBKind
      Case SyntaxKind.IdentifierToken
        mName = "SyntaxFactory.Identifier"
        If value.IsMissing Then mName = "SyntaxFactory.MissingToken"
        If value.IsMissing Then actualValue = value.VBKind Else actualValue = escapedTokenValueText
        args.AddIfNotNull(head)
        args.Add(actualValue)
        args.AddIfNotNull(tail)
      Case SyntaxKind.XmlTextLiteralToken, SyntaxKind.XmlTextLiteralNewLineToken, SyntaxKind.XmlEntityLiteralToken
        mName = "SyntaxFactory.XmlTextLiteral"
        If value.IsKind(SyntaxKind.XmlTextLiteralNewLineToken) Then
          mName = "SyntaxFactory.XmlTextNewLine"
        ElseIf value.IsKind(SyntaxKind.XmlEntityLiteralToken) Then
          mName = "SyntaxFactory.XmlEntity"
          args.Add(If(head, GetEmptyTrivia("LeadingTrivia")),
                     escapedTokenValueText, escapedTokenValueText,
                     If(tail, GetEmptyTrivia("TrailingTrivia")))
          Case Else
          If ((TypeOf value.Parent Is LiteralExpressionSyntax) OrElse
                      value.IsKindAny(SyntaxKind.StringLiteralToken, SyntaxKind.NumericLiteralToken)) AndAlso
                 value.IsKindNoneOf(SyntaxKind.TrueKeyword, SyntaxKind.FalseKeyword, SyntaxKind.NullKeyword, SyntaxKind.ArgListKeyword) Then
            mName = "SyntaxFactory.Literal"
            args.Add(If(head, GetEmptyTrivia("LeadingTrivia"), escapedTokenValueText))
            Dim escapedValue = value.ToString()
            If (value.IsKind(SyntaxKind.StringLiteralToken)) Then escapedValue = escapedTokenValueText
            args.Add(escapedValue, If(tail, GetEmptyTrivia("TrailingTrivia")))

          Else
            If value.IsMissing Then mName = "SyntaxFactory.MissingToken"
            If value.IsKind(SyntaxKind.BadToken) Then
              mName = "SyntaxFactory.BadToken"
              head = If(head, GetEmptyTrivia("LeadingTrivia"))
              tail = If(tail, GetEmptyTrivia("TrailingTrivia"))
            End If
            Dim tokenValue As Object = value.VBKind
            If value.IsKind(SyntaxKind.BadToken) Then tokenValue = escapedTokenValueText
            args.AddIfNotNull(head)
            args.Add(tokenValue)
            args.AddIfNotNull(tail)
       End Select
    Return New ApiCall(name, mName, args)
  End Function

  Private Function GetLeadingTrivia(value As SyntaxToken) As Object
    Return If(Not value.HasLeadingTrivia, Nothing, QuoteList(value.LeadingTrivia, "LeadingTrivia"))
  End Function
  Private Function GetTrailingTrivia(value As SyntaxToken) As Object
    Return If(Not value.HasTrailingTrivia, Nothing, QuoteList(value.TrailingTrivia, "TrailingTrivia"))
  End Function
  Private Function GetEmptyTrivia(parentPropertyName As String) As Object
    Return New ApiCall(parentPropertyName, "SyntaxFactory.TriviaList", arguments:=Nothing)
  End Function

  Private Function QuoteTrivia(syntaxTrivia As SyntaxTrivia) As ApiCall
    Dim factoryMethodName = "SyntaxFactory.Trivia"
    Dim text = syntaxTrivia.ToString()
    If (syntaxTrivia.FullSpan.Length = 0) OrElse
       (syntaxTrivia.IsKind(SyntaxKind.WhitespaceTrivia) AndAlso UseDefaultFormatting) Then Return Nothing
    Dim field As FieldInfo = Nothing
    If triviaFactoryFields.TryGetValue(syntaxTrivia.ToString(), field) AndAlso
       (DirectCast(field.GetValue(Nothing), SyntaxTrivia).VBKind = syntaxTrivia.VBKind) Then
      Return If(UseDefaultFormatting, Nothing, New ApiCall(Nothing, "SyntaxFactory." + field.Name))
    End If
    If Not String.IsNullOrEmpty(text) AndAlso
             String.IsNullOrWhiteSpace(text) AndAlso
        syntaxTrivia.IsKind(SyntaxKind.WhitespaceTrivia) Then

      If UseDefaultFormatting Then Return Nothing
      factoryMethodName = "SyntaxFactory.Whitespace"
    End If
    If syntaxTrivia.IsTriviaAny(SyntaxKind.CommentTrivia) Then factoryMethodName = "SyntaxFactory.Comment"
    'If syntaxTrivia.IsKind(SyntaxKind.PreprocessingMessageTrivia) Then factoryMethodName = "SyntaxFactory.PreprocessingMessage"
    If syntaxTrivia.IsKind(SyntaxKind.DisabledTextTrivia) Then factoryMethodName = "SyntaxFactory.DisabledText"
    If syntaxTrivia.IsKind(SyntaxKind.DocumentationCommentExteriorTrivia) Then factoryMethodName = "SyntaxFactory.DocumentationCommentExterior"

    Dim argument As Object = """" & Escape(syntaxTrivia.ToString()) & """"
    If syntaxTrivia.HasStructure Then argument = QuoteNode(syntaxTrivia.GetStructure(), "Structure")

    Return New ApiCall(Nothing, factoryMethodName, ArgList.Create(argument))
  End Function

  ' <summary>
  ' Escapes strings to be included within "" using C# escaping rules
  ' </summary>
  Private Function Escape(text As String, Optional escapeVerbatim As Boolean = True) As String
    'Dim sb As New StringBuilder()
    'For i = 0 To text.Length - 1
    '  Dim toAppend = text(i).ToString()
    '  If text(i) = """"c Then
    '      toAppend = If( escapeVerbatim, "\"\",toAppend = "\\\""; 
    '  ElseIf (text [ i ] == '\\' && !escapeVerbatim ) { toAppend = "\\\\"
    '    End If
    '            sb.Append(toAppend)
    '        Next
    'Return sb.ToString()
    Return text
  End Function
  ' <summary>
  ' Static methods on Roslyn.Compilers.CSharp.Syntax class that construct SyntaxNodes
  ' </summary>
  ' <example>Syntax.ClassDeclaration()</example>
  Private Shared ReadOnly factoryMethods As Dictionary(Of String, List(Of MethodInfo)) = GetFactoryMethods()
  ' <summary>
  ' Five public fields on Roslyn.Compilers.CSharp.Syntax that return trivia: CarriageReturn,
  ' LineFeed, CarriageReturnLineFeed, Space And Tab.
  ' </summary>
  Private Shared ReadOnly triviaFactoryFields As Dictionary(Of String, FieldInfo) = GetTriviaFactoryFields()

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

End If
