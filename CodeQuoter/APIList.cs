using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace CodeQuoter
{
    public class APIList : IEnumerable<ApiCall>
    {
        private List<ApiCall> APIs  = new List<ApiCall>( );
        public int Count { get { return APIs.Count; } }
        public object this [ int index ] { get { return APIs [ index ]; } }

        private APIList ( params ApiCall [ ] apis ) { APIs = new List<ApiCall>( apis ); }

        public static APIList Create ( params ApiCall [ ] apis ) { return new APIList( apis ); }

        public void Add ( params ApiCall [ ] apis ) { APIs.AddRange( apis ); }
        public void AddIfNotNull ( ApiCall api ) { if ( api != null ) { APIs.Add( api ); } }

        IEnumerator<ApiCall> IEnumerable<ApiCall>.GetEnumerator ( ) { return this.APIs.GetEnumerator( ); }
        IEnumerator IEnumerable.GetEnumerator ( ) { return this.APIs.GetEnumerator( ); }

        internal void Remove ( ApiCall quotedCodeBlock )
        {
            if ( APIs != null ) APIs.Remove( quotedCodeBlock );
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

}
