using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using CodeQuoter.Exts;

namespace CodeQuoter
{
    /// <summary>
    /// "Stringly typed" representation of a C# property or method invocation expression, with a
    /// string for the property or method name and a list of similarly loosely typed argument
    /// expressions. Simply speaking, this is a tree of strings.
    /// </summary>
    /// <example>
    /// Data structure to represent code (API calls) of simple hierarchical shape such as:
    /// A.B(C, D.E(F(G, H), I))
    /// </example>
    public class ApiCall
    {
        public string Name { get; private set; }
        public MemberCall FactoryMethodCall { get; private set; }
        public List<MethodCall> InstanceMethodCalls { get; private set; }
        public bool useCurly { get; private set; }

        public ApiCall ( string parentPropertyName ) { Name = parentPropertyName; }

        public ApiCall ( string parentPropertyName, string factoryMethodName ) : this( parentPropertyName )
        { FactoryMethodCall = new MemberCall( factoryMethodName ); }

        public ApiCall ( string parentPropertyName,
                         string factoryMethodName,
                        ArgList arguments,
                           bool useCurliesInsteadOfParentheses = false ) : this( parentPropertyName, new MethodCall( factoryMethodName, arguments ) )
        { useCurly = useCurliesInsteadOfParentheses; }

        public ApiCall ( string name, MethodCall factoryMethodCall ) : this( name ) { FactoryMethodCall = factoryMethodCall; }

        public void Add ( MethodCall methodCall ) { InstanceMethodCalls = InstanceMethodCalls ?? new List<MethodCall>( ); InstanceMethodCalls.Add( methodCall ); }
        public void Remove ( MethodCall methodCall ) { if ( InstanceMethodCalls != null ) InstanceMethodCalls.Remove( methodCall ); }
        public string ToString (CodePrint cp ) { return cp.PrintWithDefaultFormatting( this ); }

        [Obsolete("",true )]
        public override string ToString ( )
        {
            return base.ToString( );
        }

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
                var methodName = "With" + value.Name.ProperCase();
                if ( !methods.Any( m => m.Name == methodName ) ) throw new NotSupportedException( );
                methodName = "." + methodName;
                apiCall.AddModifyingCall( new MethodCall( methodName, ArgList.Create( value ) ) );
            }
        }
    }

}
