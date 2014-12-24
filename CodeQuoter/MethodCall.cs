using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodeQuoter
{
    /// <summary>
    /// Represents a method call that has a Name and an arbitrary list of Arguments.
    /// </summary>
    public class MethodCall : MemberCall
    {
        public ArgList Arguments { get; private set; } = ArgList.Create( );

        public MethodCall ( string methodName ) : base( methodName ) { }
        public MethodCall ( string methodName, ArgList argList ) : this( methodName ) { Arguments = argList; }
    }
}
