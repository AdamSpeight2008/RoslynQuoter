using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodeQuoter
{
    /// <summary>
    /// Simple data structure to represent a member call, primarily just the string Name.
    /// </summary>
    public class MemberCall
    {
        public string Name { get; private set; }
        public MemberCall ( string factoryMethodName ) { Name = factoryMethodName; }
        [Obsolete("", true)]
        public override string ToString ( )
        {
            return base.ToString( );
        }
        public string ToString ( CodePrint cp ) { return cp.Print( this ); }//, new StringBuilder( ), 0 ).ToString( ); }
    }


}
