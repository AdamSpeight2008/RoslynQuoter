using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodeQuoter
{
    public abstract class CodePrint
    {
      abstract public string Print(ApiCall node ); 
      abstract public string PrintWithDefaultFormatting( ApiCall node );
      abstract public string Print ( MemberCall node );
    }
}
