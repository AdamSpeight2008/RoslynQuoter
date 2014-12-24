using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CodeQuoter
{
  public static class Exts
    {
        public static string ProperCase ( this string str ) { return char.ToUpperInvariant( str [ 0 ] ) + str.Substring( 1 ); }

    }
}
