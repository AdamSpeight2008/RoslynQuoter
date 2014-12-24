using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CodeQuoter
{
    public abstract class BaseQuoter
    {
        public bool UseDefaultFormatting          { get; private set; } = true;
        public bool RemoveRedundantModifyingCalls { get; private set; } = true;
        protected BaseQuoter ( bool useDefaultFormatting = true, bool removeRedundantModifyingCalls = true)
        {
            this.UseDefaultFormatting = useDefaultFormatting;
            this.RemoveRedundantModifyingCalls = removeRedundantModifyingCalls;
        }

        public abstract string Quote(string sourceText );
        // string Quote<T> ( T node )
        //    where T : Microsoft.CodeAnalysis.SyntaxNode 
        //{
        //    ApiCall rootApiCall = Quote(node, name: null);
        //    if ( UseDefaultFormatting ) rootApiCall.Add( new MethodCall( ".NormalizeWhitespace" ) );
        //    return Print( rootApiCall );
        //}

    }
}
