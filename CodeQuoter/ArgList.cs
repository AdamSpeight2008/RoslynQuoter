using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodeQuoter
{
    public class ArgList : IEnumerable<object>
    {
        private List<object> Args  = new List<object>( );
        public int Count { get { return Args.Count; } }
        public object this [ int index ] { get { return Args [ index ]; } }

        private ArgList ( params object [ ] Arguments ) { Args = new List<object>( Arguments ); }

        public static ArgList Create ( params object [ ] Arguments ) { return new ArgList( Arguments ); }

        public void Add ( params object [ ] Arguments ) { Args.AddRange( Arguments ); }
        public void AddIfNotNull ( object value ) { if ( value != null ) { Args.Add( value ); } }

        IEnumerator<object> IEnumerable<object>.GetEnumerator ( ) { return this.Args.GetEnumerator( ); }
        IEnumerator IEnumerable.GetEnumerator ( ) { return this.Args.GetEnumerator( ); }

    }
}
