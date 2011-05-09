package org.nlogo.prim.etc ;

import org.nlogo.api.Dump;
import org.nlogo.api.I18N;
import org.nlogo.api.LogoException;
import org.nlogo.api.LogoList;
import org.nlogo.nvm.ArgumentTypeException;
import org.nlogo.nvm.EngineException;
import org.nlogo.nvm.Reporter;
import org.nlogo.nvm.Syntax;

public final strictfp class _removeitem
	extends Reporter
	implements org.nlogo.nvm.Pure
{
	@Override
	public Object report( final org.nlogo.nvm.Context context ) throws LogoException
	{
		int index = argEvalIntValue( context , 0 ) ;
		Object obj = args[ 1 ].report( context ) ;
		if( index < 0 )
		{
			throw new EngineException
				( context , this  , I18N.errors().getNJava("org.nlogo.prim.etc.$common.negativeIndex",
                  new String[] {Integer.toString(index)}));
		}
		if( obj instanceof LogoList )
		{
			LogoList list = (LogoList) obj ;
			if( index >= list.size() )
			{
				throw new EngineException
					( context , this  , I18N.errors().getNJava("org.nlogo.prim.etc.$common.indexExceedsListSize",
                      new String []{Integer.toString(index), Dump.logoObject(list), Integer.toString(list.size())}));

			}
			return list.removeItem( index ) ;
		}
		else if( obj instanceof String )
		{
			String string = (String) obj ;
			if( index >= string.length() )
			{
				throw new EngineException
					( context , this  , I18N.errors().getNJava("org.nlogo.prim.etc.$common.indexExceedsStringSize",
                      new String [] {Integer.toString(index), Dump.logoObject(string), Integer.toString(string.length())}));

			}
			StringBuilder buf = new StringBuilder() ;
			buf.append( string.substring( 0 , index ) ) ;
			buf.append( string.substring( index + 1 ) ) ;
			return buf.toString() ;
		}
		else 
		{
			throw new ArgumentTypeException
				( context , this , 1 , Syntax.TYPE_LIST | Syntax.TYPE_STRING , obj ) ; 
		}
	}
	@Override
	public Syntax syntax()
	{
		return Syntax.reporterSyntax
			( new int[] { Syntax.TYPE_NUMBER ,
						  Syntax.TYPE_LIST | Syntax.TYPE_STRING  } ,
			  Syntax.TYPE_LIST | Syntax.TYPE_STRING ) ;
	}
}
