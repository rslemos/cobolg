/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2013  Rodrigo Lemos
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * END COPYRIGHT NOTICE
 ******************************************************************************/
package br.eti.rslemos.cobolg;

import static br.eti.rslemos.cobolg.COBOLLexer.USERDEFINEDWORD;
import static br.eti.rslemos.cobolg.Compiler.lexerForFreeFormat;

import java.io.IOException;
import java.io.Reader;

import org.antlr.v4.runtime.TokenSource;
import org.junit.Test;

public class USERDEFINEDWORDUnitTest extends AbstractLexerUnitTest {
	
	@Test public void LETTERS()   throws IOException { setSource("LETTERS");   matchToken(USERDEFINEDWORD, "LETTERS");   }
	@Test public void L()         throws IOException { setSource("L");         matchToken(USERDEFINEDWORD, "L");         }
	@Test public void $1LETTER()  throws IOException { setSource("1LETTER");   matchToken(USERDEFINEDWORD, "1LETTER");   }
	@Test public void LETTERS_()  throws IOException { setSource("LETTERS_");  matchToken(USERDEFINEDWORD, "LETTERS_");  }
	@Test public void LET$TERS()  throws IOException { setSource("LET-TERS");  matchToken(USERDEFINEDWORD, "LET-TERS");  }
	@Test public void LETTERS$6() throws IOException { setSource("LETTERS-6"); matchToken(USERDEFINEDWORD, "LETTERS-6"); }
	
	// the hyphen cannot appear as the first or last character
	@Test(expected = AssertionError.class) public void $LETTERS() throws IOException { setSource("-LETTERS"); matchToken(USERDEFINEDWORD, "-LETTERS"); }
	@Test(expected = AssertionError.class) public void LETTERS$() throws IOException { setSource("LETTERS-"); matchToken(USERDEFINEDWORD, "LETTERS-"); }
	
	// the underscore cannot appear as the first character
	@Test(expected = AssertionError.class) public void _LETTERS() throws IOException { setSource("_LETTERS"); matchToken(USERDEFINEDWORD, "_LETTERS"); }
	
	// must contain at least one alphabetic character
	@Test(expected = AssertionError.class) public void $600()     throws IOException { setSource("600");      matchToken(USERDEFINEDWORD, "600");      }
	@Test(expected = AssertionError.class) public void $100_()    throws IOException { setSource("100_");     matchToken(USERDEFINEDWORD, "100_");     }
	@Test(expected = AssertionError.class) public void $2$0()     throws IOException { setSource("2-0");      matchToken(USERDEFINEDWORD, "2-0");      }

	@Override protected TokenSource getLexer(Reader reader) throws IOException {
		return lexerForFreeFormat(reader);
	}
}
