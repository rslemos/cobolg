/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2016  Rodrigo Lemos
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

import static br.eti.rslemos.cobolg.SimpleCompiler.parserForFixedFormat;

import java.io.IOException;
import java.io.Reader;
import java.util.ResourceBundle;

import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.AlphanumericLiteralContext;

public class AlphanumericLiteralUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.alphanumericLiteral");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<AlphanumericLiteralContext> helper = new CompilerHelper<AlphanumericLiteralContext>() {
		@Override protected AlphanumericLiteralContext parsePart() { return parser.alphanumericLiteral(); }
		@Override protected SimpleCompiler createCompiler(Reader source) throws IOException { return parserForFixedFormat(source); }
	};

	// the first tests are the same for the parser as they are for the lexer
	
	@Test public void DOUBLY_QUOTED_STRING() {
		helper.compileAndVerify(
				get("DOUBLY_QUOTED_STRING.source"),
				get("DOUBLY_QUOTED_STRING.tree")
			);
	}

	@Test public void DOUBLY_QUOTED_WITH_DOUBLE_QUOTES_STRING() {
		helper.compileAndVerify(
				get("DOUBLY_QUOTED_WITH_DOUBLE_QUOTES_STRING.source"),
				get("DOUBLY_QUOTED_WITH_DOUBLE_QUOTES_STRING.tree")
			);
	}
	
	@Test public void DOUBLY_QUOTED_WITH_SINGLE_QUOTES_STRING() {
		helper.compileAndVerify(
				get("DOUBLY_QUOTED_WITH_SINGLE_QUOTES_STRING.source"),
				get("DOUBLY_QUOTED_WITH_SINGLE_QUOTES_STRING.tree")
			);
	}
	
	@Test public void SINGLY_QUOTED_STRING() {
		helper.compileAndVerify(
				get("SINGLY_QUOTED_STRING.source"),
				get("SINGLY_QUOTED_STRING.tree")
			);
	}

	@Test public void SINGLY_QUOTED_WITH_SINGLE_QUOTES_STRING() {
		helper.compileAndVerify(
				get("SINGLY_QUOTED_WITH_SINGLE_QUOTES_STRING.source"),
				get("SINGLY_QUOTED_WITH_SINGLE_QUOTES_STRING.tree")
			);
	}
	
	@Test public void SINGLY_QUOTED_WITH_DOUBLE_QUOTES_STRING() {
		helper.compileAndVerify(
				get("SINGLY_QUOTED_WITH_DOUBLE_QUOTES_STRING.source"),
				get("SINGLY_QUOTED_WITH_DOUBLE_QUOTES_STRING.tree")
			);
	}
	
	// now the hard cases (continuation line)
	// taken from br.eti.rslemos.cobolg.FixedFormatLexerUnitTest
	@Test public void DOUBLE_QUOTE_CONTINUATION_STRING() {
		helper.compileAndVerify(
				get("DOUBLE_QUOTE_CONTINUATION_STRING.source"),
				get("DOUBLE_QUOTE_CONTINUATION_STRING.tree")
			);
	}
	
	@Test public void DOUBLE_QUOTE_CONTINUATION_IN_3_LINES_STRING() {
		helper.compileAndVerify(
				get("DOUBLE_QUOTE_CONTINUATION_IN_3_LINES_STRING.source"),
				get("DOUBLE_QUOTE_CONTINUATION_IN_3_LINES_STRING.tree")
			);
	}
	
	@Test public void SINGLE_QUOTE_CONTINUATION_STRING() {
		helper.compileAndVerify(
				get("SINGLE_QUOTE_CONTINUATION_STRING.source"),
				get("SINGLE_QUOTE_CONTINUATION_STRING.tree")
			);
	}
	
	@Test public void SINGLE_QUOTE_CONTINUATION_IN_3_LINES_STRING() {
		helper.compileAndVerify(
				get("SINGLE_QUOTE_CONTINUATION_IN_3_LINES_STRING.source"),
				get("SINGLE_QUOTE_CONTINUATION_IN_3_LINES_STRING.tree")
			);
	}
	
	// must check if these two are actually valid COBOL

	@Test public void DOUBLE_QUOTE_CONTINUATION_ON_SINGLE_QUOTE_STRING() {
		helper.compileAndVerify(
				get("DOUBLE_QUOTE_CONTINUATION_ON_SINGLE_QUOTE_STRING.source"),
				get("DOUBLE_QUOTE_CONTINUATION_ON_SINGLE_QUOTE_STRING.tree")
			);
	}
	
	@Test public void SINGLE_QUOTE_CONTINUATION_ON_DOUBLE_QUOTE_STRING() {
		helper.compileAndVerify(
				get("SINGLE_QUOTE_CONTINUATION_ON_DOUBLE_QUOTE_STRING.source"),
				get("SINGLE_QUOTE_CONTINUATION_ON_DOUBLE_QUOTE_STRING.tree")
			);
	}

	// tests for hexstring

	@Test public void DOUBLY_QUOTED_HEXSTRING() {
		helper.compileAndVerify(
				get("DOUBLY_QUOTED_HEXSTRING.source"),
				get("DOUBLY_QUOTED_HEXSTRING.tree")
			);
	}

	@Test public void SINGLY_QUOTED_HEXSTRING() {
		helper.compileAndVerify(
				get("SINGLY_QUOTED_HEXSTRING.source"),
				get("SINGLY_QUOTED_HEXSTRING.tree")
			);
	}

}
