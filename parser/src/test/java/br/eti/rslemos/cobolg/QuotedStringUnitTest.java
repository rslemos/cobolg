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

import java.io.IOException;
import java.io.Reader;
import java.util.ResourceBundle;

import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.QuotedStringContext;
import br.eti.rslemos.cobolg.Compiler.FixedFormatCompiler;

public class QuotedStringUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.quotedString");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<QuotedStringContext> helper = new CompilerHelper<QuotedStringContext>() {
		@Override protected QuotedStringContext parsePart() { return parser.quotedString(); }

		// this test only makes sense for the fixed format
		@Override protected Compiler createCompiler(Reader source) throws IOException { return new FixedFormatCompiler(source); }
	};

	// the first tests are the same for the parser as they are for the lexer
	
	@Test public void stringDoubleQuoted() {
		helper.compileAndVerify(
				get("stringDoubleQuoted.source"),
				get("stringDoubleQuoted.tree")
			);
	}

	@Test public void stringDoubleQuotedWithDoubleQuotes() {
		helper.compileAndVerify(
				get("stringDoubleQuotedWithDoubleQuotes.source"),
				get("stringDoubleQuotedWithDoubleQuotes.tree")
			);
	}
	
	@Test public void stringDoubleQuotedWithSingleQuotes() {
		helper.compileAndVerify(
				get("stringDoubleQuotedWithSingleQuotes.source"),
				get("stringDoubleQuotedWithSingleQuotes.tree")
			);
	}
	
	@Test public void stringSingleQuoted() {
		helper.compileAndVerify(
				get("stringSingleQuoted.source"),
				get("stringSingleQuoted.tree")
			);
	}

	@Test public void stringSingleQuotedWithSingleQuotes() {
		helper.compileAndVerify(
				get("stringSingleQuotedWithSingleQuotes.source"),
				get("stringSingleQuotedWithSingleQuotes.tree")
			);
	}
	
	@Test public void stringSingleQuotedWithDoubleQuotes() {
		helper.compileAndVerify(
				get("stringSingleQuotedWithDoubleQuotes.source"),
				get("stringSingleQuotedWithDoubleQuotes.tree")
			);
	}
	
	// now the hard cases (continuation line)
	// taken from br.eti.rslemos.cobolg.FixedFormatLexerUnitTest
	@Test public void stringDoubleQuoteContinuation() {
		helper.compileAndVerify(
				get("stringDoubleQuoteContinuation.source"),
				get("stringDoubleQuoteContinuation.tree")
			);
	}
	@Test public void stringDoubleQuoteContinuationIn3Lines() {
		helper.compileAndVerify(
				get("stringDoubleQuoteContinuationIn3Lines.source"),
				get("stringDoubleQuoteContinuationIn3Lines.tree")
			);
	}
	@Test public void stringSingleQuoteContinuation() {
		helper.compileAndVerify(
				get("stringSingleQuoteContinuation.source"),
				get("stringSingleQuoteContinuation.tree")
			);
	}
	@Test public void stringSingleQuoteContinuationIn3Lines() {
		helper.compileAndVerify(
				get("stringSingleQuoteContinuationIn3Lines.source"),
				get("stringSingleQuoteContinuationIn3Lines.tree")
			);
	}
	@Test public void stringDoubleQuoteContinuationOnSingleQuote() {
		helper.compileAndVerify(
				get("stringDoubleQuoteContinuationOnSingleQuote.source"),
				get("stringDoubleQuoteContinuationOnSingleQuote.tree")
			);
	}
	@Test public void stringSingleQuoteContinuationOnDoubleQuote() {
		helper.compileAndVerify(
				get("stringSingleQuoteContinuationOnDoubleQuote.source"),
				get("stringSingleQuoteContinuationOnDoubleQuote.tree")
			);
	}
}
