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

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.DiagnosticErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.ParseTree;
import org.junit.Test;

public class FreeFormatUnitTest {
	@Test
	public void testSimpleHelloWorld() throws IOException {
		final String SOURCE = join(
				"IDENTIFICATION DIVISION.",
				"PROGRAM-ID. HELLO-WORLD.",
				"PROCEDURE DIVISION.",
				"    DISPLAY 'Hello, world'.",
				"    STOP RUN."
			);
		
		ParseTree tree = compile(SOURCE);
		assertThat(tree, is(not(nullValue(ParseTree.class))));
	}
	
	private ParseTree compile(String contents) throws IOException {
		Reader reader = new StringReader(contents);
		
		return compile(reader);
	}

	static ParseTree compile(Reader reader) throws IOException {
		COBOLLexer lexer = new COBOLLexer(new ANTLRInputStream(reader));
		COBOLParser parser = new COBOLParser(new CommonTokenStream(lexer));
		
		lexer.addErrorListener(new DiagnosticErrorListener());
		lexer.addErrorListener(new BailOutErrorListener());
		
		parser.addErrorListener(new DiagnosticErrorListener());
		//parser.addErrorListener(new BailOutErrorListener());
		parser.setErrorHandler(new BailErrorStrategy());
		
		ParseTree tree = parser.program();
		
		return tree;
	}
	
	private static String join(String... lines) {
		StringBuilder builder = new StringBuilder();
		
		for (String line : lines) {
			builder.append(line).append('\n');
		}
		
		if (lines.length > 0)
			builder.setLength(builder.length() - 1);
		
		return builder.toString();
	}
}

class BailOutErrorListener extends BaseErrorListener {

	@Override
	public void syntaxError(Recognizer<?, ?> recognizer,
			Object offendingSymbol, int line, int charPositionInLine,
			String msg, RecognitionException e) {
		throw new RuntimeException(msg, e);
	}

}