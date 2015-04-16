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

import static br.eti.rslemos.cobolg.COBOLFreeFormatLexer.*;
import static br.eti.rslemos.cobolg.TextHelper.join;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

import org.antlr.v4.runtime.Token;
import org.junit.Test;

import br.eti.rslemos.cobolg.Compiler.FreeFormatCompiler;

public class FreeFormatLexerUnitTest extends AbstractLexerUnitTest {
	
	public FreeFormatLexerUnitTest() {
		super(new FreeFormatCompiler());
	}

	@Test
	public void testCommentLine() throws Exception {
		setSource("*COMMENT LINE\n");
		
		matchToken(COMMENT, "*COMMENT LINE\n", HIDDEN);
		
		matchEOF();
	}

	@Test
	public void testMisplacedCommentLine() throws Exception {
		setSource(" *COMMENT LINE\n");
		
		Token token;
		
		token = stream.nextToken();
		assertThat(token.getType(), is(equalTo(WS)));
		assertThat(token.getCharPositionInLine(), is(equalTo(0)));
		
		token = stream.nextToken();
		assertThat(token.getType(), is(not(equalTo(COMMENT))));
	}


	@Test
	public void testBothCorrectAndMisplacedCommentLine() throws Exception {
		final String SOURCE = join(
				"*COMMENT LINE",
				" *COMMENT LINE"
			);
		
		setSource(SOURCE);
		
		matchToken(COMMENT, "*COMMENT LINE\n", HIDDEN);
		
		Token token;
		
		token = stream.nextToken();		
		assertThat(token.getType(), is(equalTo(WS)));
		assertThat(token.getCharPositionInLine(), is(equalTo(0)));
		
		token = stream.nextToken();
		assertThat(token.getType(), is(not(equalTo(COMMENT))));
	}
	
	@Test
	public void testCommentLineWithCRLF() throws Exception {
		setSource("*COMMENT LINE\r\n");
		
		matchToken(COMMENT, "*COMMENT LINE\r\n", HIDDEN);
		
		matchEOF();
	}

	@Test
	public void testCommentLineWithCR() throws Exception {
		setSource("*COMMENT LINE\r");
		
		matchToken(COMMENT, "*COMMENT LINE\r", HIDDEN);
		
		matchEOF();
	}

	@Test
	public void testDoubleQuotedString() throws Exception {
		setSource("\"DOUBLE QUOTED STRING\"");
		
		matchToken(QUOTEDSTRING, "\"DOUBLE QUOTED STRING\"");
		
		matchEOF();
	}

	@Test
	public void testDoubleQuotedWithDoubleQuotesString() throws Exception {
		setSource("\"DOUBLE QUOTED STRING WITH \"\"DOUBLE QUOTES\"\"\"");
		
		matchToken(QUOTEDSTRING, "\"DOUBLE QUOTED STRING WITH \"\"DOUBLE QUOTES\"\"\"");
		
		matchEOF();
	}
}
