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

import static br.eti.rslemos.cobolg.TextHelper.join;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenSource;
import org.junit.Test;

import br.eti.rslemos.cobolg.Compiler.FreeFormatCompiler;

public class FreeFormatLexerUnitTest {
	@Test
	public void testCommentLine() throws Exception {
		TokenSource stream = new FreeFormatCompiler().decompose("*COMMENT LINE\n");
		
		Token token0 = stream.nextToken();
		Token token1 = stream.nextToken();
		
		assertThat(token0.getType(), is(equalTo(COBOLFreeFormatLexer.COMMENT)));
		assertThat(token0.getChannel(), is(equalTo(COBOLFreeFormatLexer.HIDDEN)));
		assertThat(token0.getText(), is(equalTo("*COMMENT LINE\n")));
		
		assertThat(token1.getType(), is(equalTo(Lexer.EOF)));
	}

	@Test
	public void testMisplacedCommentLine() throws Exception {
		TokenSource stream = new FreeFormatCompiler().decompose(" *COMMENT LINE\n");
		
		Token token0 = stream.nextToken();
		Token token1 = stream.nextToken();
		
		assertThat(token0.getType(), is(equalTo(COBOLFreeFormatLexer.WS)));
		assertThat(token0.getCharPositionInLine(), is(equalTo(0)));
		
		assertThat(token1.getType(), is(not(equalTo(COBOLFreeFormatLexer.COMMENT))));
	}


	@Test
	public void testBothCorrectAndMisplacedCommentLine() throws Exception {
		final String SOURCE = join(
				"*COMMENT LINE",
				" *COMMENT LINE"
			);
		
		TokenSource stream = new FreeFormatCompiler().decompose(SOURCE);
		
		Token token0 = stream.nextToken();
		Token token1 = stream.nextToken();
		Token token2 = stream.nextToken();
		
		assertThat(token0.getType(), is(equalTo(COBOLFreeFormatLexer.COMMENT)));
		assertThat(token0.getChannel(), is(equalTo(COBOLFreeFormatLexer.HIDDEN)));
		assertThat(token0.getText(), is(equalTo("*COMMENT LINE\n")));
		
		assertThat(token1.getType(), is(equalTo(COBOLFreeFormatLexer.WS)));
		assertThat(token1.getCharPositionInLine(), is(equalTo(0)));
		
		assertThat(token2.getType(), is(not(equalTo(COBOLFreeFormatLexer.COMMENT))));
		
	}
}
