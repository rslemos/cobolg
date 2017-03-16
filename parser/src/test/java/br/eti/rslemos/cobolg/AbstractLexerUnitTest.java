/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2015  Rodrigo Lemos
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

import static org.antlr.v4.runtime.Lexer.DEFAULT_TOKEN_CHANNEL;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenSource;

public abstract class AbstractLexerUnitTest {

	protected TokenSource stream;
	
	protected void setSource(String source) throws IOException {
		stream = getLexer(new StringReader(source));
	}

	protected abstract TokenSource getLexer(Reader reader) throws IOException;

	protected Token matchTokenExt(int type, String text, int channel, int line, int position) {
		return matchTokenExt(matchToken(type, text, channel), line, position);
	}

	private static Token matchTokenExt(Token token, int line, int position) {
		assertThat(token.getLine(), is(equalTo(line)));
		assertThat(token.getCharPositionInLine(), is(equalTo(position)));
		return token;
	}
	
	protected Token matchToken(int type, String text, int channel) {
		Token token = stream.nextToken();
		assertThat(token.getType(), is(equalTo(type)));
		assertThat(token.getText(), is(equalTo(text)));
		assertThat(token.getChannel(), is(equalTo(channel)));
		return token;
	}

	protected Token matchToken(int type, String text) {
		return matchToken(type, text, DEFAULT_TOKEN_CHANNEL);
	}

	protected Token matchEOFExt(int line, int position) {
		return matchTokenExt(matchEOF(), line, position);
	}
	
	protected Token matchEOF() {
		Token token = stream.nextToken();
		assertThat(token.getType(), is(equalTo(Lexer.EOF)));
		return token;
	}

}
