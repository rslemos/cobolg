/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2017  Rodrigo Lemos
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

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenSource;

public class ChainedToken implements Token {

	public static final int TRAILING_TOKEN = -10000;
	
	// basic properties
	private String text;
	private int type;
	private int line;
	private int charPositionInLine;
	private int channel;
	private int tokenIndex;
	private int startIndex;
	private int stopIndex;
	private TokenSource tokenSource;
	private CharStream inputStream;

	// chain
	private ChainedToken head = this;
	private ChainedToken next;
	
	public ChainedToken(String text, int type, int line, int charPositionInLine, int channel, int tokenIndex,
			int startIndex, int stopIndex, TokenSource tokenSource, CharStream inputStream) {
		super();
		this.text = text;
		this.type = type;
		this.line = line;
		this.charPositionInLine = charPositionInLine;
		this.channel = channel;
		this.tokenIndex = tokenIndex;
		this.startIndex = startIndex;
		this.stopIndex = stopIndex;
		this.tokenSource = tokenSource;
		this.inputStream = inputStream;
	}

	public String getFragmentText() {
		return text;
	}

	@Override
	public String getText() {
		if (head != this)
			return null;

		StringBuilder text = new StringBuilder();
		text.append(this.text);
		
		for(ChainedToken p = this.next; p != null; p = p.next) {
			text.append(p.text);
		}
		
		return text.toString();
	}

	@Override
	public int getType() {
		return type;
	}

	@Override
	public int getLine() {
		return line;
	}

	@Override
	public int getCharPositionInLine() {
		return charPositionInLine;
	}

	@Override
	public int getChannel() {
		return channel;
	}

	@Override
	public int getTokenIndex() {
		return tokenIndex;
	}

	@Override
	public int getStartIndex() {
		return startIndex;
	}

	@Override
	public int getStopIndex() {
		return stopIndex;
	}

	@Override
	public TokenSource getTokenSource() {
		return tokenSource;
	}

	@Override
	public CharStream getInputStream() {
		return inputStream;
	}
	
	public ChainedToken head() {
		return head;
	}

	public ChainedToken next() {
		return next;
	}
	
	void setNext(ChainedToken next) {
		this.next = next;
		next.head = head;
	}
}
