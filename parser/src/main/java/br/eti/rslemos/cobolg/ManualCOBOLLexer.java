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

import static br.eti.rslemos.cobolg.COBOLParser.*;

import java.io.IOException;
import java.util.LinkedList;
import java.util.Queue;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenFactory;
import org.antlr.v4.runtime.TokenSource;

import br.eti.rslemos.cobolg.io.LineReaderWithEOL;

public class ManualCOBOLLexer implements TokenSource {

	public static final int COMPILER_CHANNEL = 0;
	
	private Queue<ChainedToken> tokens = new LinkedList<ChainedToken>();

	private LineReaderWithEOL source; 
	private SegmentationStrategy segmentationStrategy;
	
	private int lineNumber = 1;
	private int charsRead = 0;
	
	public ManualCOBOLLexer(Readable source, SegmentationStrategy segmentationStrategy) {
		this.source = new LineReaderWithEOL(source);
		this.segmentationStrategy = segmentationStrategy;
	}

	@Override
	public Token nextToken() {
		try {
			if (tokens.isEmpty())
				fillTokens0();
			
			return tokens.poll();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	// only called when there is no token available
	// don't return without appending at least one token to tokens
	private void fillTokens0() throws IOException {
		String line = source.readLine();
		if (line == null) {
			tokens.add(new ChainedToken(null, Token.EOF, lineNumber, charsRead, Token.HIDDEN_CHANNEL, 0, 0, 0, this, null));
			return;
		}
		
		String[] segments = segmentationStrategy.segment(line);
		if (segments[0].length() > 0) {
			tokens.add(new ChainedToken(segments[0], SEQUENCE_NUMBER, lineNumber, charsRead, Token.HIDDEN_CHANNEL, 0, 0, 0, this, null));
			charsRead += segments[0].length();
		}
		
		if (segments[1].length() > 0) {
			int type = -2;
			if (" ".equals(segments[1]))
				type = INDICATOR_BLANK;
			if ("-".equals(segments[1]))
				type = INDICATOR_CONTINUATION;
			if ("*".equals(segments[1]))
				type = INDICATOR_COMMENT;
			tokens.add(new ChainedToken(segments[1], type, lineNumber, charsRead, Token.HIDDEN_CHANNEL, 0, 0, 0, this, null));
			charsRead += segments[1].length();
		}
		
		if (segments[2].length() > 0) {
			// do actual parsing
			tokens.add(new ChainedToken("IDENTIFICATION", IDENTIFICATION, lineNumber, charsRead, Token.DEFAULT_CHANNEL, 0, 0, 0, this, null));
			charsRead += "IDENTIFICATION".length();
			tokens.add(new ChainedToken(" ", WS, lineNumber, charsRead, Token.HIDDEN_CHANNEL, 0, 0, 0, this, null));
			charsRead += 1;
			tokens.add(new ChainedToken("DIVISION", DIVISION, lineNumber, charsRead, Token.DEFAULT_CHANNEL, 0, 0, 0, this, null));
			charsRead += "DIVISION".length();
			tokens.add(new ChainedToken(".", PERIOD, lineNumber, charsRead, Token.DEFAULT_CHANNEL, 0, 0, 0, this, null));
			charsRead += 1;
		}
		
		if (segments[3].length() > 0) {
			tokens.add(new ChainedToken(segments[3], SKIP_TO_EOL, lineNumber, charsRead, Token.HIDDEN_CHANNEL, 0, 0, 0, this, null));
			charsRead += segments[3].length();
		}
		
		if (segments[4].length() > 0) {
			tokens.add(new ChainedToken(segments[4], NEWLINE, lineNumber, charsRead, Token.HIDDEN_CHANNEL, 0, 0, 0, this, null));
			lineNumber++;
			charsRead = 0;
		} else {
			tokens.add(new ChainedToken(null, Token.EOF, lineNumber, charsRead, Token.HIDDEN_CHANNEL, 0, 0, 0, this, null));
		}
	}

	@Override
	public int getLine() {
		return 0;
	}

	@Override
	public int getCharPositionInLine() {
		return 0;
	}

	@Override
	public CharStream getInputStream() {
		throw new UnsupportedOperationException();
	}

	@Override
	public String getSourceName() {
		return null;
	}

	@Override
	public void setTokenFactory(TokenFactory<?> factory) {
		throw new UnsupportedOperationException();
	}

	@Override
	public TokenFactory<?> getTokenFactory() {
		throw new UnsupportedOperationException();
	}
}

//private static final String[] FIXED_STRINGS;
//
//static {
//	try {
//		final Field FIELD_SYMBOLIC_NAMES = COBOLParser.class.getDeclaredField("_SYMBOLIC_NAMES");
//		FIELD_SYMBOLIC_NAMES.setAccessible(true);
//		final String[] _SYMBOLIC_NAMES = (String[]) FIELD_SYMBOLIC_NAMES.get(null);
//
//		FIXED_STRINGS = new String[_SYMBOLIC_NAMES.length];
//		
//		int i = 0;
//		while(!"FOLLOWING_ARE_FIXEDSTRINGS".equals(_SYMBOLIC_NAMES[i++]));
//		
//		for (; i < _SYMBOLIC_NAMES.length; i++)
//			FIXED_STRINGS[i] = _SYMBOLIC_NAMES[i];
//		
//	} catch (Exception e) {
//		throw new RuntimeException(e);
//	}
//}
