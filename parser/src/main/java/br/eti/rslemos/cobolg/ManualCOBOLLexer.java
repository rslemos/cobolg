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

	private ChainedToken eof;

	private String[] segments = new String[5];

	private int lineType;
	private int tokenCount;

	private ChainedToken head;

	private int position;
	
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
		int state = 0;
		
		do {
			String code = readNextLine();

			if (lineType != INDICATOR_CONTINUATION) {
				switch (state) {
				case 0:
					break;
				case 1:
					// impossible
					break;
				case 2:
					System.err.printf("unterminated string\n");
					break;
				case 3:
					head.type = figureWord(head.getText());
					break;
				}
				
				state = 0;
			}
			
			if (code != null)
				state = parseCode0(state, code);
			
		} while (state != 0);
	}

	private int parseCode0(int state, String code) {
		int begin = -1;
		
		for (int i = 0; i < code.length(); i++) {
			int la = code.charAt(i);

			switch (state) {
			case 0: // ground
				begin = i;
				if (la == ' ' || la == '\t')
					state = 1;
				if (la == '"' || la == '\'')
					state = 2;
				if (la >= 'A' && la <= 'Z')
					state = 3;
				if (la == '.')
					newToken(code.substring(i, i+1), PERIOD, Token.DEFAULT_CHANNEL);
				break;
			case 1: // recognizing spaces
				if (!(la == ' ' || la == '\t')) {
					newToken(code.substring(begin, i), WS, Token.HIDDEN_CHANNEL);
					i--; // reprocess
					state = 0;
				}
				break;
			case 2: // recognizing strings
				if (!(la == '"' || la == '\'')) {
					newToken(code.substring(begin, i), QUOTEDSTRING, Token.DEFAULT_CHANNEL);
					i--; // reprocess
					state = 0;
				}
				break;
			case 3: // recognizing words
				if (!(la >= 'A' && la <= 'Z')) {
					String word = code.substring(begin, i);
					int type = figureWord(word);
					newToken(word, type, Token.DEFAULT_CHANNEL);
					i--; // reprocess
					state = 0;
				}
				break;
			}
		}
		
		switch (state) {
		case 0:
			head = null;
			break;
		case 1:
			newToken(code.substring(begin, code.length()), WS, Token.HIDDEN_CHANNEL);
			state = 0;
			head = null;
			break;
		case 2:
			head = newToken(code.substring(begin, code.length()), QUOTEDSTRING, Token.DEFAULT_CHANNEL);
			break;
		case 3:
			String word = code.substring(begin, code.length());
			head = newToken(word, USERDEFINEDWORD, Token.DEFAULT_CHANNEL);
		}
		
		return state;
	}

	private int figureWord(String word) {
		int type = -2;
		if ("IDENTIFICATION".equals(word))
			type = IDENTIFICATION;
		if ("DIVISION".equals(word))
			type = DIVISION;
		return type;
	}

	private ChainedToken newToken(String text, int type, int channel) {
		// refuse to create empty token
		if (text != null && text.length() > 0) {
			ChainedToken token = newToken0(text, type, channel);
			charsRead += text.length();
			position += text.length();
			return token;
		} else
			return null;
	}

	private ChainedToken newToken0(String text, int type, int channel) {
		ChainedToken token = new ChainedToken(text, type, lineNumber, charsRead, channel, ++tokenCount, position, 0, this, null);
		tokens.add(token);
		return token;
	}
	
	private ChainedToken eof() {
		if (eof == null)
			eof = newToken0(null, Token.EOF, Token.HIDDEN_CHANNEL);
		
		return eof;
	}
	
	private String readNextLine() throws IOException {
		newToken(segments[3], SKIP_TO_EOL, Token.HIDDEN_CHANNEL);
		
		if (newToken(segments[4], NEWLINE, Token.HIDDEN_CHANNEL) != null) {
			lineNumber++;
			charsRead = 0;
		}

		lineType = -2;
		String line = source.readLine();
		
		if (line == null) {
			eof();
			segments = null;
			return null;
		} else {
			segments = segmentationStrategy.segment(line);
			
			newToken(segments[0], SEQUENCE_NUMBER, Token.HIDDEN_CHANNEL);
			
			if (" ".equals(segments[1]))
				lineType = INDICATOR_BLANK;
			if ("-".equals(segments[1]))
				lineType = INDICATOR_CONTINUATION;
			if ("*".equals(segments[1]))
				lineType = INDICATOR_COMMENT;
			
			newToken(segments[1], lineType, Token.HIDDEN_CHANNEL);
			
			return segments[2];
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
