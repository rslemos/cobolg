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

import static br.eti.rslemos.cobolg.COBOLFixedPreLexer.*;
import static br.eti.rslemos.cobolg.TextHelper.join;

import java.io.IOException;
import java.io.Reader;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.TokenSource;
import org.junit.Test;

public class FixedFormatPreLexerUnitTest extends AbstractLexerUnitTest {

	@Test
	public void testEmptyLine() throws Exception {
		setSource("                                                                                ");
		
		matchToken(SEQUENCE_NUMBER,        "      ");
		matchToken(INDICATOR_CODE,         " ");
		matchToken(SOURCE,                 "                                                                 ");
		matchToken(TAIL,                   "        ");
		
		matchEOF();
	}

	@Test
	public void testSequenceAreaLine() throws Exception {
		setSource("012345                                                                          ");
		
		matchToken(SEQUENCE_NUMBER,        "012345");
		matchToken(INDICATOR_CODE,         " ");
		matchToken(SOURCE,                 "                                                                 ");
		matchToken(TAIL,                   "        ");
		
		matchEOF();
	}

	@Test
	public void testSkipToEolLine() throws Exception {
		setSource("                                                                        IGNORED+");
		
		matchToken(SEQUENCE_NUMBER,        "      ");
		matchToken(INDICATOR_CODE,         " ");
		matchToken(SOURCE,                 "                                                                 ");
		matchToken(TAIL,                   "IGNORED+");
		
		matchEOF();
	}

	@Test
	public void testCommentLine() throws Exception {
		setSource("000001*THIS IS A COMMENT LINE                                           IGNORED+");
		
		matchToken(SEQUENCE_NUMBER,        "000001");
		matchToken(INDICATOR_COMMENT,      "*");
		matchToken(SOURCE,                 "THIS IS A COMMENT LINE                                           ");
		matchToken(TAIL,                   "IGNORED+");
		
		matchEOF();
	}
	
	@Test
	public void testInlineComment() throws Exception {
		setSource("000001 X. *> INLINE COMMENT UNTIL END OF AREA B                         IGNORED+");
		
		matchToken(SEQUENCE_NUMBER,        "000001");
		matchToken(INDICATOR_CODE,         " ");
		matchToken(SOURCE,                 "X. *> INLINE COMMENT UNTIL END OF AREA B                         ");
		matchToken(TAIL,                   "IGNORED+");
		
		matchEOF();
	}

	@Test
	public void testDoubleQuotedStart() throws Exception {
		setSource("000000 \"DOUBLE QUOTED START                                             IGNORED+");
		
		matchToken(SEQUENCE_NUMBER,        "000000");
		matchToken(INDICATOR_CODE,         " ");
		matchToken(SOURCE,                 "\"DOUBLE QUOTED START                                             ");
		matchToken(TAIL,                   "IGNORED+");
		
		matchEOF();
	}


	@Test
	public void testDoubleQuotedWithDoubleQuotesStart() throws Exception {
		setSource("000000 \"DOUBLE QUOTED START WITH \"\"DOUBLE QUOTES\"\"                      IGNORED+");
		
		matchToken(SEQUENCE_NUMBER,        "000000");
		matchToken(INDICATOR_CODE,         " ");
		matchToken(SOURCE,                 "\"DOUBLE QUOTED START WITH \"\"DOUBLE QUOTES\"\"                      ");
		matchToken(TAIL,                   "IGNORED+");
		
		matchEOF();
	}

	@Test
	public void testDoubleQuoteContinuationString() throws Exception {
		setSource(join(
			"000000 \"DOUBLE QUOTED START WITH \"\"DOUBLE QUOTES\"\" THAT LEAKS INTO THE NIGNORED+",
			"000001-    \"EXT LINE\"                                                   IGNORED+"
		));
		
		// 1st line
		matchToken(SEQUENCE_NUMBER,        "000000");
		matchToken(INDICATOR_CODE,         " ");
		matchToken(SOURCE,                 "\"DOUBLE QUOTED START WITH \"\"DOUBLE QUOTES\"\" THAT LEAKS INTO THE N");
		matchToken(TAIL,                   "IGNORED+");
		matchToken(NEWLINE,                "\n");
		
		// 2nd line
		matchToken(SEQUENCE_NUMBER,        "000001");
		matchToken(INDICATOR_CONTINUATION, "-");
		matchToken(SOURCE,                 "    \"EXT LINE\"                                                   ");
		matchToken(TAIL,                   "IGNORED+");
		
		matchEOF();
	}

	@Test
	public void testDoubleQuoteContinuationStringIn3Lines() throws Exception {
		setSource(join(
			"000000 \"DOUBLE QUOTED START WITH \"\"DOUBLE QUOTES\"\" THAT LEAKS INTO THE NIGNORED+",
			"000001-    \"EXT LINE BUT KEEPS GOING OVER AND OVER AND OVER AND OVER ANDIGNORED+",
			"000002-    \"OVER AGAIN UNTIL THE 3RD LINE\"                              IGNORED+"
		));
		
		// 1st line
		matchToken(SEQUENCE_NUMBER,        "000000");
		matchToken(INDICATOR_CODE,         " ");
		matchToken(SOURCE,                 "\"DOUBLE QUOTED START WITH \"\"DOUBLE QUOTES\"\" THAT LEAKS INTO THE N");
		matchToken(TAIL,                   "IGNORED+");
		matchToken(NEWLINE,                "\n");
		
		// 2nd line
		matchToken(SEQUENCE_NUMBER,        "000001");
		matchToken(INDICATOR_CONTINUATION, "-");
		matchToken(SOURCE,                 "    \"EXT LINE BUT KEEPS GOING OVER AND OVER AND OVER AND OVER AND");
		matchToken(TAIL,                   "IGNORED+");
		matchToken(NEWLINE,                "\n");
		
		// 3rd line
		matchToken(SEQUENCE_NUMBER,        "000002");
		matchToken(INDICATOR_CONTINUATION, "-");
		matchToken(SOURCE,                 "    \"OVER AGAIN UNTIL THE 3RD LINE\"                              ");
		matchToken(TAIL,                   "IGNORED+");
		
		matchEOF();
	}

	@Test
	public void testLineShortOnSequence() throws Exception {
		setSource("12345");
		
		matchToken(SEQUENCE_NUMBER_SHORT,  "12345");
		
		matchEOF();
	}

	@Test
	public void testLineShortOnIndicator() throws Exception {
		// explicit newline because otherwise would hit EOF before INDICATOR_SHORT
		setSource("123456\n");
		
		matchToken(SEQUENCE_NUMBER,        "123456");
		matchToken(INDICATOR_SHORT,        "");
		matchToken(NEWLINE,                "\n");
		
		matchEOF();
	}

	@Test
	public void testLineShortOnSource() throws Exception {
		setSource("123456 IDENTIFICATION DIVISION.");
		
		matchToken(SEQUENCE_NUMBER,        "123456");
		matchToken(INDICATOR_CODE,         " ");
		matchToken(SOURCE_SHORT,           "IDENTIFICATION DIVISION.");
		
		matchEOF();
	}

	@Override
	protected TokenSource getLexer(Reader reader) throws IOException {
		return new COBOLFixedPreLexer(new ANTLRInputStream(reader));
	}
}
