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

import static br.eti.rslemos.cobolg.COBOLFixedFormatLexer.*;
import static br.eti.rslemos.cobolg.TextHelper.join;
import org.junit.Test;

import br.eti.rslemos.cobolg.Compiler.FixedFormatCompiler;

public class FixedFormatLexerUnitTest extends AbstractLexerUnitTest {

	public FixedFormatLexerUnitTest() {
		super(new FixedFormatCompiler());
	}

	@Test
	public void testEmptyLine() throws Exception {
		setSource("                                                                                ");
		
		matchToken(                  TO_SEQUENCE_MODE,          "\uEBA0",   MARK  );
		matchToken(SEQUENCE_MODE,    SEQUENCE_NUMBER,           "      ",   HIDDEN);
		matchToken(SEQUENCE_MODE,    TO_INDICATOR_MODE,         "\uEBA1",   MARK  );
		matchToken(INDICATOR_MODE,   INDICATOR_BLANK,           " ",        HIDDEN);
		matchToken(PRE_DEFAULT_MODE, TO_DEFAULT_MODE,           "\uEBA2",   MARK  );
		matchToken(                  WS,                        "                                                                 ", HIDDEN);
		matchToken(                  TO_SKIPTOEOL_MODE_DEFAULT, "\uEBA3",   MARK  );
		matchToken(SKIPTOEOL_MODE,   SKIP_TO_EOL,               "        ", HIDDEN);
		
		matchEOF();
	}

	@Test
	public void testSequenceAreaLine() throws Exception {
		setSource("012345                                                                          ");
		
		matchToken(                  TO_SEQUENCE_MODE,          "\uEBA0",   MARK  );
		matchToken(SEQUENCE_MODE,    SEQUENCE_NUMBER,           "012345",   HIDDEN);
		matchToken(SEQUENCE_MODE,    TO_INDICATOR_MODE,         "\uEBA1",   MARK  );
		matchToken(INDICATOR_MODE,   INDICATOR_BLANK,           " ",        HIDDEN);
		matchToken(PRE_DEFAULT_MODE, TO_DEFAULT_MODE,           "\uEBA2",   MARK  );
		matchToken(                  WS,                        "                                                                 ", HIDDEN);
		matchToken(                  TO_SKIPTOEOL_MODE_DEFAULT, "\uEBA3",   MARK  );
		matchToken(SKIPTOEOL_MODE,   SKIP_TO_EOL,               "        ", HIDDEN);
		
		matchEOF();
	}

	@Test
	public void testSkipToEolLine() throws Exception {
		setSource("                                                                        IGNORED+");
		
		matchToken(                  TO_SEQUENCE_MODE,          "\uEBA0",   MARK  );
		matchToken(SEQUENCE_MODE,    SEQUENCE_NUMBER,           "      ",   HIDDEN);
		matchToken(SEQUENCE_MODE,    TO_INDICATOR_MODE,         "\uEBA1",   MARK  );
		matchToken(INDICATOR_MODE,   INDICATOR_BLANK,           " ",        HIDDEN);
		matchToken(PRE_DEFAULT_MODE, TO_DEFAULT_MODE,           "\uEBA2",   MARK  );
		matchToken(                  WS,                        "                                                                 ", HIDDEN);
		matchToken(                  TO_SKIPTOEOL_MODE_DEFAULT, "\uEBA3",   MARK  );
		matchToken(SKIPTOEOL_MODE,   SKIP_TO_EOL,               "IGNORED+", HIDDEN);
		
		matchEOF();
	}

	@Test
	public void testCommentLine() throws Exception {
		setSource("000001*THIS IS A COMMENT LINE                                           IGNORED+");
		
		matchToken(                  TO_SEQUENCE_MODE,          "\uEBA0",   MARK  );
		matchToken(SEQUENCE_MODE,    SEQUENCE_NUMBER,           "000001",   HIDDEN);
		matchToken(SEQUENCE_MODE,    TO_INDICATOR_MODE,         "\uEBA1",   MARK  );
		matchToken(INDICATOR_MODE,   INDICATOR_COMMENT,         "*",        HIDDEN);
		matchToken(PRE_COMMENT_MODE, TO_COMMENT_MODE,           "\uEBA2",   MARK  );
		matchToken(COMMENT_MODE,     FIXEDCOMMENT,              "THIS IS A COMMENT LINE                                           ", HIDDEN);
		matchToken(COMMENT_MODE,     TO_SKIPTOEOL_MODE_COMMENT, "\uEBA3",   MARK  );
		matchToken(SKIPTOEOL_MODE,   SKIP_TO_EOL,               "IGNORED+", HIDDEN);
		
		matchEOF();
	}

	@Test
	public void testDoubleQuotedStart() throws Exception {
		setSource("000000 \"DOUBLE QUOTED START                                             IGNORED+");
		
		matchToken(                  TO_SEQUENCE_MODE,          "\uEBA0",   MARK  );
		matchToken(SEQUENCE_MODE,    SEQUENCE_NUMBER,           "000000",   HIDDEN);
		matchToken(SEQUENCE_MODE,    TO_INDICATOR_MODE,         "\uEBA1",   MARK  );
		matchToken(INDICATOR_MODE,   INDICATOR_BLANK,           " ",        HIDDEN);
		matchToken(PRE_DEFAULT_MODE, TO_DEFAULT_MODE,           "\uEBA2",   MARK  );
		matchToken(                  DOUBLEQUOTEDSTRING_START,  "\"DOUBLE QUOTED START                                             ");
		matchToken(                  TO_SKIPTOEOL_MODE_DEFAULT, "\uEBA3",   MARK  );
		matchToken(SKIPTOEOL_MODE,   SKIP_TO_EOL,               "IGNORED+", HIDDEN);
		
		matchEOF();
	}


	@Test
	public void testDoubleQuotedWithDoubleQuotesStart() throws Exception {
		setSource("000000 \"DOUBLE QUOTED START WITH \"\"DOUBLE QUOTES\"\"                      IGNORED+");
		
		matchToken(                  TO_SEQUENCE_MODE,          "\uEBA0",   MARK  );
		matchToken(SEQUENCE_MODE,    SEQUENCE_NUMBER,           "000000",   HIDDEN);
		matchToken(SEQUENCE_MODE,    TO_INDICATOR_MODE,         "\uEBA1",   MARK  );
		matchToken(INDICATOR_MODE,   INDICATOR_BLANK,           " ",        HIDDEN);
		matchToken(PRE_DEFAULT_MODE, TO_DEFAULT_MODE,           "\uEBA2",   MARK  );
		matchToken(                  DOUBLEQUOTEDSTRING_START,  "\"DOUBLE QUOTED START WITH \"\"DOUBLE QUOTES\"\"                      ");
		matchToken(                  TO_SKIPTOEOL_MODE_DEFAULT, "\uEBA3",   MARK  );
		matchToken(SKIPTOEOL_MODE,   SKIP_TO_EOL,               "IGNORED+", HIDDEN);
		
		matchEOF();
	}

	@Test
	public void testDoubleQuoteContinuationString() throws Exception {
		setSource(join(
			"000000 \"DOUBLE QUOTED START WITH \"\"DOUBLE QUOTES\"\" THAT LEAKS INTO THE NIGNORED+",
			"000001-    \"EXT LINE\"                                                   IGNORED+"
		));
		
		// 1st line
		matchToken(                  TO_SEQUENCE_MODE,          "\uEBA0",   MARK  );
		matchToken(SEQUENCE_MODE,    SEQUENCE_NUMBER,           "000000",   HIDDEN);
		matchToken(SEQUENCE_MODE,    TO_INDICATOR_MODE,         "\uEBA1",   MARK  );
		matchToken(INDICATOR_MODE,   INDICATOR_BLANK,           " ",        HIDDEN);
		matchToken(PRE_DEFAULT_MODE, TO_DEFAULT_MODE,           "\uEBA2",   MARK  );
		matchToken(                  DOUBLEQUOTEDSTRING_START,  "\"DOUBLE QUOTED START WITH \"\"DOUBLE QUOTES\"\" THAT LEAKS INTO THE N");
		matchToken(                  TO_SKIPTOEOL_MODE_DEFAULT, "\uEBA3",   MARK  );
		matchToken(SKIPTOEOL_MODE,   SKIP_TO_EOL,               "IGNORED+", HIDDEN);
		matchToken(SKIPTOEOL_MODE,   SKIPTOEOL_MODE_NL,         "\n",       HIDDEN);
		
		// 2nd line
		matchToken(                       TO_SEQUENCE_MODE,          "\uEBA0",   MARK  );
		matchToken(SEQUENCE_MODE,         SEQUENCE_NUMBER,           "000001",   HIDDEN);
		matchToken(SEQUENCE_MODE,         TO_INDICATOR_MODE,         "\uEBA1",   MARK  );
		matchToken(INDICATOR_MODE,        INDICATOR_CONTINUATION,    "-",        HIDDEN);
		matchToken(PRE_CONTINUATION_MODE, TO_CONTINUATION_MODE,      "\uEBA2",   MARK  );
		matchToken(CONTINUATION_MODE,     WS_CONT,                   "    ",     HIDDEN);
		matchToken(CONTINUATION_MODE,     DOUBLEQUOTEDSTRING_END,    "\"EXT LINE\""      );
		matchToken(                       WS,                        "                                                   ", HIDDEN);
		matchToken(                       TO_SKIPTOEOL_MODE_DEFAULT, "\uEBA3",   MARK  );
		matchToken(SKIPTOEOL_MODE,        SKIP_TO_EOL,               "IGNORED+", HIDDEN);
		
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
		matchToken(                  TO_SEQUENCE_MODE,          "\uEBA0",   MARK  );
		matchToken(SEQUENCE_MODE,    SEQUENCE_NUMBER,           "000000",   HIDDEN);
		matchToken(SEQUENCE_MODE,    TO_INDICATOR_MODE,         "\uEBA1",   MARK  );
		matchToken(INDICATOR_MODE,   INDICATOR_BLANK,           " ",        HIDDEN);
		matchToken(PRE_DEFAULT_MODE, TO_DEFAULT_MODE,           "\uEBA2",   MARK  );
		matchToken(                  DOUBLEQUOTEDSTRING_START,  "\"DOUBLE QUOTED START WITH \"\"DOUBLE QUOTES\"\" THAT LEAKS INTO THE N");
		matchToken(                  TO_SKIPTOEOL_MODE_DEFAULT, "\uEBA3",   MARK  );
		matchToken(SKIPTOEOL_MODE,   SKIP_TO_EOL,               "IGNORED+", HIDDEN);
		matchToken(SKIPTOEOL_MODE,   SKIPTOEOL_MODE_NL,         "\n",       HIDDEN);
		
		// 2nd line
		matchToken(                       TO_SEQUENCE_MODE,          "\uEBA0",   MARK  );
		matchToken(SEQUENCE_MODE,         SEQUENCE_NUMBER,           "000001",   HIDDEN);
		matchToken(SEQUENCE_MODE,         TO_INDICATOR_MODE,         "\uEBA1",   MARK  );
		matchToken(INDICATOR_MODE,        INDICATOR_CONTINUATION,    "-",        HIDDEN);
		matchToken(PRE_CONTINUATION_MODE, TO_CONTINUATION_MODE,      "\uEBA2",   MARK  );
		matchToken(CONTINUATION_MODE,     WS_CONT,                   "    ",     HIDDEN);
		matchToken(CONTINUATION_MODE,     DOUBLEQUOTEDSTRING_MID,    "\"EXT LINE BUT KEEPS GOING OVER AND OVER AND OVER AND OVER AND");
		matchToken(CONTINUATION_MODE,     TO_SKIPTOEOL_MODE_CONTINUATION, "\uEBA3",     MARK  );
		matchToken(SKIPTOEOL_MODE,        SKIP_TO_EOL,               "IGNORED+", HIDDEN);
		matchToken(SKIPTOEOL_MODE,        SKIPTOEOL_MODE_NL,         "\n",       HIDDEN);
		
		// 3rd line
		matchToken(                       TO_SEQUENCE_MODE,          "\uEBA0",   MARK  );
		matchToken(SEQUENCE_MODE,         SEQUENCE_NUMBER,           "000002",   HIDDEN);
		matchToken(SEQUENCE_MODE,         TO_INDICATOR_MODE,         "\uEBA1",   MARK  );
		matchToken(INDICATOR_MODE,        INDICATOR_CONTINUATION,    "-",        HIDDEN);
		matchToken(PRE_CONTINUATION_MODE, TO_CONTINUATION_MODE,      "\uEBA2",   MARK  );
		matchToken(CONTINUATION_MODE,     WS_CONT,                   "    ",     HIDDEN);
		matchToken(CONTINUATION_MODE,     DOUBLEQUOTEDSTRING_END,    "\"OVER AGAIN UNTIL THE 3RD LINE\"");
		matchToken(                       WS,                        "                              ", HIDDEN);
		matchToken(                       TO_SKIPTOEOL_MODE_DEFAULT, "\uEBA3",   MARK  );
		matchToken(SKIPTOEOL_MODE,        SKIP_TO_EOL,               "IGNORED+", HIDDEN);
		
		matchEOF();
	}
}
