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
		
		matchToken(DOUBLEQUOTEDSTRING, "\"DOUBLE QUOTED STRING\"");
		
		matchEOF();
	}

	@Test
	public void testDoubleQuotedWithDoubleQuotesString() throws Exception {
		setSource("\"DOUBLE QUOTED STRING WITH \"\"DOUBLE QUOTES\"\"\"");
		
		matchToken(DOUBLEQUOTEDSTRING, "\"DOUBLE QUOTED STRING WITH \"\"DOUBLE QUOTES\"\"\"");
		
		matchEOF();
	}
	
	@Test
	public void testProgramInFreeFormatUnitTest() throws Exception {
		setSource(FreeFormatUnitTest.SOURCE);
		
		//IDENTIFICATION DIVISION.
		matchToken(IDENTIFICATION, "IDENTIFICATION");
		matchToken(WS, " ", HIDDEN);
		matchToken(DIVISION, "DIVISION");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//PROGRAM-ID. HELLO-WORLD.
		matchToken(PROGRAM_ID, "PROGRAM-ID");
		matchToken(PERIOD, ".");
		matchToken(WS, " ", HIDDEN);
		matchToken(ID, "HELLO-WORLD");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//*COMMENT LINE\r
		matchToken(COMMENT, "*COMMENT LINE\r\n", HIDDEN);
		//matchToken(NEWLINE, "\r\n", HIDDEN);
		
		//ENVIRONMENT DIVISION.
		matchToken(ENVIRONMENT, "ENVIRONMENT");
		matchToken(WS, " ", HIDDEN);
		matchToken(DIVISION, "DIVISION");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//CONFIGURATION SECTION.
		matchToken(CONFIGURATION, "CONFIGURATION");
		matchToken(WS, " ", HIDDEN);
		matchToken(SECTION, "SECTION");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//OBJECT-COMPUTER. IBM-370-148.
		matchToken(OBJECT_COMPUTER, "OBJECT-COMPUTER");
		matchToken(PERIOD, ".");
		matchToken(WS, " ", HIDDEN);
		matchToken(ID, "IBM-370-148");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//SPECIAL-NAMES.
		matchToken(SPECIAL_NAMES, "SPECIAL-NAMES");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    C02 IS LCP-CH2.
		matchToken(WS, "    ", HIDDEN);
		matchToken(ID, "C02");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(ID, "LCP-CH2");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);

		//INPUT-OUTPUT SECTION.
		matchToken(INPUT_OUTPUT, "INPUT-OUTPUT");
		matchToken(WS, " ", HIDDEN);
		matchToken(SECTION, "SECTION");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);

		//FILE-CONTROL.
		matchToken(FILE_CONTROL, "FILE-CONTROL");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);

		//    SELECT  IMPRES      ASSIGN TO UT-S-L439161.
		matchToken(WS, "    ", HIDDEN);
		matchToken(SELECT, "SELECT");
		matchToken(WS, "  ", HIDDEN);
		matchToken(ID, "IMPRES");
		matchToken(WS, "      ", HIDDEN);
		matchToken(ASSIGN, "ASSIGN");
		matchToken(WS, " ", HIDDEN);
		matchToken(TO, "TO");
		matchToken(WS, " ", HIDDEN);
		matchToken(ID, "UT-S-L439161");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);

		//    SELECT  PRAMFIXO    ASSIGN TO UT-S-D433135.
		matchToken(WS, "    ", HIDDEN);
		matchToken(SELECT, "SELECT");
		matchToken(WS, "  ", HIDDEN);
		matchToken(ID, "PRAMFIXO");
		matchToken(WS, "    ", HIDDEN);
		matchToken(ASSIGN, "ASSIGN");
		matchToken(WS, " ", HIDDEN);
		matchToken(TO, "TO");
		matchToken(WS, " ", HIDDEN);
		matchToken(ID, "UT-S-D433135");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);

		//    SELECT  PROJEN-I    ASSIGN TO D433131
		matchToken(WS, "    ", HIDDEN);
		matchToken(SELECT, "SELECT");
		matchToken(WS, "  ", HIDDEN);
		matchToken(ID, "PROJEN-I");
		matchToken(WS, "    ", HIDDEN);
		matchToken(ASSIGN, "ASSIGN");
		matchToken(WS, " ", HIDDEN);
		matchToken(TO, "TO");
		matchToken(WS, " ", HIDDEN);
		matchToken(ID, "D433131");
		matchToken(NEWLINE, "\n", HIDDEN);

		//                        RECORD KEY CHAVE
		matchToken(WS, "                        ", HIDDEN);
		matchToken(RECORD, "RECORD");
		matchToken(WS, " ", HIDDEN);
		matchToken(KEY, "KEY");
		matchToken(WS, " ", HIDDEN);
		matchToken(ID, "CHAVE");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//                        ACCESS SEQUENTIAL
		matchToken(WS, "                        ", HIDDEN);
		matchToken(ACCESS, "ACCESS");
		matchToken(WS, " ", HIDDEN);
		matchToken(SEQUENTIAL, "SEQUENTIAL");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//                        STATUS IS PROJ-STATUS
		matchToken(WS, "                        ", HIDDEN);
		matchToken(STATUS, "STATUS");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(ID, "PROJ-STATUS");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//                        ORGANIZATION INDEXED.
		matchToken(WS, "                        ", HIDDEN);
		matchToken(ORGANIZATION, "ORGANIZATION");
		matchToken(WS, " ", HIDDEN);
		matchToken(INDEXED, "INDEXED");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//DATA DIVISION.
		matchToken(DATA, "DATA");
		matchToken(WS, " ", HIDDEN);
		matchToken(DIVISION, "DIVISION");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//PROCEDURE DIVISION.\r
		matchToken(PROCEDURE, "PROCEDURE");
		matchToken(WS, " ", HIDDEN);
		matchToken(DIVISION, "DIVISION");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\r\n", HIDDEN);
		
		//    DISPLAY 'Hello, world'.
		matchToken(WS, "    ", HIDDEN);
		matchToken(DISPLAY, "DISPLAY");
		matchToken(WS, " ", HIDDEN);
		matchToken(SINGLEQUOTEDSTRING, "'Hello, world'");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    STOP RUN.\r
		matchToken(WS, "    ", HIDDEN);
		matchToken(STOP, "STOP");
		matchToken(WS, " ", HIDDEN);
		matchToken(RUN, "RUN");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\r", HIDDEN);

		matchEOF();
	}
}
