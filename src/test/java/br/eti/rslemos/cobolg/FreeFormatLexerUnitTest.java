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
		
		//FILE SECTION.
		matchToken(FILE, "FILE");
		matchToken(WS, " ", HIDDEN);
		matchToken(SECTION, "SECTION");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);

		//FD  FD0
		matchToken(FD, "FD");
		matchToken(WS, "  ", HIDDEN);
		matchToken(ID, "FD0");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    BLOCK CONTAINS 5 TO 100 RECORDS
		matchToken(WS, "    ", HIDDEN);
		matchToken(BLOCK, "BLOCK");
		matchToken(WS, " ", HIDDEN);
		matchToken(CONTAINS, "CONTAINS");
		matchToken(WS, " ", HIDDEN);
		matchToken(INTEGER, "5");
		matchToken(WS, " ", HIDDEN);
		matchToken(TO, "TO");
		matchToken(WS, " ", HIDDEN);
		matchToken(INTEGER, "100");
		matchToken(WS, " ", HIDDEN);
		matchToken(RECORDS, "RECORDS");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    RECORD CONTAINS 80 TO 120 CHARACTERS
		matchToken(WS, "    ", HIDDEN);
		matchToken(RECORD, "RECORD");
		matchToken(WS, " ", HIDDEN);
		matchToken(CONTAINS, "CONTAINS");
		matchToken(WS, " ", HIDDEN);
		matchToken(INTEGER, "80");
		matchToken(WS, " ", HIDDEN);
		matchToken(TO, "TO");
		matchToken(WS, " ", HIDDEN);
		matchToken(INTEGER, "120");
		matchToken(WS, " ", HIDDEN);
		matchToken(CHARACTERS, "CHARACTERS");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    LABEL RECORD IS STANDARD.
		matchToken(WS, "    ", HIDDEN);
		matchToken(LABEL, "LABEL");
		matchToken(WS, " ", HIDDEN);
		matchToken(RECORD, "RECORD");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(STANDARD, "STANDARD");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//FD  FD1
		matchToken(FD, "FD");
		matchToken(WS, "  ", HIDDEN);
		matchToken(ID, "FD1");
		matchToken(NEWLINE, "\n", HIDDEN);

		//    BLOCK CONTAINS 120 CHARACTERS
		matchToken(WS, "    ", HIDDEN);
		matchToken(BLOCK, "BLOCK");
		matchToken(WS, " ", HIDDEN);
		matchToken(CONTAINS, "CONTAINS");
		matchToken(WS, " ", HIDDEN);
		matchToken(INTEGER, "120");
		matchToken(WS, " ", HIDDEN);
		matchToken(CHARACTERS, "CHARACTERS");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    RECORD IS VARYING IN SIZE FROM 10 TO 120 CHARACTERS
		matchToken(WS, "    ", HIDDEN);
		matchToken(RECORD, "RECORD");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(VARYING, "VARYING");
		matchToken(WS, " ", HIDDEN);
		matchToken(IN, "IN");
		matchToken(WS, " ", HIDDEN);
		matchToken(SIZE, "SIZE");
		matchToken(WS, " ", HIDDEN);
		matchToken(FROM, "FROM");
		matchToken(WS, " ", HIDDEN);
		matchToken(INTEGER, "10");
		matchToken(WS, " ", HIDDEN);
		matchToken(TO, "TO");
		matchToken(WS, " ", HIDDEN);
		matchToken(INTEGER, "120");
		matchToken(WS, " ", HIDDEN);
		matchToken(CHARACTERS, "CHARACTERS");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//      DEPENDING ON REC-SIZE.
		matchToken(WS, "      ", HIDDEN);
		matchToken(DEPENDING, "DEPENDING");
		matchToken(WS, " ", HIDDEN);
		matchToken(ON, "ON");
		matchToken(WS, " ", HIDDEN);
		matchToken(ID, "REC-SIZE");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//WORKING-STORAGE SECTION.
		matchToken(WORKING_STORAGE, "WORKING-STORAGE");
		matchToken(WS, " ", HIDDEN);
		matchToken(SECTION, "SECTION");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//77  WS-DEBUG             PIC ZZZ.ZZZ.ZZZ.ZZ9,999999-.
		matchToken(INTEGER, "77");
		matchToken(WS, "  ", HIDDEN);
		matchToken(ID, "WS-DEBUG");
		matchToken(WS, "             ", HIDDEN);
		matchToken(PICTURE, "PIC");
		matchToken(PICTURE_MODE, PIC_WS, " ", HIDDEN);
		matchToken(PICTURE_MODE, PICTURESTRING, "ZZZ.ZZZ.ZZZ.ZZ9,999999-");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//77  WS-DEBUG1            PIC S9(8) COMP VALUE IS ZERO.
		matchToken(INTEGER, "77");
		matchToken(WS, "  ", HIDDEN);
		matchToken(ID, "WS-DEBUG1");
		matchToken(WS, "            ", HIDDEN);
		matchToken(PICTURE, "PIC");
		matchToken(PICTURE_MODE, PIC_WS, " ", HIDDEN);
		matchToken(PICTURE_MODE, PICTURESTRING, "S9(8)");
		matchToken(WS, " ", HIDDEN);
		matchToken(COMPUTATIONAL, "COMP");
		matchToken(WS, " ", HIDDEN);
		matchToken(VALUE, "VALUE");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(ZERO, "ZERO");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//01  WS-TAB-F-PRICE.
		matchToken(INTEGER, "01");
		matchToken(WS, "  ", HIDDEN);
		matchToken(ID, "WS-TAB-F-PRICE");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    03  WS-TB-F-PRICE OCCURS 1000 TIMES
		matchToken(WS, "    ", HIDDEN);
		matchToken(INTEGER, "03");
		matchToken(WS, "  ", HIDDEN);
		matchToken(ID, "WS-TB-F-PRICE");
		matchToken(WS, " ", HIDDEN);
		matchToken(OCCURS, "OCCURS");		
		matchToken(WS, " ", HIDDEN);
		matchToken(INTEGER, "1000");
		matchToken(WS, " ", HIDDEN);
		matchToken(TIMES, "TIMES");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//        INDEXED BY IPRICE IPRICEUM IPRICEMIL IPRICELIMLOG
		matchToken(WS, "        ", HIDDEN);
		matchToken(INDEXED, "INDEXED");
		matchToken(WS, " ", HIDDEN);
		matchToken(BY, "BY");
		matchToken(WS, " ", HIDDEN);
		matchToken(ID, "IPRICE");		
		matchToken(WS, " ", HIDDEN);
		matchToken(ID, "IPRICEUM");		
		matchToken(WS, " ", HIDDEN);
		matchToken(ID, "IPRICEMIL");		
		matchToken(WS, " ", HIDDEN);
		matchToken(ID, "IPRICELIMLOG");		
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//                   IPRICELIMLOGANT.
		matchToken(WS, "                   ", HIDDEN);
		matchToken(ID, "IPRICELIMLOGANT");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//01  DESL17V00 REDEFINES DESL12V05 PIC S9(17) COMP-3.
		matchToken(INTEGER, "01");
		matchToken(WS, "  ", HIDDEN);
		matchToken(ID, "DESL17V00");
		matchToken(WS, " ", HIDDEN);
		matchToken(REDEFINES, "REDEFINES");
		matchToken(WS, " ", HIDDEN);
		matchToken(ID, "DESL12V05");
		matchToken(WS, " ", HIDDEN);
		matchToken(PICTURE, "PIC");
		matchToken(PICTURE_MODE, PIC_WS, " ", HIDDEN);
		matchToken(PICTURE_MODE, PICTURESTRING, "S9(17)");
		matchToken(WS, " ", HIDDEN);
		matchToken(COMPUTATIONAL_3, "COMP-3");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//77  WS-DEBUG2            VALUE IS ZERO PIC S9(8) COMP.
		matchToken(INTEGER, "77");
		matchToken(WS, "  ", HIDDEN);
		matchToken(ID, "WS-DEBUG2");
		matchToken(WS, "            ", HIDDEN);
		matchToken(VALUE, "VALUE");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(ZERO, "ZERO");
		matchToken(WS, " ", HIDDEN);
		matchToken(PICTURE, "PIC");
		matchToken(PICTURE_MODE, PIC_WS, " ", HIDDEN);
		matchToken(PICTURE_MODE, PICTURESTRING, "S9(8)");
		matchToken(WS, " ", HIDDEN);
		matchToken(COMPUTATIONAL, "COMP");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//LINKAGE SECTION.
		matchToken(LINKAGE, "LINKAGE");
		matchToken(WS, " ", HIDDEN);
		matchToken(SECTION, "SECTION");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);

		//01  LE-ENDI.
		matchToken(INTEGER, "01");
		matchToken(WS, "  ", HIDDEN);
		matchToken(ID, "LE-ENDI");
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
