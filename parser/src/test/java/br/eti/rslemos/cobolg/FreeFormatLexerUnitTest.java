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

import static br.eti.rslemos.cobolg.COBOLLexer.*;
import static br.eti.rslemos.cobolg.SimpleCompiler.lexerForFreeFormat;
import static br.eti.rslemos.cobolg.TextHelper.join;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.junit.Assert.assertThat;

import java.io.IOException;
import java.io.Reader;

import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenSource;
import org.junit.Test;

public class FreeFormatLexerUnitTest extends AbstractLexerUnitTest {

	@Test
	public void testCommentLine() throws Exception {
		setSource("*COMMENT LINE\n");
		
		matchToken(COMMENT, "*COMMENT LINE", HIDDEN);
		matchToken(NEWLINE, "\n", HIDDEN);
		
		matchEOF();
	}

	@Test
	public void testInlineComment() throws Exception {
		setSource("X *> INLINE COMMENT\n");
		
		matchToken(USERDEFINEDWORD, "X");
		matchToken(WS, " ", HIDDEN);
		matchToken(INLINECOMMENT, "*> INLINE COMMENT", HIDDEN);
		matchToken(NEWLINE, "\n", HIDDEN);

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
		
		matchToken(COMMENT, "*COMMENT LINE", HIDDEN);
		matchToken(NEWLINE, "\n", HIDDEN);
		
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
		
		matchToken(COMMENT, "*COMMENT LINE", HIDDEN);
		matchToken(NEWLINE, "\r\n", HIDDEN);
		
		matchEOF();
	}

	@Test
	public void testCommentLineWithCR() throws Exception {
		setSource("*COMMENT LINE\r");
		
		matchToken(COMMENT, "*COMMENT LINE", HIDDEN);
		matchToken(NEWLINE, "\r", HIDDEN);
		
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
		matchToken(USERDEFINEDWORD, "HELLO-WORLD");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//*COMMENT LINE\r
		matchToken(COMMENT, "*COMMENT LINE", HIDDEN);
		matchToken(NEWLINE, "\r\n", HIDDEN);
		
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
		matchToken(USERDEFINEDWORD, "IBM-370-148");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//SPECIAL-NAMES.
		matchToken(SPECIAL_NAMES, "SPECIAL-NAMES");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    C02 IS LCP-CH2
		matchToken(WS, "    ", HIDDEN);
		matchToken(USERDEFINEDWORD, "C02");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(USERDEFINEDWORD, "LCP-CH2");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    DECIMAL-POINT IS COMMA.
		matchToken(WS, "    ", HIDDEN);
		matchToken(DECIMAL_POINT, "DECIMAL-POINT");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(COMMA, "COMMA");
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
		matchToken(USERDEFINEDWORD, "IMPRES");
		matchToken(WS, "      ", HIDDEN);
		matchToken(ASSIGN, "ASSIGN");
		matchToken(WS, " ", HIDDEN);
		matchToken(TO, "TO");
		matchToken(WS, " ", HIDDEN);
		matchToken(USERDEFINEDWORD, "UT-S-L439161");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);

		//    SELECT  PRAMFIXO    ASSIGN TO UT-S-D433135.
		matchToken(WS, "    ", HIDDEN);
		matchToken(SELECT, "SELECT");
		matchToken(WS, "  ", HIDDEN);
		matchToken(USERDEFINEDWORD, "PRAMFIXO");
		matchToken(WS, "    ", HIDDEN);
		matchToken(ASSIGN, "ASSIGN");
		matchToken(WS, " ", HIDDEN);
		matchToken(TO, "TO");
		matchToken(WS, " ", HIDDEN);
		matchToken(USERDEFINEDWORD, "UT-S-D433135");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);

		//    SELECT  PROJEN-I    ASSIGN TO D433131
		matchToken(WS, "    ", HIDDEN);
		matchToken(SELECT, "SELECT");
		matchToken(WS, "  ", HIDDEN);
		matchToken(USERDEFINEDWORD, "PROJEN-I");
		matchToken(WS, "    ", HIDDEN);
		matchToken(ASSIGN, "ASSIGN");
		matchToken(WS, " ", HIDDEN);
		matchToken(TO, "TO");
		matchToken(WS, " ", HIDDEN);
		matchToken(USERDEFINEDWORD, "D433131");
		matchToken(NEWLINE, "\n", HIDDEN);

		//                        RECORD KEY CHAVE
		matchToken(WS, "                        ", HIDDEN);
		matchToken(RECORD, "RECORD");
		matchToken(WS, " ", HIDDEN);
		matchToken(KEY, "KEY");
		matchToken(WS, " ", HIDDEN);
		matchToken(USERDEFINEDWORD, "CHAVE");
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
		matchToken(USERDEFINEDWORD, "PROJ-STATUS");
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

		//FD  FD0 IS EXTERNAL IS GLOBAL
		matchToken(FD, "FD");
		matchToken(WS, "  ", HIDDEN);
		matchToken(USERDEFINEDWORD, "FD0");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(EXTERNAL, "EXTERNAL");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(GLOBAL, "GLOBAL");
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
		
		//    LABEL RECORD IS STANDARD
		matchToken(WS, "    ", HIDDEN);
		matchToken(LABEL, "LABEL");
		matchToken(WS, " ", HIDDEN);
		matchToken(RECORD, "RECORD");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(STANDARD, "STANDARD");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    VALUE OF SYSVAR1 IS 'SYSVAR1' SYSVAR2 IS 'SYSVAR2'
		matchToken(WS, "    ", HIDDEN);
		matchToken(VALUE, "VALUE");
		matchToken(WS, " ", HIDDEN);
		matchToken(OF, "OF");
		matchToken(WS, " ", HIDDEN);
		matchToken(USERDEFINEDWORD, "SYSVAR1");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(QUOTEDSTRING, "'SYSVAR1'");
		matchToken(WS, " ", HIDDEN);
		matchToken(USERDEFINEDWORD, "SYSVAR2");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(QUOTEDSTRING, "'SYSVAR2'");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    DATA RECORDS ARE REC1 REC2
		matchToken(WS, "    ", HIDDEN);
		matchToken(DATA, "DATA");
		matchToken(WS, " ", HIDDEN);
		matchToken(RECORDS, "RECORDS");
		matchToken(WS, " ", HIDDEN);
		matchToken(ARE, "ARE");
		matchToken(WS, " ", HIDDEN);
		matchToken(USERDEFINEDWORD, "REC1");
		matchToken(WS, " ", HIDDEN);
		matchToken(USERDEFINEDWORD, "REC2");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    LINAGE IS 2 LINES
		matchToken(WS, "    ", HIDDEN);
		matchToken(LINAGE, "LINAGE");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(INTEGER, "2");
		matchToken(WS, " ", HIDDEN);
		matchToken(LINES, "LINES");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//      WITH FOOTING AT 2
		matchToken(WS, "      ", HIDDEN);
		matchToken(WITH, "WITH");
		matchToken(WS, " ", HIDDEN);
		matchToken(FOOTING, "FOOTING");
		matchToken(WS, " ", HIDDEN);
		matchToken(AT, "AT");
		matchToken(WS, " ", HIDDEN);
		matchToken(INTEGER, "2");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//      LINES AT TOP 1
		matchToken(WS, "      ", HIDDEN);
		matchToken(LINES, "LINES");
		matchToken(WS, " ", HIDDEN);
		matchToken(AT, "AT");
		matchToken(WS, " ", HIDDEN);
		matchToken(TOP, "TOP");
		matchToken(WS, " ", HIDDEN);
		matchToken(INTEGER, "1");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//      LINES AT BOTTOM 1
		matchToken(WS, "      ", HIDDEN);
		matchToken(LINES, "LINES");
		matchToken(WS, " ", HIDDEN);
		matchToken(AT, "AT");
		matchToken(WS, " ", HIDDEN);
		matchToken(BOTTOM, "BOTTOM");
		matchToken(WS, " ", HIDDEN);
		matchToken(INTEGER, "1");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    RECORDING MODE IS V
		matchToken(WS, "    ", HIDDEN);
		matchToken(RECORDING, "RECORDING");
		matchToken(RECORDING_MODE, REC_WS, " ", HIDDEN);
		matchToken(RECORDING_MODE, REC_MODE, "MODE");
		matchToken(RECORDING_MODE, REC_WS, " ", HIDDEN);
		matchToken(RECORDING_MODE, REC_IS, "IS");
		matchToken(RECORDING_MODE, REC_WS, " ", HIDDEN);
		matchToken(RECORDING_MODE, V, "V");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    CODE-SET IS ALPHABET1.
		matchToken(WS, "    ", HIDDEN);
		matchToken(CODE_SET, "CODE-SET");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(USERDEFINEDWORD, "ALPHABET1");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//FD  FD1
		matchToken(FD, "FD");
		matchToken(WS, "  ", HIDDEN);
		matchToken(USERDEFINEDWORD, "FD1");
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
		matchToken(USERDEFINEDWORD, "REC-SIZE");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//FD  FD3  COPY XZT0190.
		matchToken(FD, "FD");
		matchToken(WS, "  ", HIDDEN);
		matchToken(USERDEFINEDWORD, "FD3");
		matchToken(WS, "  ", HIDDEN);
		matchToken(COPY, "COPY", COMPILER_CHANNEL);
		matchToken(COMPILER_ID_MODE, COMPILER_ID_WS, " ", HIDDEN);
		matchToken(COMPILER_ID_MODE, COMPILER_ID, "XZT0190", COMPILER_CHANNEL);
		matchToken(COMPILER_MODE, COMPILER_PERIOD, ".", COMPILER_CHANNEL);
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
		matchToken(USERDEFINEDWORD, "WS-DEBUG");
		matchToken(WS, "             ", HIDDEN);
		matchToken(PIC, "PIC");
		matchToken(PICTURE_MODE, PIC_WS, " ", HIDDEN);
		matchToken(PICTURE_MODE, PICTURESTRING, "ZZZ.ZZZ.ZZZ.ZZ9,999999-");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//77  WS-DEBUG1            PIC S9(8) COMP VALUE IS ZERO.
		matchToken(INTEGER, "77");
		matchToken(WS, "  ", HIDDEN);
		matchToken(USERDEFINEDWORD, "WS-DEBUG1");
		matchToken(WS, "            ", HIDDEN);
		matchToken(PIC, "PIC");
		matchToken(PICTURE_MODE, PIC_WS, " ", HIDDEN);
		matchToken(PICTURE_MODE, PICTURESTRING, "S9(8)");
		matchToken(WS, " ", HIDDEN);
		matchToken(COMP, "COMP");
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
		matchToken(USERDEFINEDWORD, "WS-TAB-F-PRICE");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    03  WS-TB-F-PRICE OCCURS 1000 TIMES
		matchToken(WS, "    ", HIDDEN);
		matchToken(INTEGER, "03");
		matchToken(WS, "  ", HIDDEN);
		matchToken(USERDEFINEDWORD, "WS-TB-F-PRICE");
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
		matchToken(USERDEFINEDWORD, "IPRICE");
		matchToken(WS, " ", HIDDEN);
		matchToken(USERDEFINEDWORD, "IPRICEUM");
		matchToken(WS, " ", HIDDEN);
		matchToken(USERDEFINEDWORD, "IPRICEMIL");
		matchToken(WS, " ", HIDDEN);
		matchToken(USERDEFINEDWORD, "IPRICELIMLOG");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//                   IPRICELIMLOGANT.
		matchToken(WS, "                   ", HIDDEN);
		matchToken(USERDEFINEDWORD, "IPRICELIMLOGANT");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//01  DESL17V00 REDEFINES DESL12V05 PIC S9(17) COMP-3.
		matchToken(INTEGER, "01");
		matchToken(WS, "  ", HIDDEN);
		matchToken(USERDEFINEDWORD, "DESL17V00");
		matchToken(WS, " ", HIDDEN);
		matchToken(REDEFINES, "REDEFINES");
		matchToken(WS, " ", HIDDEN);
		matchToken(USERDEFINEDWORD, "DESL12V05");
		matchToken(WS, " ", HIDDEN);
		matchToken(PIC, "PIC");
		matchToken(PICTURE_MODE, PIC_WS, " ", HIDDEN);
		matchToken(PICTURE_MODE, PICTURESTRING, "S9(17)");
		matchToken(WS, " ", HIDDEN);
		matchToken(COMP_3, "COMP-3");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//77  WS-DEBUG2            VALUE IS ZERO PIC S9(8) COMP.
		matchToken(INTEGER, "77");
		matchToken(WS, "  ", HIDDEN);
		matchToken(USERDEFINEDWORD, "WS-DEBUG2");
		matchToken(WS, "            ", HIDDEN);
		matchToken(VALUE, "VALUE");
		matchToken(WS, " ", HIDDEN);
		matchToken(IS, "IS");
		matchToken(WS, " ", HIDDEN);
		matchToken(ZERO, "ZERO");
		matchToken(WS, " ", HIDDEN);
		matchToken(PIC, "PIC");
		matchToken(PICTURE_MODE, PIC_WS, " ", HIDDEN);
		matchToken(PICTURE_MODE, PICTURESTRING, "S9(8)");
		matchToken(WS, " ", HIDDEN);
		matchToken(COMP, "COMP");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//01  LE-TABE.            COPY XZT0100.
		matchToken(INTEGER, "01");
		matchToken(WS, "  ", HIDDEN);
		matchToken(USERDEFINEDWORD, "LE-TABE");
		matchToken(PERIOD, ".");
		matchToken(WS, "            ", HIDDEN);
		matchToken(COPY, "COPY", COMPILER_CHANNEL);
		matchToken(COMPILER_ID_MODE, COMPILER_ID_WS, " ", HIDDEN);
		matchToken(COMPILER_ID_MODE, COMPILER_ID, "XZT0100", COMPILER_CHANNEL);
		matchToken(COMPILER_MODE, COMPILER_PERIOD, ".", COMPILER_CHANNEL);
		matchToken(NEWLINE, "\n", HIDDEN);

		//LINKAGE SECTION.
		matchToken(LINKAGE, "LINKAGE");
		matchToken(WS, " ", HIDDEN);
		matchToken(SECTION, "SECTION");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);

		//01  LE-ENDI.            COPY XZT0009.
		matchToken(INTEGER, "01");
		matchToken(WS, "  ", HIDDEN);
		matchToken(USERDEFINEDWORD, "LE-ENDI");
		matchToken(PERIOD, ".");
		matchToken(WS, "            ", HIDDEN);
		matchToken(COPY, "COPY", COMPILER_CHANNEL);
		matchToken(COMPILER_ID_MODE, COMPILER_ID_WS, " ", HIDDEN);
		matchToken(COMPILER_ID_MODE, COMPILER_ID, "XZT0009", COMPILER_CHANNEL);
		matchToken(COMPILER_MODE, COMPILER_PERIOD, ".", COMPILER_CHANNEL);
		matchToken(NEWLINE, "\n", HIDDEN);

		//EJECT
		matchToken(EJECT, "EJECT", COMPILER_CHANNEL);
		matchToken(NEWLINE, "\n", HIDDEN);

		//PROCEDURE DIVISION.\r
		matchToken(PROCEDURE, "PROCEDURE");
		matchToken(WS, " ", HIDDEN);
		matchToken(DIVISION, "DIVISION");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\r\n", HIDDEN);
		
		//    MOVE -1 TO WS-DEBUG1.
		matchToken(WS, "    ", HIDDEN);
		matchToken(MOVE, "MOVE");
		matchToken(WS, " ", HIDDEN);
		matchToken(INTEGER, "-1");
		matchToken(WS, " ", HIDDEN);
		matchToken(TO, "TO");
		matchToken(WS, " ", HIDDEN);
		matchToken(USERDEFINEDWORD, "WS-DEBUG1");
		matchToken(PERIOD, ".");
		matchToken(NEWLINE, "\n", HIDDEN);
		
		//    DISPLAY 'Hello, world'.
		matchToken(WS, "    ", HIDDEN);
		matchToken(DISPLAY, "DISPLAY");
		matchToken(WS, " ", HIDDEN);
		matchToken(QUOTEDSTRING, "'Hello, world'");
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

	@Override
	protected TokenSource getLexer(Reader reader) throws IOException {
		return lexerForFreeFormat(reader);
	}
}
