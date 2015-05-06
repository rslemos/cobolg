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

import static br.eti.rslemos.cobolg.TextHelper.join;
import static br.eti.rslemos.cobolg.TextHelper.join0;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import junit.framework.TestCase;

public class StuffingReaderUnitTest extends TestCase {
	private char[] targetBuffer;
	private Reader reader;
	
	@Override
	public void setUp() {
		targetBuffer = new char[1024];
		System.arraycopy("CANARY".toCharArray(), 0, targetBuffer, 0, "CANARY".length());
	}
	
	private void initReader(String source, int... charsToStuff) {
		reader = new StuffingReader(new StringReader(source), charsToStuff);		
	}
	
	@Override
	public void tearDown() throws IOException {
		// after the reader has been drained:
		// - read must return -1;
		// - read must NOT touch the buffer;
		
		char[] bufferBefore = targetBuffer.clone();
		assertThat(reader.read(targetBuffer), is(equalTo(-1)));
		assertThat(targetBuffer, is(equalTo(bufferBefore)));
		
		reader.close();
	}
	
	public void testEmptyReaderNoStuff() throws IOException {
		initReader("");
	}
	
	public void testEmptyReader() throws IOException {
		initReader("", 1, 'x');
	}
	
	public void testEmptyReaderReadNone() throws IOException {
		initReader("", 1, 'x');

		// read no chars
		assertThat(reader.read(targetBuffer, 0, 0), is(equalTo(0)));
		assertThat(new String(targetBuffer, 0, "CANARY".length()), is(equalTo("CANARY")));
	}
	
	public void testEmptyReaderStuffItAt0() throws IOException {
		initReader("", 0, 'x');

		// read all available chars (actually none)
		assertThat(reader.read(targetBuffer), is(equalTo(1)));
		assertThat(new String(targetBuffer, 0, "CANARY".length()), is(equalTo("xANARY")));
	}
	
	public void testSingleLineStuffItAt0() throws IOException {
		initReader("CAFEBABE", 0, 'x');
		
		readAndAssertThatBufferIs("xCAFEBABE");
	}

	public void testSingleLineStuffItAtEveryChar() throws IOException {
		initReader("CAFEBABE", 0, 'c', 1, 'a', 2, 'f', 3, 'e', 4, 'b', 5, 'a', 6, 'b', 7, 'e');
		
		readAndAssertThatBufferIs("cCaAfFeEbBaAbBeE");
	}
	
	public void testTwoLinesStuffItAt0() throws IOException {
		initReader("CAFEBABE\nCAFEBABE", 0, 'x');
		
		readAndAssertThatBufferIs("xCAFEBABE\nxCAFEBABE");
	}
	
	public void testTwoLinesStuffItAt1() throws IOException {
		initReader("CAFEBABE\nCAFEBABE", 1, 'x');
		
		readAndAssertThatBufferIs("CxAFEBABE\nCxAFEBABE");
	}

	public void testTwoLineStuffItAtEveryChar() throws IOException {
		initReader("CAFEBABE\nCAFEBABE", 0, 'c', 1, 'a', 2, 'f', 3, 'e', 4, 'b', 5, 'a', 6, 'b', 7, 'e');
		
		readAndAssertThatBufferIs("cCaAfFeEbBaAbBeE\ncCaAfFeEbBaAbBeE");
	}
	
	public void testTwoUnevenLineStuffItAtEveryChar() throws IOException {
		initReader("CAFEBABE\nCAFE", 0, 'c', 1, 'a', 2, 'f', 3, 'e', 4, 'b', 5, 'a', 6, 'b', 7, 'e');
		
		readAndAssertThatBufferIs("cCaAfFeEbBaAbBeE\ncCaAfFeEb");
	}
	
	private static final String[] REALUSECASE_SOURCE = {
			"000100 ID DIVISION.                                                     PROGRAM ",
			"000200 PROGRAM-ID. PROGRAMA.                                            PROGRAM ",
			"000300 AUTHOR. RSLEMOS.                                                 PROGRAM ",
			"000400**************************************************************************",
			"000500 ENVIRONMENT DIVISION.                                            PROGRAM ",
			"000600 CONFIGURATION SECTION.                                           PROGRAM ",
			"000700 SPECIAL-NAMES.                                                   PROGRAM ",
			"000800     DECIMAL-POINT IS COMMA.                                      PROGRAM ",
			"000900**************************************************************************",
			"001000 DATA DIVISION.                                                   PROGRAM ",
			"001100 WORKING-STORAGE SECTION.                                         PROGRAM ",
			"001200 77  FILLER                          PIC X(04)   VALUE SPACES.    PROGRAM ",
			"001300**************************************************************************",
			"001400 PROCEDURE DIVISION.                                              PROGRAM ",
			"001500     GOBACK.                                                      PROGRAM "
		};

	private static final String[] REALUSECASE_EXPECTED = {
			"000100 ID DIVISION.                                                     PROGRAM ",
			"000200 PROGRAM-ID. PROGRAMA.                                            PROGRAM ",
			"000300 AUTHOR. RSLEMOS.                                                 PROGRAM ",
			"000400**************************************************************************",
			"000500 ENVIRONMENT DIVISION.                                            PROGRAM ",
			"000600 CONFIGURATION SECTION.                                           PROGRAM ",
			"000700 SPECIAL-NAMES.                                                   PROGRAM ",
			"000800     DECIMAL-POINT IS COMMA.                                      PROGRAM ",
			"000900**************************************************************************",
			"001000 DATA DIVISION.                                                   PROGRAM ",
			"001100 WORKING-STORAGE SECTION.                                         PROGRAM ",
			"001200 77  FILLER                          PIC X(04)   VALUE SPACES.    PROGRAM ",
			"001300**************************************************************************",
			"001400 PROCEDURE DIVISION.                                              PROGRAM ",
			"001500     GOBACK.                                                      PROGRAM "
		};
	
	public void testRealUseCase() throws IOException {
		// we'll need more space than default
		targetBuffer = new char[2048];
		initReader(join(REALUSECASE_SOURCE), 0, '\uEBA0', 6, '\uEBA1', 7, '\uEBA2', 72, '\uEBA3', 80, '\uEBA4');
		
		readAndAssertThatBufferIs(join(REALUSECASE_EXPECTED));
	}
	
	public void testRealUseCaseCRLF() throws IOException {
		String CRLF = "\r\n";
		
		// we'll need more space than default
		targetBuffer = new char[2048];
		initReader(join0(CRLF, REALUSECASE_SOURCE), 0, '\uEBA0', 6, '\uEBA1', 7, '\uEBA2', 72, '\uEBA3', 80, '\uEBA4');
		
		readAndAssertThatBufferIs(join0(CRLF, REALUSECASE_EXPECTED));
	}
	
	public void testRealUseCaseLFCR() throws IOException {
		String LFCR = "\n\r";
		
		// we'll need more space than default
		targetBuffer = new char[2048];
		initReader(join0(LFCR, REALUSECASE_SOURCE), 0, '\uEBA0', 6, '\uEBA1', 7, '\uEBA2', 72, '\uEBA3', 80, '\uEBA4');
		
		readAndAssertThatBufferIs(join0(LFCR, REALUSECASE_EXPECTED));
	}

	public void testCarefullyPositionedCRLFForInternalBuffer() throws IOException {
		final int internalBufferSize = 1024;

		// we'll need more space than default
		targetBuffer = new char[2048];
		
		char[] source = new char[internalBufferSize + 1];
		source[internalBufferSize - 1] = '\r';
		source[internalBufferSize - 0] = '\n';
		initReader(new String(source),  0, '|');
		
		char[] expected = new char[internalBufferSize + 3];
		expected[0] = '|';
		expected[internalBufferSize + 0] = '\r';
		expected[internalBufferSize + 1] = '\n';
		expected[internalBufferSize + 2] = '|';
		readAndAssertThatBufferIs(new String(expected));
	}


	public void testCarefullyPositionedCRLFForExternalBuffer() throws IOException {
		final int externalBufferSize = 4;

		// we'll need more space than default
		targetBuffer = new char[externalBufferSize];
		
		char[] source = new char[externalBufferSize];
		source[externalBufferSize - 2] = '\r';
		source[externalBufferSize - 1] = '\n';
		initReader(new String(source),  0, '|');
		
		char[] expected0 = new char[externalBufferSize];
		expected0[0] = '|';
		expected0[externalBufferSize - 1] = '\r';
		readAndAssertThatBufferIs(new String(expected0));

		char[] expected1 = { '\n', '|' };
		readAndAssertThatBufferIs(new String(expected1));
	}

	private void readAndAssertThatBufferIs(String expected) throws IOException {
		assertThat(reader.read(targetBuffer), is(expected.length()));
		assertThat(new String(targetBuffer, 0, expected.length()), is(equalTo(expected)));
	}	

	@Override
	public void runTest() throws Throwable {
		try {
			super.runTest();
		} catch (AssertionError e) {
			System.out.println("BUFFER: \n" + new String(targetBuffer));
			throw e;
		}
	}
}
