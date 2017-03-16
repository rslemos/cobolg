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
package br.eti.rslemos.cobolg.io;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

import java.io.IOException;
import java.nio.CharBuffer;

import org.junit.Before;
import org.junit.Test;

public class LineReaderWithEOLUnitTest {
	
	private CharBuffer source;
	private LineReaderWithEOL lineReader;
	
	@Before public void setup() {
		// 10 lines of 80 chars + double-char line terminator
		source = CharBuffer.allocate(82*10);
		source.flip();
		// small internal buffer will let us explore interesting properties
		lineReader = new LineReaderWithEOL(source, 40);
	}
	
	@Test public void emptySource() throws IOException {
		testLines("");
	}

	@Test public void singleShortLineUnterminated() throws IOException {
		testLines("Unterminated and shorter than 40 chars");
	}
	
	@Test public void singleShortLineTerminatedInCR() throws IOException {
		testLines("Shorter than 40 chars, ends in \\r\r");
	}
	
	@Test public void singleShortLineTerminatedInLF() throws IOException {
		testLines("Shorter than 40 chars, ends in \\n\n");
	}
	
	@Test public void singleShortLineTerminatedInCRLF() throws IOException {
		testLines("Shorter than 40 chars, ends in \\r\\n\r\n");
	}
	
	@Test public void singleShortLineTerminatedInLFCR() throws IOException {
		testLines("Shorter than 40 chars, ends in \\n\\r\n\r");
	}

	@Test public void twoVeryShortLinesTerminatedInCR() throws IOException {
		testLines(
				"Line 1\r",
				"Line 2\r"
			);
	}
	
	@Test public void twoVeryShortLinesTerminatedInLF() throws IOException {
		testLines(
				"Line 1\n",
				"Line 2\n"
			);
	}
	
	@Test public void twoVeryShortLinesTerminatedInCRLF() throws IOException {
		testLines(
				"Line 1\r\n",
				"Line 2\r\n"
			);
	}
	
	@Test public void twoVeryShortLinesTerminatedInLFCR() throws IOException {
		testLines(
				"Line 1\n\r",
				"Line 2\n\r"
			);
	}
	
	@Test public void emptyLineTerminatedInCRFollowedByUnterminatedShortLine() throws IOException {
		testLines(
				"\r",
				"Line 2"
			);
	}
	
	@Test public void emptyLineTerminatedInLFFollowedByUnterminatedShortLine() throws IOException {
		testLines(
				"\n",
				"Line 2"
			);
	}
	
	@Test public void emptyLineTerminatedInCRLFFollowedByUnterminatedShortLine() throws IOException {
		testLines(
				"\r\n",
				"Line 2"
			);
	}
	
	@Test public void emptyLineTerminatedInLFCRFollowedByUnterminatedShortLine() throws IOException {
		testLines(
				"\n\r",
				"Line 2"
			);
	}
	
	@Test public void exactly40CharsLineFollowedByEmptyLineTerminatedInCR() throws IOException {
		testLines(
				"This line contains exacly 40 chars.....\n",
				"\n"
			);
	}
	
	@Test public void exactly40CharsLineFollowedByEmptyLineTerminatedInLF() throws IOException {
		testLines(
				"This line contains exacly 40 chars.....\r",
				"\r"
			);
	}
	
	@Test public void exactly40CharsLineFollowedByEmptyLineTerminatedInCRLF() throws IOException {
		testLines(
				"This line contains exacly 40 chars....\n\r",
				"\n\r"
			);
	}
	
	@Test public void exactly40CharsLineFollowedByEmptyLineTerminatedInLFCR() throws IOException {
		testLines(
				"This line contains exacly 40 chars....\r\n",
				"\r\n"
			);
	}
	
	@Test public void emptyLineFollowedByExactly40CharsTerminatedInCR() throws IOException {
		testLines(
				"\n",
				"This line contains exacly 40 chars.....\n"
			);
	}
	
	@Test public void emptyLineFollowedByExactly40CharsTerminatedInLF() throws IOException {
		testLines(
				"\r",
				"This line contains exacly 40 chars.....\r"
			);
	}
	
	@Test public void emptyLineFollowedByExactly40CharsTerminatedInCRLF() throws IOException {
		testLines(
				"\n\r",
				"This line contains exacly 40 chars....\n\r"
			);
	}
	
	@Test public void emptyLineFollowedByExactly40CharsTerminatedInLFCR() throws IOException {
		testLines(
				"\r\n",
				"This line contains exacly 40 chars....\r\n"
			);
	}
	
	@Test public void singleLongLineUnterminated() throws IOException {
		testLines("Unterminated and much much longer than 40 chars");
	}
	
	@Test public void singleLongLineTerminatedInCR() throws IOException {
		testLines("Much much longer than 40 chars, ends in \\r\r");
	}
	
	@Test public void singleLongLineTerminatedInLF() throws IOException {
		testLines("Much much longer than 40 chars, ends in \\n\n");
	}
	
	@Test public void singleLongLineTerminatedInCRLF() throws IOException {
		testLines("Much much longer than 40 chars, ends in \\r\\n\r\n");
	}
	
	@Test public void singleLongLineTerminatedInLFCR() throws IOException {
		testLines("Much much longer than 40 chars, ends in \\n\\r\n\r");
	}

	@Test public void twoLongLinesTerminatedInCR() throws IOException {
		testLines(
				"Line 1: Much much longer than 40 chars, ends in \\r\r",
				"Line 2: Much much longer than 40 chars, ends in \\r\r"
			);
	}
	
	@Test public void twoLongLinesTerminatedInLF() throws IOException {
		testLines(
				"Line 1: Much much longer than 40 chars, ends in \\n\n",
				"Line 2: Much much longer than 40 chars, ends in \\n\n"
			);
	}
	
	@Test public void twoLongLinesTerminatedInCRLF() throws IOException {
		testLines(
				"Line 1: Much much longer than 40 chars, ends in \\r\\n\r\n",
				"Line 2: Much much longer than 40 chars, ends in \\r\\n\r\n"
			);
	}
	
	@Test public void towLongLinesTerminatedInLFCR() throws IOException {
		testLines(
				"Line 1: Much much longer than 40 chars, ends in \\n\\r\n\r",
				"Line 2: Much much longer than 40 chars, ends in \\n\\r\n\r"
			);
	}

	@Test public void exactly41CharsLineFollowedByEmptyLineTerminatedInCR() throws IOException {
		testLines(
				"This line contains exacly 41 chars......\n",
				"\n"
			);
	}
	
	@Test public void exactly41CharsLineFollowedByEmptyLineTerminatedInLF() throws IOException {
		testLines(
				"This line contains exacly 41 chars......\r",
				"\r"
			);
	}
	
	@Test public void exactly41CharsLineFollowedByEmptyLineTerminatedInCRLF() throws IOException {
		testLines(
				"This line contains exacly 41 chars.....\n\r",
				"\n\r"
			);
	}
	
	@Test public void exactly41CharsLineFollowedByEmptyLineTerminatedInLFCR() throws IOException {
		testLines(
				"This line contains exacly 41 chars.....\r\n",
				"\r\n"
			);
	}
	
	private void testLines(String... lines) throws IOException {
		int unterminatedLine = -1;
		
		source.compact();
		for (int i = 0; i < lines.length; i++) {
			String line = lines[i];
			
			if (!line.endsWith("\r") && !line.endsWith("\n")) {
				if (unterminatedLine >= 0)
					throw new IllegalArgumentException(
							String.format("offending line: %d (previous unterminated line: %d)", i, unterminatedLine));

				unterminatedLine = i;
			}
			
			int iLFCR = line.indexOf("\n\r");
			int iCRLF = line.indexOf("\r\n");
			
			if (iLFCR >= 0 && iLFCR < line.length() - 2)
				throw new IllegalArgumentException(String.format("offending line: %d", i));

			if (iCRLF >= 0 && iCRLF < line.length() - 2)
				throw new IllegalArgumentException(String.format("offending line: %d", i));
			
			int iLF = line.indexOf("\n");
			int iCR = line.indexOf("\r");
			
			if (iLF >= 0 && iLF < line.length() - (iLFCR >= 0 ? 2 : 1))
				throw new IllegalArgumentException(String.format("offending line: %d", i));

			if (iCR >= 0 && iCR < line.length() - (iCRLF >= 0 ? 2 : 1))
				throw new IllegalArgumentException(String.format("offending line: %d", i));
			
			source.append(line);
		}
		source.flip();
		
		for (String line : lines)
			if (!"".equals(line))
				assertLine(line);
		
		assertEOF();
	}

	private void assertLine(String line) throws IOException {
		assertThat(lineReader.readLine(), is(equalTo(line)));
	}
	
	private void assertEOF() throws IOException {
		assertThat(lineReader.readLine(), is(nullValue()));
	}
}
