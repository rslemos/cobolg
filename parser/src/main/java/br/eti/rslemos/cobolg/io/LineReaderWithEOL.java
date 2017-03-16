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

import java.io.IOException;
import java.nio.CharBuffer;

public class LineReaderWithEOL {

	private static final int BUF_SIZE = 4096;
	
	private Readable in;
	private CharBuffer buffer;

	// mainly for testing purposes
	LineReaderWithEOL(Readable in, int bufsize) {
		this.in = in;
		this.buffer = CharBuffer.allocate(bufsize);
		this.buffer.flip();
	}
	
	public LineReaderWithEOL(Readable in) {
		this(in, BUF_SIZE);
	}

	public String readLine() throws IOException {
		StringBuilder result = new StringBuilder(130);

		char c = 0;
		while (!isNewLineChar(c) && hasMoreData()) {
			do {
				result.append(c = buffer.get());
			} while (buffer.hasRemaining() && !isNewLineChar(c));
			
			if (isNewLineChar(c) && hasMoreData() && (buffer.get(buffer.position()) ^ c) == ('\n' ^ '\r'))
				result.append(buffer.get());
		}
		
		return result.length() > 0 ? result.toString() : null;
	}

	private boolean hasMoreData() throws IOException {
		boolean hasData = buffer.hasRemaining();
		
		if (!hasData) {
			buffer.compact();
			hasData = in.read(buffer) != -1;
			buffer.flip();
		}
		
		return hasData;
	}
	
	private static boolean isNewLineChar(char c) {
		return c == '\n' || c == '\r';
	}
}
