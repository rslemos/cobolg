package br.eti.rslemos.cobolg;

import java.io.FilterReader;
import java.io.IOException;
import java.io.Reader;
import java.nio.CharBuffer;
import java.util.Arrays;

public class StuffingReader extends FilterReader {

	private final char[] charsToStuff;
	
	private final CharBuffer inputBuffer;
	private int positionInLine;

	private int lastStuffed = -1;
	
	private char ongoingNewline = 0;
	
	public StuffingReader(Reader reader, int... charsToStuff) {
		this(reader, expand(charsToStuff));
	}

	private StuffingReader(Reader reader, char... charsToStuff) {
		super(reader);
		this.charsToStuff = charsToStuff;
		
		inputBuffer = CharBuffer.allocate(1024);
		inputBuffer.flip();
		
		positionInLine = 0;
	}
	
	@Override
	public int read(char[] buffer, int offset, int length) throws IOException {		
		int read = 0;

outerloop:
		while (length > read) {
			if (inStuffPosition())
				buffer[offset + read++] = charsToStuff[lastStuffed = positionInLine];
			
			if (!hasMoreData())
				break outerloop;
			
			int i;
			for (i = 0; i < length - read && i < inputBuffer.remaining(); i++) {
				char c = inputBuffer.get(i);

				if (inStuffPosition())
					break;
				
				if (ongoingNewline != 0) {
					if ((c == '\n' || c == '\r') && c != ongoingNewline)
						i++;
					
					ongoingNewline = 0;
					positionInLine = 0;
					lastStuffed = -1;
					break;
				}
				
				if (c == '\n' || c == '\r') {
					ongoingNewline = c;
					continue;
				}
				
				positionInLine++;
			}
			read += drainInputBufferTo(buffer, offset + read, i);
		}
		
		return length > 0 && read == 0 ? -1 : read;
	}

	private int drainInputBufferTo(char[] buffer, int offset, int length) {
		inputBuffer.get(buffer, offset, length);
		return length;
	}

	private boolean inStuffPosition() {
		return positionInLine != lastStuffed && positionInLine < charsToStuff.length && charsToStuff[positionInLine] < '\uffff';
	}

	private boolean hasMoreData() throws IOException {
		final boolean hasData = inputBuffer.remaining() > 0;
		
		inputBuffer.compact();
		
		if (!hasData) {
			char[] b = new char[inputBuffer.remaining()];
			
			int size;
			if ((size = super.read(b, 0, b.length)) == -1) {
				inputBuffer.flip();
				return inputBuffer.remaining() > 0;
			}
			
			inputBuffer.put(b, 0, size);
		}
		
		inputBuffer.flip();
		
		return true;
	}

	private static char[] expand(int... charsToStuff) {
		if (charsToStuff.length % 2 != 0)
			throw new IllegalArgumentException("expected even number of elements");
		
		int max = 0;
		for (int i = 0; i < charsToStuff.length; i += 2)
			if (charsToStuff[i] > max) max = charsToStuff[i];
		
		char[] result = new char[max+1];
		Arrays.fill(result, '\uffff');
		for (int i = 0; i < charsToStuff.length; i += 2) {
			result[charsToStuff[i]] = (char)charsToStuff[i+1];
		}
		
		return result;
	}

}
