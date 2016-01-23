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

import java.util.ArrayList;
import java.util.List;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

public class CollectErrorListener extends BaseErrorListener {
	private final String fileName;
	List<String> errors = new ArrayList<String>();
	
	
	public CollectErrorListener(String fileName) {
		this.fileName = fileName;
	}

	@Override
	public void syntaxError(Recognizer<?, ?> recognizer,
							Object offendingSymbol,
							int line,
							int charPositionInLine,
							String msg,
							RecognitionException e) {
		
		char type;
		int mode = -1;
		
		if (recognizer instanceof Parser)
			type = 'P';
		else if (recognizer instanceof Lexer) {
			type = 'L';
			mode = ((Lexer)recognizer)._mode;
		} else
			type = '?';
		
		if (fileName != null)
			errors.add(String.format("[%c:%d] %s (%s:%d,%d)", type, mode, msg, fileName, line, charPositionInLine));
		else
			errors.add(String.format("[%c:%d] %s (%d,%d)", type, mode, msg, line, charPositionInLine));
	}
	
	public void verify() {
		if (!errors.isEmpty()) {
			StringBuilder message = new StringBuilder();
			
			message.append(errors.size()).append(" errors\n");
			
			for (int i = 0; i < errors.size(); i++) {
				message.append(String.format("%4d.", i + 1)).append(errors.get(i)).append("\n");
			}
			
			message.setLength(message.length() - 1);

			throw new CompilationError(message.toString());
		}
	}

	public static class CompilationError extends Error {

		private static final long serialVersionUID = 1355307576009905119L;

		public CompilationError() {
			super();
		}

		public CompilationError(String message, Throwable cause) {
			super(message, cause);
		}

		public CompilationError(String message) {
			super(message);
		}

		public CompilationError(Throwable cause) {
			super(cause);
		}
	}
}