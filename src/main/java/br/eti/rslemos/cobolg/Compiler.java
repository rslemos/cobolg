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

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.TokenSource;
import org.antlr.v4.runtime.atn.PredictionMode;

import br.eti.rslemos.cobolg.COBOLParser.ProgramContext;

public abstract class Compiler {
	
	public TokenSource decompose(String contents) throws IOException {
		return buildLexer(new StringReader(contents));
	}

	public ProgramContext compile(String contents) throws IOException {
		return compile(new StringReader(contents));
	}

	public ProgramContext compile(Reader reader) throws IOException {
		return compile(null, reader);
	}
	
	public ProgramContext compile(String fileName, Reader reader) throws IOException {
		CollectErrorListener custom = new CollectErrorListener(fileName);
		
		Lexer lexer = buildLexer(reader);
		lexer.removeErrorListeners();
		lexer.addErrorListener(custom);
		
		COBOLParser parser = buildParser(lexer);
		parser.removeErrorListeners();
		parser.addErrorListener(custom);
		parser.getInterpreter().setPredictionMode(PredictionMode.LL_EXACT_AMBIG_DETECTION);
		
		ProgramContext tree = parser.program();
		
		custom.verify();
		
		return tree;
	}

	protected COBOLParser buildParser(Lexer lexer) {
		return new COBOLParser(new CommonTokenStream(lexer));
	}

	protected abstract Lexer buildLexer(Reader reader) throws IOException;
	
	public static class FreeFormatCompiler extends Compiler {

		@Override
		protected Lexer buildLexer(Reader reader) throws IOException {
			return new COBOLFreeFormatLexer(new ANTLRInputStream(reader));
		}
	}

	public static class FixedFormatCompiler extends Compiler {

		@Override
		protected Lexer buildLexer(Reader reader) throws IOException {
			reader = new StuffingReader(reader, 0, '\uEBA0', 6, '\uEBA1', 7, '\uEBA2', 72, '\uEBA3'/*, 80, '\uEBA4'*/);
			
			return new COBOLFixedFormatLexer(new ANTLRInputStream(reader));
		}
	}
}

class CollectErrorListener extends BaseErrorListener {
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
	
	void verify() {
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
}

class CompilationError extends Error {

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
