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

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.TokenSource;
import org.antlr.v4.runtime.atn.PredictionMode;

import br.eti.rslemos.cobolg.COBOLParser.ProgramContext;

public abstract class Compiler {
	
	private ANTLRErrorListener custom;

	public TokenSource decompose(String contents) throws IOException {
		return buildLexer(new StringReader(contents));
	}

	public ProgramContext compile(String contents) throws IOException {
		return compile(new StringReader(contents));
	}

	public ProgramContext compile(Reader reader) throws IOException {
		COBOLParser parser = getParser(reader);
		ProgramContext tree = parser.program();
		return tree;
	}

	public void setCustomErrorListener(ANTLRErrorListener custom) {
		this.custom = custom;
	}

	public COBOLParser getParser(Reader reader) throws IOException {
		Lexer lexer = buildLexer(reader);
		lexer.removeErrorListeners();
		if (custom != null)
			lexer.addErrorListener(custom);
		
		COBOLParser parser = buildParser(lexer);
		parser.removeErrorListeners();
		if (custom != null)
			parser.addErrorListener(custom);
		parser.getInterpreter().setPredictionMode(PredictionMode.LL_EXACT_AMBIG_DETECTION);
		
		return parser;
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
