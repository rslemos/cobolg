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

import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.DefaultErrorStrategy;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.atn.PredictionMode;

import br.eti.rslemos.cobolg.COBOLParser.CompilerStatementsContext;
import br.eti.rslemos.cobolg.COBOLParser.BatchContext;

public abstract class Compiler {
	
	final Lexer lexer;
	
	final COBOLParser preParser;
	final COBOLParser mainParser;

	private Compiler (Lexer lexer) {
		this.lexer = lexer;
		this.lexer.removeErrorListeners();

		lexer.reset();
		CommonTokenStream preTokens = new CommonTokenStream(lexer, COBOLLexer.COMPILER_CHANNEL);
		preTokens.fill();
		
		lexer.reset();
		CommonTokenStream mainTokens = new CommonTokenStream(lexer);
		mainTokens.fill();
		
		preParser = setup(new COBOLParser(preTokens));
		mainParser = setup(new COBOLParser(mainTokens));
	}

	private static <R extends Parser> R setup(R parser) {
		parser.removeErrorListeners();
		parser.setErrorHandler(new DefaultErrorStrategy());
		parser.getInterpreter().setPredictionMode(PredictionMode.LL_EXACT_AMBIG_DETECTION);
		parser.setBuildParseTree(true);
		
		return parser;
	}

	public BatchContext batch() {
		CompilerStatementsContext preTree = this.preParser.compilerStatements();
		BatchContext mainTree = this.mainParser.batch();
		
		new CompilerStatementsProcessor().injectCompilerStatements(preTree, mainTree);
		
		return mainTree;
	}
	
	public void addErrorListener(ANTLRErrorListener listener) {
		lexer.addErrorListener(listener);
		mainParser.addErrorListener(listener);
		preParser.addErrorListener(listener);
	}
	
	public static class FreeFormatCompiler extends Compiler {
		public FreeFormatCompiler(Reader source) throws IOException {
			super(new COBOLLexer(forANTLR(source)));
		}
	}

	public static class FixedFormatCompiler extends Compiler {
		public FixedFormatCompiler(Reader source) throws IOException {
			super(new COBOLLexer(forANTLR(stuffFixedWidthChars(source))));
		}

		private static StuffingReader stuffFixedWidthChars(Reader source) {
			return new StuffingReader(source, 0, '\uEBA0', 6, '\uEBA1', 7, '\uEBA2', 72, '\uEBA3'/*, 80, '\uEBA4'*/);
		}
	}
	
	private static ANTLRInputStream forANTLR(Reader source) throws IOException {
		return new ANTLRInputStream(source);
	}
}
