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
package br.eti.rslemos.cobolg;

import static br.eti.rslemos.cobolg.SimpleCompiler.lexerForFixedFormat;
import static br.eti.rslemos.cobolg.SimpleCompiler.lexerForFreeFormat;
import static br.eti.rslemos.cobolg.SimpleCompiler.setup;

import java.io.IOException;
import java.io.Reader;

import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenSource;

import br.eti.rslemos.cobolg.COBOLParser.BatchContext;
import br.eti.rslemos.cobolg.COBOLParser.CompilerStatementsContext;

public class PostProcessingCompiler extends BaseCompiler {
	
	final COBOLParser preParser;

	private PostProcessingCompiler (TeeTokenSource tee) {
		TokenSource mainChannel = tee.splitChannel();
		TokenSource preChannel = tee.splitChannel();
		
		CommonTokenStream mainTokens = new CommonTokenStream(mainChannel);
		CommonTokenStream preTokens = new CommonTokenStream(preChannel, COBOLLexer.COMPILER_CHANNEL);

		mainParser = setup(new COBOLParser(mainTokens));
		preParser = setup(new COBOLParser(preTokens));
	}

	public BatchContext batch() {
		CompilerStatementsContext preTree = this.preParser.compilerStatements();
		BatchContext mainTree = this.mainParser.batch();
		
		new CompilerStatementsProcessor().injectCompilerStatements(preTree, mainTree);
		
		return mainTree;
	}
	
	public void addErrorListener(ANTLRErrorListener listener) {
		super.addErrorListener(listener);
		preParser.addErrorListener(listener);
	}

	public static PostProcessingCompiler newParser(COBOLLexer lexer) {
		return new PostProcessingCompiler(new TeeTokenSource(lexer));
	}
	
	public static PostProcessingCompiler parserForFreeFormat(Reader source) throws IOException {
		return newParser(lexerForFreeFormat(source));
	}

	public static PostProcessingCompiler parserForFixedFormat(Reader source) throws IOException {
		return newParser(lexerForFixedFormat(source));
	}
}
