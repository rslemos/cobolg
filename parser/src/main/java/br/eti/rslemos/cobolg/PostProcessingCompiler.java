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
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.TokenSource;

import br.eti.rslemos.cobolg.COBOLParser.BatchContext;
import br.eti.rslemos.cobolg.COBOLParser.CompilerStatementsContext;
import br.eti.rslemos.cobolg.COBOLParser.FileSectionContext;
import br.eti.rslemos.cobolg.COBOLParser.ProceduralSentenceContext;
import br.eti.rslemos.cobolg.COBOLParser.ProgramContext;
import br.eti.rslemos.cobolg.COBOLParser.WorkingStorageSectionContext;

public class PostProcessingCompiler extends BaseCompiler {
	
	final COBOLParser preParser;

	private PostProcessingCompiler (TeeTokenSource tee) {
		super(new SimpleCompiler(tee.splitChannel()));
		
		TokenSource preChannel = tee.splitChannel();
		
		CommonTokenStream preTokens = new CommonTokenStream(preChannel, COBOLLexer.COMPILER_CHANNEL);

		preParser = setup(new COBOLParser(preTokens));
	}

	@Override public BatchContext batch() {
		return postProcess(super.batch());
	}
	
	@Override public FileSectionContext fileSection() {
		return postProcess(super.fileSection());
	}

	@Override public ProceduralSentenceContext proceduralSentence() {
		return postProcess(super.proceduralSentence());
	}

	@Override public ProgramContext program() {
		return postProcess(super.program());
	}

	@Override public WorkingStorageSectionContext workingStorageSection() {
		return postProcess(super.workingStorageSection());
	}

	private <T extends ParserRuleContext> T postProcess(T mainTree) {
		CompilerStatementsContext preTree = this.preParser.compilerStatements();

		new CompilerStatementsProcessor().injectCompilerStatements(preTree, mainTree);
		
		return mainTree;
	}
	
	@Override public void addErrorListener(ANTLRErrorListener listener) {
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
