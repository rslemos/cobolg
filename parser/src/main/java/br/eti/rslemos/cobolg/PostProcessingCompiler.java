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
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.List;

import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.TokenSource;

import br.eti.rslemos.cobolg.COBOLParser.CompilerStatementsContext;

public class PostProcessingCompiler {
	private static class PostProcessingCompilerIH implements InvocationHandler { 
		private final TeeTokenSource tee;
		private final SimpleCompiler main;
	
		private PostProcessingCompilerIH (TeeTokenSource tee) {
			this.tee = tee;
			main = new SimpleCompiler(tee.splitChannel());
		}
	
		@Override public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			if (ParserRuleContext.class.isAssignableFrom(method.getReturnType())) {
				return postProcess(tee.splitChannel(), (ParserRuleContext)method.invoke(main, args));
			} else {
				return method.invoke(main, args);
			}
		}
	
		private <T extends ParserRuleContext> T postProcess(TokenSource preChannel, T mainTree) {
			tee.disconnect(preChannel);
			
			COBOLParser preParser = setup(new COBOLParser(new CommonTokenStream(preChannel, COBOLLexer.COMPILER_CHANNEL)));
			List<? extends ANTLRErrorListener> listeners = main.getErrorListeners();
			for (ANTLRErrorListener listener : listeners)
				preParser.addErrorListener(listener);
			
			CompilerStatementsContext preTree = preParser.compilerStatements();
			
			new CompilerStatementsProcessor().injectCompilerStatements(preTree, mainTree);
			
			return mainTree;
		}
	}
	
	public static Compiler newParser(COBOLLexer lexer) {
		try {
			return (Compiler) Proxy.newProxyInstance(
					Compiler.class.getClassLoader(), 
					new Class<?>[]{ Compiler.class }, 
					new PostProcessingCompilerIH(new TeeTokenSource(lexer))
				);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static Compiler parserForFreeFormat(Reader source) throws IOException {
		return newParser(lexerForFreeFormat(source));
	}

	public static Compiler parserForFixedFormat(Reader source) throws IOException {
		return newParser(lexerForFixedFormat(source));
	}
}
