/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2016  Rodrigo Lemos
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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.io.StringReader;

import org.antlr.v4.runtime.RuleContext;

import br.eti.rslemos.cobolg.Compiler.FreeFormatCompiler;

public abstract class CompilerHelper<T extends RuleContext> {
	protected abstract T parsePart();
	
	protected COBOLParser parser;
	
	public T compile(String source, CollectErrorListener... listeners) {
		prepare(source, listeners);
		return parsePart();
	}
	
	public String compileAndGetTree(String source, CollectErrorListener... listeners) {
		T tree = compile(source, listeners);
		return tree.toStringTree(parser);
	}

	public void compileAndVerify(String source, String expectedTree) {
		CollectErrorListener listener;
		String actualTree = compileAndGetTree(source, listener = new CollectErrorListener(null));
		assertThat(actualTree, is(equalTo(expectedTree)));
		listener.verify();
	}

	private void prepare(String source, CollectErrorListener... listeners) {
		try {
			FreeFormatCompiler compiler = new FreeFormatCompiler(new StringReader(source));
			
			for (CollectErrorListener listener : listeners)
				compiler.addErrorListener(listener);
			
			parser = compiler.mainParser;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	
}
