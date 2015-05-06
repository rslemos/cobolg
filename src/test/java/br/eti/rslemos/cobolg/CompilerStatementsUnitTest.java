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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.ProgramContext;
import br.eti.rslemos.cobolg.Compiler.FreeFormatCompiler;

public class CompilerStatementsUnitTest {
	private FreeFormatCompiler compiler;

	@Test
	public void testNoCompilerStatements () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"IDENTIFICATION DIVISION.",
				"PROGRAM-NAME. X.",
				"PROCEDURE DIVISION.",
				"    STOP RUN."
			)));
		
		ProgramContext mainTree = compiler.compile();
		String toString = mainTree.toStringTree(compiler.mainParser);

		assertThat(toString, is(equalTo("(program "
				+ "(identificationDivision IDENTIFICATION DIVISION . PROGRAM-NAME . X .) "
				+ "(procedureDivision PROCEDURE DIVISION . (unnamedProceduralSection (unnamedProceduralParagraph (proceduralStatement STOP RUN .)))))")));		
	}
	
	private void setSource(Reader source) throws IOException {
		compiler = new FreeFormatCompiler(source);
	}
}
