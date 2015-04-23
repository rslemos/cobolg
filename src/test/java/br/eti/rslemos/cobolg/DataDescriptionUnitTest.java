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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

import java.io.StringReader;

import org.antlr.v4.runtime.tree.TerminalNode;
import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.DataDescriptionParagraphContext;
import br.eti.rslemos.cobolg.COBOLParser.DataNameContext;
import br.eti.rslemos.cobolg.Compiler.FreeFormatCompiler;

public class DataDescriptionUnitTest {
	@Test
	public void testEmptyDeclaration () {
		DataDescriptionParagraphContext dataDescription = compile("01  EMPTY-DECLARATION.");
		assertThat(dataDescription.levelNumber().getText(), is(equalTo("01")));
		assertThat(dataDescription.dataName().ID().getText(), is(equalTo("EMPTY-DECLARATION")));
	}

	@Test
	public void testFillerDeclaration () {
		DataDescriptionParagraphContext dataDescription = compile("01  FILLER.");
		assertThat(dataDescription.levelNumber().getText(), is(equalTo("01")));
		assertThat(dataDescription.FILLER().getText(), is(equalTo("FILLER")));
	}

	@Test
	public void testAnonymousDeclaration () {
		DataDescriptionParagraphContext dataDescription = compile("01  .");
		assertThat(dataDescription.levelNumber().getText(), is(equalTo("01")));
		assertThat(dataDescription.dataName(), is(nullValue(DataNameContext.class)));
		assertThat(dataDescription.FILLER(), is(nullValue(TerminalNode.class)));
	}

	private static DataDescriptionParagraphContext compile(String source) {
		FreeFormatCompiler compiler = new FreeFormatCompiler();
		compiler.setFilename(null);
		
		try {
			DataDescriptionParagraphContext tree = compiler.getParser(new StringReader(source)).dataDescriptionParagraph();
			
			compiler.verify();
			assertThat(tree, is(not(nullValue(DataDescriptionParagraphContext.class))));
			
			return tree;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
}
