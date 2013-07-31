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

import static br.eti.rslemos.cobolg.TextHelper.join;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

import org.antlr.v4.runtime.tree.TerminalNode;
import org.junit.Before;
import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.IdentificationDivisionContext;
import br.eti.rslemos.cobolg.COBOLParser.ParagraphNameContext;
import br.eti.rslemos.cobolg.COBOLParser.ProceduralStatementContext;
import br.eti.rslemos.cobolg.COBOLParser.ProcedureDivisionContext;
import br.eti.rslemos.cobolg.COBOLParser.ProgramContext;
import br.eti.rslemos.cobolg.COBOLParser.UserDefinedProcedureSectionContext;
import br.eti.rslemos.cobolg.Compiler.FreeFormatCompiler;

public class FreeFormatUnitTest {
	private static final String SOURCE = join(
			"IDENTIFICATION DIVISION.",
			"PROGRAM-ID. HELLO-WORLD.",
			"*COMMENT LINE",
			"PROCEDURE DIVISION.",
			"    DISPLAY 'Hello, world'.",
			"    STOP RUN."
		);
	
	private ProgramContext tree;
	
	@Before
	public void setup() throws Exception {
		tree = new FreeFormatCompiler().compile(SOURCE);
		assertThat(tree, is(not(nullValue(ProgramContext.class))));
	}
	
	@Test
	public void testIdentificationDivisionPresence() {
		assertThat(tree.identificationDivision(), is(not(nullValue(IdentificationDivisionContext.class))));
	}

	@Test
	public void testIdentificationDivision() {
		IdentificationDivisionContext idDivision = tree.identificationDivision();
		assertThat(idDivision.programName().getText(), is(equalTo("HELLO-WORLD")));
	}

	@Test
	public void testProcedureDivisionPresence() {
		assertThat(tree.procedureDivision(), is(not(nullValue(ProcedureDivisionContext.class))));
	}

	@Test
	public void testProcedureDivision() {
		ProcedureDivisionContext procDivision = tree.procedureDivision();
		assertThat(procDivision.userDefinedProcedureSection().size(), is(equalTo(1)));
		
		UserDefinedProcedureSectionContext userDefinedProcedureSection_0 = procDivision.userDefinedProcedureSection(0);
		assertThat(userDefinedProcedureSection_0.paragraphName(), is(nullValue(ParagraphNameContext.class)));
		assertThat(userDefinedProcedureSection_0.proceduralStatement().size(), is(equalTo(2)));
		
		ProceduralStatementContext statement_0_0 = userDefinedProcedureSection_0.proceduralStatement(0);
		assertThat(statement_0_0.DISPLAY(), is(not(nullValue(TerminalNode.class))));
		
		ProceduralStatementContext statement_0_1 = userDefinedProcedureSection_0.proceduralStatement(1);
		assertThat(statement_0_1.STOP(), is(not(nullValue(TerminalNode.class))));
	}
}
