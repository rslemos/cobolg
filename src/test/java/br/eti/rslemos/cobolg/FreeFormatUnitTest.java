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
import org.junit.BeforeClass;
import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.ConfigurationSectionContext;
import br.eti.rslemos.cobolg.COBOLParser.DataDivisionContext;
import br.eti.rslemos.cobolg.COBOLParser.EnvironmentDivisionContext;
import br.eti.rslemos.cobolg.COBOLParser.FileControlParagraphContext;
import br.eti.rslemos.cobolg.COBOLParser.FileOrganizationIndexedContext;
import br.eti.rslemos.cobolg.COBOLParser.IdentificationDivisionContext;
import br.eti.rslemos.cobolg.COBOLParser.InputOutputSectionContext;
import br.eti.rslemos.cobolg.COBOLParser.ObjectComputerParagraphContext;
import br.eti.rslemos.cobolg.COBOLParser.ParagraphNameContext;
import br.eti.rslemos.cobolg.COBOLParser.ProceduralStatementContext;
import br.eti.rslemos.cobolg.COBOLParser.ProcedureDivisionContext;
import br.eti.rslemos.cobolg.COBOLParser.ProgramContext;
import br.eti.rslemos.cobolg.COBOLParser.SelectFileSentenceContext;
import br.eti.rslemos.cobolg.COBOLParser.SpecialNamesParagraphContext;
import br.eti.rslemos.cobolg.COBOLParser.SpecialNamesSentenceContext;
import br.eti.rslemos.cobolg.COBOLParser.UserDefinedProcedureSectionContext;
import br.eti.rslemos.cobolg.Compiler.FreeFormatCompiler;

public class FreeFormatUnitTest {
	public static final String SOURCE = join(
			"IDENTIFICATION DIVISION.",
			"PROGRAM-ID. HELLO-WORLD.",
			"*COMMENT LINE\r",
			"ENVIRONMENT DIVISION.",
			"CONFIGURATION SECTION.",
			"OBJECT-COMPUTER. IBM-370-148.",
			"SPECIAL-NAMES.",
			"    C02 IS LCP-CH2.",
			"INPUT-OUTPUT SECTION.",
			"FILE-CONTROL.",
			"    SELECT  IMPRES      ASSIGN TO UT-S-L439161.",
			"    SELECT  PRAMFIXO    ASSIGN TO UT-S-D433135.",
			"    SELECT  PROJEN-I    ASSIGN TO D433131",
			"                        RECORD KEY CHAVE",
			"                        ACCESS SEQUENTIAL",
			"                        STATUS IS PROJ-STATUS",
			"                        ORGANIZATION INDEXED.",
			"DATA DIVISION.",
			"PROCEDURE DIVISION.\r",
			"    DISPLAY 'Hello, world'.",
			"    STOP RUN.\r"
		);
	
	private static ProgramContext tree;
	
	@BeforeClass
	public static void compile() throws Exception {
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
	public void testEnvironmentDivisionPresence() {
		assertThat(tree.environmentDivision(), is(not(nullValue(EnvironmentDivisionContext.class))));
	}

	@Test
	public void testConfigurationSectionPresence() {
		assertThat(tree.environmentDivision().configurationSection(), is(not(nullValue(ConfigurationSectionContext.class))));
	}

	@Test
	public void testObjectComputerParagraphPresence() {
		assertThat(tree.environmentDivision().configurationSection().objectComputerParagraph(), is(not(nullValue(ObjectComputerParagraphContext.class))));
	}
	
	@Test
	public void testObjectComputerParagraph() {
		ObjectComputerParagraphContext objCompParagraph = tree.environmentDivision().configurationSection().objectComputerParagraph();
		
		assertThat(objCompParagraph.ID().getText(), is(equalTo("IBM-370-148")));
	}
	
	@Test
	public void testSpecialNamesParagraphPresence() {
		assertThat(tree.environmentDivision().configurationSection().specialNamesParagraph(), is(not(nullValue(SpecialNamesParagraphContext.class))));
	}
	
	@Test
	public void testSpecialNamesParagraph() {
		SpecialNamesParagraphContext specNamesParagraph = tree.environmentDivision().configurationSection().specialNamesParagraph();
		assertThat(specNamesParagraph.specialNamesSentence().size(), is(equalTo(1)));
		
		SpecialNamesSentenceContext specNamesSentence_0 = specNamesParagraph.specialNamesSentence(0);
		assertThat(specNamesSentence_0.ID(0).getText(), is(equalTo("C02")));
		assertThat(specNamesSentence_0.IS(), is(not(nullValue(TerminalNode.class))));
		assertThat(specNamesSentence_0.ID(1).getText(), is(equalTo("LCP-CH2")));
		
	}
	
	@Test
	public void testInputOutputSectionPresence() {
		assertThat(tree.environmentDivision().inputOutputSection(), is(not(nullValue(InputOutputSectionContext.class))));
	}

	@Test
	public void testFileControlParagraphPresence() {
		assertThat(tree.environmentDivision().inputOutputSection().fileControlParagraph(), is(not(nullValue(FileControlParagraphContext.class))));
	}

	@Test
	public void testFileControlParagraph() {
		FileControlParagraphContext fileCtlParagraph = tree.environmentDivision().inputOutputSection().fileControlParagraph();
		assertThat(fileCtlParagraph.selectFileSentence().size(), is(equalTo(3)));
		
		// "    SELECT  IMPRES      ASSIGN TO UT-S-L439161.",
		SelectFileSentenceContext selectFileSentence_0 = fileCtlParagraph.selectFileSentence(0);
		assertThat(selectFileSentence_0.ID(0).getText(), is(equalTo("IMPRES")));
		assertThat(selectFileSentence_0.ID(1).getText(), is(equalTo("UT-S-L439161")));
		// "    SELECT  PRAMFIXO    ASSIGN TO UT-S-D433135.",
		SelectFileSentenceContext selectFileSentence_1 = fileCtlParagraph.selectFileSentence(1);
		assertThat(selectFileSentence_1.ID(0).getText(), is(equalTo("PRAMFIXO")));
		assertThat(selectFileSentence_1.ID(1).getText(), is(equalTo("UT-S-D433135")));
		
	}

	@Test
	public void test3rdSelectFileSentence() {
		// "    SELECT  PROJEN-I    ASSIGN TO D433131",
		// "                        RECORD KEY CHAVE",
		// "                        ACCESS SEQUENTIAL",
		// "                        STATUS IS PROJ-STATUS",
		// "                        ORGANIZATION INDEXED.",
		SelectFileSentenceContext selectFileSentence_2 = tree.environmentDivision().inputOutputSection().fileControlParagraph().selectFileSentence(2);
		assertThat(selectFileSentence_2.ID(0).getText(), is(equalTo("PROJEN-I")));
		assertThat(selectFileSentence_2.ID(1).getText(), is(equalTo("D433131")));
		
		FileOrganizationIndexedContext fileOrganization = selectFileSentence_2.fileOrganizationIndexed();
		assertThat(fileOrganization, is(not(nullValue(FileOrganizationIndexedContext.class))));

		assertThat(fileOrganization.ID(0).getText(), is(equalTo("CHAVE")));
		assertThat(fileOrganization.ID(1).getText(), is(equalTo("PROJ-STATUS")));
	}

	@Test
	public void testDataDivisionPresence() {
		assertThat(tree.dataDivision(), is(not(nullValue(DataDivisionContext.class))));
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
