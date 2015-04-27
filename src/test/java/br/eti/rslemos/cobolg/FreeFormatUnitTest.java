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

import java.util.Iterator;

import org.antlr.v4.runtime.tree.TerminalNode;
import org.junit.BeforeClass;
import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.ConfigurationSectionContext;
import br.eti.rslemos.cobolg.COBOLParser.DataDescriptionParagraphContext;
import br.eti.rslemos.cobolg.COBOLParser.DataDivisionContext;
import br.eti.rslemos.cobolg.COBOLParser.EnvironmentDivisionContext;
import br.eti.rslemos.cobolg.COBOLParser.FdBlockClauseContext;
import br.eti.rslemos.cobolg.COBOLParser.FdDataRecordClauseContext;
import br.eti.rslemos.cobolg.COBOLParser.FdLabelRecordClauseContext;
import br.eti.rslemos.cobolg.COBOLParser.FdLinageClauseContext;
import br.eti.rslemos.cobolg.COBOLParser.FdRecordClauseContext;
import br.eti.rslemos.cobolg.COBOLParser.FdRecordingModeClauseContext;
import br.eti.rslemos.cobolg.COBOLParser.FdValueOfClauseContext;
import br.eti.rslemos.cobolg.COBOLParser.FileControlParagraphContext;
import br.eti.rslemos.cobolg.COBOLParser.FileDescriptionParagraphContext;
import br.eti.rslemos.cobolg.COBOLParser.FileOrganizationIndexedContext;
import br.eti.rslemos.cobolg.COBOLParser.FileSectionContext;
import br.eti.rslemos.cobolg.COBOLParser.IdentificationDivisionContext;
import br.eti.rslemos.cobolg.COBOLParser.IndexNameContext;
import br.eti.rslemos.cobolg.COBOLParser.InputOutputSectionContext;
import br.eti.rslemos.cobolg.COBOLParser.LinkageSectionContext;
import br.eti.rslemos.cobolg.COBOLParser.ObjectComputerParagraphContext;
import br.eti.rslemos.cobolg.COBOLParser.ParagraphNameContext;
import br.eti.rslemos.cobolg.COBOLParser.ProceduralStatementContext;
import br.eti.rslemos.cobolg.COBOLParser.ProcedureDivisionContext;
import br.eti.rslemos.cobolg.COBOLParser.ProgramContext;
import br.eti.rslemos.cobolg.COBOLParser.SelectFileSentenceContext;
import br.eti.rslemos.cobolg.COBOLParser.SpecialNamesParagraphContext;
import br.eti.rslemos.cobolg.COBOLParser.SpecialNamesSentenceContext;
import br.eti.rslemos.cobolg.COBOLParser.UserDefinedProcedureSectionContext;
import br.eti.rslemos.cobolg.COBOLParser.WorkingStorageSectionContext;
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
			"FILE SECTION.",
			"FD  FD0",
			"    BLOCK CONTAINS 5 TO 100 RECORDS",
			"    RECORD CONTAINS 80 TO 120 CHARACTERS",
			"    LABEL RECORD IS STANDARD",
			"    VALUE OF SYSVAR1 IS 'SYSVAR1' SYSVAR2 IS 'SYSVAR2'",
			"    DATA RECORDS ARE REC1 REC2",
			"    LINAGE IS 2 LINES",
			"      WITH FOOTING AT 2",
			"      LINES AT TOP 1",
			"      LINES AT BOTTOM 1",
			"    RECORDING MODE IS V.",
			"FD  FD1",
			"    BLOCK CONTAINS 120 CHARACTERS",
			"    RECORD IS VARYING IN SIZE FROM 10 TO 120 CHARACTERS",
			"      DEPENDING ON REC-SIZE.",
			"WORKING-STORAGE SECTION.",
			"77  WS-DEBUG             PIC ZZZ.ZZZ.ZZZ.ZZ9,999999-.",
			"77  WS-DEBUG1            PIC S9(8) COMP VALUE IS ZERO.",
			"01  WS-TAB-F-PRICE.",
			"    03  WS-TB-F-PRICE OCCURS 1000 TIMES",
			"        INDEXED BY IPRICE IPRICEUM IPRICEMIL IPRICELIMLOG",
			"                   IPRICELIMLOGANT.",
			"01  DESL17V00 REDEFINES DESL12V05 PIC S9(17) COMP-3.",
			"77  WS-DEBUG2            VALUE IS ZERO PIC S9(8) COMP.",
			"LINKAGE SECTION.",
			"01  LE-ENDI.",
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
	public void testFileSectionPresence() {
		assertThat(tree.dataDivision().fileSection(), is(not(nullValue(FileSectionContext.class))));
	}

	@Test
	public void testFileDescriptor0Presence() {
		FileDescriptionParagraphContext fd0 = tree.dataDivision().fileSection().fileDescriptionParagraph(0);
		assertThat(fd0.fileName().getText(), is(equalTo("FD0")));
	}

	@Test
	public void testFD0BlockClause() {
		FdBlockClauseContext blockClause = tree.dataDivision().fileSection().fileDescriptionParagraph(0).fdBlockClause();
		assertThat(blockClause.from.getText(), is(equalTo("5")));
		assertThat(blockClause.to.getText(), is(equalTo("100")));
		assertThat(blockClause.RECORDS(), is(not(nullValue(TerminalNode.class))));
	}

	@Test
	public void testFD0RecordClause() {
		FdRecordClauseContext recordClause = tree.dataDivision().fileSection().fileDescriptionParagraph(0).fdRecordClause();
		assertThat(recordClause.from.getText(), is(equalTo("80")));
		assertThat(recordClause.to.getText(), is(equalTo("120")));
	}

	@Test
	public void testFD0LabelRecordClause() {
		FdLabelRecordClauseContext labelRecordClause = tree.dataDivision().fileSection().fileDescriptionParagraph(0).fdLabelRecordClause();
		assertThat(labelRecordClause.STANDARD(), is(not(nullValue(TerminalNode.class))));
	}

	@Test
	public void testFD0ValueOfClause() {
		FdValueOfClauseContext valueOfClause = tree.dataDivision().fileSection().fileDescriptionParagraph(0).fdValueOfClause();
		assertThat(valueOfClause.systemName(0).getText(), is(equalTo("SYSVAR1")));
		assertThat(valueOfClause.literal(0).getText(), is(equalTo("'SYSVAR1'")));
		assertThat(valueOfClause.systemName(1).getText(), is(equalTo("SYSVAR2")));
		assertThat(valueOfClause.literal(1).getText(), is(equalTo("'SYSVAR2'")));
	}

	@Test
	public void testFD0DataRecordClause() {
		FdDataRecordClauseContext dataRecordClause = tree.dataDivision().fileSection().fileDescriptionParagraph(0).fdDataRecordClause();
		assertThat(dataRecordClause.dataName(0).getText(), is(equalTo("REC1")));
		assertThat(dataRecordClause.dataName(1).getText(), is(equalTo("REC2")));
	}

	@Test
	public void testFD0LinageClause() {
		FdLinageClauseContext linageClause = tree.dataDivision().fileSection().fileDescriptionParagraph(0).fdLinageClause();
		assertThat(linageClause.INTEGER().getText(), is(equalTo("2")));
		assertThat(linageClause.footingAt().INTEGER().getText(), is(equalTo("2")));
		assertThat(linageClause.linesAtBottom().INTEGER().getText(), is(equalTo("1")));
		assertThat(linageClause.linesAtTop().INTEGER().getText(), is(equalTo("1")));
	}

	@Test
	public void testFD0RecordingModeClause() {
		FdRecordingModeClauseContext recordingModeClause = tree.dataDivision().fileSection().fileDescriptionParagraph(0).fdRecordingModeClause();
		assertThat(recordingModeClause.V(), is(not(nullValue(TerminalNode.class))));
	}

	@Test
	public void testFileDescriptor1Presence() {
		FileDescriptionParagraphContext fd1 = tree.dataDivision().fileSection().fileDescriptionParagraph(1);
		assertThat(fd1.fileName().getText(), is(equalTo("FD1")));
	}

	@Test
	public void testFD1BlockClause() {
		FdBlockClauseContext blockClause = tree.dataDivision().fileSection().fileDescriptionParagraph(1).fdBlockClause();
		assertThat(blockClause.to.getText(), is(equalTo("120")));
		assertThat(blockClause.CHARACTERS(), is(not(nullValue(TerminalNode.class))));
	}

	@Test
	public void testFD1RecordClause() {
		FdRecordClauseContext recordClause = tree.dataDivision().fileSection().fileDescriptionParagraph(1).fdRecordClause();
		assertThat(recordClause.from.getText(), is(equalTo("10")));
		assertThat(recordClause.to.getText(), is(equalTo("120")));
		assertThat(recordClause.dependingOn.getText(), is(equalTo("REC-SIZE")));
	}

	@Test
	public void testWorkingSectionPresence() {
		assertThat(tree.dataDivision().workingStorageSection(), is(not(nullValue(WorkingStorageSectionContext.class))));
	}
	
	@Test
	public void testDataDescriptionParagraph1() {
		DataDescriptionParagraphContext dataDescriptionParagraph = tree.dataDivision().workingStorageSection().dataDescriptionParagraph(0);
		
		// 77  WS-DEBUG             PIC ZZZ.ZZZ.ZZZ.ZZ9,999999-.
		assertThat(dataDescriptionParagraph.levelNumber().getText(), is(equalTo("77")));
		assertThat(dataDescriptionParagraph.dataName().ID().getText(), is(equalTo("WS-DEBUG")));
		assertThat(dataDescriptionParagraph.pictureClause_.PICTURESTRING().getText(), is(equalTo("ZZZ.ZZZ.ZZZ.ZZ9,999999-")));
	}

	@Test
	public void testDataDescriptionParagraph2() {
		DataDescriptionParagraphContext dataDescriptionParagraph = tree.dataDivision().workingStorageSection().dataDescriptionParagraph(1);
		
		// 77  WS-DEBUG1            PIC S9(8) COMP VALUE IS ZERO.
		assertThat(dataDescriptionParagraph.levelNumber().getText(), is(equalTo("77")));
		assertThat(dataDescriptionParagraph.dataName().ID().getText(), is(equalTo("WS-DEBUG1")));
		assertThat(dataDescriptionParagraph.pictureClause_.PICTURESTRING().getText(), is(equalTo("S9(8)")));
		assertThat(dataDescriptionParagraph.usageClause_.usage().COMPUTATIONAL().getText(), is(equalTo("COMP")));
		assertThat(dataDescriptionParagraph.valueClause_.literal().figurativeConstant().ZERO().getText(), is(equalTo("ZERO")));
	}

	@Test
	public void testDataDescriptionParagraph4() {
		DataDescriptionParagraphContext dataDescriptionParagraph = tree.dataDivision().workingStorageSection().dataDescriptionParagraph(3);
		
		//    03  WS-TB-F-PRICE OCCURS 1000 TIMES
		//        INDEXED BY IPRICE IPRICEUM IPRICEMIL IPRICELIMLOG
		//                   IPRICELIMLOGANT.
		assertThat(dataDescriptionParagraph.levelNumber().getText(), is(equalTo("03")));
		assertThat(dataDescriptionParagraph.dataName().ID().getText(), is(equalTo("WS-TB-F-PRICE")));
		assertThat(dataDescriptionParagraph.occursClause_.INTEGER().getText(), is(equalTo("1000")));
		
		Iterator<IndexNameContext> it = dataDescriptionParagraph.occursClause_.indexName().iterator();
		for (String indexName : new String[] {"IPRICE", "IPRICEUM", "IPRICEMIL", "IPRICELIMLOG", "IPRICELIMLOGANT"}) {
			assertThat(it.next().getText(), is(equalTo(indexName))); 
		}
	}

	@Test
	public void testDataDescriptionParagraph5() {
		DataDescriptionParagraphContext dataDescriptionParagraph = tree.dataDivision().workingStorageSection().dataDescriptionParagraph(4);
		
		// 01  DESL17V00 REDEFINES DESL12V05 PIC S9(17) COMP-3.
		assertThat(dataDescriptionParagraph.levelNumber().getText(), is(equalTo("01")));
		assertThat(dataDescriptionParagraph.dataName().ID().getText(), is(equalTo("DESL17V00")));
		assertThat(dataDescriptionParagraph.redefinesClause().dataName().ID().getText(), is(equalTo("DESL12V05")));
		assertThat(dataDescriptionParagraph.pictureClause_.PICTURESTRING().getText(), is(equalTo("S9(17)")));
		assertThat(dataDescriptionParagraph.usageClause_.usage().COMPUTATIONAL_3().getText(), is(equalTo("COMP-3")));
	}

	@Test
	public void testDataDescriptionParagraph6() {
		DataDescriptionParagraphContext dataDescriptionParagraph = tree.dataDivision().workingStorageSection().dataDescriptionParagraph(5);
		
		// 77  WS-DEBUG2            VALUE IS ZERO PIC S9(8) COMP.
		assertThat(dataDescriptionParagraph.levelNumber().getText(), is(equalTo("77")));
		assertThat(dataDescriptionParagraph.dataName().ID().getText(), is(equalTo("WS-DEBUG2")));
		assertThat(dataDescriptionParagraph.pictureClause_.PICTURESTRING().getText(), is(equalTo("S9(8)")));
		assertThat(dataDescriptionParagraph.usageClause_.usage().COMPUTATIONAL().getText(), is(equalTo("COMP")));
		assertThat(dataDescriptionParagraph.valueClause_.literal().figurativeConstant().ZERO().getText(), is(equalTo("ZERO")));
	}

	@Test
	public void testLinkageSectionPresence() {
		assertThat(tree.dataDivision().linkageSection(), is(not(nullValue(LinkageSectionContext.class))));
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
