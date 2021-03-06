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

import static br.eti.rslemos.cobolg.PostProcessingCompiler.parserForFreeFormat;
import static br.eti.rslemos.cobolg.TextHelper.join;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

import java.io.StringReader;
import java.util.Iterator;
import java.util.List;

import org.antlr.v4.runtime.RuleContext;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.junit.BeforeClass;
import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.*;

public class FreeFormatUnitTest {
	public static final String SOURCE = join(
			"IDENTIFICATION DIVISION.",
			"PROGRAM-ID. HELLO-WORLD.",
			"*COMMENT LINE\r",
			"ENVIRONMENT DIVISION.",
			"CONFIGURATION SECTION.",
			"OBJECT-COMPUTER. IBM-370-148.",
			"SPECIAL-NAMES.",
			"    C02 IS LCP-CH2",
			"    DECIMAL-POINT IS COMMA.",
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
			"FD  FD0 IS EXTERNAL IS GLOBAL",
			"    BLOCK CONTAINS 5 TO 100 RECORDS",
			"    RECORD CONTAINS 80 TO 120 CHARACTERS",
			"    LABEL RECORD IS STANDARD",
			"    VALUE OF SYSVAR1 IS 'SYSVAR1' SYSVAR2 IS 'SYSVAR2'",
			"    DATA RECORDS ARE REC1 REC2",
			"    LINAGE IS 2 LINES",
			"      WITH FOOTING AT 2",
			"      LINES AT TOP 1",
			"      LINES AT BOTTOM 1",
			"    RECORDING MODE IS V",
			"    CODE-SET IS ALPHABET1.",
			"FD  FD1",
			"    BLOCK CONTAINS 120 CHARACTERS",
			"    RECORD IS VARYING IN SIZE FROM 10 TO 120 CHARACTERS",
			"      DEPENDING ON REC-SIZE.",
			"FD  FD3  COPY XZT0190.",
			"WORKING-STORAGE SECTION.",
			"77  WS-DEBUG             PIC ZZZ.ZZZ.ZZZ.ZZ9,999999-.",
			"77  WS-DEBUG1            PIC S9(8) COMP VALUE IS ZERO.",
			"01  WS-TAB-F-PRICE.",
			"    03  WS-TB-F-PRICE OCCURS 1000 TIMES",
			"        INDEXED BY IPRICE IPRICEUM IPRICEMIL IPRICELIMLOG",
			"                   IPRICELIMLOGANT.",
			"01  DESL17V00 REDEFINES DESL12V05 PIC S9(17) COMP-3.",
			"77  WS-DEBUG2            VALUE IS ZERO PIC S9(8) COMP.",
			"01  LE-TABE.            COPY XZT0100.",
			"LINKAGE SECTION.",
			"01  LE-ENDI.            COPY XZT0009.",
			"EJECT",
			"PROCEDURE DIVISION.\r",
			"    MOVE -1 TO WS-DEBUG1.",
			"    DISPLAY 'Hello, world'.",
			"    STOP RUN.\r"
		);
	
	private static ProgramContext tree;
	
	@BeforeClass
	public static void compile() throws Exception {
		Compiler compiler = parserForFreeFormat(new StringReader(SOURCE));
		BatchContext tree0 = compiler.batch();
		assertThat(tree0, is(not(nullValue(BatchContext.class))));
		tree = tree0.program(0);
		assertThat(tree, is(not(nullValue(ProgramContext.class))));
	}
	
	@Test
	public void testIdentificationDivisionPresence() {
		assertThat(tree.identificationDivision(), is(not(nullValue(IdentificationDivisionContext.class))));
	}

	@Test
	public void testIdentificationDivision() {
		assertThat(tree.identificationDivision().programName().getText(), is(equalTo("HELLO-WORLD")));
	}

	@Test
	public void testEnvironmentDivisionPresence() {
		assertThat(tree.environmentDivision(), is(not(nullValue(EnvironmentDivisionContext.class))));
	}

	@Test
	public void testConfigurationSectionPresence() {
		assertThat(tree.environmentDivision().environmentDivisionContent().configurationSection(), is(not(nullValue(ConfigurationSectionContext.class))));
	}

	@Test
	public void testObjectComputerParagraphPresence() {
		assertThat(tree.environmentDivision().environmentDivisionContent().configurationSection().objectComputerParagraph(), is(not(nullValue(ObjectComputerParagraphContext.class))));
	}
	
	@Test
	public void testObjectComputerParagraph() {
		ObjectComputerParagraphContext objCompParagraph = tree.environmentDivision().environmentDivisionContent().configurationSection().objectComputerParagraph();
		
		assertThat(objCompParagraph.computerName().getText(), is(equalTo("IBM-370-148")));
	}
	
	@Test
	public void testSpecialNamesParagraphPresence() {
		assertThat(tree.environmentDivision().environmentDivisionContent().configurationSection().specialNamesParagraph(), is(not(nullValue(SpecialNamesParagraphContext.class))));
	}
	
	@Test
	public void testSpecialNamesParagraph() {
		SpecialNamesParagraphContext specNamesParagraph = tree.environmentDivision().environmentDivisionContent().configurationSection().specialNamesParagraph();
		assertThat(specNamesParagraph.specialNamesClause().size(), is(equalTo(2)));
		
		EnvironmentAssignmentClauseContext environmentAssignmentClause_0 = specNamesParagraph.specialNamesClause(0).environmentAssignmentClause();
		assertThat(environmentAssignmentClause_0.environmentName().getText(), is(equalTo("C02")));
		assertThat(environmentAssignmentClause_0.IS(), is(not(nullValue(TerminalNode.class))));
		assertThat(environmentAssignmentClause_0.mnemonicName().getText(), is(equalTo("LCP-CH2")));
		
	}
	
	@Test
	public void testInputOutputSectionPresence() {
		assertThat(tree.environmentDivision().environmentDivisionContent().inputOutputSection(), is(not(nullValue(InputOutputSectionContext.class))));
	}

	@Test
	public void testFileControlParagraphPresence() {
		assertThat(tree.environmentDivision().environmentDivisionContent().inputOutputSection().fileControlParagraph(), is(not(nullValue(FileControlParagraphContext.class))));
	}

	@Test
	public void testFileControlParagraph() {
		FileControlParagraphContext fileCtlParagraph = tree.environmentDivision().environmentDivisionContent().inputOutputSection().fileControlParagraph();
		assertThat(fileCtlParagraph.selectEntry().size(), is(equalTo(3)));
		
		// "    SELECT  IMPRES      ASSIGN TO UT-S-L439161.",
		SequentialFileControlEntryContext selectEntry_0 = fileCtlParagraph.selectEntry(0).sequentialFileControlEntry();
		assertThat(selectEntry_0.selectClause().fileName().getText(), is(equalTo("IMPRES")));
		assertThat(selectEntry_0.assignClause().assignmentName(0).getText(), is(equalTo("UT-S-L439161")));
		// "    SELECT  PRAMFIXO    ASSIGN TO UT-S-D433135.",
		SequentialFileControlEntryContext selectEntry_1 = fileCtlParagraph.selectEntry(1).sequentialFileControlEntry();
		assertThat(selectEntry_1.selectClause().fileName().getText(), is(equalTo("PRAMFIXO")));
		assertThat(selectEntry_1.assignClause().assignmentName(0).getText(), is(equalTo("UT-S-D433135")));
	}

	@Test
	public void test3rdSelectFileSentence() {
		// "    SELECT  PROJEN-I    ASSIGN TO D433131",
		// "                        RECORD KEY CHAVE",
		// "                        ACCESS SEQUENTIAL",
		// "                        STATUS IS PROJ-STATUS",
		// "                        ORGANIZATION INDEXED.",
		IndexedFileControlEntryContext selectEntry_2 = tree.environmentDivision().environmentDivisionContent().inputOutputSection().fileControlParagraph().selectEntry(2).indexedFileControlEntry();
		assertThat(selectEntry_2.selectClause().fileName().getText(), is(equalTo("PROJEN-I")));
		assertThat(selectEntry_2.assignClause().assignmentName(0).getText(), is(equalTo("D433131")));

		assertThat(selectEntry_2.organizationIsIndexed(), is(not(nullValue(RuleContext.class))));

		List<IndexedFileControlEntryClauseContext> indexedClauses = selectEntry_2.indexedFileControlEntryClause();
		assertThat(indexedClauses.size(), is(equalTo(3)));
		assertThat(indexedClauses.get(0).recordKeyClause().refDataName().getText(), is(equalTo("CHAVE")));
		assertThat(indexedClauses.get(1).accessModeClause().accessMode().SEQUENTIAL(), is(not(nullValue(TerminalNode.class))));
		assertThat(indexedClauses.get(2).fileStatusClause().refDataName(0).getText(), is(equalTo("PROJ-STATUS")));
	}

	@Test
	public void testDataDivisionPresence() {
		assertThat(tree.dataDivision(), is(not(nullValue(DataDivisionContext.class))));
	}

	@Test
	public void testFileSectionPresence() {
		assertThat(tree.dataDivision().dataDivisionContent().fileSection(), is(not(nullValue(FileSectionContext.class))));
	}

	@Test
	public void testFileDescriptor0Presence() {
		FileDescriptionParagraphContext fd0 = tree.dataDivision().dataDivisionContent().fileSection().fileDescriptionParagraph(0);
		assertThat(fd0.fileDescriptionEntry().fileName().getText(), is(equalTo("FD0")));
		assertThat(fd0.fileDescriptionEntry().fileDescriptionEntryClauses().fileDescriptionEntryClause(0).externalClause().EXTERNAL(), is(not(nullValue(TerminalNode.class))));
		assertThat(fd0.fileDescriptionEntry().fileDescriptionEntryClauses().fileDescriptionEntryClause(1).globalClause().GLOBAL(), is(not(nullValue(TerminalNode.class))));
	}

	@Test
	public void testFD0BlockClause() {
		BlockContainsClauseContext blockClause = tree.dataDivision().dataDivisionContent().fileSection().fileDescriptionParagraph(0).fileDescriptionEntry().fileDescriptionEntryClauses().fileDescriptionEntryClause(2).blockContainsClause();
		assertThat(blockClause.INTEGER(0).getText(), is(equalTo("5")));
		assertThat(blockClause.INTEGER(1).getText(), is(equalTo("100")));
		assertThat(blockClause.RECORDS(), is(not(nullValue(TerminalNode.class))));
	}

	@Test
	public void testFD0RecordClause() {
		RecordClauseContext recordClause = tree.dataDivision().dataDivisionContent().fileSection().fileDescriptionParagraph(0).fileDescriptionEntry().fileDescriptionEntryClauses().fileDescriptionEntryClause(3).recordClause();
		assertThat(recordClause.INTEGER(0).getText(), is(equalTo("80")));
		assertThat(recordClause.INTEGER(1).getText(), is(equalTo("120")));
	}

	@Test
	public void testFD0LabelRecordClause() {
		LabelRecordClauseContext labelRecordClause = tree.dataDivision().dataDivisionContent().fileSection().fileDescriptionParagraph(0).fileDescriptionEntry().fileDescriptionEntryClauses().fileDescriptionEntryClause(4).labelRecordClause();
		assertThat(labelRecordClause.STANDARD(), is(not(nullValue(TerminalNode.class))));
	}

	@Test
	public void testFD0ValueOfClause() {
		ValueOfClauseContext valueOfClause = tree.dataDivision().dataDivisionContent().fileSection().fileDescriptionParagraph(0).fileDescriptionEntry().fileDescriptionEntryClauses().fileDescriptionEntryClause(5).valueOfClause();
		assertThat(valueOfClause.systemName(0).getText(), is(equalTo("SYSVAR1")));
		assertThat(valueOfClause.literal(0).getText(), is(equalTo("'SYSVAR1'")));
		assertThat(valueOfClause.systemName(1).getText(), is(equalTo("SYSVAR2")));
		assertThat(valueOfClause.literal(1).getText(), is(equalTo("'SYSVAR2'")));
	}

	@Test
	public void testFD0DataRecordClause() {
		DataRecordClauseContext dataRecordClause = tree.dataDivision().dataDivisionContent().fileSection().fileDescriptionParagraph(0).fileDescriptionEntry().fileDescriptionEntryClauses().fileDescriptionEntryClause(6).dataRecordClause();
		assertThat(dataRecordClause.dataName(0).getText(), is(equalTo("REC1")));
		assertThat(dataRecordClause.dataName(1).getText(), is(equalTo("REC2")));
	}

	@Test
	public void testFD0LinageClause() {
		LinageClauseContext linageClause = tree.dataDivision().dataDivisionContent().fileSection().fileDescriptionParagraph(0).fileDescriptionEntry().fileDescriptionEntryClauses().fileDescriptionEntryClause(7).linageClause();
		assertThat(linageClause.INTEGER().getText(), is(equalTo("2")));
		assertThat(linageClause.footingAtPhrase().INTEGER().getText(), is(equalTo("2")));
		assertThat(linageClause.linesAtBottomPhrase().INTEGER().getText(), is(equalTo("1")));
		assertThat(linageClause.linesAtTopPhrase().INTEGER().getText(), is(equalTo("1")));
	}

	@Test
	public void testFD0RecordingModeClause() {
		RecordingModeClauseContext recordingModeClause = tree.dataDivision().dataDivisionContent().fileSection().fileDescriptionParagraph(0).fileDescriptionEntry().fileDescriptionEntryClauses().fileDescriptionEntryClause(8).recordingModeClause();
		assertThat(recordingModeClause.V(), is(not(nullValue(TerminalNode.class))));
	}

	@Test
	public void testFD0CodeSetClause() {
		CodeSetClauseContext codeSetClause = tree.dataDivision().dataDivisionContent().fileSection().fileDescriptionParagraph(0).fileDescriptionEntry().fileDescriptionEntryClauses().fileDescriptionEntryClause(9).codeSetClause();
		assertThat(codeSetClause.alphabetName().USERDEFINEDWORD().getText(), is(equalTo("ALPHABET1")));
	}

	@Test
	public void testFileDescriptor1Presence() {
		FileDescriptionParagraphContext fd1 = tree.dataDivision().dataDivisionContent().fileSection().fileDescriptionParagraph(1);
		assertThat(fd1.fileDescriptionEntry().fileName().getText(), is(equalTo("FD1")));
	}

	@Test
	public void testFD1BlockClause() {
		BlockContainsClauseContext blockClause = tree.dataDivision().dataDivisionContent().fileSection().fileDescriptionParagraph(1).fileDescriptionEntry().fileDescriptionEntryClauses().fileDescriptionEntryClause(0).blockContainsClause();
		assertThat(blockClause.INTEGER(0).getText(), is(equalTo("120")));
		assertThat(blockClause.CHARACTERS(), is(not(nullValue(TerminalNode.class))));
	}

	@Test
	public void testFD1RecordClause() {
		RecordClauseContext recordClause = tree.dataDivision().dataDivisionContent().fileSection().fileDescriptionParagraph(1).fileDescriptionEntry().fileDescriptionEntryClauses().fileDescriptionEntryClause(1).recordClause();
		assertThat(recordClause.INTEGER(0).getText(), is(equalTo("10")));
		assertThat(recordClause.INTEGER(1).getText(), is(equalTo("120")));
		assertThat(recordClause.dataName().getText(), is(equalTo("REC-SIZE")));
	}

	@Test
	public void testWorkingSectionPresence() {
		assertThat(tree.dataDivision().dataDivisionContent().workingStorageSection(), is(not(nullValue(WorkingStorageSectionContext.class))));
	}
	
	@Test
	public void testRecordDescriptionEntry1() {
		// 77  WS-DEBUG             PIC ZZZ.ZZZ.ZZZ.ZZ9,999999-.
		RecordDescriptionEntryContext recordDescriptionEntry = tree.dataDivision().dataDivisionContent().workingStorageSection().recordDescriptionEntry(0);
		
		DataDescriptionEntryContext dataDescriptionEntry = recordDescriptionEntry.dataDescriptionEntry();
		assertThat(dataDescriptionEntry.levelNumber().getText(), is(equalTo("77")));
		assertThat(dataDescriptionEntry.dataName().USERDEFINEDWORD().getText(), is(equalTo("WS-DEBUG")));
		assertThat(dataDescriptionEntry.dataDescriptionClauses().dataDescriptionClause(0).pictureClause().PICTURESTRING().getText(), is(equalTo("ZZZ.ZZZ.ZZZ.ZZ9,999999-")));
		
		assertThat(recordDescriptionEntry.recordDescriptionEntry().size(), is(equalTo(0)));
	}

	@Test
	public void testRecordDescriptionEntry2() {
		// 77  WS-DEBUG1            PIC S9(8) COMP VALUE IS ZERO.
		RecordDescriptionEntryContext recordDescriptionEntry = tree.dataDivision().dataDivisionContent().workingStorageSection().recordDescriptionEntry(1);
		
		DataDescriptionEntryContext dataDescriptionEntry = recordDescriptionEntry.dataDescriptionEntry();
		assertThat(dataDescriptionEntry.levelNumber().getText(), is(equalTo("77")));
		assertThat(dataDescriptionEntry.dataName().USERDEFINEDWORD().getText(), is(equalTo("WS-DEBUG1")));
		assertThat(dataDescriptionEntry.dataDescriptionClauses().dataDescriptionClause(0).pictureClause().PICTURESTRING().getText(), is(equalTo("S9(8)")));
		assertThat(dataDescriptionEntry.dataDescriptionClauses().dataDescriptionClause(1).usageClause().usage().COMP().getText(), is(equalTo("COMP")));
		assertThat(dataDescriptionEntry.dataDescriptionClauses().dataDescriptionClause(2).valueClause().literal(0).figurativeConstant().ZERO().getText(), is(equalTo("ZERO")));
		
		assertThat(recordDescriptionEntry.recordDescriptionEntry().size(), is(equalTo(0)));
	}

	@Test
	public void testRecordDescriptionEntry3() {
		// 01  WS-TAB-F-PRICE.
		//     03  WS-TB-F-PRICE OCCURS 1000 TIMES
		//         INDEXED BY IPRICE IPRICEUM IPRICEMIL IPRICELIMLOG
		//                    IPRICELIMLOGANT.
		RecordDescriptionEntryContext recordDescriptionEntry = tree.dataDivision().dataDivisionContent().workingStorageSection().recordDescriptionEntry(2);
		
		DataDescriptionEntryContext dataDescriptionEntry = recordDescriptionEntry.dataDescriptionEntry();
		assertThat(dataDescriptionEntry.levelNumber().getText(), is(equalTo("01")));
		assertThat(dataDescriptionEntry.dataName().USERDEFINEDWORD().getText(), is(equalTo("WS-TAB-F-PRICE")));
		
		assertThat(recordDescriptionEntry.recordDescriptionEntry().size(), is(equalTo(1)));
		
		recordDescriptionEntry = recordDescriptionEntry.recordDescriptionEntry(0);
		
		dataDescriptionEntry = recordDescriptionEntry.dataDescriptionEntry();
		assertThat(dataDescriptionEntry.levelNumber().getText(), is(equalTo("03")));
		assertThat(dataDescriptionEntry.dataName().USERDEFINEDWORD().getText(), is(equalTo("WS-TB-F-PRICE")));
		assertThat(dataDescriptionEntry.dataDescriptionClauses().dataDescriptionClause(0).occursClause().INTEGER(0).getText(), is(equalTo("1000")));
		
		Iterator<IndexNameContext> it = dataDescriptionEntry.dataDescriptionClauses().dataDescriptionClause(0).occursClause().indexedByPhrase().indexName().iterator();
		for (String indexName : new String[] {"IPRICE", "IPRICEUM", "IPRICEMIL", "IPRICELIMLOG", "IPRICELIMLOGANT"}) {
			assertThat(it.next().getText(), is(equalTo(indexName)));
		}
	}

	@Test
	public void testRecordDescriptionEntry4() {
		// 01  DESL17V00 REDEFINES DESL12V05 PIC S9(17) COMP-3.
		RecordDescriptionEntryContext recordDescriptionEntry = tree.dataDivision().dataDivisionContent().workingStorageSection().recordDescriptionEntry(3);
		
		DataDescriptionEntryContext dataDescriptionEntry = recordDescriptionEntry.dataDescriptionEntry();
		assertThat(dataDescriptionEntry.levelNumber().getText(), is(equalTo("01")));
		assertThat(dataDescriptionEntry.dataName().USERDEFINEDWORD().getText(), is(equalTo("DESL17V00")));
		assertThat(dataDescriptionEntry.redefinesClause().dataName().USERDEFINEDWORD().getText(), is(equalTo("DESL12V05")));
		assertThat(dataDescriptionEntry.dataDescriptionClauses().dataDescriptionClause(0).pictureClause().PICTURESTRING().getText(), is(equalTo("S9(17)")));
		assertThat(dataDescriptionEntry.dataDescriptionClauses().dataDescriptionClause(1).usageClause().usage().COMP_3().getText(), is(equalTo("COMP-3")));
		
		assertThat(recordDescriptionEntry.recordDescriptionEntry().size(), is(equalTo(0)));
	}

	@Test
	public void testRecordDescriptionEntry5() {
		// 77  WS-DEBUG2            VALUE IS ZERO PIC S9(8) COMP.
		RecordDescriptionEntryContext recordDescriptionEntry = tree.dataDivision().dataDivisionContent().workingStorageSection().recordDescriptionEntry(4);
		
		DataDescriptionEntryContext dataDescriptionEntry = recordDescriptionEntry.dataDescriptionEntry();
		assertThat(dataDescriptionEntry.levelNumber().getText(), is(equalTo("77")));
		assertThat(dataDescriptionEntry.dataName().USERDEFINEDWORD().getText(), is(equalTo("WS-DEBUG2")));
		assertThat(dataDescriptionEntry.dataDescriptionClauses().dataDescriptionClause(0).valueClause().literal(0).figurativeConstant().ZERO().getText(), is(equalTo("ZERO")));
		assertThat(dataDescriptionEntry.dataDescriptionClauses().dataDescriptionClause(1).pictureClause().PICTURESTRING().getText(), is(equalTo("S9(8)")));
		assertThat(dataDescriptionEntry.dataDescriptionClauses().dataDescriptionClause(2).usageClause().usage().COMP().getText(), is(equalTo("COMP")));
		
		assertThat(recordDescriptionEntry.recordDescriptionEntry().size(), is(equalTo(0)));
	}
	
	@Test
	public void testCompilerStatements() {
		// we can't access the statements direcly
		// we have to iterate over children
		
		// 0 - IDENTIFICATION DIVISION
		// 1 - ENVIRONMENT DIVISION
		// 2 - DATA DIVISION
		// 3 - COPY XZT0009.
		// 4 - EJECT
		// 5 - PROCEDURE DIVISION
		CompilerStatementContext eject = (CompilerStatementContext)tree.children.get(4);
		assertThat(eject.EJECT().getText(), is(equalTo("EJECT")));

		FileDescriptionParagraphContext fd3 = tree.dataDivision().dataDivisionContent().fileSection().fileDescriptionParagraph(2);
		// 0 - FD
		// 1 - FD3
		// 2 - COPY XZT0190.
		CompilerStatementContext copyXZT0190 = (CompilerStatementContext) fd3.children.get(1);
		assertThat(copyXZT0190.COPY().getText(), is(equalTo("COPY")));
		assertThat(copyXZT0190.COMPILER_ID().getText(), is(equalTo("XZT0190")));
		
		// 0 - FILE SECTION ...
		// 1 - WORKING-STORAGE SECTION ...
		// 2 - COPY XZT0100.
		CompilerStatementContext copyXZT0100 = (CompilerStatementContext) tree.dataDivision().dataDivisionContent().children.get(2);
		assertThat(copyXZT0100.COPY().getText(), is(equalTo("COPY")));
		assertThat(copyXZT0100.COMPILER_ID().getText(), is(equalTo("XZT0100")));
		
		// 0 - IDENTIFICATION DIVISION
		// 1 - ENVIRONMENT DIVISION
		// 2 - DATA DIVISION
		// 3 - COPY XZT0009.
		// 4 - EJECT
		// 5 - PROCEDURE DIVISION
		CompilerStatementContext copyXZT0009 = (CompilerStatementContext) tree.children.get(3);
		assertThat(copyXZT0009.COPY().getText(), is(equalTo("COPY")));
		assertThat(copyXZT0009.COMPILER_ID().getText(), is(equalTo("XZT0009")));
	}

	@Test
	public void testLinkageSectionPresence() {
		assertThat(tree.dataDivision().dataDivisionContent().linkageSection(), is(not(nullValue(LinkageSectionContext.class))));
	}

	@Test
	public void testProcedureDivisionPresence() {
		assertThat(tree.procedureDivision(), is(not(nullValue(ProcedureDivisionContext.class))));
	}

	@Test
	public void testProcedureDivision() {
		ProcedureDivisionContentContext procDivision = tree.procedureDivision().procedureDivisionContent();
		
		assertThat(procDivision.namedProceduralSection().size(), is(equalTo(0)));
		UnnamedProceduralSectionContext unnamedProceduralSection = procDivision.unnamedProceduralSection();

		assertThat(unnamedProceduralSection.namedProceduralParagraph().size(), is(equalTo(0)));
		UnnamedProceduralParagraphContext unnamedProceduralParagraph = unnamedProceduralSection.unnamedProceduralParagraph();

		assertThat(unnamedProceduralParagraph.proceduralSentence().size(), is(equalTo(3)));

		ProceduralStatementContext statement_0 = unnamedProceduralParagraph.proceduralSentence(0).proceduralStatement(0);
		assertThat(statement_0.stmtMOVE().MOVE(), is(not(nullValue(TerminalNode.class))));
		
		ProceduralStatementContext statement_1 = unnamedProceduralParagraph.proceduralSentence(1).proceduralStatement(0);
		assertThat(statement_1.stmtDISPLAY().DISPLAY(), is(not(nullValue(TerminalNode.class))));
		
		ProceduralStatementContext statement_2 = unnamedProceduralParagraph.proceduralSentence(2).proceduralStatement(0);
		assertThat(statement_2.stmtSTOPRUN().STOP(), is(not(nullValue(TerminalNode.class))));
	}
}
