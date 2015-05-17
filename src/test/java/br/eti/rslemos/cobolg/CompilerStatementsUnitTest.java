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

import br.eti.rslemos.cobolg.COBOLParser.CompilerStatementsContext;
import br.eti.rslemos.cobolg.COBOLParser.FileSectionContext;
import br.eti.rslemos.cobolg.COBOLParser.ProceduralStatementContext;
import br.eti.rslemos.cobolg.COBOLParser.ProgramContext;
import br.eti.rslemos.cobolg.COBOLParser.WorkingStorageSectionContext;
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

	@Test
	public void testEJECTBetweenDivisions () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"IDENTIFICATION DIVISION.",
				"PROGRAM-NAME. X.",
				"EJECT",
				"PROCEDURE DIVISION.",
				"    STOP RUN."
			)));
		
		ProgramContext mainTree = compiler.compile();
		String toString = mainTree.toStringTree(compiler.mainParser);

		assertThat(toString, is(equalTo("(program "
				+ "(identificationDivision IDENTIFICATION DIVISION . PROGRAM-NAME . X .) "
				+ "(compilerStatement EJECT) "
				+ "(procedureDivision PROCEDURE DIVISION . (unnamedProceduralSection (unnamedProceduralParagraph (proceduralStatement STOP RUN .)))))")));		
	}

	@Test
	public void testEJECTInsideDivision () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"IDENTIFICATION DIVISION.",
				"PROGRAM-NAME. X.",
				"PROCEDURE DIVISION.",
				"EJECT",
				"    STOP RUN."
			)));
		
		ProgramContext mainTree = compiler.compile();
		String toString = mainTree.toStringTree(compiler.mainParser);

		assertThat(toString, is(equalTo("(program "
				+ "(identificationDivision IDENTIFICATION DIVISION . PROGRAM-NAME . X .) "
				+ "(procedureDivision PROCEDURE DIVISION . "
					+ "(compilerStatement EJECT) "
					+ "(unnamedProceduralSection (unnamedProceduralParagraph (proceduralStatement STOP RUN .)))))")));		

	}

	@Test
	public void testDoubleEJECTBetweenDivisions () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"IDENTIFICATION DIVISION.",
				"PROGRAM-NAME. X.",
				"EJECT",
				"EJECT",
				"PROCEDURE DIVISION.",
				"    STOP RUN."
			)));
		
		ProgramContext mainTree = compiler.compile();
		String toString = mainTree.toStringTree(compiler.mainParser);

		assertThat(toString, is(equalTo("(program "
				+ "(identificationDivision IDENTIFICATION DIVISION . PROGRAM-NAME . X .) "
				+ "(compilerStatement EJECT) "
				+ "(compilerStatement EJECT) "
				+ "(procedureDivision PROCEDURE DIVISION . (unnamedProceduralSection (unnamedProceduralParagraph (proceduralStatement STOP RUN .)))))")));		
	}

	@Test
	public void testDoubleEJECTInsideDivision () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"IDENTIFICATION DIVISION.",
				"PROGRAM-NAME. X.",
				"PROCEDURE DIVISION.",
				"EJECT",
				"EJECT",
				"    STOP RUN."
			)));
		
		ProgramContext mainTree = compiler.compile();
		String toString = mainTree.toStringTree(compiler.mainParser);

		assertThat(toString, is(equalTo("(program "
				+ "(identificationDivision IDENTIFICATION DIVISION . PROGRAM-NAME . X .) "
				+ "(procedureDivision PROCEDURE DIVISION . "
					+ "(compilerStatement EJECT) "
					+ "(compilerStatement EJECT) "
					+ "(unnamedProceduralSection (unnamedProceduralParagraph (proceduralStatement STOP RUN .)))))")));		

	}
	
	@Test
	public void testCOPYStatementOutsideDataDeclaration () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"WORKING-STORAGE SECTION.",
				"77  DECL-1. COPY COPY-LIB-FOR-DECL-1.",
				"77  DECL-2."
			)));
		
		CompilerStatementsContext preTree = compiler.preParser.compilerStatements();
		WorkingStorageSectionContext mainTree = compiler.mainParser.workingStorageSection();
		compiler.preProcess(preTree, mainTree);
		
		String string = mainTree.toStringTree(compiler.mainParser);

		assertThat(string, is(equalTo("(workingStorageSection WORKING-STORAGE SECTION . "
				+ "(dataDescriptionParagraph (levelNumber 77) (dataName DECL-1) .) "
				+ "(compilerStatement COPY COPY-LIB-FOR-DECL-1 .) "
				+ "(dataDescriptionParagraph (levelNumber 77) (dataName DECL-2) .))")));
	}
	
	@Test
	public void testCOPYStatementOutsideFileDeclaration () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"FILE SECTION.",
				"FD  FD0. COPY COPY-LIB-FOR-FD0.",
				"FD  FD1."
			)));
		
		CompilerStatementsContext preTree = compiler.preParser.compilerStatements();
		FileSectionContext mainTree = compiler.mainParser.fileSection();
		compiler.preProcess(preTree, mainTree);
		
		String string = mainTree.toStringTree(compiler.mainParser);

		assertThat(string, is(equalTo("(fileSection FILE SECTION . "
				+ "(fileDescriptionParagraph FD (fileName FD0) .) "
				+ "(compilerStatement COPY COPY-LIB-FOR-FD0 .) "
				+ "(fileDescriptionParagraph FD (fileName FD1) .))")));
	}
	
	@Test
	public void testEJECTAtTheEnd () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"IDENTIFICATION DIVISION.",
				"PROGRAM-NAME. X.",
				"PROCEDURE DIVISION.",
				"    STOP RUN.",
				"EJECT"
			)));
		
		ProgramContext mainTree = compiler.compile();
		String toString = mainTree.toStringTree(compiler.mainParser);

		assertThat(toString, is(equalTo("(program "
				+ "(identificationDivision IDENTIFICATION DIVISION . PROGRAM-NAME . X .) "
				+ "(procedureDivision PROCEDURE DIVISION . (unnamedProceduralSection (unnamedProceduralParagraph (proceduralStatement STOP RUN .)))) "
				+ "(compilerStatement EJECT))")));		
	}
	
	@Test
	public void testCOPYStatementOutsideLastDataDeclaration () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"WORKING-STORAGE SECTION.",
				"77  DECL-1. COPY COPY-LIB-FOR-DECL-1."
			)));
		
		CompilerStatementsContext preTree = compiler.preParser.compilerStatements();
		WorkingStorageSectionContext mainTree = compiler.mainParser.workingStorageSection();
		compiler.preProcess(preTree, mainTree);
		
		String string = mainTree.toStringTree(compiler.mainParser);

		assertThat(string, is(equalTo("(workingStorageSection WORKING-STORAGE SECTION . "
				+ "(dataDescriptionParagraph (levelNumber 77) (dataName DECL-1) .) "
				+ "(compilerStatement COPY COPY-LIB-FOR-DECL-1 .))")));
	}
	
	@Test
	public void testCOPYStatementOutsideLastFileDeclaration () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"FILE SECTION.",
				"FD  FD0. COPY COPY-LIB-FOR-FD0."
			)));
		
		CompilerStatementsContext preTree = compiler.preParser.compilerStatements();
		FileSectionContext mainTree = compiler.mainParser.fileSection();
		compiler.preProcess(preTree, mainTree);
		
		String string = mainTree.toStringTree(compiler.mainParser);

		assertThat(string, is(equalTo("(fileSection FILE SECTION . "
				+ "(fileDescriptionParagraph FD (fileName FD0) .) "
				+ "(compilerStatement COPY COPY-LIB-FOR-FD0 .))")));
	}
	
	@Test
	public void testSoleCOPYStatement () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"COPY ENTIRE-PROGRAM."
			)));
		
		CompilerStatementsContext preTree = compiler.preParser.compilerStatements();
		ProgramContext mainTree = compiler.mainParser.program();
		compiler.preProcess(preTree, mainTree);
		
		String string = mainTree.toStringTree(compiler.mainParser);

		assertThat(string, is(equalTo("(program identificationDivision procedureDivision "
				+ "(compilerStatement COPY ENTIRE-PROGRAM .))")));
	}
	
	@Test
	public void testDoubleEJECTAtTheEnd () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"IDENTIFICATION DIVISION.",
				"PROGRAM-NAME. X.",
				"PROCEDURE DIVISION.",
				"    STOP RUN.",
				"EJECT",
				"EJECT"
			)));
		
		ProgramContext mainTree = compiler.compile();
		String toString = mainTree.toStringTree(compiler.mainParser);

		assertThat(toString, is(equalTo("(program "
				+ "(identificationDivision IDENTIFICATION DIVISION . PROGRAM-NAME . X .) "
				+ "(procedureDivision PROCEDURE DIVISION . (unnamedProceduralSection (unnamedProceduralParagraph (proceduralStatement STOP RUN .)))) "
				+ "(compilerStatement EJECT) "
				+ "(compilerStatement EJECT))")));
	}
	
	@Test
	public void testEJECTAtTheBeginning () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"EJECT",
				"IDENTIFICATION DIVISION.",
				"PROGRAM-NAME. X.",
				"PROCEDURE DIVISION.",
				"    STOP RUN."
			)));
		
		ProgramContext mainTree = compiler.compile();
		String toString = mainTree.toStringTree(compiler.mainParser);

		assertThat(toString, is(equalTo("(program "
				+ "(compilerStatement EJECT) "
				+ "(identificationDivision IDENTIFICATION DIVISION . PROGRAM-NAME . X .) "
				+ "(procedureDivision PROCEDURE DIVISION . (unnamedProceduralSection (unnamedProceduralParagraph (proceduralStatement STOP RUN .)))))")));
	}

	@Test
	public void testDoubleEJECTAtTheBeginning () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"EJECT",
				"EJECT",
				"IDENTIFICATION DIVISION.",
				"PROGRAM-NAME. X.",
				"PROCEDURE DIVISION.",
				"    STOP RUN."
			)));
		
		ProgramContext mainTree = compiler.compile();
		String toString = mainTree.toStringTree(compiler.mainParser);

		assertThat(toString, is(equalTo("(program "
				+ "(compilerStatement EJECT) "
				+ "(compilerStatement EJECT) "
				+ "(identificationDivision IDENTIFICATION DIVISION . PROGRAM-NAME . X .) "
				+ "(procedureDivision PROCEDURE DIVISION . (unnamedProceduralSection (unnamedProceduralParagraph (proceduralStatement STOP RUN .)))))")));
	}
	
	@Test
	public void testCOPYStatementWithMissingPERIOD () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"DISPLAY '' COPY STRING."
			)));
		
		CompilerStatementsContext preTree = compiler.preParser.compilerStatements();
		ProceduralStatementContext mainTree = compiler.mainParser.proceduralStatement();
		compiler.preProcess(preTree, mainTree);
		
		String string = mainTree.toStringTree(compiler.mainParser);

		assertThat(string, is(equalTo("(proceduralStatement DISPLAY "
				+ "(literal (alphanumericLiteral (quotedString ''))) "
				+ "(compilerStatement COPY STRING .) "
				+ "<missing PERIOD>)")));
	}
	
	@Test
	public void testCOPYStatementInsideFileDeclarationWithValueMissingOf () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"FILE SECTION.",
				"FD  FD0 VALUE COPY VALUE-OF. SYSTEM-NAME 10."
			)));
		
		CompilerStatementsContext preTree = compiler.preParser.compilerStatements();
		FileSectionContext mainTree = compiler.mainParser.fileSection();
		compiler.preProcess(preTree, mainTree);
		
		String string = mainTree.toStringTree(compiler.mainParser);

		assertThat(string, is(equalTo("(fileSection FILE SECTION . "
				+ "(fileDescriptionParagraph FD (fileName FD0) "
					+ "(fdValueOfClause VALUE <missing OF> (compilerStatement COPY VALUE-OF .) "
					+ "(systemName SYSTEM-NAME) (literal (numericLiteral 10))) .))")));
	}
	
	@Test
	public void testCOPYStatementInsideDataDeclaration () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"WORKING-STORAGE SECTION.",
				"77  DECL-1 COPY COPY-LIB-FOR-DECL-1.",
				"77  DECL-2."
			)));
		
		CompilerStatementsContext preTree = compiler.preParser.compilerStatements();
		WorkingStorageSectionContext mainTree = compiler.mainParser.workingStorageSection();
		compiler.preProcess(preTree, mainTree);
		
		String string = mainTree.toStringTree(compiler.mainParser);

		assertThat(string, is(equalTo("(workingStorageSection WORKING-STORAGE SECTION . "
				+ "(dataDescriptionParagraph (levelNumber 77) (dataName DECL-1) (compilerStatement COPY COPY-LIB-FOR-DECL-1 .) <missing PERIOD>) "
				+ "(dataDescriptionParagraph (levelNumber 77) (dataName DECL-2) .))")));
	}
	
	@Test
	public void testCOPYStatementInsideLastDataDeclaration () throws IOException {
		setSource(new StringReader(TextHelper.join(
				"WORKING-STORAGE SECTION.",
				"77  DECL-1 COPY COPY-LIB-FOR-DECL-1."
			)));
		
		CompilerStatementsContext preTree = compiler.preParser.compilerStatements();
		WorkingStorageSectionContext mainTree = compiler.mainParser.workingStorageSection();
		compiler.preProcess(preTree, mainTree);
		
		String string = mainTree.toStringTree(compiler.mainParser);

		assertThat(string, is(equalTo("(workingStorageSection WORKING-STORAGE SECTION . "
				+ "(dataDescriptionParagraph (levelNumber 77) (dataName DECL-1) (compilerStatement COPY COPY-LIB-FOR-DECL-1 .) <missing PERIOD>))")));
	}
	
	private void setSource(Reader source) throws IOException {
		compiler = new FreeFormatCompiler(source);
	}
}
