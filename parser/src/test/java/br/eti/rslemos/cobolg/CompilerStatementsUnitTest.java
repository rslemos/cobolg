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

import static br.eti.rslemos.cobolg.PostProcessingCompiler.parserForFreeFormat;

import java.io.IOException;
import java.io.Reader;
import java.util.ResourceBundle;

import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.ParserRuleContext;
import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.CompilerStatementsContext;
import br.eti.rslemos.cobolg.COBOLParser.FileSectionContext;
import br.eti.rslemos.cobolg.COBOLParser.ProceduralSentenceContext;
import br.eti.rslemos.cobolg.COBOLParser.ProgramContext;
import br.eti.rslemos.cobolg.COBOLParser.WorkingStorageSectionContext;

public class CompilerStatementsUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.compilerStatements");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static abstract class PreCompilerHelper<T extends ParserRuleContext> extends CompilerHelper<T> {
		@Override public T compile(String source, ANTLRErrorListener... listeners) {
			T mainTree = super.compile(source/*, listeners*/); // to ignore "missing '.'" errors
			CompilerStatementsContext preTree = ((PostProcessingCompiler)compiler).preParser.compilerStatements();
			new CompilerStatementsProcessor().injectCompilerStatements(preTree, mainTree);
			return mainTree;
		}
		
		protected BaseCompiler createCompiler(Reader source) throws IOException {
			return parserForFreeFormat(source);
		}
	}
	
	private static CompilerHelper<ProgramContext> programHelper = new PreCompilerHelper<ProgramContext>() {
		@Override protected ProgramContext parsePart() { return compiler.mainParser.program(); }
	};

	private static CompilerHelper<WorkingStorageSectionContext> wssHelper = new PreCompilerHelper<WorkingStorageSectionContext>() {
		@Override protected WorkingStorageSectionContext parsePart() { return compiler.mainParser.workingStorageSection(); }
	};

	private static CompilerHelper<FileSectionContext> fsHelper = new PreCompilerHelper<FileSectionContext>() {
		@Override protected FileSectionContext parsePart() { return compiler.mainParser.fileSection(); }
	};

	private static CompilerHelper<ProceduralSentenceContext> psHelper = new PreCompilerHelper<ProceduralSentenceContext>() {
		@Override protected ProceduralSentenceContext parsePart() { return compiler.mainParser.proceduralSentence(); }
	};
	
	@Test public void NoCompilerStatements () {
		programHelper.compileAndVerify(
				get("NoCompilerStatements.source"),
				get("NoCompilerStatements.tree")
			);
	}

	@Test public void EJECTBetweenDivisions () {
		programHelper.compileAndVerify(
				get("EJECTBetweenDivisions.source"),
				get("EJECTBetweenDivisions.tree")
			);
	}

	@Test public void EJECTInsideDivision () {
		programHelper.compileAndVerify(
				get("EJECTInsideDivision.source"),
				get("EJECTInsideDivision.tree")
			);
	}

	@Test public void DoubleEJECTBetweenDivisions () {
		programHelper.compileAndVerify(
				get("DoubleEJECTBetweenDivisions.source"),
				get("DoubleEJECTBetweenDivisions.tree")
			);
	}

	@Test public void DoubleEJECTInsideDivision () {
		programHelper.compileAndVerify(
				get("DoubleEJECTInsideDivision.source"),
				get("DoubleEJECTInsideDivision.tree")
			);
	}

	@Test public void COPYStatementOutsideDataDeclaration () {
		wssHelper.compileAndVerify(
				get("COPYStatementOutsideDataDeclaration.source"),
				get("COPYStatementOutsideDataDeclaration.tree")
			);
	}
	
	@Test public void COPYStatementOutsideFileDeclaration () {
		fsHelper.compileAndVerify(
				get("COPYStatementOutsideFileDeclaration.source"),
				get("COPYStatementOutsideFileDeclaration.tree")
			);
	}
	
	@Test public void COPYStatementWithString () {
		fsHelper.compileAndVerify(
				get("COPYStatementWithString.source"),
				get("COPYStatementWithString.tree")
			);
	}
	
	@Test public void EJECTAtTheEnd () {
		programHelper.compileAndVerify(
				get("EJECTAtTheEnd.source"),
				get("EJECTAtTheEnd.tree")
			);
	}
	
	@Test public void COPYStatementOutsideLastDataDeclaration () {
		wssHelper.compileAndVerify(
				get("COPYStatementOutsideLastDataDeclaration.source"),
				get("COPYStatementOutsideLastDataDeclaration.tree")
			);
	}
	
	@Test public void COPYStatementOutsideLastFileDeclaration () {
		fsHelper.compileAndVerify(
				get("COPYStatementOutsideLastFileDeclaration.source"),
				get("COPYStatementOutsideLastFileDeclaration.tree")
			);
	}
	
	@Test public void SoleCOPYStatement () {
		programHelper.compileAndVerify(
				get("SoleCOPYStatement.source"),
				get("SoleCOPYStatement.tree")
			);
	}
	
	@Test public void DoubleEJECTAtTheEnd () {
		programHelper.compileAndVerify(
				get("DoubleEJECTAtTheEnd.source"),
				get("DoubleEJECTAtTheEnd.tree")
			);
	}
	
	@Test public void EJECTAtTheBeginning () {
		programHelper.compileAndVerify(
				get("EJECTAtTheBeginning.source"),
				get("EJECTAtTheBeginning.tree")
			);
	}

	@Test public void DoubleEJECTAtTheBeginning () {
		programHelper.compileAndVerify(
				get("DoubleEJECTAtTheBeginning.source"),
				get("DoubleEJECTAtTheBeginning.tree")
			);
	}
	
	@Test public void COPYStatementWithMissingPERIOD () {
		psHelper.compileAndVerify(
				get("COPYStatementWithMissingPERIOD.source"),
				get("COPYStatementWithMissingPERIOD.tree")
			);
	}
	
	@Test public void COPYStatementInsideFileDeclarationWithValueMissingOf () {
		fsHelper.compileAndVerify(
				get("COPYStatementInsideFileDeclarationWithValueMissingOf.source"),
				get("COPYStatementInsideFileDeclarationWithValueMissingOf.tree")
			);
	}
	
	@Test public void COPYStatementInsideDataDeclaration () {
		wssHelper.compileAndVerify(
				get("COPYStatementInsideDataDeclaration.source"),
				get("COPYStatementInsideDataDeclaration.tree")
			);
	}
	
	@Test public void COPYStatementInsideLastDataDeclaration () {
		wssHelper.compileAndVerify(
				get("COPYStatementInsideLastDataDeclaration.source"),
				get("COPYStatementInsideLastDataDeclaration.tree")
			);
	}
	
	@Test public void COPYStatementInsideFileDeclaration () {
		fsHelper.compileAndVerify(
				get("COPYStatementInsideFileDeclaration.source"),
				get("COPYStatementInsideFileDeclaration.tree")
			);
	}
	
	@Test public void COPYStatementInsideLastFileDeclaration () {
		fsHelper.compileAndVerify(
				get("COPYStatementInsideLastFileDeclaration.source"),
				get("COPYStatementInsideLastFileDeclaration.tree")
			);
	}
	
	@Test public void TwoCOPYStatementsInsideFileDeclaration () {
		fsHelper.compileAndVerify(
				get("TwoCOPYStatementsInsideFileDeclaration.source"),
				get("TwoCOPYStatementsInsideFileDeclaration.tree")
			);
	}
	
	@Test public void COPYStatementInsideFileDeclarationWithMissingFilename () {
		fsHelper.compileAndVerify(
				get("COPYStatementInsideFileDeclarationWithMissingFilename.source"),
				get("COPYStatementInsideFileDeclarationWithMissingFilename.tree")
			);
	}
	
	@Test public void UnterminatedCOPYStatementsInsideFileDeclaration () {
		fsHelper.compileAndVerify(
				get("UnterminatedCOPYStatementsInsideFileDeclaration.source"),
				get("UnterminatedCOPYStatementsInsideFileDeclaration.tree")
			);
	}
	
	@Test public void COPYEntireProceduralParagraph () {
		programHelper.compileAndVerify(
				get("COPYEntireProceduralParagraph.source"),
				get("COPYEntireProceduralParagraph.tree")
			);
	}
	
	@Test public void COPYEntireProceduralAnonymousParagraph () {
		programHelper.compileAndVerify(
				get("COPYEntireProceduralAnonymousParagraph.source"),
				get("COPYEntireProceduralAnonymousParagraph.tree")
			);
	}
	
	@Test public void COPYEntireProceduralAnonymousParagraphFollowedByNamedParagraph () {
		programHelper.compileAndVerify(
				get("COPYEntireProceduralAnonymousParagraphFollowedByNamedParagraph.source"),
				get("COPYEntireProceduralAnonymousParagraphFollowedByNamedParagraph.tree")
			);
	}
}
