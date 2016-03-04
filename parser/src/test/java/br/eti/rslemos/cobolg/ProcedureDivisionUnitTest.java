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

import java.util.ResourceBundle;

import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.ProcedureDivisionContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

public class ProcedureDivisionUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.procedureDivision");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<ProcedureDivisionContext> helper = new CompilerHelper<ProcedureDivisionContext>() {
		@Override protected ProcedureDivisionContext parsePart() { return parser.procedureDivision(); }
	};

	@Waive({CompilationError.SYNTAX_ERROR})
	@Test public void PROCEDURE_DIVISION() {
		helper.compileAndVerify(
				get("PROCEDURE_DIVISION.source"),
				get("PROCEDURE_DIVISION.tree")
			);
	}
	
	@Test public void PROCEDURE_DIVISION_STOP_RUN() {
		helper.compileAndVerify(
				get("PROCEDURE_DIVISION_STOP_RUN.source"),
				get("PROCEDURE_DIVISION_STOP_RUN.tree")
			);
	}
	
	@Test public void PROCEDURE_DIVISION_STOP_RUN_STOP_RUN() {
		helper.compileAndVerify(
				get("PROCEDURE_DIVISION_STOP_RUN_STOP_RUN.source"),
				get("PROCEDURE_DIVISION_STOP_RUN_STOP_RUN.tree")
			);
	}
	
	@Test public void PROCEDURE_DIVISION_SECTION_1_SECTION_STOP_RUN() {
		helper.compileAndVerify(
				get("PROCEDURE_DIVISION_SECTION_1_SECTION_STOP_RUN.source"),
				get("PROCEDURE_DIVISION_SECTION_1_SECTION_STOP_RUN.tree")
			);
	}
	
	@Test public void PROCEDURE_DIVISION_PARAGRAPH_A_STOP_RUN() {
		helper.compileAndVerify(
				get("PROCEDURE_DIVISION_PARAGRAPH_A_STOP_RUN.source"),
				get("PROCEDURE_DIVISION_PARAGRAPH_A_STOP_RUN.tree")
			);
	}
	
	@Test public void PROCEDURE_DIVISION_SECTION_1_SECTION_PARAGRAPH_A_STOP_RUN() {
		helper.compileAndVerify(
				get("PROCEDURE_DIVISION_SECTION_1_SECTION_PARAGRAPH_A_STOP_RUN.source"),
				get("PROCEDURE_DIVISION_SECTION_1_SECTION_PARAGRAPH_A_STOP_RUN.tree")
			);
	}
	
	@Test public void PROCEDURE_DIVISION_STOP_RUN_PARAGRAPH_B_STOP_RUN_SECTION_1_SECTION_STOP_RUN_PARAGRAPH_A_STOP_RUN() {
		helper.compileAndVerify(
				get("PROCEDURE_DIVISION_STOP_RUN_PARAGRAPH_B_STOP_RUN_SECTION_1_SECTION_STOP_RUN_PARAGRAPH_A_STOP_RUN.source"),
				get("PROCEDURE_DIVISION_STOP_RUN_PARAGRAPH_B_STOP_RUN_SECTION_1_SECTION_STOP_RUN_PARAGRAPH_A_STOP_RUN.tree")
			);
	}
	
	@Test public void PROCEDURE_DIVISION_USING_DATA_1_DATA_2_STOP_RUN() {
		helper.compileAndVerify(
				get("PROCEDURE_DIVISION_USING_DATA_1_DATA_2_STOP_RUN.source"),
				get("PROCEDURE_DIVISION_USING_DATA_1_DATA_2_STOP_RUN.tree")
			);
	}
	
	@Test public void PROCEDURE_DIVISION_USING_BY_VALUE_DATA_1_DATA_2_STOP_RUN() {
		helper.compileAndVerify(
				get("PROCEDURE_DIVISION_USING_BY_VALUE_DATA_1_DATA_2_STOP_RUN.source"),
				get("PROCEDURE_DIVISION_USING_BY_VALUE_DATA_1_DATA_2_STOP_RUN.tree")
			);
	}
	
	@Test public void PROCEDURE_DIVISION_USING_BY_REFERENCE_DATA_1_DATA_2_STOP_RUN() {
		helper.compileAndVerify(
				get("PROCEDURE_DIVISION_USING_BY_REFERENCE_DATA_1_DATA_2_STOP_RUN.source"),
				get("PROCEDURE_DIVISION_USING_BY_REFERENCE_DATA_1_DATA_2_STOP_RUN.tree")
			);
	}
	
	@Test public void PROCEDURE_DIVISION_USING_BY_VALUE_DATA_1_BY_REFERENCE_DATA_2_STOP_RUN() {
		helper.compileAndVerify(
				get("PROCEDURE_DIVISION_USING_BY_VALUE_DATA_1_BY_REFERENCE_DATA_2_STOP_RUN.source"),
				get("PROCEDURE_DIVISION_USING_BY_VALUE_DATA_1_BY_REFERENCE_DATA_2_STOP_RUN.tree")
			);
	}
	
	@Test public void PROCEDURE_DIVISION_USING_BY_REFERENCE_DATA_1_BY_VALUE_DATA_2_STOP_RUN() {
		helper.compileAndVerify(
				get("PROCEDURE_DIVISION_USING_BY_REFERENCE_DATA_1_BY_VALUE_DATA_2_STOP_RUN.source"),
				get("PROCEDURE_DIVISION_USING_BY_REFERENCE_DATA_1_BY_VALUE_DATA_2_STOP_RUN.tree")
			);
	}
}
