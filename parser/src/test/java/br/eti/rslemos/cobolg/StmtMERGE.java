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

import br.eti.rslemos.cobolg.COBOLParser.StmtMERGEContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
public class StmtMERGE {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtMERGE");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtMERGEContext> helper = new CompilerHelper<StmtMERGEContext>() {
		@Override protected StmtMERGEContext parsePart() { return parser.stmtMERGE(); }
	};
	
	@Test public void MERGE_FILE_1_ON_ASCENDING_KEY_DATA_1_DATA_2_ON_DESCENDING_KEY_DATA_3_USING_FILE_2_FILE_3_FILE_4_GIVING_FILE_6_FILE_7_FILE_8() {
		helper.compileAndVerify(
				get("MERGE_FILE_1_ON_ASCENDING_KEY_DATA_1_DATA_2_ON_DESCENDING_KEY_DATA_3_USING_FILE_2_FILE_3_FILE_4_GIVING_FILE_6_FILE_7_FILE_8.source"),
				get("MERGE_FILE_1_ON_ASCENDING_KEY_DATA_1_DATA_2_ON_DESCENDING_KEY_DATA_3_USING_FILE_2_FILE_3_FILE_4_GIVING_FILE_6_FILE_7_FILE_8.tree")
			);
	}

	@Test public void MERGE_FILE_1_ON_DESCENDING_DATA_1_ASCENDING_KEY_DATA_2_DATA_3_USING_FILE_2_FILE_3_FILE_4_FILE_5_OUTPUT_PROCEDURE_IS_PROC_1() {
		helper.compileAndVerify(
				get("MERGE_FILE_1_ON_DESCENDING_DATA_1_ASCENDING_KEY_DATA_2_DATA_3_USING_FILE_2_FILE_3_FILE_4_FILE_5_OUTPUT_PROCEDURE_IS_PROC_1.source"),
				get("MERGE_FILE_1_ON_DESCENDING_DATA_1_ASCENDING_KEY_DATA_2_DATA_3_USING_FILE_2_FILE_3_FILE_4_FILE_5_OUTPUT_PROCEDURE_IS_PROC_1.tree")
			);
	}

	@Test public void MERGE_FILE_1_ASCENDING_DATA_1_ON_ASCENDING_DATA_2_USING_FILE_2_FILE_3_FILE_4_FILE_5_OUTPUT_PROCEDURE_IS_PROC_1_THROUGH_PROC_2() {
		helper.compileAndVerify(
				get("MERGE_FILE_1_ASCENDING_DATA_1_ON_ASCENDING_DATA_2_USING_FILE_2_FILE_3_FILE_4_FILE_5_OUTPUT_PROCEDURE_IS_PROC_1_THROUGH_PROC_2.source"),
				get("MERGE_FILE_1_ASCENDING_DATA_1_ON_ASCENDING_DATA_2_USING_FILE_2_FILE_3_FILE_4_FILE_5_OUTPUT_PROCEDURE_IS_PROC_1_THROUGH_PROC_2.tree")
			);
	}

	@Test public void MERGE_FILE_1_ON_ASCENDING_KEY_DATA_1_COLLATING_SEQUENCE_ALPHABET_1_USING_FILE_2_FILE_3_FILE_4_GIVING_FILE_6_FILE_7_FILE_8() {
		helper.compileAndVerify(
				get("MERGE_FILE_1_ON_ASCENDING_KEY_DATA_1_COLLATING_SEQUENCE_ALPHABET_1_USING_FILE_2_FILE_3_FILE_4_GIVING_FILE_6_FILE_7_FILE_8.source"),
				get("MERGE_FILE_1_ON_ASCENDING_KEY_DATA_1_COLLATING_SEQUENCE_ALPHABET_1_USING_FILE_2_FILE_3_FILE_4_GIVING_FILE_6_FILE_7_FILE_8.tree")
			);
	}

	@Test public void MERGE_FILE_1_ON_DESCENDING_DATA_1_DATA_2_SEQUENCE_IS_ALPHABET_1_USING_FILE_2_FILE_3_FILE_4_OUTPUT_PROCEDURE_IS_PROC_1() {
		helper.compileAndVerify(
				get("MERGE_FILE_1_ON_DESCENDING_DATA_1_DATA_2_SEQUENCE_IS_ALPHABET_1_USING_FILE_2_FILE_3_FILE_4_OUTPUT_PROCEDURE_IS_PROC_1.source"),
				get("MERGE_FILE_1_ON_DESCENDING_DATA_1_DATA_2_SEQUENCE_IS_ALPHABET_1_USING_FILE_2_FILE_3_FILE_4_OUTPUT_PROCEDURE_IS_PROC_1.tree")
			);
	}

	@Test public void MERGE_FILE_1_ASCENDING_DATA_1_DATA_2_COLLATING_SEQUENCE_IS_ALPHABET_1_USING_FILE_2_FILE_3_OUTPUT_PROCEDURE_PROC_1_THRU_PROC_2() {
		helper.compileAndVerify(
				get("MERGE_FILE_1_ASCENDING_DATA_1_DATA_2_COLLATING_SEQUENCE_IS_ALPHABET_1_USING_FILE_2_FILE_3_OUTPUT_PROCEDURE_PROC_1_THRU_PROC_2.source"),
				get("MERGE_FILE_1_ASCENDING_DATA_1_DATA_2_COLLATING_SEQUENCE_IS_ALPHABET_1_USING_FILE_2_FILE_3_OUTPUT_PROCEDURE_PROC_1_THRU_PROC_2.tree")
			);
	}
}
