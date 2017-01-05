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

import br.eti.rslemos.cobolg.COBOLParser.StmtSEARCHconditionalContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
public class StmtSEARCHconditional {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtSEARCHconditional");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtSEARCHconditionalContext> helper = new CompilerHelper<StmtSEARCHconditionalContext>() {
		@Override protected StmtSEARCHconditionalContext parsePart() { return parser.stmtSEARCHconditional(); }
	};
	
	@Test public void SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_STOP_RUN() {
		helper.compileAndVerify(
				get("SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_STOP_RUN.source"),
				get("SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_STOP_RUN.tree")
			);
	}
	
	@Test public void SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_NEXT_SENTENCE() {
		helper.compileAndVerify(
				get("SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_NEXT_SENTENCE.source"),
				get("SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_NEXT_SENTENCE.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_STOP_RUN_WHEN_X_2_IS_NOT_POSITIVE_STOP_RUN() {
		helper.compileAndVerify(
				get("SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_STOP_RUN_WHEN_X_2_IS_NOT_POSITIVE_STOP_RUN.source"),
				get("SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_STOP_RUN_WHEN_X_2_IS_NOT_POSITIVE_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_STOP_RUN_WHEN_X_2_IS_NOT_POSITIVE_NEXT_SENTENCE() {
		helper.compileAndVerify(
				get("SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_STOP_RUN_WHEN_X_2_IS_NOT_POSITIVE_NEXT_SENTENCE.source"),
				get("SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_STOP_RUN_WHEN_X_2_IS_NOT_POSITIVE_NEXT_SENTENCE.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_NEXT_SENTENCE_WHEN_X_2_IS_NOT_POSITIVE_STOP_RUN() {
		helper.compileAndVerify(
				get("SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_NEXT_SENTENCE_WHEN_X_2_IS_NOT_POSITIVE_STOP_RUN.source"),
				get("SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_NEXT_SENTENCE_WHEN_X_2_IS_NOT_POSITIVE_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_NEXT_SENTENCE_WHEN_X_2_IS_NOT_POSITIVE_NEXT_SENTENCE() {
		helper.compileAndVerify(
				get("SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_NEXT_SENTENCE_WHEN_X_2_IS_NOT_POSITIVE_NEXT_SENTENCE.source"),
				get("SEARCH_X_1_VARYING_I_WHEN_CONDITION_2_NEXT_SENTENCE_WHEN_X_2_IS_NOT_POSITIVE_NEXT_SENTENCE.tree")
			);
	}
	
	@Test public void SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_STOP_RUN() {
		helper.compileAndVerify(
				get("SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_STOP_RUN.source"),
				get("SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_STOP_RUN.tree")
			);
	}
	
	@Test public void SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_NEXT_SENTENCE() {
		helper.compileAndVerify(
				get("SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_NEXT_SENTENCE.source"),
				get("SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_NEXT_SENTENCE.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_STOP_RUN_WHEN_X_2_IS_NOT_POSITIVE_STOP_RUN() {
		helper.compileAndVerify(
				get("SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_STOP_RUN_WHEN_X_2_IS_NOT_POSITIVE_STOP_RUN.source"),
				get("SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_STOP_RUN_WHEN_X_2_IS_NOT_POSITIVE_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_STOP_RUN_WHEN_X_2_IS_NOT_POSITIVE_NEXT_SENTENCE() {
		helper.compileAndVerify(
				get("SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_STOP_RUN_WHEN_X_2_IS_NOT_POSITIVE_NEXT_SENTENCE.source"),
				get("SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_STOP_RUN_WHEN_X_2_IS_NOT_POSITIVE_NEXT_SENTENCE.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_NEXT_SENTENCE_WHEN_X_2_IS_NOT_POSITIVE_STOP_RUN() {
		helper.compileAndVerify(
				get("SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_NEXT_SENTENCE_WHEN_X_2_IS_NOT_POSITIVE_STOP_RUN.source"),
				get("SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_NEXT_SENTENCE_WHEN_X_2_IS_NOT_POSITIVE_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_NEXT_SENTENCE_WHEN_X_2_IS_NOT_POSITIVE_NEXT_SENTENCE() {
		helper.compileAndVerify(
				get("SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_NEXT_SENTENCE_WHEN_X_2_IS_NOT_POSITIVE_NEXT_SENTENCE.source"),
				get("SEARCH_X_1_VARYING_I_END_STOP_RUN_WHEN_CONDITION_2_NEXT_SENTENCE_WHEN_X_2_IS_NOT_POSITIVE_NEXT_SENTENCE.tree")
			);
	}
	
	@Test public void SEARCH_ALL_X_1_WHEN_DATA_1_IS_EQUAL_ID_1_OP_PLUS_10_STOP_RUN() {
		helper.compileAndVerify(
				get("SEARCH_ALL_X_1_WHEN_DATA_1_IS_EQUAL_ID_1_OP_PLUS_10_STOP_RUN.source"),
				get("SEARCH_ALL_X_1_WHEN_DATA_1_IS_EQUAL_ID_1_OP_PLUS_10_STOP_RUN.tree")
			);
	}
	
	@Test public void SEARCH_ALL_X_1_WHEN_DATA_1_OP_EQUAL_ID_1_OP_PLUS_10_NEXT_SENTENCE() {
		helper.compileAndVerify(
				get("SEARCH_ALL_X_1_WHEN_DATA_1_OP_EQUAL_ID_1_OP_PLUS_10_NEXT_SENTENCE.source"),
				get("SEARCH_ALL_X_1_WHEN_DATA_1_OP_EQUAL_ID_1_OP_PLUS_10_NEXT_SENTENCE.tree")
			);
	}
	
	@Test public void SEARCH_ALL_X_1_WHEN_DATA_1_IS_EQUAL_TO_QUOTED_D1_AND_DATA_1_OP_EQUAL_ID_1_STOP_RUN() {
		helper.compileAndVerify(
				get("SEARCH_ALL_X_1_WHEN_DATA_1_IS_EQUAL_TO_QUOTED_D1_AND_DATA_1_OP_EQUAL_ID_1_STOP_RUN.source"),
				get("SEARCH_ALL_X_1_WHEN_DATA_1_IS_EQUAL_TO_QUOTED_D1_AND_DATA_1_OP_EQUAL_ID_1_STOP_RUN.tree")
			);
	}
	
	@Test public void SEARCH_ALL_X_1_WHEN_DATA_1_IS_OP_EQUAL_QUOTED_D1_AND_DATA_1_IS_EQUAL_ID_1_NEXT_SENTENCE() {
		helper.compileAndVerify(
				get("SEARCH_ALL_X_1_WHEN_DATA_1_IS_OP_EQUAL_QUOTED_D1_AND_DATA_1_IS_EQUAL_ID_1_NEXT_SENTENCE.source"),
				get("SEARCH_ALL_X_1_WHEN_DATA_1_IS_OP_EQUAL_QUOTED_D1_AND_DATA_1_IS_EQUAL_ID_1_NEXT_SENTENCE.tree")
			);
	}
	
	@Test public void SEARCH_ALL_X_1_WHEN_CONDITION_1_AND_DATA_1_IS_OP_EQUAL_ID_1_AND_DATA_1_IS_EQUAL_ID_1_STOP_RUN() {
		helper.compileAndVerify(
				get("SEARCH_ALL_X_1_WHEN_CONDITION_1_AND_DATA_1_IS_OP_EQUAL_ID_1_AND_DATA_1_IS_EQUAL_ID_1_STOP_RUN.source"),
				get("SEARCH_ALL_X_1_WHEN_CONDITION_1_AND_DATA_1_IS_OP_EQUAL_ID_1_AND_DATA_1_IS_EQUAL_ID_1_STOP_RUN.tree")
			);
	}
	
	@Test public void SEARCH_ALL_X_1_WHEN_CONDITION_1_AND_DATA_1_IS_EQUAL_TO_ID_1_AND_DATA_1_OP_EQUAL_ID_1_NEXT_SENTENCE() {
		helper.compileAndVerify(
				get("SEARCH_ALL_X_1_WHEN_CONDITION_1_AND_DATA_1_IS_EQUAL_TO_ID_1_AND_DATA_1_OP_EQUAL_ID_1_NEXT_SENTENCE.source"),
				get("SEARCH_ALL_X_1_WHEN_CONDITION_1_AND_DATA_1_IS_EQUAL_TO_ID_1_AND_DATA_1_OP_EQUAL_ID_1_NEXT_SENTENCE.tree")
			);
	}
	
	@Test public void SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_STOP_RUN() {
		helper.compileAndVerify(
				get("SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_STOP_RUN.source"),
				get("SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_STOP_RUN.tree")
			);
	}
	
	@Test public void SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_NEXT_SENTENCE() {
		helper.compileAndVerify(
				get("SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_NEXT_SENTENCE.source"),
				get("SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_NEXT_SENTENCE.tree")
			);
	}
	
	@Test public void SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_AND_DATA_1_IS_EQUAL_QUOTED_D1_STOP_RUN() {
		helper.compileAndVerify(
				get("SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_AND_DATA_1_IS_EQUAL_QUOTED_D1_STOP_RUN.source"),
				get("SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_AND_DATA_1_IS_EQUAL_QUOTED_D1_STOP_RUN.tree")
			);
	}
	
	@Test public void SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_AND_DATA_1_OP_EQUAL_QUOTED_D1_NEXT_SENTENCE() {
		helper.compileAndVerify(
				get("SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_AND_DATA_1_OP_EQUAL_QUOTED_D1_NEXT_SENTENCE.source"),
				get("SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_AND_DATA_1_OP_EQUAL_QUOTED_D1_NEXT_SENTENCE.tree")
			);
	}
	
	@Test public void SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_AND_DATA_1_IS_EQUAL_TO_ID_1_AND_DATA_1_OP_EQUAL_ID_1_STOP_RUN() {
		helper.compileAndVerify(
				get("SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_AND_DATA_1_IS_EQUAL_TO_ID_1_AND_DATA_1_OP_EQUAL_ID_1_STOP_RUN.source"),
				get("SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_AND_DATA_1_IS_EQUAL_TO_ID_1_AND_DATA_1_OP_EQUAL_ID_1_STOP_RUN.tree")
			);
	}
	
	@Test public void SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_AND_DATA_1_IS_OP_EQUAL_ID_1_AND_DATA_1_IS_EQUAL_ID_1_NEXT_SENTENCE() {
		helper.compileAndVerify(
				get("SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_AND_DATA_1_IS_OP_EQUAL_ID_1_AND_DATA_1_IS_EQUAL_ID_1_NEXT_SENTENCE.source"),
				get("SEARCH_ALL_X_1_END_STOP_RUN_WHEN_CONDITION_1_AND_DATA_1_IS_OP_EQUAL_ID_1_AND_DATA_1_IS_EQUAL_ID_1_NEXT_SENTENCE.tree")
			);
	}
}
