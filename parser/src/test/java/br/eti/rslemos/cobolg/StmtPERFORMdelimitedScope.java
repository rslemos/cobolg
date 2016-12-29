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

import br.eti.rslemos.cobolg.COBOLParser.StmtPERFORMdelimitedScopeContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
public class StmtPERFORMdelimitedScope {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtPERFORMdelimitedScope");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtPERFORMdelimitedScopeContext> helper = new CompilerHelper<StmtPERFORMdelimitedScopeContext>() {
		@Override protected StmtPERFORMdelimitedScopeContext parsePart() { return parser.stmtPERFORMdelimitedScope(); }
	};

	@Test public void PERFORM_STOP_RUN_STOP_RUN_END_PERFORM() {
		helper.compileAndVerify(
				get("PERFORM_STOP_RUN_STOP_RUN_END_PERFORM.source"),
				get("PERFORM_STOP_RUN_STOP_RUN_END_PERFORM.tree")
			);
	}

	@Test public void PERFORM_N_TIMES_STOP_RUN_STOP_RUN_END_PERFORM() {
		helper.compileAndVerify(
				get("PERFORM_N_TIMES_STOP_RUN_STOP_RUN_END_PERFORM.source"),
				get("PERFORM_N_TIMES_STOP_RUN_STOP_RUN_END_PERFORM.tree")
			);
	}

	@Test public void PERFORM_10_TIMES_STOP_RUN_STOP_RUN_END_PERFORM() {
		helper.compileAndVerify(
				get("PERFORM_10_TIMES_STOP_RUN_STOP_RUN_END_PERFORM.source"),
				get("PERFORM_10_TIMES_STOP_RUN_STOP_RUN_END_PERFORM.tree")
			);
	}

	@Test public void PERFORM_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM() {
		helper.compileAndVerify(
				get("PERFORM_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.source"),
				get("PERFORM_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.tree")
			);
	}

	@Test public void PERFORM_WITH_TEST_BEFORE_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM() {
		helper.compileAndVerify(
				get("PERFORM_WITH_TEST_BEFORE_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.source"),
				get("PERFORM_WITH_TEST_BEFORE_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.tree")
			);
	}

	@Test public void PERFORM_WITH_TEST_AFTER_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM() {
		helper.compileAndVerify(
				get("PERFORM_WITH_TEST_AFTER_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.source"),
				get("PERFORM_WITH_TEST_AFTER_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.tree")
			);
	}

	@Test public void PERFORM_WITH_TEST_BEFORE_VARYING_N_FROM_N_LOW_BY_N_STEP_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM() {
		helper.compileAndVerify(
				get("PERFORM_WITH_TEST_BEFORE_VARYING_N_FROM_N_LOW_BY_N_STEP_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.source"),
				get("PERFORM_WITH_TEST_BEFORE_VARYING_N_FROM_N_LOW_BY_N_STEP_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.tree")
			);
	}

	@Test public void PERFORM_WITH_TEST_AFTER_VARYING_N_FROM_N_LOW_BY__2_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM() {
		helper.compileAndVerify(
				get("PERFORM_WITH_TEST_AFTER_VARYING_N_FROM_N_LOW_BY__2_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.source"),
				get("PERFORM_WITH_TEST_AFTER_VARYING_N_FROM_N_LOW_BY__2_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.tree")
			);
	}

	@Test public void PERFORM_VARYING_N_FROM_0_BY_N_STEP_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM() {
		helper.compileAndVerify(
				get("PERFORM_VARYING_N_FROM_0_BY_N_STEP_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.source"),
				get("PERFORM_VARYING_N_FROM_0_BY_N_STEP_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.tree")
			);
	}

	@Test public void PERFORM_VARYING_N_FROM_0_BY__2_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM() {
		helper.compileAndVerify(
				get("PERFORM_VARYING_N_FROM_0_BY__2_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.source"),
				get("PERFORM_VARYING_N_FROM_0_BY__2_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.tree")
			);
	}

	@Test public void PERFORM_TEST_AFTER_VARYING_N_FROM_N_LOW_BY_N_STEP_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM() {
		helper.compileAndVerify(
				get("PERFORM_TEST_AFTER_VARYING_N_FROM_N_LOW_BY_N_STEP_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.source"),
				get("PERFORM_TEST_AFTER_VARYING_N_FROM_N_LOW_BY_N_STEP_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.tree")
			);
	}

	@Test public void PERFORM_TEST_BEFORE_VARYING_N_FROM_N_LOW_BY__2_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM() {
		helper.compileAndVerify(
				get("PERFORM_TEST_BEFORE_VARYING_N_FROM_N_LOW_BY__2_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.source"),
				get("PERFORM_TEST_BEFORE_VARYING_N_FROM_N_LOW_BY__2_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.tree")
			);
	}

	@Test public void PERFORM_TEST_BEFORE_VARYING_N_FROM_0_BY_N_STEP_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM() {
		helper.compileAndVerify(
				get("PERFORM_TEST_BEFORE_VARYING_N_FROM_0_BY_N_STEP_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.source"),
				get("PERFORM_TEST_BEFORE_VARYING_N_FROM_0_BY_N_STEP_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.tree")
			);
	}

	@Test public void PERFORM_TEST_AFTER_VARYING_N_FROM_0_BY__2_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM() {
		helper.compileAndVerify(
				get("PERFORM_TEST_AFTER_VARYING_N_FROM_0_BY__2_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.source"),
				get("PERFORM_TEST_AFTER_VARYING_N_FROM_0_BY__2_UNTIL_N_OP_LESS_0_STOP_RUN_STOP_RUN_END_PERFORM.tree")
			);
	}
}
