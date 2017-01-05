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

import br.eti.rslemos.cobolg.COBOLParser.StmtEVALUATEconditionalContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
public class StmtEVALUATEconditional {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtEVALUATEconditional");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtEVALUATEconditionalContext> helper = new CompilerHelper<StmtEVALUATEconditionalContext>() {
		@Override protected StmtEVALUATEconditionalContext parsePart() { return parser.stmtEVALUATEconditional(); }
	};
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void EVALUATE_ID_1_WHEN_NOT_ID_4_THRU_ID_4_OP_STAR_40_DISPLAY_QUOTED_BRANCH_1_WHEN_ID_4_OP_GREATER_QUOTED_A_DISPLAY_QUOTED_BRANCH_2_WHEN_TRUE_DISPLAY_QUOTED_BRANCH_3() {
		helper.compileAndVerify(
				get("EVALUATE_ID_1_WHEN_NOT_ID_4_THRU_ID_4_OP_STAR_40_DISPLAY_QUOTED_BRANCH_1_WHEN_ID_4_OP_GREATER_QUOTED_A_DISPLAY_QUOTED_BRANCH_2_WHEN_TRUE_DISPLAY_QUOTED_BRANCH_3.source"),
				get("EVALUATE_ID_1_WHEN_NOT_ID_4_THRU_ID_4_OP_STAR_40_DISPLAY_QUOTED_BRANCH_1_WHEN_ID_4_OP_GREATER_QUOTED_A_DISPLAY_QUOTED_BRANCH_2_WHEN_TRUE_DISPLAY_QUOTED_BRANCH_3.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void EVALUATE_QUOTED_X_WHEN_QUOTED_A_DISPLAY_QUOTED_BRANCH_1_WHEN_NOT_ID_4_DISPLAY_QUOTED_BRANCH_2_WHEN_ID_4_THROUGH_ID_5_DISPLAY_QUOTED_BRANCH_3_WHEN_OTHER_DISPLAY_QUOTED_OTHER() {
		helper.compileAndVerify(
				get("EVALUATE_QUOTED_X_WHEN_QUOTED_A_DISPLAY_QUOTED_BRANCH_1_WHEN_NOT_ID_4_DISPLAY_QUOTED_BRANCH_2_WHEN_ID_4_THROUGH_ID_5_DISPLAY_QUOTED_BRANCH_3_WHEN_OTHER_DISPLAY_QUOTED_OTHER.source"),
				get("EVALUATE_QUOTED_X_WHEN_QUOTED_A_DISPLAY_QUOTED_BRANCH_1_WHEN_NOT_ID_4_DISPLAY_QUOTED_BRANCH_2_WHEN_ID_4_THROUGH_ID_5_DISPLAY_QUOTED_BRANCH_3_WHEN_OTHER_DISPLAY_QUOTED_OTHER.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void EVALUATE_ID_1_OP_PLUS_10_ALSO_TRUE_WHEN_ID_4_OP_STAR_40_ALSO_FALSE_DISPLAY_QUOTED_BRANCH_1_WHEN_40_THROUGH_50_DISPLAY_QUOTED_BRANCH_2() {
		helper.compileAndVerify(
				get("EVALUATE_ID_1_OP_PLUS_10_ALSO_TRUE_WHEN_ID_4_OP_STAR_40_ALSO_FALSE_DISPLAY_QUOTED_BRANCH_1_WHEN_40_THROUGH_50_DISPLAY_QUOTED_BRANCH_2.source"),
				get("EVALUATE_ID_1_OP_PLUS_10_ALSO_TRUE_WHEN_ID_4_OP_STAR_40_ALSO_FALSE_DISPLAY_QUOTED_BRANCH_1_WHEN_40_THROUGH_50_DISPLAY_QUOTED_BRANCH_2.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void EVALUATE_FALSE_ALSO_ID_2_WHEN_ID_4_ALSO_ID_5___10_THROUGH_ID_5_OP_PLUS_10_DISPLAY_QUOTED_BRANCH_1_WHEN_NOT_ID_6___60_THRU_ID_6_OP_PLUS_60_DISPLAY_QUOTED_BRANCH_2_WHEN_OTHER_DISPLAY_QUOTED_OTHER() {
		helper.compileAndVerify(
				get("EVALUATE_FALSE_ALSO_ID_2_WHEN_ID_4_ALSO_ID_5___10_THROUGH_ID_5_OP_PLUS_10_DISPLAY_QUOTED_BRANCH_1_WHEN_NOT_ID_6___60_THRU_ID_6_OP_PLUS_60_DISPLAY_QUOTED_BRANCH_2_WHEN_OTHER_DISPLAY_QUOTED_OTHER.source"),
				get("EVALUATE_FALSE_ALSO_ID_2_WHEN_ID_4_ALSO_ID_5___10_THROUGH_ID_5_OP_PLUS_10_DISPLAY_QUOTED_BRANCH_1_WHEN_NOT_ID_6___60_THRU_ID_6_OP_PLUS_60_DISPLAY_QUOTED_BRANCH_2_WHEN_OTHER_DISPLAY_QUOTED_OTHER.tree")
			);
	}
	
	@Test public void EVALUATE_QUOTED_X_ALSO_ID_2___20_ALSO_ID_3_WHEN_QUOTED_A_THRU_QUOTED_Z_ALSO_NOT_10_THRU_20_ALSO_NOT_QUOTED_A_DISPLAY_QUOTED_BRANCH_1() {
		helper.compileAndVerify(
				get("EVALUATE_QUOTED_X_ALSO_ID_2___20_ALSO_ID_3_WHEN_QUOTED_A_THRU_QUOTED_Z_ALSO_NOT_10_THRU_20_ALSO_NOT_QUOTED_A_DISPLAY_QUOTED_BRANCH_1.source"),
				get("EVALUATE_QUOTED_X_ALSO_ID_2___20_ALSO_ID_3_WHEN_QUOTED_A_THRU_QUOTED_Z_ALSO_NOT_10_THRU_20_ALSO_NOT_QUOTED_A_DISPLAY_QUOTED_BRANCH_1.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void EVALUATE_FALSE_ALSO_ID_2_ALSO_30_WHEN_ANY_ALSO_ID_4_OP_STAR_40_THROUGH_80_ALSO_NOT_10_THRU_ID_5_DISPLAY_QUOTED_BRANCH_1_WHEN_OTHER_DISPLAY_QUOTED_OTHER() {
		helper.compileAndVerify(
				get("EVALUATE_FALSE_ALSO_ID_2_ALSO_30_WHEN_ANY_ALSO_ID_4_OP_STAR_40_THROUGH_80_ALSO_NOT_10_THRU_ID_5_DISPLAY_QUOTED_BRANCH_1_WHEN_OTHER_DISPLAY_QUOTED_OTHER.source"),
				get("EVALUATE_FALSE_ALSO_ID_2_ALSO_30_WHEN_ANY_ALSO_ID_4_OP_STAR_40_THROUGH_80_ALSO_NOT_10_THRU_ID_5_DISPLAY_QUOTED_BRANCH_1_WHEN_OTHER_DISPLAY_QUOTED_OTHER.tree")
			);
	}
}
