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

import br.eti.rslemos.cobolg.COBOLParser.StmtPageWRITEdelimitedScopeContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

public class StmtPageWRITEdelimitedScope {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtPageWRITEdelimitedScope");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtPageWRITEdelimitedScopeContext> helper = new CompilerHelper<StmtPageWRITEdelimitedScopeContext>() {
		@Override protected StmtPageWRITEdelimitedScopeContext parsePart() { return parser.stmtPageWRITEdelimitedScope(); }
	};
	
	@Test public void WRITE_REC_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_END_WRITE.source"),
				get("WRITE_REC_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_END_WRITE.source"),
				get("WRITE_REC_FROM_X_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_BEFORE_1_LINE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_1_LINE_END_WRITE.source"),
				get("WRITE_REC_BEFORE_1_LINE_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_BEFORE_N_LINES_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_N_LINES_END_WRITE.source"),
				get("WRITE_REC_BEFORE_N_LINES_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_BEFORE_LCP_CH2_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_LCP_CH2_END_WRITE.source"),
				get("WRITE_REC_BEFORE_LCP_CH2_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_BEFORE_PAGE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_PAGE_END_WRITE.source"),
				get("WRITE_REC_BEFORE_PAGE_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_BEFORE_ADVANCING_1_LINE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_ADVANCING_1_LINE_END_WRITE.source"),
				get("WRITE_REC_BEFORE_ADVANCING_1_LINE_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_BEFORE_ADVANCING_N_LINES_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_ADVANCING_N_LINES_END_WRITE.source"),
				get("WRITE_REC_BEFORE_ADVANCING_N_LINES_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_BEFORE_ADVANCING_LCP_CH2_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_ADVANCING_LCP_CH2_END_WRITE.source"),
				get("WRITE_REC_BEFORE_ADVANCING_LCP_CH2_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_BEFORE_ADVANCING_PAGE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_ADVANCING_PAGE_END_WRITE.source"),
				get("WRITE_REC_BEFORE_ADVANCING_PAGE_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_AFTER_1_LINE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_1_LINE_END_WRITE.source"),
				get("WRITE_REC_AFTER_1_LINE_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_AFTER_N_LINES_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_N_LINES_END_WRITE.source"),
				get("WRITE_REC_AFTER_N_LINES_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_AFTER_LCP_CH2_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_LCP_CH2_END_WRITE.source"),
				get("WRITE_REC_AFTER_LCP_CH2_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_AFTER_PAGE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_PAGE_END_WRITE.source"),
				get("WRITE_REC_AFTER_PAGE_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_AFTER_ADVANCING_1_LINE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_ADVANCING_1_LINE_END_WRITE.source"),
				get("WRITE_REC_AFTER_ADVANCING_1_LINE_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_AFTER_ADVANCING_N_LINES_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_ADVANCING_N_LINES_END_WRITE.source"),
				get("WRITE_REC_AFTER_ADVANCING_N_LINES_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_AFTER_ADVANCING_LCP_CH2_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_ADVANCING_LCP_CH2_END_WRITE.source"),
				get("WRITE_REC_AFTER_ADVANCING_LCP_CH2_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_AFTER_ADVANCING_PAGE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_ADVANCING_PAGE_END_WRITE.source"),
				get("WRITE_REC_AFTER_ADVANCING_PAGE_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_BEFORE_1_LINE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_1_LINE_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_1_LINE_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_BEFORE_N_LINES_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_N_LINES_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_N_LINES_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_BEFORE_LCP_CH2_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_LCP_CH2_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_LCP_CH2_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_BEFORE_PAGE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_PAGE_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_PAGE_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_BEFORE_ADVANCING_1_LINE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_1_LINE_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_1_LINE_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_BEFORE_ADVANCING_N_LINES_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_N_LINES_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_N_LINES_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_BEFORE_ADVANCING_LCP_CH2_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_LCP_CH2_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_LCP_CH2_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_BEFORE_ADVANCING_PAGE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_PAGE_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_PAGE_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_AFTER_1_LINE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_1_LINE_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_1_LINE_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_AFTER_N_LINES_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_N_LINES_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_N_LINES_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_AFTER_LCP_CH2_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_LCP_CH2_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_LCP_CH2_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_AFTER_PAGE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_PAGE_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_PAGE_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_AFTER_ADVANCING_1_LINE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_1_LINE_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_1_LINE_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_AFTER_ADVANCING_N_LINES_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_N_LINES_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_N_LINES_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_AFTER_ADVANCING_LCP_CH2_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_LCP_CH2_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_LCP_CH2_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_REC_FROM_X_AFTER_ADVANCING_PAGE_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_PAGE_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_PAGE_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_BEFORE_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_BEFORE_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_BEFORE_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_BEFORE_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_BEFORE_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_BEFORE_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_BEFORE_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_BEFORE_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_BEFORE_ADVANCING_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_ADVANCING_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_BEFORE_ADVANCING_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_BEFORE_ADVANCING_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_ADVANCING_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_BEFORE_ADVANCING_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_BEFORE_ADVANCING_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_ADVANCING_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_BEFORE_ADVANCING_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_BEFORE_ADVANCING_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_BEFORE_ADVANCING_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_BEFORE_ADVANCING_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_AFTER_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_AFTER_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_AFTER_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_AFTER_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_AFTER_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_AFTER_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_AFTER_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_AFTER_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_AFTER_ADVANCING_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_ADVANCING_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_AFTER_ADVANCING_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_AFTER_ADVANCING_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_ADVANCING_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_AFTER_ADVANCING_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_AFTER_ADVANCING_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_ADVANCING_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_AFTER_ADVANCING_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_AFTER_ADVANCING_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_AFTER_ADVANCING_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_AFTER_ADVANCING_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_BEFORE_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_BEFORE_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_BEFORE_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_BEFORE_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_BEFORE_ADVANCING_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_BEFORE_ADVANCING_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_BEFORE_ADVANCING_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_BEFORE_ADVANCING_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_BEFORE_ADVANCING_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_AFTER_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_AFTER_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_AFTER_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_AFTER_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_AFTER_ADVANCING_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_1_LINE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_AFTER_ADVANCING_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_N_LINES_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_AFTER_ADVANCING_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_LCP_CH2_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_REC_FROM_X_AFTER_ADVANCING_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.source"),
				get("WRITE_REC_FROM_X_AFTER_ADVANCING_PAGE_EOP_STOP_RUN_NOT_EOP_STOP_RUN_END_WRITE.tree")
			);
	}
}
