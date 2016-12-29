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

import br.eti.rslemos.cobolg.COBOLParser.ProceduralStatementContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

public class StmtDIVIDE {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtDIVIDE");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<ProceduralStatementContext> helper = new CompilerHelper<ProceduralStatementContext>() {
		@Override protected ProceduralStatementContext parsePart() { return parser.proceduralStatement(); }
	};
	
	@Test public void DIVIDE_X_INTO_Y_ROUNDED_W() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_Y_ROUNDED_W.source"),
				get("DIVIDE_X_INTO_Y_ROUNDED_W.tree")
			);
	}
	
	@Test public void DIVIDE_20_INTO_Y_ROUNDED_W() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_Y_ROUNDED_W.source"),
				get("DIVIDE_20_INTO_Y_ROUNDED_W.tree")
			);
	}
	
	@Test public void DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_W() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_W.source"),
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_W.tree")
			);
	}
	
	@Test public void DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_W() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_W.source"),
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_W.tree")
			);
	}
	
	@Test public void DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_W() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_W.source"),
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_W.tree")
			);
	}
	
	@Test public void DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_W() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_W.source"),
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_W.tree")
			);
	}
	
	@Test public void DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_W() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_W.source"),
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_W.tree")
			);
	}
	
	@Test public void DIVIDE_X_BY_10_GIVING_Y_ROUNDED_W() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_W.source"),
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_W.tree")
			);
	}
	
	@Test public void DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_W() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_W.source"),
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_W.tree")
			);
	}
	
	@Test public void DIVIDE_20_BY_10_GIVING_Y_ROUNDED_W() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_W.source"),
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_W.tree")
			);
	}
	
	@Test public void DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W.source"),
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W.tree")
			);
	}
	
	@Test public void DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W.source"),
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W.tree")
			);
	}
	
	@Test public void DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W.source"),
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W.tree")
			);
	}
	
	@Test public void DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W.source"),
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W.tree")
			);
	}
	
	@Test public void DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W.source"),
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W.tree")
			);
	}
	
	@Test public void DIVIDE_X_BY_10_GIVING_Y_ROUNDED_REMAINDER_W() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_REMAINDER_W.source"),
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_REMAINDER_W.tree")
			);
	}
	
	@Test public void DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W.source"),
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W.tree")
			);
	}
	
	@Test public void DIVIDE_20_BY_10_GIVING_Y_ROUNDED_REMAINDER_W() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_REMAINDER_W.source"),
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_REMAINDER_W.tree")
			);
	}
	
	@Test public void DIVIDE_X_INTO_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_X_INTO_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_20_INTO_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_20_INTO_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_X_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_20_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_X_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void DIVIDE_20_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_X_INTO_Y_ROUNDED_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_Y_ROUNDED_W_END_DIVIDE.source"),
				get("DIVIDE_X_INTO_Y_ROUNDED_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_20_INTO_Y_ROUNDED_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_Y_ROUNDED_W_END_DIVIDE.source"),
				get("DIVIDE_20_INTO_Y_ROUNDED_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_W_END_DIVIDE.source"),
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_W_END_DIVIDE.source"),
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_W_END_DIVIDE.source"),
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_W_END_DIVIDE.source"),
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_W_END_DIVIDE.source"),
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_X_BY_10_GIVING_Y_ROUNDED_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_W_END_DIVIDE.source"),
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_W_END_DIVIDE.source"),
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_20_BY_10_GIVING_Y_ROUNDED_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_W_END_DIVIDE.source"),
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.source"),
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.source"),
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.source"),
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.source"),
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.source"),
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_X_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.source"),
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.source"),
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DIVIDE_20_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.source"),
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_X_INTO_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_X_INTO_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_20_INTO_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_20_INTO_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_X_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_20_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_X_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_X_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_20_INTO_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_20_INTO_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_X_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_X_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_X_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_20_BY_Z_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
	
	@Test public void DIVIDE_20_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE() {
		helper.compileAndVerify(
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.source"),
				get("DIVIDE_20_BY_10_GIVING_Y_ROUNDED_REMAINDER_W_ON_SIZE_ERROR_STOP_RUN_END_DIVIDE.tree")
			);
	}
}
