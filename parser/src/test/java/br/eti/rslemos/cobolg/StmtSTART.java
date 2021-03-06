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

import br.eti.rslemos.cobolg.COBOLParser.StmtSTARTContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

public class StmtSTART {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtSTART");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtSTARTContext> helper = new CompilerHelper<StmtSTARTContext>() {
		@Override protected StmtSTARTContext parsePart() { return parser.stmtSTART(true); }
	};
	
	@Test public void START_FILENAME() {
		helper.compileAndVerify(
				get("START_FILENAME.source"),
				get("START_FILENAME.tree")
			);
	}
	
	@Test public void START_FILENAME_KEY_EQUAL_K() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_EQUAL_K.source"),
				get("START_FILENAME_KEY_EQUAL_K.tree")
			);
	}
	
	@Test public void START_FILENAME_KEY_OP_EQUAL_K() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_OP_EQUAL_K.source"),
				get("START_FILENAME_KEY_OP_EQUAL_K.tree")
			);
	}
	
	@Test public void START_FILENAME_KEY_GREATER_K() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_GREATER_K.source"),
				get("START_FILENAME_KEY_GREATER_K.tree")
			);
	}
	
	@Test public void START_FILENAME_KEY_OP_GREATER_K() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_OP_GREATER_K.source"),
				get("START_FILENAME_KEY_OP_GREATER_K.tree")
			);
	}
	
	@Test public void START_FILENAME_KEY_NOT_LESS_K() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_NOT_LESS_K.source"),
				get("START_FILENAME_KEY_NOT_LESS_K.tree")
			);
	}
	
	@Test public void START_FILENAME_KEY_NOT_OP_LESS_K() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_NOT_OP_LESS_K.source"),
				get("START_FILENAME_KEY_NOT_OP_LESS_K.tree")
			);
	}
	
	@Test public void START_FILENAME_KEY_GREATER_OR_EQUAL_K() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_GREATER_OR_EQUAL_K.source"),
				get("START_FILENAME_KEY_GREATER_OR_EQUAL_K.tree")
			);
	}
	
	@Test public void START_FILENAME_KEY_OP_NOTLESS_K() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_OP_NOTLESS_K.source"),
				get("START_FILENAME_KEY_OP_NOTLESS_K.tree")
			);
	}
	
	@Test public void START_FILENAME_KEY_IS_EQUAL_TO_K() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_EQUAL_TO_K.source"),
				get("START_FILENAME_KEY_IS_EQUAL_TO_K.tree")
			);
	}
	
	@Test public void START_FILENAME_KEY_IS_GREATER_THAN_K() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_GREATER_THAN_K.source"),
				get("START_FILENAME_KEY_IS_GREATER_THAN_K.tree")
			);
	}
	
	@Test public void START_FILENAME_KEY_IS_NOT_LESS_THAN_K() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_NOT_LESS_THAN_K.source"),
				get("START_FILENAME_KEY_IS_NOT_LESS_THAN_K.tree")
			);
	}
	
	@Test public void START_FILENAME_KEY_IS_GREATER_THAN_OR_EQUAL_TO_K() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_GREATER_THAN_OR_EQUAL_TO_K.source"),
				get("START_FILENAME_KEY_IS_GREATER_THAN_OR_EQUAL_TO_K.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN() {
		helper.compileAndVerify(
				get("START_FILENAME_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.source"),
				get("START_FILENAME_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.source"),
				get("START_FILENAME_KEY_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_OP_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_OP_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.source"),
				get("START_FILENAME_KEY_OP_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_GREATER_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_GREATER_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.source"),
				get("START_FILENAME_KEY_GREATER_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_OP_GREATER_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_OP_GREATER_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.source"),
				get("START_FILENAME_KEY_OP_GREATER_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_NOT_LESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_NOT_LESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.source"),
				get("START_FILENAME_KEY_NOT_LESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_NOT_OP_LESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_NOT_OP_LESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.source"),
				get("START_FILENAME_KEY_NOT_OP_LESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_GREATER_OR_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_GREATER_OR_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.source"),
				get("START_FILENAME_KEY_GREATER_OR_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_OP_NOTLESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_OP_NOTLESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.source"),
				get("START_FILENAME_KEY_OP_NOTLESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_IS_EQUAL_TO_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_EQUAL_TO_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.source"),
				get("START_FILENAME_KEY_IS_EQUAL_TO_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_IS_GREATER_THAN_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_GREATER_THAN_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.source"),
				get("START_FILENAME_KEY_IS_GREATER_THAN_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_IS_NOT_LESS_THAN_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_NOT_LESS_THAN_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.source"),
				get("START_FILENAME_KEY_IS_NOT_LESS_THAN_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_IS_GREATER_THAN_OR_EQUAL_TO_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_GREATER_THAN_OR_EQUAL_TO_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.source"),
				get("START_FILENAME_KEY_IS_GREATER_THAN_OR_EQUAL_TO_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_END_START.source"),
				get("START_FILENAME_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_EQUAL_K_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_EQUAL_K_END_START.source"),
				get("START_FILENAME_KEY_EQUAL_K_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_OP_EQUAL_K_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_OP_EQUAL_K_END_START.source"),
				get("START_FILENAME_KEY_OP_EQUAL_K_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_GREATER_K_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_GREATER_K_END_START.source"),
				get("START_FILENAME_KEY_GREATER_K_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_OP_GREATER_K_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_OP_GREATER_K_END_START.source"),
				get("START_FILENAME_KEY_OP_GREATER_K_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_NOT_LESS_K_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_NOT_LESS_K_END_START.source"),
				get("START_FILENAME_KEY_NOT_LESS_K_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_NOT_OP_LESS_K_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_NOT_OP_LESS_K_END_START.source"),
				get("START_FILENAME_KEY_NOT_OP_LESS_K_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_GREATER_OR_EQUAL_K_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_GREATER_OR_EQUAL_K_END_START.source"),
				get("START_FILENAME_KEY_GREATER_OR_EQUAL_K_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_OP_NOTLESS_K_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_OP_NOTLESS_K_END_START.source"),
				get("START_FILENAME_KEY_OP_NOTLESS_K_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_IS_EQUAL_TO_K_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_EQUAL_TO_K_END_START.source"),
				get("START_FILENAME_KEY_IS_EQUAL_TO_K_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_IS_GREATER_THAN_K_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_GREATER_THAN_K_END_START.source"),
				get("START_FILENAME_KEY_IS_GREATER_THAN_K_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_IS_NOT_LESS_THAN_K_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_NOT_LESS_THAN_K_END_START.source"),
				get("START_FILENAME_KEY_IS_NOT_LESS_THAN_K_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_IS_GREATER_THAN_OR_EQUAL_TO_K_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_GREATER_THAN_OR_EQUAL_TO_K_END_START.source"),
				get("START_FILENAME_KEY_IS_GREATER_THAN_OR_EQUAL_TO_K_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.source"),
				get("START_FILENAME_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.source"),
				get("START_FILENAME_KEY_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_OP_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_OP_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.source"),
				get("START_FILENAME_KEY_OP_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_GREATER_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_GREATER_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.source"),
				get("START_FILENAME_KEY_GREATER_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_OP_GREATER_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_OP_GREATER_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.source"),
				get("START_FILENAME_KEY_OP_GREATER_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_NOT_LESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_NOT_LESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.source"),
				get("START_FILENAME_KEY_NOT_LESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_NOT_OP_LESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_NOT_OP_LESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.source"),
				get("START_FILENAME_KEY_NOT_OP_LESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_GREATER_OR_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_GREATER_OR_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.source"),
				get("START_FILENAME_KEY_GREATER_OR_EQUAL_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_OP_NOTLESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_OP_NOTLESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.source"),
				get("START_FILENAME_KEY_OP_NOTLESS_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_IS_EQUAL_TO_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_EQUAL_TO_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.source"),
				get("START_FILENAME_KEY_IS_EQUAL_TO_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_IS_GREATER_THAN_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_GREATER_THAN_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.source"),
				get("START_FILENAME_KEY_IS_GREATER_THAN_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_IS_NOT_LESS_THAN_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_NOT_LESS_THAN_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.source"),
				get("START_FILENAME_KEY_IS_NOT_LESS_THAN_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void START_FILENAME_KEY_IS_GREATER_THAN_OR_EQUAL_TO_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START() {
		helper.compileAndVerify(
				get("START_FILENAME_KEY_IS_GREATER_THAN_OR_EQUAL_TO_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.source"),
				get("START_FILENAME_KEY_IS_GREATER_THAN_OR_EQUAL_TO_K_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_START.tree")
			);
	}
}
