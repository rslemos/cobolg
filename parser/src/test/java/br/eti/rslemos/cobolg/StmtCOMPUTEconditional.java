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

import br.eti.rslemos.cobolg.COBOLParser.StmtCOMPUTEconditionalContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

public class StmtCOMPUTEconditional {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtCOMPUTEconditional");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtCOMPUTEconditionalContext> helper = new CompilerHelper<StmtCOMPUTEconditionalContext>() {
		@Override protected StmtCOMPUTEconditionalContext parsePart() { return parser.stmtCOMPUTEconditional(); }
	};
	
	@Test public void COMPUTE_X_ROUNDED_Y_EQUAL_Z_OP_STARSTAR_W() {
		helper.compileAndVerify(
				get("COMPUTE_X_ROUNDED_Y_EQUAL_Z_OP_STARSTAR_W.source"),
				get("COMPUTE_X_ROUNDED_Y_EQUAL_Z_OP_STARSTAR_W.tree")
			);
	}
	
	@Test public void COMPUTE_X_Y_ROUNDED_OP_EQUAL_Z_OP_STAR_W() {
		helper.compileAndVerify(
				get("COMPUTE_X_Y_ROUNDED_OP_EQUAL_Z_OP_STAR_W.source"),
				get("COMPUTE_X_Y_ROUNDED_OP_EQUAL_Z_OP_STAR_W.tree")
			);
	}
	
	@Test public void COMPUTE_X_ROUNDED_Y_EQUAL_Z_OP_STARSTAR_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("COMPUTE_X_ROUNDED_Y_EQUAL_Z_OP_STARSTAR_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("COMPUTE_X_ROUNDED_Y_EQUAL_Z_OP_STARSTAR_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void COMPUTE_X_Y_ROUNDED_OP_EQUAL_Z_OP_STAR_W_ON_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("COMPUTE_X_Y_ROUNDED_OP_EQUAL_Z_OP_STAR_W_ON_SIZE_ERROR_STOP_RUN.source"),
				get("COMPUTE_X_Y_ROUNDED_OP_EQUAL_Z_OP_STAR_W_ON_SIZE_ERROR_STOP_RUN.tree")
			);
	}
}
