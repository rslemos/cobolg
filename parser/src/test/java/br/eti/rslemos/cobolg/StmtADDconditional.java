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

import br.eti.rslemos.cobolg.COBOLParser.StmtADDconditionalContext;

public class StmtADDconditional {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtADDconditional");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtADDconditionalContext> helper = new CompilerHelper<StmtADDconditionalContext>() {
		@Override protected StmtADDconditionalContext parsePart() { return parser.stmtADDconditional(); }
	};
	
	@Test public void ADD_10_TO_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_10_TO_X_Y_ROUNDED.source"),
				get("ADD_10_TO_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_10_20_TO_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_10_20_TO_X_Y_ROUNDED.source"),
				get("ADD_10_20_TO_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_10_Z_TO_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_10_Z_TO_X_Y_ROUNDED.source"),
				get("ADD_10_Z_TO_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_Z_TO_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_Z_TO_X_Y_ROUNDED.source"),
				get("ADD_Z_TO_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_Z_W_TO_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_Z_W_TO_X_Y_ROUNDED.source"),
				get("ADD_Z_W_TO_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_Z_10_TO_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_Z_10_TO_X_Y_ROUNDED.source"),
				get("ADD_Z_10_TO_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_10_TO_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_10_TO_30_GIVING_X_Y_ROUNDED.source"),
				get("ADD_10_TO_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_10_20_TO_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_10_20_TO_30_GIVING_X_Y_ROUNDED.source"),
				get("ADD_10_20_TO_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_10_Z_TO_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_10_Z_TO_30_GIVING_X_Y_ROUNDED.source"),
				get("ADD_10_Z_TO_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_Z_TO_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_Z_TO_30_GIVING_X_Y_ROUNDED.source"),
				get("ADD_Z_TO_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_Z_W_TO_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_Z_W_TO_30_GIVING_X_Y_ROUNDED.source"),
				get("ADD_Z_W_TO_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_Z_10_TO_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_Z_10_TO_30_GIVING_X_Y_ROUNDED.source"),
				get("ADD_Z_10_TO_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_10_TO_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_10_TO_K_GIVING_X_Y_ROUNDED.source"),
				get("ADD_10_TO_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_10_20_TO_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_10_20_TO_K_GIVING_X_Y_ROUNDED.source"),
				get("ADD_10_20_TO_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_10_Z_TO_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_10_Z_TO_K_GIVING_X_Y_ROUNDED.source"),
				get("ADD_10_Z_TO_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_Z_TO_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_Z_TO_K_GIVING_X_Y_ROUNDED.source"),
				get("ADD_Z_TO_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_Z_W_TO_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_Z_W_TO_K_GIVING_X_Y_ROUNDED.source"),
				get("ADD_Z_W_TO_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_Z_10_TO_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_Z_10_TO_K_GIVING_X_Y_ROUNDED.source"),
				get("ADD_Z_10_TO_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_10_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_10_30_GIVING_X_Y_ROUNDED.source"),
				get("ADD_10_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_10_20_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_10_20_30_GIVING_X_Y_ROUNDED.source"),
				get("ADD_10_20_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_10_Z_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_10_Z_30_GIVING_X_Y_ROUNDED.source"),
				get("ADD_10_Z_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_Z_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_Z_30_GIVING_X_Y_ROUNDED.source"),
				get("ADD_Z_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_Z_W_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_Z_W_30_GIVING_X_Y_ROUNDED.source"),
				get("ADD_Z_W_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_Z_10_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_Z_10_30_GIVING_X_Y_ROUNDED.source"),
				get("ADD_Z_10_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_10_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_10_K_GIVING_X_Y_ROUNDED.source"),
				get("ADD_10_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_10_20_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_10_20_K_GIVING_X_Y_ROUNDED.source"),
				get("ADD_10_20_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_10_Z_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_10_Z_K_GIVING_X_Y_ROUNDED.source"),
				get("ADD_10_Z_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_Z_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_Z_K_GIVING_X_Y_ROUNDED.source"),
				get("ADD_Z_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_Z_W_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_Z_W_K_GIVING_X_Y_ROUNDED.source"),
				get("ADD_Z_W_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_Z_10_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_Z_10_K_GIVING_X_Y_ROUNDED.source"),
				get("ADD_Z_10_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_CORR_X_TO_Y_ROUNDED() {
		helper.compileAndVerify(
				get("ADD_CORR_X_TO_Y_ROUNDED.source"),
				get("ADD_CORR_X_TO_Y_ROUNDED.tree")
			);
	}
	
	@Test public void ADD_10_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_10_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_10_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_10_20_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_10_20_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_10_20_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_10_Z_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_10_Z_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_10_Z_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_Z_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_Z_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_Z_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_Z_W_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_Z_W_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_Z_W_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_Z_10_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_Z_10_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_Z_10_TO_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_10_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_10_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_10_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_10_20_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_10_20_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_10_20_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_10_Z_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_10_Z_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_10_Z_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_Z_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_Z_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_Z_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_Z_W_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_Z_W_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_Z_W_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_Z_10_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_Z_10_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_Z_10_TO_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_10_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_10_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_10_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_10_20_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_10_20_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_10_20_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_10_Z_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_10_Z_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_10_Z_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_Z_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_Z_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_Z_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_Z_W_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_Z_W_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_Z_W_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_Z_10_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_Z_10_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_Z_10_TO_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_10_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_10_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_10_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_10_20_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_10_20_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_10_20_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_10_Z_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_10_Z_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_10_Z_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_Z_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_Z_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_Z_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_Z_W_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_Z_W_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_Z_W_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_Z_10_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_Z_10_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_Z_10_30_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_10_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_10_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_10_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_10_20_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_10_20_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_10_20_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_10_Z_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_10_Z_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_10_Z_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_Z_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_Z_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_Z_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_Z_W_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_Z_W_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_Z_W_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_Z_10_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_Z_10_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_Z_10_K_GIVING_X_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
	
	@Test public void ADD_CORR_X_TO_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN() {
		helper.compileAndVerify(
				get("ADD_CORR_X_TO_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.source"),
				get("ADD_CORR_X_TO_Y_ROUNDED_SIZE_ERROR_STOP_RUN_NOT_SIZE_ERROR_STOP_RUN.tree")
			);
	}
}
