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

public class StmtSUBTRACT {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtSUBTRACT");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<ProceduralStatementContext> helper = new CompilerHelper<ProceduralStatementContext>() {
		@Override protected ProceduralStatementContext parsePart() { return parser.proceduralStatement(); }
	};
	
	@Test public void SUBTRACT_10_FROM_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_10_FROM_X_Y_ROUNDED.source"),
				get("SUBTRACT_10_FROM_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_10_20_FROM_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_10_20_FROM_X_Y_ROUNDED.source"),
				get("SUBTRACT_10_20_FROM_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_10_Z_FROM_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_10_Z_FROM_X_Y_ROUNDED.source"),
				get("SUBTRACT_10_Z_FROM_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_Z_FROM_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_Z_FROM_X_Y_ROUNDED.source"),
				get("SUBTRACT_Z_FROM_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_Z_W_FROM_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_Z_W_FROM_X_Y_ROUNDED.source"),
				get("SUBTRACT_Z_W_FROM_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_Z_10_FROM_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_Z_10_FROM_X_Y_ROUNDED.source"),
				get("SUBTRACT_Z_10_FROM_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_10_FROM_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_10_FROM_30_GIVING_X_Y_ROUNDED.source"),
				get("SUBTRACT_10_FROM_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_10_20_FROM_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_10_20_FROM_30_GIVING_X_Y_ROUNDED.source"),
				get("SUBTRACT_10_20_FROM_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_10_Z_FROM_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_10_Z_FROM_30_GIVING_X_Y_ROUNDED.source"),
				get("SUBTRACT_10_Z_FROM_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_Z_FROM_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_Z_FROM_30_GIVING_X_Y_ROUNDED.source"),
				get("SUBTRACT_Z_FROM_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_Z_W_FROM_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_Z_W_FROM_30_GIVING_X_Y_ROUNDED.source"),
				get("SUBTRACT_Z_W_FROM_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_Z_10_FROM_30_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_Z_10_FROM_30_GIVING_X_Y_ROUNDED.source"),
				get("SUBTRACT_Z_10_FROM_30_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_10_FROM_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_10_FROM_K_GIVING_X_Y_ROUNDED.source"),
				get("SUBTRACT_10_FROM_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_10_20_FROM_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_10_20_FROM_K_GIVING_X_Y_ROUNDED.source"),
				get("SUBTRACT_10_20_FROM_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_10_Z_FROM_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_10_Z_FROM_K_GIVING_X_Y_ROUNDED.source"),
				get("SUBTRACT_10_Z_FROM_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_Z_FROM_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_Z_FROM_K_GIVING_X_Y_ROUNDED.source"),
				get("SUBTRACT_Z_FROM_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_Z_W_FROM_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_Z_W_FROM_K_GIVING_X_Y_ROUNDED.source"),
				get("SUBTRACT_Z_W_FROM_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_Z_10_FROM_K_GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_Z_10_FROM_K_GIVING_X_Y_ROUNDED.source"),
				get("SUBTRACT_Z_10_FROM_K_GIVING_X_Y_ROUNDED.tree")
			);
	}
	
	@Test public void SUBTRACT_CORR_X_FROM_Y_ROUNDED() {
		helper.compileAndVerify(
				get("SUBTRACT_CORR_X_FROM_Y_ROUNDED.source"),
				get("SUBTRACT_CORR_X_FROM_Y_ROUNDED.tree")
			);
	}
}
