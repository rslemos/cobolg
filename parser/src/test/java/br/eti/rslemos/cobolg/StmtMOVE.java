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

import br.eti.rslemos.cobolg.COBOLParser.StmtMOVEContext;

public class StmtMOVE {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtMOVE");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtMOVEContext> helper = new CompilerHelper<StmtMOVEContext>() {
		@Override protected StmtMOVEContext parsePart() { return parser.stmtMOVE(); }
	};
	
	@Test public void MOVE_A_TO_X() {
		helper.compileAndVerify(
				get("MOVE_A_TO_X.source"),
				get("MOVE_A_TO_X.tree")
			);
	}

	@Test public void MOVE_A_TO_X_Y() {
		helper.compileAndVerify(
				get("MOVE_A_TO_X_Y.source"),
				get("MOVE_A_TO_X_Y.tree")
			);
	}

	@Test public void MOVE_10_TO_X() {
		helper.compileAndVerify(
				get("MOVE_10_TO_X.source"),
				get("MOVE_10_TO_X.tree")
			);
	}

	@Test public void MOVE_10_TO_X_Y() {
		helper.compileAndVerify(
				get("MOVE_10_TO_X_Y.source"),
				get("MOVE_10_TO_X_Y.tree")
			);
	}

	@Test public void MOVE_CORRESPONDING_A_TO_X() {
		helper.compileAndVerify(
				get("MOVE_CORRESPONDING_A_TO_X.source"),
				get("MOVE_CORRESPONDING_A_TO_X.tree")
			);
	}

	@Test public void MOVE_CORR_A_TO_X() {
		helper.compileAndVerify(
				get("MOVE_CORR_A_TO_X.source"),
				get("MOVE_CORR_A_TO_X.tree")
			);
	}
}
