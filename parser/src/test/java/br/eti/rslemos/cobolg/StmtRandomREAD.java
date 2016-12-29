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

public class StmtRandomREAD {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtRandomREAD");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<ProceduralStatementContext> helper = new CompilerHelper<ProceduralStatementContext>() {
		@Override protected ProceduralStatementContext parsePart() { return parser.proceduralStatement(); }
	};
	
	@Test public void READ_FILENAME_KEY_K() {
		helper.compileAndVerify(
				get("READ_FILENAME_KEY_K.source"),
				get("READ_FILENAME_KEY_K.tree")
			);
	}
	
	@Test public void READ_FILENAME_INTO_X_KEY_K() {
		helper.compileAndVerify(
				get("READ_FILENAME_INTO_X_KEY_K.source"),
				get("READ_FILENAME_INTO_X_KEY_K.tree")
			);
	}
	
	@Test public void READ_FILENAME_RECORD_KEY_IS_K() {
		helper.compileAndVerify(
				get("READ_FILENAME_RECORD_KEY_IS_K.source"),
				get("READ_FILENAME_RECORD_KEY_IS_K.tree")
			);
	}
	
	@Test public void READ_FILENAME_RECORD_INTO_X_KEY_IS_K() {
		helper.compileAndVerify(
				get("READ_FILENAME_RECORD_INTO_X_KEY_IS_K.source"),
				get("READ_FILENAME_RECORD_INTO_X_KEY_IS_K.tree")
			);
	}
	
	@Test public void READ_FILENAME_KEY_K_INVALID_KEY_STOP_RUN_NOT_INVALID_KEY_STOP_RUN() {
		helper.compileAndVerify(
				get("READ_FILENAME_KEY_K_INVALID_KEY_STOP_RUN_NOT_INVALID_KEY_STOP_RUN.source"),
				get("READ_FILENAME_KEY_K_INVALID_KEY_STOP_RUN_NOT_INVALID_KEY_STOP_RUN.tree")
			);
	}
	
	@Test public void READ_FILENAME_INTO_X_KEY_K_INVALID_KEY_STOP_RUN_NOT_INVALID_KEY_STOP_RUN() {
		helper.compileAndVerify(
				get("READ_FILENAME_INTO_X_KEY_K_INVALID_KEY_STOP_RUN_NOT_INVALID_KEY_STOP_RUN.source"),
				get("READ_FILENAME_INTO_X_KEY_K_INVALID_KEY_STOP_RUN_NOT_INVALID_KEY_STOP_RUN.tree")
			);
	}
	
	@Test public void READ_FILENAME_RECORD_KEY_IS_K_INVALID_KEY_STOP_RUN_NOT_INVALID_KEY_STOP_RUN() {
		helper.compileAndVerify(
				get("READ_FILENAME_RECORD_KEY_IS_K_INVALID_KEY_STOP_RUN_NOT_INVALID_KEY_STOP_RUN.source"),
				get("READ_FILENAME_RECORD_KEY_IS_K_INVALID_KEY_STOP_RUN_NOT_INVALID_KEY_STOP_RUN.tree")
			);
	}
	
	@Test public void READ_FILENAME_RECORD_INTO_X_KEY_IS_K_INVALID_KEY_STOP_RUN_NOT_INVALID_KEY_STOP_RUN() {
		helper.compileAndVerify(
				get("READ_FILENAME_RECORD_INTO_X_KEY_IS_K_INVALID_KEY_STOP_RUN_NOT_INVALID_KEY_STOP_RUN.source"),
				get("READ_FILENAME_RECORD_INTO_X_KEY_IS_K_INVALID_KEY_STOP_RUN_NOT_INVALID_KEY_STOP_RUN.tree")
			);
	}
}
