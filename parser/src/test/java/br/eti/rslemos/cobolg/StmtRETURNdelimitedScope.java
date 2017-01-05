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

import br.eti.rslemos.cobolg.COBOLParser.StmtRETURNdelimitedScopeContext;

public class StmtRETURNdelimitedScope {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtRETURNdelimitedScope");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtRETURNdelimitedScopeContext> helper = new CompilerHelper<StmtRETURNdelimitedScopeContext>() {
		@Override protected StmtRETURNdelimitedScopeContext parsePart() { return parser.stmtRETURNdelimitedScope(); }
	};
	
	@Test public void RETURN_FILE_1_END_RETURN() {
		helper.compileAndVerify(
				get("RETURN_FILE_1_END_RETURN.source"),
				get("RETURN_FILE_1_END_RETURN.tree")
			);
	}
	
	@Test public void RETURN_FILE_1_RECORD_END_RETURN() {
		helper.compileAndVerify(
				get("RETURN_FILE_1_RECORD_END_RETURN.source"),
				get("RETURN_FILE_1_RECORD_END_RETURN.tree")
			);
	}
	
	@Test public void RETURN_FILE_1_INTO_ID_1_END_RETURN() {
		helper.compileAndVerify(
				get("RETURN_FILE_1_INTO_ID_1_END_RETURN.source"),
				get("RETURN_FILE_1_INTO_ID_1_END_RETURN.tree")
			);
	}
	
	@Test public void RETURN_FILE_1_RECORD_INTO_ID_1_END_RETURN() {
		helper.compileAndVerify(
				get("RETURN_FILE_1_RECORD_INTO_ID_1_END_RETURN.source"),
				get("RETURN_FILE_1_RECORD_INTO_ID_1_END_RETURN.tree")
			);
	}
	
	@Test public void RETURN_FILE_1_AT_END_STOP_RUN_END_RETURN() {
		helper.compileAndVerify(
				get("RETURN_FILE_1_AT_END_STOP_RUN_END_RETURN.source"),
				get("RETURN_FILE_1_AT_END_STOP_RUN_END_RETURN.tree")
			);
	}
	
	@Test public void RETURN_FILE_1_RECORD_AT_END_STOP_RUN_END_RETURN() {
		helper.compileAndVerify(
				get("RETURN_FILE_1_RECORD_AT_END_STOP_RUN_END_RETURN.source"),
				get("RETURN_FILE_1_RECORD_AT_END_STOP_RUN_END_RETURN.tree")
			);
	}
	
	@Test public void RETURN_FILE_1_INTO_ID_1_AT_END_STOP_RUN_END_RETURN() {
		helper.compileAndVerify(
				get("RETURN_FILE_1_INTO_ID_1_AT_END_STOP_RUN_END_RETURN.source"),
				get("RETURN_FILE_1_INTO_ID_1_AT_END_STOP_RUN_END_RETURN.tree")
			);
	}
	
	@Test public void RETURN_FILE_1_RECORD_INTO_ID_1_AT_END_STOP_RUN_END_RETURN() {
		helper.compileAndVerify(
				get("RETURN_FILE_1_RECORD_INTO_ID_1_AT_END_STOP_RUN_END_RETURN.source"),
				get("RETURN_FILE_1_RECORD_INTO_ID_1_AT_END_STOP_RUN_END_RETURN.tree")
			);
	}
}
