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

import br.eti.rslemos.cobolg.COBOLParser.StmtSequentialREADdelimitedScopeContext;

public class StmtSequentialREADdelimitedScope {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtSequentialREADdelimitedScope");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtSequentialREADdelimitedScopeContext> helper = new CompilerHelper<StmtSequentialREADdelimitedScopeContext>() {
		@Override protected StmtSequentialREADdelimitedScopeContext parsePart() { return parser.stmtSequentialREADdelimitedScope(); }
	};
	
	@Test public void READ_FILENAME_END_READ() {
		helper.compileAndVerify(
				get("READ_FILENAME_END_READ.source"),
				get("READ_FILENAME_END_READ.tree")
			);
	}

	@Test public void READ_FILENAME_NEXT_RECORD_END_READ() {
		helper.compileAndVerify(
				get("READ_FILENAME_NEXT_RECORD_END_READ.source"),
				get("READ_FILENAME_NEXT_RECORD_END_READ.tree")
			);
	}

	@Test public void READ_FILENAME_INTO_X_END_READ() {
		helper.compileAndVerify(
				get("READ_FILENAME_INTO_X_END_READ.source"),
				get("READ_FILENAME_INTO_X_END_READ.tree")
			);
	}

	@Test public void READ_FILENAME_NEXT_RECORD_INTO_X_END_READ() {
		helper.compileAndVerify(
				get("READ_FILENAME_NEXT_RECORD_INTO_X_END_READ.source"),
				get("READ_FILENAME_NEXT_RECORD_INTO_X_END_READ.tree")
			);
	}

	@Test public void READ_FILENAME_AT_END_STOP_RUN_NOT_AT_END_STOP_RUN_END_READ() {
		helper.compileAndVerify(
				get("READ_FILENAME_AT_END_STOP_RUN_NOT_AT_END_STOP_RUN_END_READ.source"),
				get("READ_FILENAME_AT_END_STOP_RUN_NOT_AT_END_STOP_RUN_END_READ.tree")
			);
	}

	@Test public void READ_FILENAME_NEXT_RECORD_AT_END_STOP_RUN_NOT_AT_END_STOP_RUN_END_READ() {
		helper.compileAndVerify(
				get("READ_FILENAME_NEXT_RECORD_AT_END_STOP_RUN_NOT_AT_END_STOP_RUN_END_READ.source"),
				get("READ_FILENAME_NEXT_RECORD_AT_END_STOP_RUN_NOT_AT_END_STOP_RUN_END_READ.tree")
			);
	}

	@Test public void READ_FILENAME_INTO_X_AT_END_STOP_RUN_NOT_AT_END_STOP_RUN_END_READ() {
		helper.compileAndVerify(
				get("READ_FILENAME_INTO_X_AT_END_STOP_RUN_NOT_AT_END_STOP_RUN_END_READ.source"),
				get("READ_FILENAME_INTO_X_AT_END_STOP_RUN_NOT_AT_END_STOP_RUN_END_READ.tree")
			);
	}

	@Test public void READ_FILENAME_NEXT_RECORD_INTO_X_AT_END_STOP_RUN_NOT_AT_END_STOP_RUN_END_READ() {
		helper.compileAndVerify(
				get("READ_FILENAME_NEXT_RECORD_INTO_X_AT_END_STOP_RUN_NOT_AT_END_STOP_RUN_END_READ.source"),
				get("READ_FILENAME_NEXT_RECORD_INTO_X_AT_END_STOP_RUN_NOT_AT_END_STOP_RUN_END_READ.tree")
			);
	}
}
