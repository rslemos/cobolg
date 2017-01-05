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

import br.eti.rslemos.cobolg.COBOLParser.StmtSequentialREADimperativeContext;

public class StmtSequentialREADimperative {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtSequentialREADimperative");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtSequentialREADimperativeContext> helper = new CompilerHelper<StmtSequentialREADimperativeContext>() {
		@Override protected StmtSequentialREADimperativeContext parsePart() { return parser.stmtSequentialREADimperative(); }
	};
	
	@Test public void READ_FILENAME() {
		helper.compileAndVerify(
				get("READ_FILENAME.source"),
				get("READ_FILENAME.tree")
			);
	}
	
	@Test public void READ_FILENAME_NEXT_RECORD() {
		helper.compileAndVerify(
				get("READ_FILENAME_NEXT_RECORD.source"),
				get("READ_FILENAME_NEXT_RECORD.tree")
			);
	}
	
	@Test public void READ_FILENAME_INTO_X() {
		helper.compileAndVerify(
				get("READ_FILENAME_INTO_X.source"),
				get("READ_FILENAME_INTO_X.tree")
			);
	}
	
	@Test public void READ_FILENAME_NEXT_RECORD_INTO_X() {
		helper.compileAndVerify(
				get("READ_FILENAME_NEXT_RECORD_INTO_X.source"),
				get("READ_FILENAME_NEXT_RECORD_INTO_X.tree")
			);
	}
}