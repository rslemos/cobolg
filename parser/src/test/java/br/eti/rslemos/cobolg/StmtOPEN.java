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

import org.junit.Ignore;
import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.StmtOPENContext;

public class StmtOPEN {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtOPEN");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtOPENContext> helper = new CompilerHelper<StmtOPENContext>() {
		@Override protected StmtOPENContext parsePart() { return parser.stmtOPEN(); }
	};
	
	@Ignore
	@Test public void OPEN_INPUT_FILENAME_1() {
		helper.compileAndVerify(
				get("OPEN_INPUT_FILENAME_1.source"),
				get("OPEN_INPUT_FILENAME_1.tree")
			);
	}
	
	@Test public void OPEN_INPUT_FILENAME_1_REVERSED() {
		helper.compileAndVerify(
				get("OPEN_INPUT_FILENAME_1_REVERSED.source"),
				get("OPEN_INPUT_FILENAME_1_REVERSED.tree")
			);
	}
	
	@Test public void OPEN_INPUT_FILENAME_1_NO_REWIND() {
		helper.compileAndVerify(
				get("OPEN_INPUT_FILENAME_1_NO_REWIND.source"),
				get("OPEN_INPUT_FILENAME_1_NO_REWIND.tree")
			);
	}
	
	@Test public void OPEN_INPUT_FILENAME_1_WITH_NO_REWIND() {
		helper.compileAndVerify(
				get("OPEN_INPUT_FILENAME_1_WITH_NO_REWIND.source"),
				get("OPEN_INPUT_FILENAME_1_WITH_NO_REWIND.tree")
			);
	}
	
	@Test public void OPEN_INPUT_FILENAME_1_FILENAME_2_REVERSED() {
		helper.compileAndVerify(
				get("OPEN_INPUT_FILENAME_1_FILENAME_2_REVERSED.source"),
				get("OPEN_INPUT_FILENAME_1_FILENAME_2_REVERSED.tree")
			);
	}
	
	@Test public void OPEN_INPUT_FILENAME_1_REVERSED_FILENAME_2_NO_REWIND() {
		helper.compileAndVerify(
				get("OPEN_INPUT_FILENAME_1_REVERSED_FILENAME_2_NO_REWIND.source"),
				get("OPEN_INPUT_FILENAME_1_REVERSED_FILENAME_2_NO_REWIND.tree")
			);
	}
	
	@Test public void OPEN_INPUT_FILENAME_1_NO_REWIND_FILENAME_2_WITH_NO_REWIND() {
		helper.compileAndVerify(
				get("OPEN_INPUT_FILENAME_1_NO_REWIND_FILENAME_2_WITH_NO_REWIND.source"),
				get("OPEN_INPUT_FILENAME_1_NO_REWIND_FILENAME_2_WITH_NO_REWIND.tree")
			);
	}
	
	@Ignore
	@Test public void OPEN_INPUT_FILENAME_1_WITH_NO_REWIND_FILENAME_2() {
		helper.compileAndVerify(
				get("OPEN_INPUT_FILENAME_1_WITH_NO_REWIND_FILENAME_2.source"),
				get("OPEN_INPUT_FILENAME_1_WITH_NO_REWIND_FILENAME_2.tree")
			);
	}
	
	@Test public void OPEN_OUTPUT_FILENAME_1() {
		helper.compileAndVerify(
				get("OPEN_OUTPUT_FILENAME_1.source"),
				get("OPEN_OUTPUT_FILENAME_1.tree")
			);
	}
	
	@Test public void OPEN_OUTPUT_FILENAME_1_NO_REWIND() {
		helper.compileAndVerify(
				get("OPEN_OUTPUT_FILENAME_1_NO_REWIND.source"),
				get("OPEN_OUTPUT_FILENAME_1_NO_REWIND.tree")
			);
	}
	
	@Test public void OPEN_OUTPUT_FILENAME_1_WITH_NO_REWIND() {
		helper.compileAndVerify(
				get("OPEN_OUTPUT_FILENAME_1_WITH_NO_REWIND.source"),
				get("OPEN_OUTPUT_FILENAME_1_WITH_NO_REWIND.tree")
			);
	}
	
	@Test public void OPEN_OUTPUT_FILENAME_1_FILENAME_2_NO_REWIND() {
		helper.compileAndVerify(
				get("OPEN_OUTPUT_FILENAME_1_FILENAME_2_NO_REWIND.source"),
				get("OPEN_OUTPUT_FILENAME_1_FILENAME_2_NO_REWIND.tree")
			);
	}
	
	@Test public void OPEN_OUTPUT_FILENAME_1_NO_REWIND_FILENAME_2_WITH_NO_REWIND() {
		helper.compileAndVerify(
				get("OPEN_OUTPUT_FILENAME_1_NO_REWIND_FILENAME_2_WITH_NO_REWIND.source"),
				get("OPEN_OUTPUT_FILENAME_1_NO_REWIND_FILENAME_2_WITH_NO_REWIND.tree")
			);
	}
	
	@Test public void OPEN_OUTPUT_FILENAME_1_WITH_NO_REWIND_FILENAME_2() {
		helper.compileAndVerify(
				get("OPEN_OUTPUT_FILENAME_1_WITH_NO_REWIND_FILENAME_2.source"),
				get("OPEN_OUTPUT_FILENAME_1_WITH_NO_REWIND_FILENAME_2.tree")
			);
	}
	
	@Test public void OPEN_I_O_FILENAME_1() {
		helper.compileAndVerify(
				get("OPEN_I_O_FILENAME_1.source"),
				get("OPEN_I_O_FILENAME_1.tree")
			);
	}
	
	@Test public void OPEN_I_O_FILENAME_1_FILENAME_2() {
		helper.compileAndVerify(
				get("OPEN_I_O_FILENAME_1_FILENAME_2.source"),
				get("OPEN_I_O_FILENAME_1_FILENAME_2.tree")
			);
	}
	
	@Test public void OPEN_EXTEND_FILENAME_1() {
		helper.compileAndVerify(
				get("OPEN_EXTEND_FILENAME_1.source"),
				get("OPEN_EXTEND_FILENAME_1.tree")
			);
	}
	
	@Test public void OPEN_EXTEND_FILENAME_1_FILENAME_2() {
		helper.compileAndVerify(
				get("OPEN_EXTEND_FILENAME_1_FILENAME_2.source"),
				get("OPEN_EXTEND_FILENAME_1_FILENAME_2.tree")
			);
	}
}
