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

import br.eti.rslemos.cobolg.COBOLParser.StmtCLOSEContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

// funny how it seems
@Waive(CompilationError.SYNTAX_ERROR)
public class StmtCLOSE {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtCLOSE");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtCLOSEContext> helper = new CompilerHelper<StmtCLOSEContext>() {
		@Override protected StmtCLOSEContext parsePart() { return parser.stmtCLOSE(); }
	};
	
	@Test public void CLOSE_FILENAME_1() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1.source"),
				get("CLOSE_FILENAME_1.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_REEL_REMOVAL() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_REEL_REMOVAL.source"),
				get("CLOSE_FILENAME_1_REEL_REMOVAL.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_REEL_FOR_REMOVAL() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_REEL_FOR_REMOVAL.source"),
				get("CLOSE_FILENAME_1_REEL_FOR_REMOVAL.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_REEL_WITH_NO_REWIND() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_REEL_WITH_NO_REWIND.source"),
				get("CLOSE_FILENAME_1_REEL_WITH_NO_REWIND.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_UNIT_REMOVAL() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_UNIT_REMOVAL.source"),
				get("CLOSE_FILENAME_1_UNIT_REMOVAL.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_UNIT_FOR_REMOVAL() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_UNIT_FOR_REMOVAL.source"),
				get("CLOSE_FILENAME_1_UNIT_FOR_REMOVAL.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_UNIT_WITH_NO_REWIND() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_UNIT_WITH_NO_REWIND.source"),
				get("CLOSE_FILENAME_1_UNIT_WITH_NO_REWIND.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_NO_REWIND() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_NO_REWIND.source"),
				get("CLOSE_FILENAME_1_NO_REWIND.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_LOCK() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_LOCK.source"),
				get("CLOSE_FILENAME_1_LOCK.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_WITH_NO_REWIND() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_WITH_NO_REWIND.source"),
				get("CLOSE_FILENAME_1_WITH_NO_REWIND.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_WITH_LOCK() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_WITH_LOCK.source"),
				get("CLOSE_FILENAME_1_WITH_LOCK.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_FILENAME_2() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_FILENAME_2.source"),
				get("CLOSE_FILENAME_1_FILENAME_2.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_REEL_REMOVAL_FILENAME_2_REEL_FOR_REMOVAL() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_REEL_REMOVAL_FILENAME_2_REEL_FOR_REMOVAL.source"),
				get("CLOSE_FILENAME_1_REEL_REMOVAL_FILENAME_2_REEL_FOR_REMOVAL.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_REEL_FOR_REMOVAL_FILENAME_2_REEL_WITH_NO_REWIND() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_REEL_FOR_REMOVAL_FILENAME_2_REEL_WITH_NO_REWIND.source"),
				get("CLOSE_FILENAME_1_REEL_FOR_REMOVAL_FILENAME_2_REEL_WITH_NO_REWIND.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_REEL_WITH_NO_REWIND_FILENAME_2_UNIT_REMOVAL() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_REEL_WITH_NO_REWIND_FILENAME_2_UNIT_REMOVAL.source"),
				get("CLOSE_FILENAME_1_REEL_WITH_NO_REWIND_FILENAME_2_UNIT_REMOVAL.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_UNIT_REMOVAL_FILENAME_2_UNIT_FOR_REMOVAL() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_UNIT_REMOVAL_FILENAME_2_UNIT_FOR_REMOVAL.source"),
				get("CLOSE_FILENAME_1_UNIT_REMOVAL_FILENAME_2_UNIT_FOR_REMOVAL.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_UNIT_FOR_REMOVAL_FILENAME_2_UNIT_WITH_NO_REWIND() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_UNIT_FOR_REMOVAL_FILENAME_2_UNIT_WITH_NO_REWIND.source"),
				get("CLOSE_FILENAME_1_UNIT_FOR_REMOVAL_FILENAME_2_UNIT_WITH_NO_REWIND.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_UNIT_WITH_NO_REWIND_FILENAME_2_NO_REWIND() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_UNIT_WITH_NO_REWIND_FILENAME_2_NO_REWIND.source"),
				get("CLOSE_FILENAME_1_UNIT_WITH_NO_REWIND_FILENAME_2_NO_REWIND.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_NO_REWIND_FILENAME_2_LOCK() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_NO_REWIND_FILENAME_2_LOCK.source"),
				get("CLOSE_FILENAME_1_NO_REWIND_FILENAME_2_LOCK.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_LOCK_FILENAME_2_WITH_NO_REWIND() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_LOCK_FILENAME_2_WITH_NO_REWIND.source"),
				get("CLOSE_FILENAME_1_LOCK_FILENAME_2_WITH_NO_REWIND.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_WITH_NO_REWIND_FILENAME_2_WITH_LOCK() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_WITH_NO_REWIND_FILENAME_2_WITH_LOCK.source"),
				get("CLOSE_FILENAME_1_WITH_NO_REWIND_FILENAME_2_WITH_LOCK.tree")
			);
	}
	
	@Test public void CLOSE_FILENAME_1_WITH_LOCK_FILENAME_2_REEL_REMOVAL() {
		helper.compileAndVerify(
				get("CLOSE_FILENAME_1_WITH_LOCK_FILENAME_2_REEL_REMOVAL.source"),
				get("CLOSE_FILENAME_1_WITH_LOCK_FILENAME_2_REEL_REMOVAL.tree")
			);
	}
}
