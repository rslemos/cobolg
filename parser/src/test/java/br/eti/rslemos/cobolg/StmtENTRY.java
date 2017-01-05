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
 * but WITHOUT ENTRY WARRANTY; without even the implied warranty of
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

import br.eti.rslemos.cobolg.COBOLParser.StmtENTRYContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

public class StmtENTRY {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtENTRY");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtENTRYContext> helper = new CompilerHelper<StmtENTRYContext>() {
		@Override protected StmtENTRYContext parsePart() { return parser.stmtENTRY(); }
	};
	
	@Test public void ENTRY_QUOTED_ENTRY_POINT_1() {
		helper.compileAndVerify(
				get("ENTRY_QUOTED_ENTRY_POINT_1.source"),
				get("ENTRY_QUOTED_ENTRY_POINT_1.tree")
			);
	}
	
	@Test public void ENTRY_QUOTED_ENTRY_POINT_1_USING_PARM_1() {
		helper.compileAndVerify(
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_PARM_1.source"),
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_PARM_1.tree")
			);
	}
	
	@Test public void ENTRY_QUOTED_ENTRY_POINT_1_USING_BY_REFERENCE_PARM_1() {
		helper.compileAndVerify(
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_BY_REFERENCE_PARM_1.source"),
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_BY_REFERENCE_PARM_1.tree")
			);
	}
	
	@Test public void ENTRY_QUOTED_ENTRY_POINT_1_USING_BY_VALUE_PARM_1() {
		helper.compileAndVerify(
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_BY_VALUE_PARM_1.source"),
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_BY_VALUE_PARM_1.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void ENTRY_QUOTED_ENTRY_POINT_1_USING_PARM_1_PARM_2() {
		helper.compileAndVerify(
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_PARM_1_PARM_2.source"),
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_PARM_1_PARM_2.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void ENTRY_QUOTED_ENTRY_POINT_1_USING_BY_REFERENCE_PARM_1_PARM_2() {
		helper.compileAndVerify(
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_BY_REFERENCE_PARM_1_PARM_2.source"),
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_BY_REFERENCE_PARM_1_PARM_2.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void ENTRY_QUOTED_ENTRY_POINT_1_USING_BY_VALUE_PARM_1_PARM_2() {
		helper.compileAndVerify(
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_BY_VALUE_PARM_1_PARM_2.source"),
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_BY_VALUE_PARM_1_PARM_2.tree")
			);
	}
	
	@Test public void ENTRY_QUOTED_ENTRY_POINT_1_USING_PARM_1_VALUE_PARM_2() {
		helper.compileAndVerify(
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_PARM_1_VALUE_PARM_2.source"),
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_PARM_1_VALUE_PARM_2.tree")
			);
	}
	
	@Test public void ENTRY_QUOTED_ENTRY_POINT_1_USING_REFERENCE_PARM_1_VALUE_PARM_2() {
		helper.compileAndVerify(
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_REFERENCE_PARM_1_VALUE_PARM_2.source"),
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_REFERENCE_PARM_1_VALUE_PARM_2.tree")
			);
	}
	
	@Test public void ENTRY_QUOTED_ENTRY_POINT_1_USING_VALUE_PARM_1_REFERENCE_PARM_2() {
		helper.compileAndVerify(
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_VALUE_PARM_1_REFERENCE_PARM_2.source"),
				get("ENTRY_QUOTED_ENTRY_POINT_1_USING_VALUE_PARM_1_REFERENCE_PARM_2.tree")
			);
	}
}
