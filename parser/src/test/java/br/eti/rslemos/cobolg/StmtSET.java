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

import br.eti.rslemos.cobolg.COBOLParser.StmtSETContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

public class StmtSET {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtSET");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtSETContext> helper = new CompilerHelper<StmtSETContext>() {
		@Override protected StmtSETContext parsePart() { return parser.stmtSET(); }
	};
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void SET_IDX_1_TO_IDX_0() {
		helper.compileAndVerify(
				get("SET_IDX_1_TO_IDX_0.source"),
				get("SET_IDX_1_TO_IDX_0.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void SET_IDX_1_IDX_2_TO_IDX_0() {
		helper.compileAndVerify(
				get("SET_IDX_1_IDX_2_TO_IDX_0.source"),
				get("SET_IDX_1_IDX_2_TO_IDX_0.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void SET_IDX_1_TO_7() {
		helper.compileAndVerify(
				get("SET_IDX_1_TO_7.source"),
				get("SET_IDX_1_TO_7.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void SET_IDX_1_IDX_2_TO_7() {
		helper.compileAndVerify(
				get("SET_IDX_1_IDX_2_TO_7.source"),
				get("SET_IDX_1_IDX_2_TO_7.tree")
			);
	}
	
	@Test public void SET_IDX_1_UP_BY_ID_0() {
		helper.compileAndVerify(
				get("SET_IDX_1_UP_BY_ID_0.source"),
				get("SET_IDX_1_UP_BY_ID_0.tree")
			);
	}
	
	@Test public void SET_IDX_1_IDX_2_UP_BY_ID_0() {
		helper.compileAndVerify(
				get("SET_IDX_1_IDX_2_UP_BY_ID_0.source"),
				get("SET_IDX_1_IDX_2_UP_BY_ID_0.tree")
			);
	}
	
	@Test public void SET_IDX_1_DOWN_BY_ID_0() {
		helper.compileAndVerify(
				get("SET_IDX_1_DOWN_BY_ID_0.source"),
				get("SET_IDX_1_DOWN_BY_ID_0.tree")
			);
	}
	
	@Test public void SET_IDX_1_IDX_2_DOWN_BY_ID_0() {
		helper.compileAndVerify(
				get("SET_IDX_1_IDX_2_DOWN_BY_ID_0.source"),
				get("SET_IDX_1_IDX_2_DOWN_BY_ID_0.tree")
			);
	}
	
	@Test public void SET_IDX_1_UP_BY_2() {
		helper.compileAndVerify(
				get("SET_IDX_1_UP_BY_2.source"),
				get("SET_IDX_1_UP_BY_2.tree")
			);
	}
	
	@Test public void SET_IDX_1_IDX_2_UP_BY_2() {
		helper.compileAndVerify(
				get("SET_IDX_1_IDX_2_UP_BY_2.source"),
				get("SET_IDX_1_IDX_2_UP_BY_2.tree")
			);
	}
	
	@Test public void SET_IDX_1_DOWN_BY_2() {
		helper.compileAndVerify(
				get("SET_IDX_1_DOWN_BY_2.source"),
				get("SET_IDX_1_DOWN_BY_2.tree")
			);
	}
	
	@Test public void SET_IDX_1_IDX_2_DOWN_BY_2() {
		helper.compileAndVerify(
				get("SET_IDX_1_IDX_2_DOWN_BY_2.source"),
				get("SET_IDX_1_IDX_2_DOWN_BY_2.tree")
			);
	}
	
	@Test public void SET_MN_1_TO_ON() {
		helper.compileAndVerify(
				get("SET_MN_1_TO_ON.source"),
				get("SET_MN_1_TO_ON.tree")
			);
	}
	
	@Test public void SET_MN_1_MN_2_TO_ON() {
		helper.compileAndVerify(
				get("SET_MN_1_MN_2_TO_ON.source"),
				get("SET_MN_1_MN_2_TO_ON.tree")
			);
	}
	
	@Test public void SET_MN_1_TO_OFF() {
		helper.compileAndVerify(
				get("SET_MN_1_TO_OFF.source"),
				get("SET_MN_1_TO_OFF.tree")
			);
	}
	
	@Test public void SET_MN_1_MN_2_TO_OFF() {
		helper.compileAndVerify(
				get("SET_MN_1_MN_2_TO_OFF.source"),
				get("SET_MN_1_MN_2_TO_OFF.tree")
			);
	}
	
	@Test public void SET_MN_1_TO_ON_MN_3_TO_ON() {
		helper.compileAndVerify(
				get("SET_MN_1_TO_ON_MN_3_TO_ON.source"),
				get("SET_MN_1_TO_ON_MN_3_TO_ON.tree")
			);
	}
	
	@Test public void SET_MN_1_TO_ON_MN_3_MN_4_TO_ON() {
		helper.compileAndVerify(
				get("SET_MN_1_TO_ON_MN_3_MN_4_TO_ON.source"),
				get("SET_MN_1_TO_ON_MN_3_MN_4_TO_ON.tree")
			);
	}
	
	@Test public void SET_MN_1_TO_ON_MN_3_TO_OFF() {
		helper.compileAndVerify(
				get("SET_MN_1_TO_ON_MN_3_TO_OFF.source"),
				get("SET_MN_1_TO_ON_MN_3_TO_OFF.tree")
			);
	}
	
	@Test public void SET_MN_1_TO_ON_MN_3_MN_4_TO_OFF() {
		helper.compileAndVerify(
				get("SET_MN_1_TO_ON_MN_3_MN_4_TO_OFF.source"),
				get("SET_MN_1_TO_ON_MN_3_MN_4_TO_OFF.tree")
			);
	}
	
	@Test public void SET_MN_1_MN_2_TO_ON_MN_3_TO_ON() {
		helper.compileAndVerify(
				get("SET_MN_1_MN_2_TO_ON_MN_3_TO_ON.source"),
				get("SET_MN_1_MN_2_TO_ON_MN_3_TO_ON.tree")
			);
	}
	
	@Test public void SET_MN_1_MN_2_TO_ON_MN_3_MN_4_TO_ON() {
		helper.compileAndVerify(
				get("SET_MN_1_MN_2_TO_ON_MN_3_MN_4_TO_ON.source"),
				get("SET_MN_1_MN_2_TO_ON_MN_3_MN_4_TO_ON.tree")
			);
	}
	
	@Test public void SET_MN_1_MN_2_TO_ON_MN_3_TO_OFF() {
		helper.compileAndVerify(
				get("SET_MN_1_MN_2_TO_ON_MN_3_TO_OFF.source"),
				get("SET_MN_1_MN_2_TO_ON_MN_3_TO_OFF.tree")
			);
	}
	
	@Test public void SET_MN_1_MN_2_TO_ON_MN_3_MN_4_TO_OFF() {
		helper.compileAndVerify(
				get("SET_MN_1_MN_2_TO_ON_MN_3_MN_4_TO_OFF.source"),
				get("SET_MN_1_MN_2_TO_ON_MN_3_MN_4_TO_OFF.tree")
			);
	}
	
	@Test public void SET_MN_1_TO_OFF_MN_3_TO_ON() {
		helper.compileAndVerify(
				get("SET_MN_1_TO_OFF_MN_3_TO_ON.source"),
				get("SET_MN_1_TO_OFF_MN_3_TO_ON.tree")
			);
	}
	
	@Test public void SET_MN_1_TO_OFF_MN_3_MN_4_TO_ON() {
		helper.compileAndVerify(
				get("SET_MN_1_TO_OFF_MN_3_MN_4_TO_ON.source"),
				get("SET_MN_1_TO_OFF_MN_3_MN_4_TO_ON.tree")
			);
	}
	
	@Test public void SET_MN_1_TO_OFF_MN_3_TO_OFF() {
		helper.compileAndVerify(
				get("SET_MN_1_TO_OFF_MN_3_TO_OFF.source"),
				get("SET_MN_1_TO_OFF_MN_3_TO_OFF.tree")
			);
	}
	
	@Test public void SET_MN_1_TO_OFF_MN_3_MN_4_TO_OFF() {
		helper.compileAndVerify(
				get("SET_MN_1_TO_OFF_MN_3_MN_4_TO_OFF.source"),
				get("SET_MN_1_TO_OFF_MN_3_MN_4_TO_OFF.tree")
			);
	}
	
	@Test public void SET_MN_1_MN_2_TO_OFF_MN_3_TO_ON() {
		helper.compileAndVerify(
				get("SET_MN_1_MN_2_TO_OFF_MN_3_TO_ON.source"),
				get("SET_MN_1_MN_2_TO_OFF_MN_3_TO_ON.tree")
			);
	}
	
	@Test public void SET_MN_1_MN_2_TO_OFF_MN_3_MN_4_TO_ON() {
		helper.compileAndVerify(
				get("SET_MN_1_MN_2_TO_OFF_MN_3_MN_4_TO_ON.source"),
				get("SET_MN_1_MN_2_TO_OFF_MN_3_MN_4_TO_ON.tree")
			);
	}
	
	@Test public void SET_MN_1_MN_2_TO_OFF_MN_3_TO_OFF() {
		helper.compileAndVerify(
				get("SET_MN_1_MN_2_TO_OFF_MN_3_TO_OFF.source"),
				get("SET_MN_1_MN_2_TO_OFF_MN_3_TO_OFF.tree")
			);
	}
	
	@Test public void SET_MN_1_MN_2_TO_OFF_MN_3_MN_4_TO_OFF() {
		helper.compileAndVerify(
				get("SET_MN_1_MN_2_TO_OFF_MN_3_MN_4_TO_OFF.source"),
				get("SET_MN_1_MN_2_TO_OFF_MN_3_MN_4_TO_OFF.tree")
			);
	}
	
	@Test public void SET_COND_1_TO_TRUE() {
		helper.compileAndVerify(
				get("SET_COND_1_TO_TRUE.source"),
				get("SET_COND_1_TO_TRUE.tree")
			);
	}
	
	@Test public void SET_COND_1_COND_2_TO_TRUE() {
		helper.compileAndVerify(
				get("SET_COND_1_COND_2_TO_TRUE.source"),
				get("SET_COND_1_COND_2_TO_TRUE.tree")
			);
	}
	
	@Test public void SET_ID_1_TO_ADDRESS_OF_ID_2() {
		helper.compileAndVerify(
				get("SET_ID_1_TO_ADDRESS_OF_ID_2.source"),
				get("SET_ID_1_TO_ADDRESS_OF_ID_2.tree")
			);
	}
	
	@Test public void SET_ID_1_TO_NULL() {
		helper.compileAndVerify(
				get("SET_ID_1_TO_NULL.source"),
				get("SET_ID_1_TO_NULL.tree")
			);
	}
	
	@Test public void SET_ID_1_TO_NULLS() {
		helper.compileAndVerify(
				get("SET_ID_1_TO_NULLS.source"),
				get("SET_ID_1_TO_NULLS.tree")
			);
	}
	
	@Test public void SET_ADDRESS_OF_ID_1_TO_ID_2() {
		helper.compileAndVerify(
				get("SET_ADDRESS_OF_ID_1_TO_ID_2.source"),
				get("SET_ADDRESS_OF_ID_1_TO_ID_2.tree")
			);
	}
	
	@Test public void SET_ADDRESS_OF_ID_1_TO_ADDRESS_OF_ID_2() {
		helper.compileAndVerify(
				get("SET_ADDRESS_OF_ID_1_TO_ADDRESS_OF_ID_2.source"),
				get("SET_ADDRESS_OF_ID_1_TO_ADDRESS_OF_ID_2.tree")
			);
	}
	
	@Test public void SET_ADDRESS_OF_ID_1_TO_NULL() {
		helper.compileAndVerify(
				get("SET_ADDRESS_OF_ID_1_TO_NULL.source"),
				get("SET_ADDRESS_OF_ID_1_TO_NULL.tree")
			);
	}
	
	@Test public void SET_ADDRESS_OF_ID_1_TO_NULLS() {
		helper.compileAndVerify(
				get("SET_ADDRESS_OF_ID_1_TO_NULLS.source"),
				get("SET_ADDRESS_OF_ID_1_TO_NULLS.tree")
			);
	}
	
	@Test public void SET_ID_1_ID_3_TO_ADDRESS_OF_ID_2() {
		helper.compileAndVerify(
				get("SET_ID_1_ID_3_TO_ADDRESS_OF_ID_2.source"),
				get("SET_ID_1_ID_3_TO_ADDRESS_OF_ID_2.tree")
			);
	}
	
	@Test public void SET_ID_1_ID_3_TO_NULL() {
		helper.compileAndVerify(
				get("SET_ID_1_ID_3_TO_NULL.source"),
				get("SET_ID_1_ID_3_TO_NULL.tree")
			);
	}
	
	@Test public void SET_ID_1_ID_3_TO_NULLS() {
		helper.compileAndVerify(
				get("SET_ID_1_ID_3_TO_NULLS.source"),
				get("SET_ID_1_ID_3_TO_NULLS.tree")
			);
	}
	
	@Test public void SET_ADDRESS_OF_ID_1_ID_3_TO_ID_2() {
		helper.compileAndVerify(
				get("SET_ADDRESS_OF_ID_1_ID_3_TO_ID_2.source"),
				get("SET_ADDRESS_OF_ID_1_ID_3_TO_ID_2.tree")
			);
	}
	
	@Test public void SET_ADDRESS_OF_ID_1_ID_3_TO_ADDRESS_OF_ID_2() {
		helper.compileAndVerify(
				get("SET_ADDRESS_OF_ID_1_ID_3_TO_ADDRESS_OF_ID_2.source"),
				get("SET_ADDRESS_OF_ID_1_ID_3_TO_ADDRESS_OF_ID_2.tree")
			);
	}
	
	@Test public void SET_ADDRESS_OF_ID_1_ID_3_TO_NULL() {
		helper.compileAndVerify(
				get("SET_ADDRESS_OF_ID_1_ID_3_TO_NULL.source"),
				get("SET_ADDRESS_OF_ID_1_ID_3_TO_NULL.tree")
			);
	}
	
	@Test public void SET_ADDRESS_OF_ID_1_ID_3_TO_NULLS() {
		helper.compileAndVerify(
				get("SET_ADDRESS_OF_ID_1_ID_3_TO_NULLS.source"),
				get("SET_ADDRESS_OF_ID_1_ID_3_TO_NULLS.tree")
			);
	}
	
	@Test public void SET_ID_1_ADDRESS_OF_ID_3_TO_ID_2() {
		helper.compileAndVerify(
				get("SET_ID_1_ADDRESS_OF_ID_3_TO_ID_2.source"),
				get("SET_ID_1_ADDRESS_OF_ID_3_TO_ID_2.tree")
			);
	}
	
	@Test public void SET_ID_1_ADDRESS_OF_ID_3_TO_ADDRESS_OF_ID_2() {
		helper.compileAndVerify(
				get("SET_ID_1_ADDRESS_OF_ID_3_TO_ADDRESS_OF_ID_2.source"),
				get("SET_ID_1_ADDRESS_OF_ID_3_TO_ADDRESS_OF_ID_2.tree")
			);
	}
	
	@Test public void SET_ID_1_ADDRESS_OF_ID_3_TO_NULL() {
		helper.compileAndVerify(
				get("SET_ID_1_ADDRESS_OF_ID_3_TO_NULL.source"),
				get("SET_ID_1_ADDRESS_OF_ID_3_TO_NULL.tree")
			);
	}
	
	@Test public void SET_ID_1_ADDRESS_OF_ID_3_TO_NULLS() {
		helper.compileAndVerify(
				get("SET_ID_1_ADDRESS_OF_ID_3_TO_NULLS.source"),
				get("SET_ID_1_ADDRESS_OF_ID_3_TO_NULLS.tree")
			);
	}
	
	@Test public void SET_ADDRESS_OF_ID_1_ADDRESS_OF_ID_3_TO_ID_2() {
		helper.compileAndVerify(
				get("SET_ADDRESS_OF_ID_1_ADDRESS_OF_ID_3_TO_ID_2.source"),
				get("SET_ADDRESS_OF_ID_1_ADDRESS_OF_ID_3_TO_ID_2.tree")
			);
	}
	
	@Test public void SET_ADDRESS_OF_ID_1_ADDRESS_OF_ID_3_TO_ADDRESS_OF_ID_2() {
		helper.compileAndVerify(
				get("SET_ADDRESS_OF_ID_1_ADDRESS_OF_ID_3_TO_ADDRESS_OF_ID_2.source"),
				get("SET_ADDRESS_OF_ID_1_ADDRESS_OF_ID_3_TO_ADDRESS_OF_ID_2.tree")
			);
	}
	
	@Test public void SET_ADDRESS_OF_ID_1_ADDRESS_OF_ID_3_TO_NULL() {
		helper.compileAndVerify(
				get("SET_ADDRESS_OF_ID_1_ADDRESS_OF_ID_3_TO_NULL.source"),
				get("SET_ADDRESS_OF_ID_1_ADDRESS_OF_ID_3_TO_NULL.tree")
			);
	}
	
	@Test public void SET_ADDRESS_OF_ID_1_ADDRESS_OF_ID_3_TO_NULLS() {
		helper.compileAndVerify(
				get("SET_ADDRESS_OF_ID_1_ADDRESS_OF_ID_3_TO_NULLS.source"),
				get("SET_ADDRESS_OF_ID_1_ADDRESS_OF_ID_3_TO_NULLS.tree")
			);
	}
}
