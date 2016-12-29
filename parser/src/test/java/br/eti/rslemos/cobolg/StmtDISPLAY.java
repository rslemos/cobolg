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

import br.eti.rslemos.cobolg.COBOLParser.StmtDISPLAYContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
public class StmtDISPLAY {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtDISPLAY");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtDISPLAYContext> helper = new CompilerHelper<StmtDISPLAYContext>() {
		@Override protected StmtDISPLAYContext parsePart() { return parser.stmtDISPLAY(); }
	};
	
	@Test public void DISPLAY_QUOTED_HELLO_WORLD() {
		helper.compileAndVerify(
				get("DISPLAY_QUOTED_HELLO_WORLD.source"),
				get("DISPLAY_QUOTED_HELLO_WORLD.tree")
			);
	}
	
	@Test public void DISPLAY_HELLOWORLD() {
		helper.compileAndVerify(
				get("DISPLAY_HELLOWORLD.source"),
				get("DISPLAY_HELLOWORLD.tree")
			);
	}
	
	@Test public void DISPLAY_HELLO_WORLD() {
		helper.compileAndVerify(
				get("DISPLAY_HELLO_WORLD.source"),
				get("DISPLAY_HELLO_WORLD.tree")
			);
	}
	
	@Test public void DISPLAY_HELLO_WORLD_QUOTED_EXMARK() {
		helper.compileAndVerify(
				get("DISPLAY_HELLO_WORLD_QUOTED_EXMARK.source"),
				get("DISPLAY_HELLO_WORLD_QUOTED_EXMARK.tree")
			);
	}
	
	@Test public void DISPLAY_QUOTED_HELLO_WORLD_UPON_LCP_CH2() {
		helper.compileAndVerify(
				get("DISPLAY_QUOTED_HELLO_WORLD_UPON_LCP_CH2.source"),
				get("DISPLAY_QUOTED_HELLO_WORLD_UPON_LCP_CH2.tree")
			);
	}
	
	@Test public void DISPLAY_HELLOWORLD_UPON_LCP_CH2() {
		helper.compileAndVerify(
				get("DISPLAY_HELLOWORLD_UPON_LCP_CH2.source"),
				get("DISPLAY_HELLOWORLD_UPON_LCP_CH2.tree")
			);
	}
	
	@Test public void DISPLAY_HELLO_WORLD_UPON_LCP_CH2() {
		helper.compileAndVerify(
				get("DISPLAY_HELLO_WORLD_UPON_LCP_CH2.source"),
				get("DISPLAY_HELLO_WORLD_UPON_LCP_CH2.tree")
			);
	}
	
	@Test public void DISPLAY_HELLO_WORLD_QUOTED_EXMARK_UPON_LCP_CH2() {
		helper.compileAndVerify(
				get("DISPLAY_HELLO_WORLD_QUOTED_EXMARK_UPON_LCP_CH2.source"),
				get("DISPLAY_HELLO_WORLD_QUOTED_EXMARK_UPON_LCP_CH2.tree")
			);
	}
	
	@Test public void DISPLAY_QUOTED_HELLO_WORLD_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_QUOTED_HELLO_WORLD_NO_ADVANCING.source"),
				get("DISPLAY_QUOTED_HELLO_WORLD_NO_ADVANCING.tree")
			);
	}
	
	@Test public void DISPLAY_HELLOWORLD_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_HELLOWORLD_NO_ADVANCING.source"),
				get("DISPLAY_HELLOWORLD_NO_ADVANCING.tree")
			);
	}
	
	@Test public void DISPLAY_HELLO_WORLD_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_HELLO_WORLD_NO_ADVANCING.source"),
				get("DISPLAY_HELLO_WORLD_NO_ADVANCING.tree")
			);
	}
	
	@Test public void DISPLAY_HELLO_WORLD_QUOTED_EXMARK_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_HELLO_WORLD_QUOTED_EXMARK_NO_ADVANCING.source"),
				get("DISPLAY_HELLO_WORLD_QUOTED_EXMARK_NO_ADVANCING.tree")
			);
	}
	
	@Test public void DISPLAY_QUOTED_HELLO_WORLD_UPON_LCP_CH2_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_QUOTED_HELLO_WORLD_UPON_LCP_CH2_NO_ADVANCING.source"),
				get("DISPLAY_QUOTED_HELLO_WORLD_UPON_LCP_CH2_NO_ADVANCING.tree")
			);
	}
	
	@Test public void DISPLAY_HELLOWORLD_UPON_LCP_CH2_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_HELLOWORLD_UPON_LCP_CH2_NO_ADVANCING.source"),
				get("DISPLAY_HELLOWORLD_UPON_LCP_CH2_NO_ADVANCING.tree")
			);
	}
	
	@Test public void DISPLAY_HELLO_WORLD_UPON_LCP_CH2_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_HELLO_WORLD_UPON_LCP_CH2_NO_ADVANCING.source"),
				get("DISPLAY_HELLO_WORLD_UPON_LCP_CH2_NO_ADVANCING.tree")
			);
	}
	
	@Test public void DISPLAY_HELLO_WORLD_QUOTED_EXMARK_UPON_LCP_CH2_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_HELLO_WORLD_QUOTED_EXMARK_UPON_LCP_CH2_NO_ADVANCING.source"),
				get("DISPLAY_HELLO_WORLD_QUOTED_EXMARK_UPON_LCP_CH2_NO_ADVANCING.tree")
			);
	}
	
	@Test public void DISPLAY_QUOTED_HELLO_WORLD_WITH_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_QUOTED_HELLO_WORLD_WITH_NO_ADVANCING.source"),
				get("DISPLAY_QUOTED_HELLO_WORLD_WITH_NO_ADVANCING.tree")
			);
	}
	
	@Test public void DISPLAY_HELLOWORLD_WITH_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_HELLOWORLD_WITH_NO_ADVANCING.source"),
				get("DISPLAY_HELLOWORLD_WITH_NO_ADVANCING.tree")
			);
	}
	
	@Test public void DISPLAY_HELLO_WORLD_WITH_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_HELLO_WORLD_WITH_NO_ADVANCING.source"),
				get("DISPLAY_HELLO_WORLD_WITH_NO_ADVANCING.tree")
			);
	}
	
	@Test public void DISPLAY_HELLO_WORLD_QUOTED_EXMARK_WITH_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_HELLO_WORLD_QUOTED_EXMARK_WITH_NO_ADVANCING.source"),
				get("DISPLAY_HELLO_WORLD_QUOTED_EXMARK_WITH_NO_ADVANCING.tree")
			);
	}
	
	@Test public void DISPLAY_QUOTED_HELLO_WORLD_UPON_LCP_CH2_WITH_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_QUOTED_HELLO_WORLD_UPON_LCP_CH2_WITH_NO_ADVANCING.source"),
				get("DISPLAY_QUOTED_HELLO_WORLD_UPON_LCP_CH2_WITH_NO_ADVANCING.tree")
			);
	}
	
	@Test public void DISPLAY_HELLOWORLD_UPON_LCP_CH2_WITH_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_HELLOWORLD_UPON_LCP_CH2_WITH_NO_ADVANCING.source"),
				get("DISPLAY_HELLOWORLD_UPON_LCP_CH2_WITH_NO_ADVANCING.tree")
			);
	}
	
	@Test public void DISPLAY_HELLO_WORLD_UPON_LCP_CH2_WITH_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_HELLO_WORLD_UPON_LCP_CH2_WITH_NO_ADVANCING.source"),
				get("DISPLAY_HELLO_WORLD_UPON_LCP_CH2_WITH_NO_ADVANCING.tree")
			);
	}
	
	@Test public void DISPLAY_HELLO_WORLD_QUOTED_EXMARK_UPON_LCP_CH2_WITH_NO_ADVANCING() {
		helper.compileAndVerify(
				get("DISPLAY_HELLO_WORLD_QUOTED_EXMARK_UPON_LCP_CH2_WITH_NO_ADVANCING.source"),
				get("DISPLAY_HELLO_WORLD_QUOTED_EXMARK_UPON_LCP_CH2_WITH_NO_ADVANCING.tree")
			);
	}
}
