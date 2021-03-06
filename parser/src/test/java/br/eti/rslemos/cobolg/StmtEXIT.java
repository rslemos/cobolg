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

import br.eti.rslemos.cobolg.COBOLParser.StmtEXITContext;

public class StmtEXIT {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtEXIT");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtEXITContext> helper = new CompilerHelper<StmtEXITContext>() {
		@Override protected StmtEXITContext parsePart() { return parser.stmtEXIT(); }
	};
	
	@Ignore
	@Test public void EXIT() {
		helper.compileAndVerify(
				get("EXIT.source"),
				get("EXIT.tree")
			);
	}
	
	@Test public void EXIT_PROGRAM() {
		helper.compileAndVerify(
				get("EXIT_PROGRAM.source"),
				get("EXIT_PROGRAM.tree")
			);
	}
	
	@Test public void EXIT_METHOD() {
		helper.compileAndVerify(
				get("EXIT_METHOD.source"),
				get("EXIT_METHOD.tree")
			);
	}

	@Test public void EXIT_PERFORM() {
		helper.compileAndVerify(
				get("EXIT_PERFORM.source"),
				get("EXIT_PERFORM.tree")
			);
	}
	
	@Test public void EXIT_PERFORM_CYCLE() {
		helper.compileAndVerify(
				get("EXIT_PERFORM_CYCLE.source"),
				get("EXIT_PERFORM_CYCLE.tree")
			);
	}
	
	@Test public void EXIT_PARAGRAPH() {
		helper.compileAndVerify(
				get("EXIT_PARAGRAPH.source"),
				get("EXIT_PARAGRAPH.tree")
			);
	}
	
	@Test public void EXIT_SECTION() {
		helper.compileAndVerify(
				get("EXIT_SECTION.source"),
				get("EXIT_SECTION.tree")
			);
	}
}
