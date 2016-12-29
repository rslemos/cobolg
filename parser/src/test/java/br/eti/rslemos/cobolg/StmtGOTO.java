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
 * but WITHOUT GOTO WARRANTY; without even the implied warranty of
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

import br.eti.rslemos.cobolg.COBOLParser.StmtGOTOContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
public class StmtGOTO {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtGOTO");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtGOTOContext> helper = new CompilerHelper<StmtGOTOContext>() {
		@Override protected StmtGOTOContext parsePart() { return parser.stmtGOTO(); }
	};
	
	@Test public void GO() {
		helper.compileAndVerify(
				get("GO.source"),
				get("GO.tree")
			);
	}

	@Test public void GO_PROC_1() {
		helper.compileAndVerify(
				get("GO_PROC_1.source"),
				get("GO_PROC_1.tree")
			);
	}

	@Test public void GO_PROC_1_PROC_2_DEPENDING_ID_1() {
		helper.compileAndVerify(
				get("GO_PROC_1_PROC_2_DEPENDING_ID_1.source"),
				get("GO_PROC_1_PROC_2_DEPENDING_ID_1.tree")
			);
	}

	@Test public void GO_TO() {
		helper.compileAndVerify(
				get("GO_TO.source"),
				get("GO_TO.tree")
			);
	}

	@Test public void GO_TO_PROC_1() {
		helper.compileAndVerify(
				get("GO_TO_PROC_1.source"),
				get("GO_TO_PROC_1.tree")
			);
	}

	@Test public void GO_TO_PROC_1_PROC_2_DEPENDING_ON_ID_1() {
		helper.compileAndVerify(
				get("GO_TO_PROC_1_PROC_2_DEPENDING_ON_ID_1.source"),
				get("GO_TO_PROC_1_PROC_2_DEPENDING_ON_ID_1.tree")
			);
	}
}
