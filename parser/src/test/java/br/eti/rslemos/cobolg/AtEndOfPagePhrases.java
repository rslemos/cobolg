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
 * END_OF_PAGE COPYRIGHT NOTICE
 ******************************************************************************/
package br.eti.rslemos.cobolg;

import java.util.ResourceBundle;

import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.AtEndOfPagePhrasesContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

public class AtEndOfPagePhrases {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.atEndOfPagePhrases");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<AtEndOfPagePhrasesContext> helper = new CompilerHelper<AtEndOfPagePhrasesContext>() {
		@Override protected AtEndOfPagePhrasesContext parsePart() { return parser.atEndOfPagePhrases(); }
	};
	
	@Test public void $() {
		helper.compileAndVerify(
				get("$.source"),
				get("$.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void AT_END_OF_PAGE_STOP_RUN() {
		helper.compileAndVerify(
				get("AT_END_OF_PAGE_STOP_RUN.source"),
				get("AT_END_OF_PAGE_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void NOT_AT_END_OF_PAGE_STOP_RUN() {
		helper.compileAndVerify(
				get("NOT_AT_END_OF_PAGE_STOP_RUN.source"),
				get("NOT_AT_END_OF_PAGE_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void AT_END_OF_PAGE_STOP_RUN_NOT_AT_END_OF_PAGE_STOP_RUN() {
		helper.compileAndVerify(
				get("AT_END_OF_PAGE_STOP_RUN_NOT_AT_END_OF_PAGE_STOP_RUN.source"),
				get("AT_END_OF_PAGE_STOP_RUN_NOT_AT_END_OF_PAGE_STOP_RUN.tree")
			);
	}
	
	@Test public void END_OF_PAGE_STOP_RUN() {
		helper.compileAndVerify(
				get("END_OF_PAGE_STOP_RUN.source"),
				get("END_OF_PAGE_STOP_RUN.tree")
			);
	}
	
	@Test public void NOT_END_OF_PAGE_STOP_RUN() {
		helper.compileAndVerify(
				get("NOT_END_OF_PAGE_STOP_RUN.source"),
				get("NOT_END_OF_PAGE_STOP_RUN.tree")
			);
	}
	
	@Test public void END_OF_PAGE_STOP_RUN_NOT_END_OF_PAGE_STOP_RUN() {
		helper.compileAndVerify(
				get("END_OF_PAGE_STOP_RUN_NOT_END_OF_PAGE_STOP_RUN.source"),
				get("END_OF_PAGE_STOP_RUN_NOT_END_OF_PAGE_STOP_RUN.tree")
			);
	}
	
	@Test public void AT_EOP_STOP_RUN() {
		helper.compileAndVerify(
				get("AT_EOP_STOP_RUN.source"),
				get("AT_EOP_STOP_RUN.tree")
			);
	}
	
	@Test public void NOT_AT_EOP_STOP_RUN() {
		helper.compileAndVerify(
				get("NOT_AT_EOP_STOP_RUN.source"),
				get("NOT_AT_EOP_STOP_RUN.tree")
			);
	}
	
	@Test public void AT_EOP_STOP_RUN_NOT_AT_EOP_STOP_RUN() {
		helper.compileAndVerify(
				get("AT_EOP_STOP_RUN_NOT_AT_EOP_STOP_RUN.source"),
				get("AT_EOP_STOP_RUN_NOT_AT_EOP_STOP_RUN.tree")
			);
	}
	
	@Test public void EOP_STOP_RUN() {
		helper.compileAndVerify(
				get("EOP_STOP_RUN.source"),
				get("EOP_STOP_RUN.tree")
			);
	}
	
	@Test public void NOT_EOP_STOP_RUN() {
		helper.compileAndVerify(
				get("NOT_EOP_STOP_RUN.source"),
				get("NOT_EOP_STOP_RUN.tree")
			);
	}
	
	@Test public void EOP_STOP_RUN_NOT_EOP_STOP_RUN() {
		helper.compileAndVerify(
				get("EOP_STOP_RUN_NOT_EOP_STOP_RUN.source"),
				get("EOP_STOP_RUN_NOT_EOP_STOP_RUN.tree")
			);
	}
}
