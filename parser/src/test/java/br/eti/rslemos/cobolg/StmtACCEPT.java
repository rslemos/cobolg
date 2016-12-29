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

import br.eti.rslemos.cobolg.COBOLParser.StmtACCEPTContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

public class StmtACCEPT {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtACCEPT");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtACCEPTContext> helper = new CompilerHelper<StmtACCEPTContext>() {
		@Override protected StmtACCEPTContext parsePart() { return parser.stmtACCEPT(); }
	};
	
	@Test public void ACCEPT_PARAM() {
		helper.compileAndVerify(
				get("ACCEPT_PARAM.source"),
				get("ACCEPT_PARAM.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void ACCEPT_PARAM_FROM_SOURCE() {
		helper.compileAndVerify(
				get("ACCEPT_PARAM_FROM_SOURCE.source"),
				get("ACCEPT_PARAM_FROM_SOURCE.tree")
			);
	}
	
	@Test public void ACCEPT_ST_FROM_DATE() {
		helper.compileAndVerify(
				get("ACCEPT_ST_FROM_DATE.source"),
				get("ACCEPT_ST_FROM_DATE.tree")
			);
	}
	
	@Test public void ACCEPT_ST_FROM_DATE_YYYYMMDD() {
		helper.compileAndVerify(
				get("ACCEPT_ST_FROM_DATE_YYYYMMDD.source"),
				get("ACCEPT_ST_FROM_DATE_YYYYMMDD.tree")
			);
	}
	
	@Test public void ACCEPT_ST_FROM_DAY() {
		helper.compileAndVerify(
				get("ACCEPT_ST_FROM_DAY.source"),
				get("ACCEPT_ST_FROM_DAY.tree")
			);
	}
	
	@Test public void ACCEPT_ST_FROM_DAY_YYYYDDD() {
		helper.compileAndVerify(
				get("ACCEPT_ST_FROM_DAY_YYYYDDD.source"),
				get("ACCEPT_ST_FROM_DAY_YYYYDDD.tree")
			);
	}
	
	@Test public void ACCEPT_ST_FROM_DAY_OF_WEEK() {
		helper.compileAndVerify(
				get("ACCEPT_ST_FROM_DAY_OF_WEEK.source"),
				get("ACCEPT_ST_FROM_DAY_OF_WEEK.tree")
			);
	}
	
	@Test public void ACCEPT_ST_FROM_TIME() {
		helper.compileAndVerify(
				get("ACCEPT_ST_FROM_TIME.source"),
				get("ACCEPT_ST_FROM_TIME.tree")
			);
	}
}
