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

import br.eti.rslemos.cobolg.COBOLParser.StmtMULTIPLYdelimitedScopeContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

public class StmtMULTIPLYdelimitedScope {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtMULTIPLYdelimitedScope");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtMULTIPLYdelimitedScopeContext> helper = new CompilerHelper<StmtMULTIPLYdelimitedScopeContext>() {
		@Override protected StmtMULTIPLYdelimitedScopeContext parsePart() { return parser.stmtMULTIPLYdelimitedScope(); }
	};
	
	@Test public void MULTIPLY_X_BY_Y_ROUNDED_W_END_MULTIPLY() {
		helper.compileAndVerify(
				get("MULTIPLY_X_BY_Y_ROUNDED_W_END_MULTIPLY.source"),
				get("MULTIPLY_X_BY_Y_ROUNDED_W_END_MULTIPLY.tree")
			);
	}
	
	@Test public void MULTIPLY_20_BY_Y_ROUNDED_W_END_MULTIPLY() {
		helper.compileAndVerify(
				get("MULTIPLY_20_BY_Y_ROUNDED_W_END_MULTIPLY.source"),
				get("MULTIPLY_20_BY_Y_ROUNDED_W_END_MULTIPLY.tree")
			);
	}
	
	@Test public void MULTIPLY_X_BY_Z_GIVING_Y_ROUNDED_W_END_MULTIPLY() {
		helper.compileAndVerify(
				get("MULTIPLY_X_BY_Z_GIVING_Y_ROUNDED_W_END_MULTIPLY.source"),
				get("MULTIPLY_X_BY_Z_GIVING_Y_ROUNDED_W_END_MULTIPLY.tree")
			);
	}
	
	@Test public void MULTIPLY_X_BY_10_GIVING_Y_ROUNDED_W_END_MULTIPLY() {
		helper.compileAndVerify(
				get("MULTIPLY_X_BY_10_GIVING_Y_ROUNDED_W_END_MULTIPLY.source"),
				get("MULTIPLY_X_BY_10_GIVING_Y_ROUNDED_W_END_MULTIPLY.tree")
			);
	}
	
	@Test public void MULTIPLY_20_BY_Z_GIVING_Y_ROUNDED_W_END_MULTIPLY() {
		helper.compileAndVerify(
				get("MULTIPLY_20_BY_Z_GIVING_Y_ROUNDED_W_END_MULTIPLY.source"),
				get("MULTIPLY_20_BY_Z_GIVING_Y_ROUNDED_W_END_MULTIPLY.tree")
			);
	}
	
	@Test public void MULTIPLY_20_BY_10_GIVING_Y_ROUNDED_W_END_MULTIPLY() {
		helper.compileAndVerify(
				get("MULTIPLY_20_BY_10_GIVING_Y_ROUNDED_W_END_MULTIPLY.source"),
				get("MULTIPLY_20_BY_10_GIVING_Y_ROUNDED_W_END_MULTIPLY.tree")
			);
	}
	
	@Test public void MULTIPLY_X_BY_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY() {
		helper.compileAndVerify(
				get("MULTIPLY_X_BY_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY.source"),
				get("MULTIPLY_X_BY_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY.tree")
			);
	}
	
	@Test public void MULTIPLY_20_BY_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY() {
		helper.compileAndVerify(
				get("MULTIPLY_20_BY_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY.source"),
				get("MULTIPLY_20_BY_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY.tree")
			);
	}
	
	@Test public void MULTIPLY_X_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY() {
		helper.compileAndVerify(
				get("MULTIPLY_X_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY.source"),
				get("MULTIPLY_X_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY.tree")
			);
	}
	
	@Test public void MULTIPLY_X_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY() {
		helper.compileAndVerify(
				get("MULTIPLY_X_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY.source"),
				get("MULTIPLY_X_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY.tree")
			);
	}
	
	@Test public void MULTIPLY_20_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY() {
		helper.compileAndVerify(
				get("MULTIPLY_20_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY.source"),
				get("MULTIPLY_20_BY_Z_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY.tree")
			);
	}
	
	@Test public void MULTIPLY_20_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY() {
		helper.compileAndVerify(
				get("MULTIPLY_20_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY.source"),
				get("MULTIPLY_20_BY_10_GIVING_Y_ROUNDED_W_ON_SIZE_ERROR_STOP_RUN_END_MULTIPLY.tree")
			);
	}
}
