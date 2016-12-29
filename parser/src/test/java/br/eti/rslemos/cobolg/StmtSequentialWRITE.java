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

import br.eti.rslemos.cobolg.COBOLParser.ProceduralStatementContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

public class StmtSequentialWRITE {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtSequentialWRITE");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<ProceduralStatementContext> helper = new CompilerHelper<ProceduralStatementContext>() {
		@Override protected ProceduralStatementContext parsePart() { return parser.proceduralStatement(); }
	};
	
	@Ignore
	@Test public void WRITE_RECNAME() {
		helper.compileAndVerify(
				get("WRITE_RECNAME.source"),
				get("WRITE_RECNAME.tree")
			);
	}
	
	@Ignore
	@Test public void WRITE_RECNAME_FROM_X() {
		helper.compileAndVerify(
				get("WRITE_RECNAME_FROM_X.source"),
				get("WRITE_RECNAME_FROM_X.tree")
			);
	}

	@Test public void WRITE_RECNAME_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN() {
		helper.compileAndVerify(
				get("WRITE_RECNAME_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.source"),
				get("WRITE_RECNAME_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.tree")
			);
	}
	
	@Test public void WRITE_RECNAME_FROM_X_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN() {
		helper.compileAndVerify(
				get("WRITE_RECNAME_FROM_X_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.source"),
				get("WRITE_RECNAME_FROM_X_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN.tree")
			);
	}
	
	@Ignore
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_RECNAME_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_RECNAME_END_WRITE.source"),
				get("WRITE_RECNAME_END_WRITE.tree")
			);
	}
	
	@Ignore
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void WRITE_RECNAME_FROM_X_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_RECNAME_FROM_X_END_WRITE.source"),
				get("WRITE_RECNAME_FROM_X_END_WRITE.tree")
			);
	}

	@Test public void WRITE_RECNAME_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_RECNAME_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_WRITE.source"),
				get("WRITE_RECNAME_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_WRITE.tree")
			);
	}
	
	@Test public void WRITE_RECNAME_FROM_X_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_WRITE() {
		helper.compileAndVerify(
				get("WRITE_RECNAME_FROM_X_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_WRITE.source"),
				get("WRITE_RECNAME_FROM_X_INVALID_STOP_RUN_NOT_INVALID_STOP_RUN_END_WRITE.tree")
			);
	}
}
