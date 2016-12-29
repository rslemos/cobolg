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

import br.eti.rslemos.cobolg.COBOLParser.StmtCALLimperativeContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

public class StmtCALLimperative {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtCALLimperative");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtCALLimperativeContext> helper = new CompilerHelper<StmtCALLimperativeContext>() {
		@Override protected StmtCALLimperativeContext parsePart() { return parser.stmtCALLimperative(); }
	};
	
	@Test public void CALL_PROC_1() {
		helper.compileAndVerify(
				get("CALL_PROC_1.source"),
				get("CALL_PROC_1.tree")
			);
	}
	
	@Test public void CALL_QUOTED_PROC() {
		helper.compileAndVerify(
				get("CALL_QUOTED_PROC.source"),
				get("CALL_QUOTED_PROC.tree")
			);
	}
	
	@Test public void CALL_PROC_1_USING_BY_REFERENCE_ARG_1_BY_CONTENT_ARG_2_BY_VALUE_ARG_3() {
		helper.compileAndVerify(
				get("CALL_PROC_1_USING_BY_REFERENCE_ARG_1_BY_CONTENT_ARG_2_BY_VALUE_ARG_3.source"),
				get("CALL_PROC_1_USING_BY_REFERENCE_ARG_1_BY_CONTENT_ARG_2_BY_VALUE_ARG_3.tree")
			);
	}
	
	@Ignore
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void CALL_PROC_1_USING_VALUE_ADDRESS_OF_ARG_1_CONTENT_LENGTH_OF_ARG_2_ARG_3_ARG_4() {
		helper.compileAndVerify(
				get("CALL_PROC_1_USING_VALUE_ADDRESS_OF_ARG_1_CONTENT_LENGTH_OF_ARG_2_ARG_3_ARG_4.source"),
				get("CALL_PROC_1_USING_VALUE_ADDRESS_OF_ARG_1_CONTENT_LENGTH_OF_ARG_2_ARG_3_ARG_4.tree")
			);
	}
	
	@Test public void CALL_QUOTED_PROC_USING_BY_CONTENT_10_BY_VALUE_20_30_40() {
		helper.compileAndVerify(
				get("CALL_QUOTED_PROC_USING_BY_CONTENT_10_BY_VALUE_20_30_40.source"),
				get("CALL_QUOTED_PROC_USING_BY_CONTENT_10_BY_VALUE_20_30_40.tree")
			);
	}
	
	@Test public void CALL_QUOTED_PROC_USING_BY_CONTENT_OMITTED_BY_REFERENCE_OMITTED_BY_VALUE_LENGTH_OF_ARG_3_40() {
		helper.compileAndVerify(
				get("CALL_QUOTED_PROC_USING_BY_CONTENT_OMITTED_BY_REFERENCE_OMITTED_BY_VALUE_LENGTH_OF_ARG_3_40.source"),
				get("CALL_QUOTED_PROC_USING_BY_CONTENT_OMITTED_BY_REFERENCE_OMITTED_BY_VALUE_LENGTH_OF_ARG_3_40.tree")
			);
	}
	
	@Test public void CALL_PROC_1_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("CALL_PROC_1_RETURNING_RESULT.source"),
				get("CALL_PROC_1_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void CALL_QUOTED_PROC_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("CALL_QUOTED_PROC_RETURNING_RESULT.source"),
				get("CALL_QUOTED_PROC_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void CALL_PROC_1_USING_BY_CONTENT_ADDRESS_OF_ARG_1_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("CALL_PROC_1_USING_BY_CONTENT_ADDRESS_OF_ARG_1_RETURNING_RESULT.source"),
				get("CALL_PROC_1_USING_BY_CONTENT_ADDRESS_OF_ARG_1_RETURNING_RESULT.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void CALL_PROC_1_USING_ADDRESS_OF_ARG_1_ARG_2_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("CALL_PROC_1_USING_ADDRESS_OF_ARG_1_ARG_2_RETURNING_RESULT.source"),
				get("CALL_PROC_1_USING_ADDRESS_OF_ARG_1_ARG_2_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void CALL_QUOTED_PROC_USING_VALUE_10_20_30_40_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("CALL_QUOTED_PROC_USING_VALUE_10_20_30_40_RETURNING_RESULT.source"),
				get("CALL_QUOTED_PROC_USING_VALUE_10_20_30_40_RETURNING_RESULT.tree")
			);
	}
	
	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void CALL_QUOTED_PROC_USING_OMITTED_ARG_2_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("CALL_QUOTED_PROC_USING_OMITTED_ARG_2_RETURNING_RESULT.source"),
				get("CALL_QUOTED_PROC_USING_OMITTED_ARG_2_RETURNING_RESULT.tree")
			);
	}
}
