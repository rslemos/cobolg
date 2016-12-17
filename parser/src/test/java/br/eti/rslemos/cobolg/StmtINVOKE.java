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

import br.eti.rslemos.cobolg.COBOLParser.ProceduralStatementContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
public class StmtINVOKE {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtINVOKE");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<ProceduralStatementContext> helper = new CompilerHelper<ProceduralStatementContext>() {
		@Override protected ProceduralStatementContext parsePart() { return parser.proceduralStatement(); }
	};
	
	@Test public void INVOKE_CLS_1_QUOTED_METHOD() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_QUOTED_METHOD.source"),
				get("INVOKE_CLS_1_QUOTED_METHOD.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_METHOD_1() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_METHOD_1.source"),
				get("INVOKE_CLS_1_METHOD_1.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_NEW() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_NEW.source"),
				get("INVOKE_CLS_1_NEW.tree")
			);
	}
	
	@Test public void INVOKE_SELF_QUOTED_METHOD() {
		helper.compileAndVerify(
				get("INVOKE_SELF_QUOTED_METHOD.source"),
				get("INVOKE_SELF_QUOTED_METHOD.tree")
			);
	}
	
	@Test public void INVOKE_SELF_METHOD_1() {
		helper.compileAndVerify(
				get("INVOKE_SELF_METHOD_1.source"),
				get("INVOKE_SELF_METHOD_1.tree")
			);
	}
	
	@Test public void INVOKE_SELF_NEW() {
		helper.compileAndVerify(
				get("INVOKE_SELF_NEW.source"),
				get("INVOKE_SELF_NEW.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_QUOTED_METHOD() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_QUOTED_METHOD.source"),
				get("INVOKE_SUPER_QUOTED_METHOD.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_METHOD_1() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_METHOD_1.source"),
				get("INVOKE_SUPER_METHOD_1.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_NEW() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_NEW.source"),
				get("INVOKE_SUPER_NEW.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_QUOTED_METHOD_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_QUOTED_METHOD_RETURNING_RESULT.source"),
				get("INVOKE_CLS_1_QUOTED_METHOD_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_METHOD_1_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_METHOD_1_RETURNING_RESULT.source"),
				get("INVOKE_CLS_1_METHOD_1_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_NEW_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_NEW_RETURNING_RESULT.source"),
				get("INVOKE_CLS_1_NEW_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SELF_QUOTED_METHOD_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_SELF_QUOTED_METHOD_RETURNING_RESULT.source"),
				get("INVOKE_SELF_QUOTED_METHOD_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SELF_METHOD_1_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_SELF_METHOD_1_RETURNING_RESULT.source"),
				get("INVOKE_SELF_METHOD_1_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SELF_NEW_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_SELF_NEW_RETURNING_RESULT.source"),
				get("INVOKE_SELF_NEW_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_QUOTED_METHOD_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_QUOTED_METHOD_RETURNING_RESULT.source"),
				get("INVOKE_SUPER_QUOTED_METHOD_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_METHOD_1_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_METHOD_1_RETURNING_RESULT.source"),
				get("INVOKE_SUPER_METHOD_1_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_NEW_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_NEW_RETURNING_RESULT.source"),
				get("INVOKE_SUPER_NEW_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_QUOTED_METHOD_USING_BY_VALUE_ARG_1_ARG_2_ARG_3_40() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_QUOTED_METHOD_USING_BY_VALUE_ARG_1_ARG_2_ARG_3_40.source"),
				get("INVOKE_CLS_1_QUOTED_METHOD_USING_BY_VALUE_ARG_1_ARG_2_ARG_3_40.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_METHOD_1_USING_BY_VALUE_ARG_1_BY_VALUE_20_BY_VALUE_ARG_3() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_METHOD_1_USING_BY_VALUE_ARG_1_BY_VALUE_20_BY_VALUE_ARG_3.source"),
				get("INVOKE_CLS_1_METHOD_1_USING_BY_VALUE_ARG_1_BY_VALUE_20_BY_VALUE_ARG_3.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_NEW_USING_VALUE_LENGTH_OF_ARG_1_20() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_NEW_USING_VALUE_LENGTH_OF_ARG_1_20.source"),
				get("INVOKE_CLS_1_NEW_USING_VALUE_LENGTH_OF_ARG_1_20.tree")
			);
	}
	
	@Test public void INVOKE_SELF_QUOTED_METHOD_USING_BY_VALUE_ARG_1_LENGTH_OF_ARG_2_VALUE_30() {
		helper.compileAndVerify(
				get("INVOKE_SELF_QUOTED_METHOD_USING_BY_VALUE_ARG_1_LENGTH_OF_ARG_2_VALUE_30.source"),
				get("INVOKE_SELF_QUOTED_METHOD_USING_BY_VALUE_ARG_1_LENGTH_OF_ARG_2_VALUE_30.tree")
			);
	}
	
	@Test public void INVOKE_SELF_METHOD_1_USING_BY_VALUE_ARG_1_ARG_2_LENGTH_OF_ARG_3_40() {
		helper.compileAndVerify(
				get("INVOKE_SELF_METHOD_1_USING_BY_VALUE_ARG_1_ARG_2_LENGTH_OF_ARG_3_40.source"),
				get("INVOKE_SELF_METHOD_1_USING_BY_VALUE_ARG_1_ARG_2_LENGTH_OF_ARG_3_40.tree")
			);
	}
	
	@Test public void INVOKE_SELF_NEW_USING_BY_VALUE_LENGTH_OF_ARG_1_LENGTH_OF_ARG_2_30() {
		helper.compileAndVerify(
				get("INVOKE_SELF_NEW_USING_BY_VALUE_LENGTH_OF_ARG_1_LENGTH_OF_ARG_2_30.source"),
				get("INVOKE_SELF_NEW_USING_BY_VALUE_LENGTH_OF_ARG_1_LENGTH_OF_ARG_2_30.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_QUOTED_METHOD_USING_BY_VALUE_ARG_1_BY_VALUE_LENGTH_OF_ARG_2() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_QUOTED_METHOD_USING_BY_VALUE_ARG_1_BY_VALUE_LENGTH_OF_ARG_2.source"),
				get("INVOKE_SUPER_QUOTED_METHOD_USING_BY_VALUE_ARG_1_BY_VALUE_LENGTH_OF_ARG_2.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_METHOD_1_USING_BY_VALUE_10_LENGTH_OF_ARG_2_VALUE_ARG_3_40() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_METHOD_1_USING_BY_VALUE_10_LENGTH_OF_ARG_2_VALUE_ARG_3_40.source"),
				get("INVOKE_SUPER_METHOD_1_USING_BY_VALUE_10_LENGTH_OF_ARG_2_VALUE_ARG_3_40.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_NEW_USING_BY_VALUE_LENGTH_OF_ARG_1_20_VALUE_LENGTH_OF_ARG_3_40() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_NEW_USING_BY_VALUE_LENGTH_OF_ARG_1_20_VALUE_LENGTH_OF_ARG_3_40.source"),
				get("INVOKE_SUPER_NEW_USING_BY_VALUE_LENGTH_OF_ARG_1_20_VALUE_LENGTH_OF_ARG_3_40.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_QUOTED_METHOD_USING_VALUE_10_VALUE_ARG_2_30_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_QUOTED_METHOD_USING_VALUE_10_VALUE_ARG_2_30_RETURNING_RESULT.source"),
				get("INVOKE_CLS_1_QUOTED_METHOD_USING_VALUE_10_VALUE_ARG_2_30_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_METHOD_1_USING_VALUE_10_ARG_2_VALUE_ARG_3_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_METHOD_1_USING_VALUE_10_ARG_2_VALUE_ARG_3_RETURNING_RESULT.source"),
				get("INVOKE_CLS_1_METHOD_1_USING_VALUE_10_ARG_2_VALUE_ARG_3_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_NEW_USING_VALUE_10_ARG_2_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_NEW_USING_VALUE_10_ARG_2_RETURNING_RESULT.source"),
				get("INVOKE_CLS_1_NEW_USING_VALUE_10_ARG_2_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SELF_QUOTED_METHOD_USING_VALUE_ARG_1_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_SELF_QUOTED_METHOD_USING_VALUE_ARG_1_RETURNING_RESULT.source"),
				get("INVOKE_SELF_QUOTED_METHOD_USING_VALUE_ARG_1_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SELF_METHOD_1_USING_VALUE_ARG_1_VALUE_20_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_SELF_METHOD_1_USING_VALUE_ARG_1_VALUE_20_RETURNING_RESULT.source"),
				get("INVOKE_SELF_METHOD_1_USING_VALUE_ARG_1_VALUE_20_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SELF_NEW_USING_VALUE_ARG_1_ARG_2_ARG_3_ARG_4_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_SELF_NEW_USING_VALUE_ARG_1_ARG_2_ARG_3_ARG_4_RETURNING_RESULT.source"),
				get("INVOKE_SELF_NEW_USING_VALUE_ARG_1_ARG_2_ARG_3_ARG_4_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_QUOTED_METHOD_USING_VALUE_10_20_VALUE_30_40_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_QUOTED_METHOD_USING_VALUE_10_20_VALUE_30_40_RETURNING_RESULT.source"),
				get("INVOKE_SUPER_QUOTED_METHOD_USING_VALUE_10_20_VALUE_30_40_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_METHOD_1_USING_VALUE_ARG_1_VALUE_ARG_2_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_METHOD_1_USING_VALUE_ARG_1_VALUE_ARG_2_RETURNING_RESULT.source"),
				get("INVOKE_SUPER_METHOD_1_USING_VALUE_ARG_1_VALUE_ARG_2_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_NEW_USING_VALUE_ARG_1_20_30_ARG_4_RETURNING_RESULT() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_NEW_USING_VALUE_ARG_1_20_30_ARG_4_RETURNING_RESULT.source"),
				get("INVOKE_SUPER_NEW_USING_VALUE_ARG_1_20_30_ARG_4_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_QUOTED_METHOD_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_QUOTED_METHOD.source"),
				get("INVOKE_CLS_1_QUOTED_METHOD.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_METHOD_1_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_METHOD_1.source"),
				get("INVOKE_CLS_1_METHOD_1.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_NEW_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_NEW.source"),
				get("INVOKE_CLS_1_NEW.tree")
			);
	}
	
	@Test public void INVOKE_SELF_QUOTED_METHOD_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SELF_QUOTED_METHOD.source"),
				get("INVOKE_SELF_QUOTED_METHOD.tree")
			);
	}
	
	@Test public void INVOKE_SELF_METHOD_1_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SELF_METHOD_1.source"),
				get("INVOKE_SELF_METHOD_1.tree")
			);
	}
	
	@Test public void INVOKE_SELF_NEW_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SELF_NEW.source"),
				get("INVOKE_SELF_NEW.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_QUOTED_METHOD_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_QUOTED_METHOD.source"),
				get("INVOKE_SUPER_QUOTED_METHOD.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_METHOD_1_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_METHOD_1.source"),
				get("INVOKE_SUPER_METHOD_1.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_NEW_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_NEW.source"),
				get("INVOKE_SUPER_NEW.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_QUOTED_METHOD_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_QUOTED_METHOD_RETURNING_RESULT.source"),
				get("INVOKE_CLS_1_QUOTED_METHOD_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_METHOD_1_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_METHOD_1_RETURNING_RESULT.source"),
				get("INVOKE_CLS_1_METHOD_1_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_NEW_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_NEW_RETURNING_RESULT.source"),
				get("INVOKE_CLS_1_NEW_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SELF_QUOTED_METHOD_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SELF_QUOTED_METHOD_RETURNING_RESULT.source"),
				get("INVOKE_SELF_QUOTED_METHOD_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SELF_METHOD_1_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SELF_METHOD_1_RETURNING_RESULT.source"),
				get("INVOKE_SELF_METHOD_1_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SELF_NEW_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SELF_NEW_RETURNING_RESULT.source"),
				get("INVOKE_SELF_NEW_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_QUOTED_METHOD_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_QUOTED_METHOD_RETURNING_RESULT.source"),
				get("INVOKE_SUPER_QUOTED_METHOD_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_METHOD_1_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_METHOD_1_RETURNING_RESULT.source"),
				get("INVOKE_SUPER_METHOD_1_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_NEW_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_NEW_RETURNING_RESULT.source"),
				get("INVOKE_SUPER_NEW_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_QUOTED_METHOD_USING_BY_VALUE_ARG_1_ARG_2_ARG_3_40_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_QUOTED_METHOD_USING_BY_VALUE_ARG_1_ARG_2_ARG_3_40.source"),
				get("INVOKE_CLS_1_QUOTED_METHOD_USING_BY_VALUE_ARG_1_ARG_2_ARG_3_40.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_METHOD_1_USING_BY_VALUE_ARG_1_BY_VALUE_20_BY_VALUE_ARG_3_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_METHOD_1_USING_BY_VALUE_ARG_1_BY_VALUE_20_BY_VALUE_ARG_3.source"),
				get("INVOKE_CLS_1_METHOD_1_USING_BY_VALUE_ARG_1_BY_VALUE_20_BY_VALUE_ARG_3.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_NEW_USING_VALUE_LENGTH_OF_ARG_1_20_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_NEW_USING_VALUE_LENGTH_OF_ARG_1_20.source"),
				get("INVOKE_CLS_1_NEW_USING_VALUE_LENGTH_OF_ARG_1_20.tree")
			);
	}
	
	@Test public void INVOKE_SELF_QUOTED_METHOD_USING_BY_VALUE_ARG_1_LENGTH_OF_ARG_2_VALUE_30_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SELF_QUOTED_METHOD_USING_BY_VALUE_ARG_1_LENGTH_OF_ARG_2_VALUE_30.source"),
				get("INVOKE_SELF_QUOTED_METHOD_USING_BY_VALUE_ARG_1_LENGTH_OF_ARG_2_VALUE_30.tree")
			);
	}
	
	@Test public void INVOKE_SELF_METHOD_1_USING_BY_VALUE_ARG_1_ARG_2_LENGTH_OF_ARG_3_40_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SELF_METHOD_1_USING_BY_VALUE_ARG_1_ARG_2_LENGTH_OF_ARG_3_40.source"),
				get("INVOKE_SELF_METHOD_1_USING_BY_VALUE_ARG_1_ARG_2_LENGTH_OF_ARG_3_40.tree")
			);
	}
	
	@Test public void INVOKE_SELF_NEW_USING_BY_VALUE_LENGTH_OF_ARG_1_LENGTH_OF_ARG_2_30_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SELF_NEW_USING_BY_VALUE_LENGTH_OF_ARG_1_LENGTH_OF_ARG_2_30.source"),
				get("INVOKE_SELF_NEW_USING_BY_VALUE_LENGTH_OF_ARG_1_LENGTH_OF_ARG_2_30.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_QUOTED_METHOD_USING_BY_VALUE_ARG_1_BY_VALUE_LENGTH_OF_ARG_2_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_QUOTED_METHOD_USING_BY_VALUE_ARG_1_BY_VALUE_LENGTH_OF_ARG_2.source"),
				get("INVOKE_SUPER_QUOTED_METHOD_USING_BY_VALUE_ARG_1_BY_VALUE_LENGTH_OF_ARG_2.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_METHOD_1_USING_BY_VALUE_10_LENGTH_OF_ARG_2_VALUE_ARG_3_40_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_METHOD_1_USING_BY_VALUE_10_LENGTH_OF_ARG_2_VALUE_ARG_3_40.source"),
				get("INVOKE_SUPER_METHOD_1_USING_BY_VALUE_10_LENGTH_OF_ARG_2_VALUE_ARG_3_40.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_NEW_USING_BY_VALUE_LENGTH_OF_ARG_1_20_VALUE_LENGTH_OF_ARG_3_40_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_NEW_USING_BY_VALUE_LENGTH_OF_ARG_1_20_VALUE_LENGTH_OF_ARG_3_40.source"),
				get("INVOKE_SUPER_NEW_USING_BY_VALUE_LENGTH_OF_ARG_1_20_VALUE_LENGTH_OF_ARG_3_40.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_QUOTED_METHOD_USING_VALUE_10_VALUE_ARG_2_30_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_QUOTED_METHOD_USING_VALUE_10_VALUE_ARG_2_30_RETURNING_RESULT.source"),
				get("INVOKE_CLS_1_QUOTED_METHOD_USING_VALUE_10_VALUE_ARG_2_30_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_METHOD_1_USING_VALUE_10_ARG_2_VALUE_ARG_3_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_METHOD_1_USING_VALUE_10_ARG_2_VALUE_ARG_3_RETURNING_RESULT.source"),
				get("INVOKE_CLS_1_METHOD_1_USING_VALUE_10_ARG_2_VALUE_ARG_3_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_CLS_1_NEW_USING_VALUE_10_ARG_2_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_CLS_1_NEW_USING_VALUE_10_ARG_2_RETURNING_RESULT.source"),
				get("INVOKE_CLS_1_NEW_USING_VALUE_10_ARG_2_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SELF_QUOTED_METHOD_USING_VALUE_ARG_1_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SELF_QUOTED_METHOD_USING_VALUE_ARG_1_RETURNING_RESULT.source"),
				get("INVOKE_SELF_QUOTED_METHOD_USING_VALUE_ARG_1_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SELF_METHOD_1_USING_VALUE_ARG_1_VALUE_20_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SELF_METHOD_1_USING_VALUE_ARG_1_VALUE_20_RETURNING_RESULT.source"),
				get("INVOKE_SELF_METHOD_1_USING_VALUE_ARG_1_VALUE_20_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SELF_NEW_USING_VALUE_ARG_1_ARG_2_ARG_3_ARG_4_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SELF_NEW_USING_VALUE_ARG_1_ARG_2_ARG_3_ARG_4_RETURNING_RESULT.source"),
				get("INVOKE_SELF_NEW_USING_VALUE_ARG_1_ARG_2_ARG_3_ARG_4_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_QUOTED_METHOD_USING_VALUE_10_20_VALUE_30_40_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_QUOTED_METHOD_USING_VALUE_10_20_VALUE_30_40_RETURNING_RESULT.source"),
				get("INVOKE_SUPER_QUOTED_METHOD_USING_VALUE_10_20_VALUE_30_40_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_METHOD_1_USING_VALUE_ARG_1_VALUE_ARG_2_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_METHOD_1_USING_VALUE_ARG_1_VALUE_ARG_2_RETURNING_RESULT.source"),
				get("INVOKE_SUPER_METHOD_1_USING_VALUE_ARG_1_VALUE_ARG_2_RETURNING_RESULT.tree")
			);
	}
	
	@Test public void INVOKE_SUPER_NEW_USING_VALUE_ARG_1_20_30_ARG_4_RETURNING_RESULT_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("INVOKE_SUPER_NEW_USING_VALUE_ARG_1_20_30_ARG_4_RETURNING_RESULT.source"),
				get("INVOKE_SUPER_NEW_USING_VALUE_ARG_1_20_30_ARG_4_RETURNING_RESULT.tree")
			);
	}
}
