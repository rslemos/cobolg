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

import br.eti.rslemos.cobolg.COBOLParser.StmtINSPECTContext;

public class StmtINSPECT {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtINSPECT");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtINSPECTContext> helper = new CompilerHelper<StmtINSPECTContext>() {
		@Override protected StmtINSPECTContext parsePart() { return parser.stmtINSPECT(); }
	};
	
	@Test public void INSPECT_X_TALLYING_ID_1_FOR_CHARACTERS() {
		helper.compileAndVerify(
				get("INSPECT_X_TALLYING_ID_1_FOR_CHARACTERS.source"),
				get("INSPECT_X_TALLYING_ID_1_FOR_CHARACTERS.tree")
			);
	}
	
	@Test public void INSPECT_X_TALLYING_ID_1_FOR_ALL_TF_2_LEADING_Z() {
		helper.compileAndVerify(
				get("INSPECT_X_TALLYING_ID_1_FOR_ALL_TF_2_LEADING_Z.source"),
				get("INSPECT_X_TALLYING_ID_1_FOR_ALL_TF_2_LEADING_Z.tree")
			);
	}
	
	@Test public void INSPECT_X_TALLYING_ID_1_FOR_CHARACTERS_BEFORE_TF_1_ID_2_FOR_ALL_QUOTED_TF3_QUOTED_TF4() {
		helper.compileAndVerify(
				get("INSPECT_X_TALLYING_ID_1_FOR_CHARACTERS_BEFORE_TF_1_ID_2_FOR_ALL_QUOTED_TF3_QUOTED_TF4.source"),
				get("INSPECT_X_TALLYING_ID_1_FOR_CHARACTERS_BEFORE_TF_1_ID_2_FOR_ALL_QUOTED_TF3_QUOTED_TF4.tree")
			);
	}
	
	@Test public void INSPECT_X_TALLYING_ID_1_FOR_LEADING_QUOTED_Z_BEFORE_W_ID_2_FOR_CHARACTERS_AFTER_TF_1_ALL_TF_2_BEFORE_QUOTED_TF3() {
		helper.compileAndVerify(
				get("INSPECT_X_TALLYING_ID_1_FOR_LEADING_QUOTED_Z_BEFORE_W_ID_2_FOR_CHARACTERS_AFTER_TF_1_ALL_TF_2_BEFORE_QUOTED_TF3.source"),
				get("INSPECT_X_TALLYING_ID_1_FOR_LEADING_QUOTED_Z_BEFORE_W_ID_2_FOR_CHARACTERS_AFTER_TF_1_ALL_TF_2_BEFORE_QUOTED_TF3.tree")
			);
	}
	
	@Test public void INSPECT_X_REPLACING_LEADING_SCHAR_1_BY_TCHAR_1_AFTER_INITIAL_ACHAR_1_SCHAR_2_BY_QUOTED_T_BEFORE_BCHAR_1_BEFORE_INITIAL_BCHAR_1() {
		helper.compileAndVerify(
				get("INSPECT_X_REPLACING_LEADING_SCHAR_1_BY_TCHAR_1_AFTER_INITIAL_ACHAR_1_SCHAR_2_BY_QUOTED_T_BEFORE_BCHAR_1_BEFORE_INITIAL_BCHAR_1.source"),
				get("INSPECT_X_REPLACING_LEADING_SCHAR_1_BY_TCHAR_1_AFTER_INITIAL_ACHAR_1_SCHAR_2_BY_QUOTED_T_BEFORE_BCHAR_1_BEFORE_INITIAL_BCHAR_1.tree")
			);
	}
	
	@Test public void INSPECT_X_REPLACING_ALL_QUOTED_S_BY_QUOTED_T_BEFORE_INITIAL_BCHAR_1_QUOTED_R_BY_QUOTED_U_AFTER_INITIAL_QUOTED_A_BEFORE_INITIAL_BCHAR_1_FIRST_SCHAR_1_BY_QUOTED_T_BEFORE_QUOTED_B_QUOTED_S_BY_TCHAR_1_AFTER_INITIAL_QUOTED_A_AFTER_INITIAL_ACHAR_1() {
		helper.compileAndVerify(
				get("INSPECT_X_REPLACING_ALL_QUOTED_S_BY_QUOTED_T_BEFORE_INITIAL_BCHAR_1_QUOTED_R_BY_QUOTED_U_AFTER_INITIAL_QUOTED_A_BEFORE_INITIAL_BCHAR_1_FIRST_SCHAR_1_BY_QUOTED_T_BEFORE_QUOTED_B_QUOTED_S_BY_TCHAR_1_AFTER_INITIAL_QUOTED_A_AFTER_INITIAL_ACHAR_1.source"),
				get("INSPECT_X_REPLACING_ALL_QUOTED_S_BY_QUOTED_T_BEFORE_INITIAL_BCHAR_1_QUOTED_R_BY_QUOTED_U_AFTER_INITIAL_QUOTED_A_BEFORE_INITIAL_BCHAR_1_FIRST_SCHAR_1_BY_QUOTED_T_BEFORE_QUOTED_B_QUOTED_S_BY_TCHAR_1_AFTER_INITIAL_QUOTED_A_AFTER_INITIAL_ACHAR_1.tree")
			);
	}
	
	@Test public void INSPECT_X_TALLYING_ID_1_FOR_LEADING_Z_AFTER_INITIAL_W_BEFORE_INITIAL_QUOTED_V_REPLACING_LEADING_SCHAR_1_BY_QUOTED_T_AFTER_ACHAR_1_SCHAR_2_BY_TCHAR_1_BEFORE_QUOTED_B_BEFORE_INITIAL_QUOTED_B() {
		helper.compileAndVerify(
				get("INSPECT_X_TALLYING_ID_1_FOR_LEADING_Z_AFTER_INITIAL_W_BEFORE_INITIAL_QUOTED_V_REPLACING_LEADING_SCHAR_1_BY_QUOTED_T_AFTER_ACHAR_1_SCHAR_2_BY_TCHAR_1_BEFORE_QUOTED_B_BEFORE_INITIAL_QUOTED_B.source"),
				get("INSPECT_X_TALLYING_ID_1_FOR_LEADING_Z_AFTER_INITIAL_W_BEFORE_INITIAL_QUOTED_V_REPLACING_LEADING_SCHAR_1_BY_QUOTED_T_AFTER_ACHAR_1_SCHAR_2_BY_TCHAR_1_BEFORE_QUOTED_B_BEFORE_INITIAL_QUOTED_B.tree")
			);
	}
	
	@Test public void INSPECT_X_TALLYING_ID_1_FOR_CHARACTERS_AFTER_QUOTED_TF1_ALL_QUOTED_TF3_AFTER_INITIAL_TF_2_TF_3_BEFORE_INITIAL_TF_4_REPLACING_FIRST_QUOTED_S_BY_TCHAR_1_BEFORE_INITIAL_QUOTED_B_SCHAR_1_BY_QUOTED_T_BEFORE_QUOTED_B_AFTER_QUOTED_A() {
		helper.compileAndVerify(
				get("INSPECT_X_TALLYING_ID_1_FOR_CHARACTERS_AFTER_QUOTED_TF1_ALL_QUOTED_TF3_AFTER_INITIAL_TF_2_TF_3_BEFORE_INITIAL_TF_4_REPLACING_FIRST_QUOTED_S_BY_TCHAR_1_BEFORE_INITIAL_QUOTED_B_SCHAR_1_BY_QUOTED_T_BEFORE_QUOTED_B_AFTER_QUOTED_A.source"),
				get("INSPECT_X_TALLYING_ID_1_FOR_CHARACTERS_AFTER_QUOTED_TF1_ALL_QUOTED_TF3_AFTER_INITIAL_TF_2_TF_3_BEFORE_INITIAL_TF_4_REPLACING_FIRST_QUOTED_S_BY_TCHAR_1_BEFORE_INITIAL_QUOTED_B_SCHAR_1_BY_QUOTED_T_BEFORE_QUOTED_B_AFTER_QUOTED_A.tree")
			);
	}
	
	@Test public void INSPECT_X_TALLYING_ID_1_FOR_LEADING_QUOTED_Z_W_BEFORE_QUOTED_W_ID_2_FOR_CHARACTERS_BEFORE_INITIAL_TF_1_AFTER_QUOTED_TF1_REPLACING_ALL_QUOTED_S_BY_TCHAR_1_BEFORE_INITIAL_BCHAR_1_QUOTED_R_BY_QUOTED_T_AFTER_QUOTED_A_BEFORE_BCHAR_1() {
		helper.compileAndVerify(
				get("INSPECT_X_TALLYING_ID_1_FOR_LEADING_QUOTED_Z_W_BEFORE_QUOTED_W_ID_2_FOR_CHARACTERS_BEFORE_INITIAL_TF_1_AFTER_QUOTED_TF1_REPLACING_ALL_QUOTED_S_BY_TCHAR_1_BEFORE_INITIAL_BCHAR_1_QUOTED_R_BY_QUOTED_T_AFTER_QUOTED_A_BEFORE_BCHAR_1.source"),
				get("INSPECT_X_TALLYING_ID_1_FOR_LEADING_QUOTED_Z_W_BEFORE_QUOTED_W_ID_2_FOR_CHARACTERS_BEFORE_INITIAL_TF_1_AFTER_QUOTED_TF1_REPLACING_ALL_QUOTED_S_BY_TCHAR_1_BEFORE_INITIAL_BCHAR_1_QUOTED_R_BY_QUOTED_T_AFTER_QUOTED_A_BEFORE_BCHAR_1.tree")
			);
	}
	
	@Test public void INSPECT_X_TALLYING_ID_1_FOR_ALL_QUOTED_TF3_AFTER_QUOTED_TF4_BEFORE_INITIAL_TF_2_ID_2_FOR_LEADING_Z_AFTER_INITIAL_QUOTED_W_BEFORE_QUOTED_Y_QUOTED_V_CHARACTERS_BEFORE_INITIAL_QUOTED_TF1_AFTER_INITIAL_TF_1_REPLACING_CHARACTERS_BY_TCHAR_1_AFTER_INITIAL_QUOTED_A_BEFORE_INITIAL_BCHAR_1() {
		helper.compileAndVerify(
				get("INSPECT_X_TALLYING_ID_1_FOR_ALL_QUOTED_TF3_AFTER_QUOTED_TF4_BEFORE_INITIAL_TF_2_ID_2_FOR_LEADING_Z_AFTER_INITIAL_QUOTED_W_BEFORE_QUOTED_Y_QUOTED_V_CHARACTERS_BEFORE_INITIAL_QUOTED_TF1_AFTER_INITIAL_TF_1_REPLACING_CHARACTERS_BY_TCHAR_1_AFTER_INITIAL_QUOTED_A_BEFORE_INITIAL_BCHAR_1.source"),
				get("INSPECT_X_TALLYING_ID_1_FOR_ALL_QUOTED_TF3_AFTER_QUOTED_TF4_BEFORE_INITIAL_TF_2_ID_2_FOR_LEADING_Z_AFTER_INITIAL_QUOTED_W_BEFORE_QUOTED_Y_QUOTED_V_CHARACTERS_BEFORE_INITIAL_QUOTED_TF1_AFTER_INITIAL_TF_1_REPLACING_CHARACTERS_BY_TCHAR_1_AFTER_INITIAL_QUOTED_A_BEFORE_INITIAL_BCHAR_1.tree")
			);
	}
	
	@Test public void INSPECT_X_TALLYING_ID_1_FOR_ALL_Z_BEFORE_INITIAL_W_AFTER_QUOTED_Y_V_BEFORE_QUOTED_U_AFTER_INITIAL_T_REPLACING_CHARACTERS_BY_QUOTED_T_AFTER_QUOTED_A_BEFORE_QUOTED_B_LEADING_QUOTED_S_BY_QUOTED_T_QUOTED_R_BY_QUOTED_U() {
		helper.compileAndVerify(
				get("INSPECT_X_TALLYING_ID_1_FOR_ALL_Z_BEFORE_INITIAL_W_AFTER_QUOTED_Y_V_BEFORE_QUOTED_U_AFTER_INITIAL_T_REPLACING_CHARACTERS_BY_QUOTED_T_AFTER_QUOTED_A_BEFORE_QUOTED_B_LEADING_QUOTED_S_BY_QUOTED_T_QUOTED_R_BY_QUOTED_U.source"),
				get("INSPECT_X_TALLYING_ID_1_FOR_ALL_Z_BEFORE_INITIAL_W_AFTER_QUOTED_Y_V_BEFORE_QUOTED_U_AFTER_INITIAL_T_REPLACING_CHARACTERS_BY_QUOTED_T_AFTER_QUOTED_A_BEFORE_QUOTED_B_LEADING_QUOTED_S_BY_QUOTED_T_QUOTED_R_BY_QUOTED_U.tree")
			);
	}
	
	@Test public void INSPECT_X_TALLYING_ID_1_FOR_LEADING_QUOTED_Z_BEFORE_INITIAL_W_Y_AFTER_QUOTED_V_BEFORE_U_CHARACTERS_AFTER_INITIAL_TF_1_BEFORE_QUOTED_TF1_REPLACING_ALL_SCHAR_1_BY_QUOTED_T_QUOTED_S_BY_QUOTED_U_CHARACTERS_BY_QUOTED_T_BEFORE_BCHAR_1() {
		helper.compileAndVerify(
				get("INSPECT_X_TALLYING_ID_1_FOR_LEADING_QUOTED_Z_BEFORE_INITIAL_W_Y_AFTER_QUOTED_V_BEFORE_U_CHARACTERS_AFTER_INITIAL_TF_1_BEFORE_QUOTED_TF1_REPLACING_ALL_SCHAR_1_BY_QUOTED_T_QUOTED_S_BY_QUOTED_U_CHARACTERS_BY_QUOTED_T_BEFORE_BCHAR_1.source"),
				get("INSPECT_X_TALLYING_ID_1_FOR_LEADING_QUOTED_Z_BEFORE_INITIAL_W_Y_AFTER_QUOTED_V_BEFORE_U_CHARACTERS_AFTER_INITIAL_TF_1_BEFORE_QUOTED_TF1_REPLACING_ALL_SCHAR_1_BY_QUOTED_T_QUOTED_S_BY_QUOTED_U_CHARACTERS_BY_QUOTED_T_BEFORE_BCHAR_1.tree")
			);
	}
	
	@Test public void INSPECT_X_TALLYING_ID_1_FOR_ALL_Z_BEFORE_INITIAL_W_AFTER_Y_V_BEFORE_QUOTED_U_AFTER_INITIAL_T_ID_2_FOR_LEADING_Z_BEFORE_INITIAL_QUOTED_W_Y_AFTER_V_BEFORE_INITIAL_U_REPLACING_FIRST_SCHAR_1_BY_TCHAR_1_LEADING_QUOTED_S_BY_QUOTED_T() {
		helper.compileAndVerify(
				get("INSPECT_X_TALLYING_ID_1_FOR_ALL_Z_BEFORE_INITIAL_W_AFTER_Y_V_BEFORE_QUOTED_U_AFTER_INITIAL_T_ID_2_FOR_LEADING_Z_BEFORE_INITIAL_QUOTED_W_Y_AFTER_V_BEFORE_INITIAL_U_REPLACING_FIRST_SCHAR_1_BY_TCHAR_1_LEADING_QUOTED_S_BY_QUOTED_T.source"),
				get("INSPECT_X_TALLYING_ID_1_FOR_ALL_Z_BEFORE_INITIAL_W_AFTER_Y_V_BEFORE_QUOTED_U_AFTER_INITIAL_T_ID_2_FOR_LEADING_Z_BEFORE_INITIAL_QUOTED_W_Y_AFTER_V_BEFORE_INITIAL_U_REPLACING_FIRST_SCHAR_1_BY_TCHAR_1_LEADING_QUOTED_S_BY_QUOTED_T.tree")
			);
	}
	
	@Test public void INSPECT_X_TALLYING_ID_1_FOR_CHARACTERS_AFTER_INITIAL_QUOTED_TF1_BEFORE_INITIAL_TF_1_ID_2_FOR_ALL_QUOTED_Z_BEFORE_INITIAL_QUOTED_W_AFTER_QUOTED_Y_V_BEFORE_QUOTED_U_AFTER_INITIAL_T_LEADING_Z_BEFORE_INITIAL_QUOTED_W_QUOTED_Y_AFTER_QUOTED_V_BEFORE_INITIAL_QUOTED_U_REPLACING_ALL_SCHAR_1_BY_QUOTED_T_CHARACTERS_BY_TCHAR_1() {
		helper.compileAndVerify(
				get("INSPECT_X_TALLYING_ID_1_FOR_CHARACTERS_AFTER_INITIAL_QUOTED_TF1_BEFORE_INITIAL_TF_1_ID_2_FOR_ALL_QUOTED_Z_BEFORE_INITIAL_QUOTED_W_AFTER_QUOTED_Y_V_BEFORE_QUOTED_U_AFTER_INITIAL_T_LEADING_Z_BEFORE_INITIAL_QUOTED_W_QUOTED_Y_AFTER_QUOTED_V_BEFORE_INITIAL_QUOTED_U_REPLACING_ALL_SCHAR_1_BY_QUOTED_T_CHARACTERS_BY_TCHAR_1.source"),
				get("INSPECT_X_TALLYING_ID_1_FOR_CHARACTERS_AFTER_INITIAL_QUOTED_TF1_BEFORE_INITIAL_TF_1_ID_2_FOR_ALL_QUOTED_Z_BEFORE_INITIAL_QUOTED_W_AFTER_QUOTED_Y_V_BEFORE_QUOTED_U_AFTER_INITIAL_T_LEADING_Z_BEFORE_INITIAL_QUOTED_W_QUOTED_Y_AFTER_QUOTED_V_BEFORE_INITIAL_QUOTED_U_REPLACING_ALL_SCHAR_1_BY_QUOTED_T_CHARACTERS_BY_TCHAR_1.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_ID_1_TO_ID_2_BEFORE_ID_A() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_ID_1_TO_ID_2_BEFORE_ID_A.source"),
				get("INSPECT_X_CONVERTING_ID_1_TO_ID_2_BEFORE_ID_A.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_ID_1_TO_ID_2_BEFORE_INITIAL_QUOTED_Z() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_ID_1_TO_ID_2_BEFORE_INITIAL_QUOTED_Z.source"),
				get("INSPECT_X_CONVERTING_ID_1_TO_ID_2_BEFORE_INITIAL_QUOTED_Z.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_ID_1_TO_ID_2_AFTER_ID_A_BEFORE_INITIAL_ID_B() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_ID_1_TO_ID_2_AFTER_ID_A_BEFORE_INITIAL_ID_B.source"),
				get("INSPECT_X_CONVERTING_ID_1_TO_ID_2_AFTER_ID_A_BEFORE_INITIAL_ID_B.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_ID_1_TO_ID_2_AFTER_INITIAL_QUOTED_Z_BEFORE_QUOTED_W() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_ID_1_TO_ID_2_AFTER_INITIAL_QUOTED_Z_BEFORE_QUOTED_W.source"),
				get("INSPECT_X_CONVERTING_ID_1_TO_ID_2_AFTER_INITIAL_QUOTED_Z_BEFORE_QUOTED_W.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_ID_1_TO_QUOTED_B_AFTER_ID_A() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_ID_1_TO_QUOTED_B_AFTER_ID_A.source"),
				get("INSPECT_X_CONVERTING_ID_1_TO_QUOTED_B_AFTER_ID_A.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_ID_1_TO_QUOTED_B_AFTER_INITIAL_QUOTED_Z() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_ID_1_TO_QUOTED_B_AFTER_INITIAL_QUOTED_Z.source"),
				get("INSPECT_X_CONVERTING_ID_1_TO_QUOTED_B_AFTER_INITIAL_QUOTED_Z.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_ID_1_TO_QUOTED_B_BEFORE_ID_A_AFTER_INITIAL_QUOTED_W() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_ID_1_TO_QUOTED_B_BEFORE_ID_A_AFTER_INITIAL_QUOTED_W.source"),
				get("INSPECT_X_CONVERTING_ID_1_TO_QUOTED_B_BEFORE_ID_A_AFTER_INITIAL_QUOTED_W.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_ID_1_TO_QUOTED_B_BEFORE_INITIAL_QUOTED_Z_AFTER_ID_B() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_ID_1_TO_QUOTED_B_BEFORE_INITIAL_QUOTED_Z_AFTER_ID_B.source"),
				get("INSPECT_X_CONVERTING_ID_1_TO_QUOTED_B_BEFORE_INITIAL_QUOTED_Z_AFTER_ID_B.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_QUOTED_A_TO_ID_2_BEFORE_ID_A() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_ID_2_BEFORE_ID_A.source"),
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_ID_2_BEFORE_ID_A.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_QUOTED_A_TO_ID_2_BEFORE_INITIAL_QUOTED_Z() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_ID_2_BEFORE_INITIAL_QUOTED_Z.source"),
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_ID_2_BEFORE_INITIAL_QUOTED_Z.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_QUOTED_A_TO_ID_2_AFTER_ID_A_BEFORE_INITIAL_QUOTED_W() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_ID_2_AFTER_ID_A_BEFORE_INITIAL_QUOTED_W.source"),
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_ID_2_AFTER_ID_A_BEFORE_INITIAL_QUOTED_W.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_QUOTED_A_TO_ID_2_AFTER_INITIAL_QUOTED_Z_BEFORE_ID_B() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_ID_2_AFTER_INITIAL_QUOTED_Z_BEFORE_ID_B.source"),
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_ID_2_AFTER_INITIAL_QUOTED_Z_BEFORE_ID_B.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_QUOTED_A_TO_QUOTED_B_AFTER_ID_A() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_QUOTED_B_AFTER_ID_A.source"),
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_QUOTED_B_AFTER_ID_A.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_QUOTED_A_TO_QUOTED_B_AFTER_INITIAL_QUOTED_Z() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_QUOTED_B_AFTER_INITIAL_QUOTED_Z.source"),
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_QUOTED_B_AFTER_INITIAL_QUOTED_Z.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_QUOTED_A_TO_QUOTED_B_BEFORE_QUOTED_Z_AFTER_INITIAL_ID_B() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_QUOTED_B_BEFORE_QUOTED_Z_AFTER_INITIAL_ID_B.source"),
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_QUOTED_B_BEFORE_QUOTED_Z_AFTER_INITIAL_ID_B.tree")
			);
	}
	
	@Test public void INSPECT_X_CONVERTING_QUOTED_A_TO_QUOTED_B_BEFORE_INITIAL_ID_A_AFTER_QUOTED_W() {
		helper.compileAndVerify(
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_QUOTED_B_BEFORE_INITIAL_ID_A_AFTER_QUOTED_W.source"),
				get("INSPECT_X_CONVERTING_QUOTED_A_TO_QUOTED_B_BEFORE_INITIAL_ID_A_AFTER_QUOTED_W.tree")
			);
	}
}
