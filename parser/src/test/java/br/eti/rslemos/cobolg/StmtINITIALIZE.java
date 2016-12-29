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

import br.eti.rslemos.cobolg.COBOLParser.StmtINITIALIZEContext;

public class StmtINITIALIZE {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtINITIALIZE");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtINITIALIZEContext> helper = new CompilerHelper<StmtINITIALIZEContext>() {
		@Override protected StmtINITIALIZEContext parsePart() { return parser.stmtINITIALIZE(); }
	};
	
	@Test public void INITIALIZE_X() {
		helper.compileAndVerify(
				get("INITIALIZE_X.source"),
				get("INITIALIZE_X.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y.source"),
				get("INITIALIZE_X_Y.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_ALPHABETIC_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_ALPHABETIC_BY_A.source"),
				get("INITIALIZE_X_REPLACING_ALPHABETIC_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_ALPHABETIC_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_ALPHABETIC_BY_A.source"),
				get("INITIALIZE_X_Y_REPLACING_ALPHABETIC_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_ALPHABETIC_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_ALPHABETIC_BY_QUOTED_B.source"),
				get("INITIALIZE_X_REPLACING_ALPHABETIC_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_ALPHABETIC_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_ALPHABETIC_BY_QUOTED_B.source"),
				get("INITIALIZE_X_Y_REPLACING_ALPHABETIC_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_ALPHANUMERIC_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_BY_A.source"),
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_BY_A.source"),
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_ALPHANUMERIC_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_BY_QUOTED_B.source"),
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_BY_QUOTED_B.source"),
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_ALPHANUMERIC_EDITED_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_EDITED_BY_A.source"),
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_EDITED_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_EDITED_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_EDITED_BY_A.source"),
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_EDITED_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_ALPHANUMERIC_EDITED_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_EDITED_BY_QUOTED_B.source"),
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_EDITED_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_EDITED_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_EDITED_BY_QUOTED_B.source"),
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_EDITED_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NATIONAL_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NATIONAL_BY_A.source"),
				get("INITIALIZE_X_REPLACING_NATIONAL_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NATIONAL_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_BY_A.source"),
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NATIONAL_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NATIONAL_BY_QUOTED_B.source"),
				get("INITIALIZE_X_REPLACING_NATIONAL_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NATIONAL_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_BY_QUOTED_B.source"),
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NATIONAL_EDITED_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NATIONAL_EDITED_BY_A.source"),
				get("INITIALIZE_X_REPLACING_NATIONAL_EDITED_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NATIONAL_EDITED_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_EDITED_BY_A.source"),
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_EDITED_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NATIONAL_EDITED_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NATIONAL_EDITED_BY_QUOTED_B.source"),
				get("INITIALIZE_X_REPLACING_NATIONAL_EDITED_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NATIONAL_EDITED_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_EDITED_BY_QUOTED_B.source"),
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_EDITED_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NUMERIC_BY_N() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NUMERIC_BY_N.source"),
				get("INITIALIZE_X_REPLACING_NUMERIC_BY_N.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NUMERIC_BY_N() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_BY_N.source"),
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_BY_N.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NUMERIC_BY_10() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NUMERIC_BY_10.source"),
				get("INITIALIZE_X_REPLACING_NUMERIC_BY_10.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NUMERIC_BY_10() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_BY_10.source"),
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_BY_10.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NUMERIC_EDITED_BY_N() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NUMERIC_EDITED_BY_N.source"),
				get("INITIALIZE_X_REPLACING_NUMERIC_EDITED_BY_N.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NUMERIC_EDITED_BY_N() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_EDITED_BY_N.source"),
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_EDITED_BY_N.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NUMERIC_EDITED_BY_10() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NUMERIC_EDITED_BY_10.source"),
				get("INITIALIZE_X_REPLACING_NUMERIC_EDITED_BY_10.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NUMERIC_EDITED_BY_10() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_EDITED_BY_10.source"),
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_EDITED_BY_10.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_DBCS_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_DBCS_BY_A.source"),
				get("INITIALIZE_X_REPLACING_DBCS_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_DBCS_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_DBCS_BY_A.source"),
				get("INITIALIZE_X_Y_REPLACING_DBCS_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_DBCS_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_DBCS_BY_QUOTED_B.source"),
				get("INITIALIZE_X_REPLACING_DBCS_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_DBCS_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_DBCS_BY_QUOTED_B.source"),
				get("INITIALIZE_X_Y_REPLACING_DBCS_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_EGCS_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_EGCS_BY_A.source"),
				get("INITIALIZE_X_REPLACING_EGCS_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_EGCS_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_EGCS_BY_A.source"),
				get("INITIALIZE_X_Y_REPLACING_EGCS_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_EGCS_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_EGCS_BY_QUOTED_B.source"),
				get("INITIALIZE_X_REPLACING_EGCS_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_EGCS_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_EGCS_BY_QUOTED_B.source"),
				get("INITIALIZE_X_Y_REPLACING_EGCS_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_ALPHABETIC_DATA_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_ALPHABETIC_DATA_BY_A.source"),
				get("INITIALIZE_X_REPLACING_ALPHABETIC_DATA_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_ALPHABETIC_DATA_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_ALPHABETIC_DATA_BY_A.source"),
				get("INITIALIZE_X_Y_REPLACING_ALPHABETIC_DATA_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_ALPHABETIC_DATA_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_ALPHABETIC_DATA_BY_QUOTED_B.source"),
				get("INITIALIZE_X_REPLACING_ALPHABETIC_DATA_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_ALPHABETIC_DATA_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_ALPHABETIC_DATA_BY_QUOTED_B.source"),
				get("INITIALIZE_X_Y_REPLACING_ALPHABETIC_DATA_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_ALPHANUMERIC_DATA_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_DATA_BY_A.source"),
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_DATA_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_DATA_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_DATA_BY_A.source"),
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_DATA_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_ALPHANUMERIC_DATA_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_DATA_BY_QUOTED_B.source"),
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_DATA_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_DATA_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_DATA_BY_QUOTED_B.source"),
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_DATA_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_ALPHANUMERIC_EDITED_DATA_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_EDITED_DATA_BY_A.source"),
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_EDITED_DATA_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_EDITED_DATA_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_EDITED_DATA_BY_A.source"),
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_EDITED_DATA_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_ALPHANUMERIC_EDITED_DATA_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_EDITED_DATA_BY_QUOTED_B.source"),
				get("INITIALIZE_X_REPLACING_ALPHANUMERIC_EDITED_DATA_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_EDITED_DATA_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_EDITED_DATA_BY_QUOTED_B.source"),
				get("INITIALIZE_X_Y_REPLACING_ALPHANUMERIC_EDITED_DATA_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NATIONAL_DATA_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NATIONAL_DATA_BY_A.source"),
				get("INITIALIZE_X_REPLACING_NATIONAL_DATA_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NATIONAL_DATA_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_DATA_BY_A.source"),
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_DATA_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NATIONAL_DATA_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NATIONAL_DATA_BY_QUOTED_B.source"),
				get("INITIALIZE_X_REPLACING_NATIONAL_DATA_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NATIONAL_DATA_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_DATA_BY_QUOTED_B.source"),
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_DATA_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NATIONAL_EDITED_DATA_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NATIONAL_EDITED_DATA_BY_A.source"),
				get("INITIALIZE_X_REPLACING_NATIONAL_EDITED_DATA_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NATIONAL_EDITED_DATA_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_EDITED_DATA_BY_A.source"),
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_EDITED_DATA_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NATIONAL_EDITED_DATA_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NATIONAL_EDITED_DATA_BY_QUOTED_B.source"),
				get("INITIALIZE_X_REPLACING_NATIONAL_EDITED_DATA_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NATIONAL_EDITED_DATA_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_EDITED_DATA_BY_QUOTED_B.source"),
				get("INITIALIZE_X_Y_REPLACING_NATIONAL_EDITED_DATA_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NUMERIC_DATA_BY_N() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NUMERIC_DATA_BY_N.source"),
				get("INITIALIZE_X_REPLACING_NUMERIC_DATA_BY_N.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NUMERIC_DATA_BY_N() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_DATA_BY_N.source"),
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_DATA_BY_N.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NUMERIC_DATA_BY_10() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NUMERIC_DATA_BY_10.source"),
				get("INITIALIZE_X_REPLACING_NUMERIC_DATA_BY_10.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NUMERIC_DATA_BY_10() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_DATA_BY_10.source"),
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_DATA_BY_10.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NUMERIC_EDITED_DATA_BY_N() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NUMERIC_EDITED_DATA_BY_N.source"),
				get("INITIALIZE_X_REPLACING_NUMERIC_EDITED_DATA_BY_N.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NUMERIC_EDITED_DATA_BY_N() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_EDITED_DATA_BY_N.source"),
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_EDITED_DATA_BY_N.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_NUMERIC_EDITED_DATA_BY_10() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_NUMERIC_EDITED_DATA_BY_10.source"),
				get("INITIALIZE_X_REPLACING_NUMERIC_EDITED_DATA_BY_10.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_NUMERIC_EDITED_DATA_BY_10() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_EDITED_DATA_BY_10.source"),
				get("INITIALIZE_X_Y_REPLACING_NUMERIC_EDITED_DATA_BY_10.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_DBCS_DATA_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_DBCS_DATA_BY_A.source"),
				get("INITIALIZE_X_REPLACING_DBCS_DATA_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_DBCS_DATA_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_DBCS_DATA_BY_A.source"),
				get("INITIALIZE_X_Y_REPLACING_DBCS_DATA_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_DBCS_DATA_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_DBCS_DATA_BY_QUOTED_B.source"),
				get("INITIALIZE_X_REPLACING_DBCS_DATA_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_DBCS_DATA_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_DBCS_DATA_BY_QUOTED_B.source"),
				get("INITIALIZE_X_Y_REPLACING_DBCS_DATA_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_EGCS_DATA_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_EGCS_DATA_BY_A.source"),
				get("INITIALIZE_X_REPLACING_EGCS_DATA_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_EGCS_DATA_BY_A() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_EGCS_DATA_BY_A.source"),
				get("INITIALIZE_X_Y_REPLACING_EGCS_DATA_BY_A.tree")
			);
	}
	
	@Test public void INITIALIZE_X_REPLACING_EGCS_DATA_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_REPLACING_EGCS_DATA_BY_QUOTED_B.source"),
				get("INITIALIZE_X_REPLACING_EGCS_DATA_BY_QUOTED_B.tree")
			);
	}
	
	@Test public void INITIALIZE_X_Y_REPLACING_EGCS_DATA_BY_QUOTED_B() {
		helper.compileAndVerify(
				get("INITIALIZE_X_Y_REPLACING_EGCS_DATA_BY_QUOTED_B.source"),
				get("INITIALIZE_X_Y_REPLACING_EGCS_DATA_BY_QUOTED_B.tree")
			);
	}
}
