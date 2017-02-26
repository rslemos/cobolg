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
 * but WITHOUT CANCEL WARRANTY; without even the implied warranty of
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

import br.eti.rslemos.cobolg.COBOLParser.StmtCANCELContext;

public class StmtCANCEL {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtCANCEL");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtCANCELContext> helper = new CompilerHelper<StmtCANCELContext>() {
		@Override protected StmtCANCELContext parsePart() { return parser.stmtCANCEL(); }
	};

	@Test public void CANCEL_ID_1() {
		helper.compileAndVerify(
				get("CANCEL_ID_1.source"),
				get("CANCEL_ID_1.tree")
			);
	}

	@Test public void CANCEL_QUOTED_I1() {
		helper.compileAndVerify(
				get("CANCEL_QUOTED_I1.source"),
				get("CANCEL_QUOTED_I1.tree")
			);
	}

	@Test public void CANCEL_ID_1_ID_2() {
		helper.compileAndVerify(
				get("CANCEL_ID_1_ID_2.source"),
				get("CANCEL_ID_1_ID_2.tree")
			);
	}

	@Test public void CANCEL_ID_1_QUOTED_I2() {
		helper.compileAndVerify(
				get("CANCEL_ID_1_QUOTED_I2.source"),
				get("CANCEL_ID_1_QUOTED_I2.tree")
			);
	}

	@Test public void CANCEL_QUOTED_I1_ID_2() {
		helper.compileAndVerify(
				get("CANCEL_QUOTED_I1_ID_2.source"),
				get("CANCEL_QUOTED_I1_ID_2.tree")
			);
	}

	@Test public void CANCEL_QUOTED_I1_QUOTED_I2() {
		helper.compileAndVerify(
				get("CANCEL_QUOTED_I1_QUOTED_I2.source"),
				get("CANCEL_QUOTED_I1_QUOTED_I2.tree")
			);
	}
}
