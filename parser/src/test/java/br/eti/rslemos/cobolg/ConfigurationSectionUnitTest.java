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

import br.eti.rslemos.cobolg.COBOLParser.ConfigurationSectionContext;

public class ConfigurationSectionUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.configurationSection");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<ConfigurationSectionContext> helper = new CompilerHelper<ConfigurationSectionContext>() {
		@Override protected ConfigurationSectionContext parsePart() { return parser.configurationSection(); }
	};

	@Test public void EMPTY() {
		helper.compileAndVerify(
				get("EMPTY.source"),
				get("EMPTY.tree")
			);
	}

	@Test public void SOURCE_COMPUTER_XYZ() {
		helper.compileAndVerify(
				get("SOURCE_COMPUTER_XYZ.source"),
				get("SOURCE_COMPUTER_XYZ.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ.source"),
				get("OBJECT_COMPUTER_XYZ.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ABC_IS_XYZ() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ABC_IS_XYZ.source"),
				get("SPECIAL_NAMES_ABC_IS_XYZ.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ABC_XYZ() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ABC_XYZ.source"),
				get("SPECIAL_NAMES_ABC_XYZ.tree")
			);
	}
}
