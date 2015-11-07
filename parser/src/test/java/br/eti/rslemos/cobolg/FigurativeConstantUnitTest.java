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

import br.eti.rslemos.cobolg.COBOLParser.FigurativeConstantContext;

public class FigurativeConstantUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.figurativeConstant");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<FigurativeConstantContext> helper = new CompilerHelper<FigurativeConstantContext>() {
		@Override protected FigurativeConstantContext parsePart() { return parser.figurativeConstant(); }
	};

	@Test public void ZERO() {
		helper.compileAndVerify(
				get("ZERO.source"),
				get("ZERO.tree")
			);
	}

	@Test public void ZEROS() {
		helper.compileAndVerify(
				get("ZEROS.source"),
				get("ZEROS.tree")
			);
	}

	@Test public void ZEROES() {
		helper.compileAndVerify(
				get("ZEROES.source"),
				get("ZEROES.tree")
			);
	}

	@Test public void SPACE() {
		helper.compileAndVerify(
				get("SPACE.source"),
				get("SPACE.tree")
			);
	}

	@Test public void SPACES() {
		helper.compileAndVerify(
				get("SPACES.source"),
				get("SPACES.tree")
			);
	}

	@Test public void HIGH_VALUE() {
		helper.compileAndVerify(
				get("HIGH_VALUE.source"),
				get("HIGH_VALUE.tree")
			);
	}

	@Test public void HIGH_VALUES() {
		helper.compileAndVerify(
				get("HIGH_VALUES.source"),
				get("HIGH_VALUES.tree")
			);
	}

	@Test public void LOW_VALUE() {
		helper.compileAndVerify(
				get("LOW_VALUE.source"),
				get("LOW_VALUE.tree")
			);
	}

	@Test public void LOW_VALUES() {
		helper.compileAndVerify(
				get("LOW_VALUES.source"),
				get("LOW_VALUES.tree")
			);
	}

	@Test public void QUOTE() {
		helper.compileAndVerify(
				get("QUOTE.source"),
				get("QUOTE.tree")
			);
	}

	@Test public void QUOTES() {
		helper.compileAndVerify(
				get("QUOTES.source"),
				get("QUOTES.tree")
			);
	}

	@Test public void NULL() {
		helper.compileAndVerify(
				get("NULL.source"),
				get("NULL.tree")
			);
	}

	@Test public void NULLS() {
		helper.compileAndVerify(
				get("NULLS.source"),
				get("NULLS.tree")
			);
	}

	@Test public void ALL_0() {
		helper.compileAndVerify(
				get("ALL_0.source"),
				get("ALL_0.tree")
			);
	}

	@Test public void ALL_X() {
		helper.compileAndVerify(
				get("ALL_X.source"),
				get("ALL_X.tree")
			);
	}

	@Test public void ALL_SPACES() {
		helper.compileAndVerify(
				get("ALL_SPACES.source"),
				get("ALL_SPACES.tree")
			);
	}

	@Test public void ALL_NULLS() {
		helper.compileAndVerify(
				get("ALL_NULLS.source"),
				get("ALL_NULLS.tree")
			);
	}
}
