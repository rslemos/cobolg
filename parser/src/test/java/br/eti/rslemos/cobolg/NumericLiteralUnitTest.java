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

import br.eti.rslemos.cobolg.COBOLParser.NumericLiteralContext;

public class NumericLiteralUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.numericLiteral");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<NumericLiteralContext> helper = new CompilerHelper<NumericLiteralContext>() {
		@Override protected NumericLiteralContext parsePart() { return parser.numericLiteral(); }
	};

	@Test public void SIGNED_POSITIVE_INTEGER() {
		helper.compileAndVerify(
				get("SIGNED_POSITIVE_INTEGER.source"), 
				get("SIGNED_POSITIVE_INTEGER.tree")
			);
	}
	
	@Test public void POSITIVE_INTEGER() {
		helper.compileAndVerify(
				get("POSITIVE_INTEGER.source"), 
				get("POSITIVE_INTEGER.tree")
			);
	}
	
	@Test public void NEGATIVE_INTEGER() {
		helper.compileAndVerify(
				get("NEGATIVE_INTEGER.source"), 
				get("NEGATIVE_INTEGER.tree")
			);
	}
	
	@Test public void SIGNED_POSITIVE_FIXEDPOINT() {
		helper.compileAndVerify(
				get("SIGNED_POSITIVE_FIXEDPOINT.source"),
				get("SIGNED_POSITIVE_FIXEDPOINT.tree")
			);
	}

	@Test public void POSITIVE_FIXEDPOINT() {
		helper.compileAndVerify(
				get("POSITIVE_FIXEDPOINT.source"),
				get("POSITIVE_FIXEDPOINT.tree")
			);
	}

	@Test public void NEGATIVE_FIXEDPOINT() {
		helper.compileAndVerify(
				get("NEGATIVE_FIXEDPOINT.source"),
				get("NEGATIVE_FIXEDPOINT.tree")
			);
	}

	@Test public void SIGNED_POSITIVE_FLOATINGPOINT() {
		helper.compileAndVerify(
				get("SIGNED_POSITIVE_FLOATINGPOINT.source"),
				get("SIGNED_POSITIVE_FLOATINGPOINT.tree")
			);
	}

	@Test public void POSITIVE_FLOATINGPOINT() {
		helper.compileAndVerify(
				get("POSITIVE_FLOATINGPOINT.source"),
				get("POSITIVE_FLOATINGPOINT.tree")
			);
	}

	@Test public void NEGATIVE_FLOATINGPOINT() {
		helper.compileAndVerify(
				get("NEGATIVE_FLOATINGPOINT.source"),
				get("NEGATIVE_FLOATINGPOINT.tree")
			);
	}

	@Test public void SIGNED_POSITIVE_FLOATINGPOINT_COMMA() {
		helper.compileAndVerify(
				get("SIGNED_POSITIVE_FLOATINGPOINT_COMMA.source"),
				get("SIGNED_POSITIVE_FLOATINGPOINT_COMMA.tree")
			);
	}

	@Test public void POSITIVE_FLOATINGPOINT_COMMA() {
		helper.compileAndVerify(
				get("POSITIVE_FLOATINGPOINT_COMMA.source"),
				get("POSITIVE_FLOATINGPOINT_COMMA.tree")
			);
	}

	@Test public void NEGATIVE_FLOATINGPOINT_COMMA() {
		helper.compileAndVerify(
				get("NEGATIVE_FLOATINGPOINT_COMMA.source"),
				get("NEGATIVE_FLOATINGPOINT_COMMA.tree")
			);
	}
}
