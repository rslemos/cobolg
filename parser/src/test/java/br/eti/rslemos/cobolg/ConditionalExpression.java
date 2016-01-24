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

import br.eti.rslemos.cobolg.COBOLParser.ConditionalExpressionContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
public class ConditionalExpression {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.conditionalExpression");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<ConditionalExpressionContext> helper = new CompilerHelper<ConditionalExpressionContext>() {
		@Override protected ConditionalExpressionContext parsePart() { return parser.conditionalExpression(); }
	};
	
	@Test public void X_NUMERIC() {
		helper.compileAndVerify(
				get("X_NUMERIC.source"),
				get("X_NUMERIC.tree")
			);
	}

	@Test public void X_ALPHABETIC() {
		helper.compileAndVerify(
				get("X_ALPHABETIC.source"),
				get("X_ALPHABETIC.tree")
			);
	}

	@Test public void X_ALPHABETIC_LOWER() {
		helper.compileAndVerify(
				get("X_ALPHABETIC_LOWER.source"),
				get("X_ALPHABETIC_LOWER.tree")
			);
	}

	@Test public void X_ALPHABETIC_UPPER() {
		helper.compileAndVerify(
				get("X_ALPHABETIC_UPPER.source"),
				get("X_ALPHABETIC_UPPER.tree")
			);
	}

	@Test public void X_IS_NUMERIC() {
		helper.compileAndVerify(
				get("X_IS_NUMERIC.source"),
				get("X_IS_NUMERIC.tree")
			);
	}

	@Test public void X_IS_ALPHABETIC() {
		helper.compileAndVerify(
				get("X_IS_ALPHABETIC.source"),
				get("X_IS_ALPHABETIC.tree")
			);
	}

	@Test public void X_IS_ALPHABETIC_LOWER() {
		helper.compileAndVerify(
				get("X_IS_ALPHABETIC_LOWER.source"),
				get("X_IS_ALPHABETIC_LOWER.tree")
			);
	}

	@Test public void X_IS_ALPHABETIC_UPPER() {
		helper.compileAndVerify(
				get("X_IS_ALPHABETIC_UPPER.source"),
				get("X_IS_ALPHABETIC_UPPER.tree")
			);
	}

	@Test public void X_NOT_NUMERIC() {
		helper.compileAndVerify(
				get("X_NOT_NUMERIC.source"),
				get("X_NOT_NUMERIC.tree")
			);
	}

	@Test public void X_NOT_ALPHABETIC() {
		helper.compileAndVerify(
				get("X_NOT_ALPHABETIC.source"),
				get("X_NOT_ALPHABETIC.tree")
			);
	}

	@Test public void X_NOT_ALPHABETIC_LOWER() {
		helper.compileAndVerify(
				get("X_NOT_ALPHABETIC_LOWER.source"),
				get("X_NOT_ALPHABETIC_LOWER.tree")
			);
	}

	@Test public void X_NOT_ALPHABETIC_UPPER() {
		helper.compileAndVerify(
				get("X_NOT_ALPHABETIC_UPPER.source"),
				get("X_NOT_ALPHABETIC_UPPER.tree")
			);
	}

	@Test public void X_IS_NOT_NUMERIC() {
		helper.compileAndVerify(
				get("X_IS_NOT_NUMERIC.source"),
				get("X_IS_NOT_NUMERIC.tree")
			);
	}

	@Test public void X_IS_NOT_ALPHABETIC() {
		helper.compileAndVerify(
				get("X_IS_NOT_ALPHABETIC.source"),
				get("X_IS_NOT_ALPHABETIC.tree")
			);
	}

	@Test public void X_IS_NOT_ALPHABETIC_LOWER() {
		helper.compileAndVerify(
				get("X_IS_NOT_ALPHABETIC_LOWER.source"),
				get("X_IS_NOT_ALPHABETIC_LOWER.tree")
			);
	}

	@Test public void X_IS_NOT_ALPHABETIC_UPPER() {
		helper.compileAndVerify(
				get("X_IS_NOT_ALPHABETIC_UPPER.source"),
				get("X_IS_NOT_ALPHABETIC_UPPER.tree")
			);
	}

	@Test public void CONDITION() {
		helper.compileAndVerify(
				get("CONDITION.source"),
				get("CONDITION.tree")
			);
	}

	@Test public void X_GREATER_Y() {
		helper.compileAndVerify(
				get("X_GREATER_Y.source"),
				get("X_GREATER_Y.tree")
			);
	}

	@Test public void X_OP_GREATER_Y() {
		helper.compileAndVerify(
				get("X_OP_GREATER_Y.source"),
				get("X_OP_GREATER_Y.tree")
			);
	}

	@Test public void X_LESS_Y() {
		helper.compileAndVerify(
				get("X_LESS_Y.source"),
				get("X_LESS_Y.tree")
			);
	}

	@Test public void X_OP_LESS_Y() {
		helper.compileAndVerify(
				get("X_OP_LESS_Y.source"),
				get("X_OP_LESS_Y.tree")
			);
	}

	@Test public void X_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_EQUAL_Y.source"),
				get("X_EQUAL_Y.tree")
			);
	}

	@Test public void X_OP_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_OP_EQUAL_Y.source"),
				get("X_OP_EQUAL_Y.tree")
			);
	}

	@Test public void X_GREATER_OR_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_GREATER_OR_EQUAL_Y.source"),
				get("X_GREATER_OR_EQUAL_Y.tree")
			);
	}

	@Test public void X_OP_NOTLESS_Y() {
		helper.compileAndVerify(
				get("X_OP_NOTLESS_Y.source"),
				get("X_OP_NOTLESS_Y.tree")
			);
	}

	@Test public void X_LESS_OR_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_LESS_OR_EQUAL_Y.source"),
				get("X_LESS_OR_EQUAL_Y.tree")
			);
	}

	@Test public void X_OP_NOTGREATER_Y() {
		helper.compileAndVerify(
				get("X_OP_NOTGREATER_Y.source"),
				get("X_OP_NOTGREATER_Y.tree")
			);
	}

	@Test public void $10_GREATER_Y() {
		helper.compileAndVerify(
				get("10_GREATER_Y.source"),
				get("10_GREATER_Y.tree")
			);
	}

	@Test public void $10_OP_GREATER_Y() {
		helper.compileAndVerify(
				get("10_OP_GREATER_Y.source"),
				get("10_OP_GREATER_Y.tree")
			);
	}

	@Test public void $10_LESS_Y() {
		helper.compileAndVerify(
				get("10_LESS_Y.source"),
				get("10_LESS_Y.tree")
			);
	}

	@Test public void $10_OP_LESS_Y() {
		helper.compileAndVerify(
				get("10_OP_LESS_Y.source"),
				get("10_OP_LESS_Y.tree")
			);
	}

	@Test public void $10_EQUAL_Y() {
		helper.compileAndVerify(
				get("10_EQUAL_Y.source"),
				get("10_EQUAL_Y.tree")
			);
	}

	@Test public void $10_OP_EQUAL_Y() {
		helper.compileAndVerify(
				get("10_OP_EQUAL_Y.source"),
				get("10_OP_EQUAL_Y.tree")
			);
	}

	@Test public void $10_GREATER_OR_EQUAL_Y() {
		helper.compileAndVerify(
				get("10_GREATER_OR_EQUAL_Y.source"),
				get("10_GREATER_OR_EQUAL_Y.tree")
			);
	}

	@Test public void $10_OP_NOTLESS_Y() {
		helper.compileAndVerify(
				get("10_OP_NOTLESS_Y.source"),
				get("10_OP_NOTLESS_Y.tree")
			);
	}

	@Test public void $10_LESS_OR_EQUAL_Y() {
		helper.compileAndVerify(
				get("10_LESS_OR_EQUAL_Y.source"),
				get("10_LESS_OR_EQUAL_Y.tree")
			);
	}

	@Test public void $10_OP_NOTGREATER_Y() {
		helper.compileAndVerify(
				get("10_OP_NOTGREATER_Y.source"),
				get("10_OP_NOTGREATER_Y.tree")
			);
	}

	@Test public void X_OP_PLUS_1_GREATER_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_Y.source"),
				get("X_OP_PLUS_1_GREATER_Y.tree")
			);
	}

	@Test public void X_OP_PLUS_1_OP_GREATER_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_GREATER_Y.source"),
				get("X_OP_PLUS_1_OP_GREATER_Y.tree")
			);
	}

	@Test public void X_OP_PLUS_1_LESS_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_Y.source"),
				get("X_OP_PLUS_1_LESS_Y.tree")
			);
	}

	@Test public void X_OP_PLUS_1_OP_LESS_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_LESS_Y.source"),
				get("X_OP_PLUS_1_OP_LESS_Y.tree")
			);
	}

	@Test public void X_OP_PLUS_1_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_EQUAL_Y.source"),
				get("X_OP_PLUS_1_EQUAL_Y.tree")
			);
	}

	@Test public void X_OP_PLUS_1_OP_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_EQUAL_Y.source"),
				get("X_OP_PLUS_1_OP_EQUAL_Y.tree")
			);
	}

	@Test public void X_OP_PLUS_1_GREATER_OR_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_Y.source"),
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_Y.tree")
			);
	}

	@Test public void X_OP_PLUS_1_OP_NOTLESS_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTLESS_Y.source"),
				get("X_OP_PLUS_1_OP_NOTLESS_Y.tree")
			);
	}

	@Test public void X_OP_PLUS_1_LESS_OR_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_OR_EQUAL_Y.source"),
				get("X_OP_PLUS_1_LESS_OR_EQUAL_Y.tree")
			);
	}

	@Test public void X_OP_PLUS_1_OP_NOTGREATER_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTGREATER_Y.source"),
				get("X_OP_PLUS_1_OP_NOTGREATER_Y.tree")
			);
	}

	@Test public void X_GREATER_10() {
		helper.compileAndVerify(
				get("X_GREATER_10.source"),
				get("X_GREATER_10.tree")
			);
	}

	@Test public void X_OP_GREATER_10() {
		helper.compileAndVerify(
				get("X_OP_GREATER_10.source"),
				get("X_OP_GREATER_10.tree")
			);
	}

	@Test public void X_LESS_10() {
		helper.compileAndVerify(
				get("X_LESS_10.source"),
				get("X_LESS_10.tree")
			);
	}

	@Test public void X_OP_LESS_10() {
		helper.compileAndVerify(
				get("X_OP_LESS_10.source"),
				get("X_OP_LESS_10.tree")
			);
	}

	@Test public void X_EQUAL_10() {
		helper.compileAndVerify(
				get("X_EQUAL_10.source"),
				get("X_EQUAL_10.tree")
			);
	}

	@Test public void X_OP_EQUAL_10() {
		helper.compileAndVerify(
				get("X_OP_EQUAL_10.source"),
				get("X_OP_EQUAL_10.tree")
			);
	}

	@Test public void X_GREATER_OR_EQUAL_10() {
		helper.compileAndVerify(
				get("X_GREATER_OR_EQUAL_10.source"),
				get("X_GREATER_OR_EQUAL_10.tree")
			);
	}

	@Test public void X_OP_NOTLESS_10() {
		helper.compileAndVerify(
				get("X_OP_NOTLESS_10.source"),
				get("X_OP_NOTLESS_10.tree")
			);
	}

	@Test public void X_LESS_OR_EQUAL_10() {
		helper.compileAndVerify(
				get("X_LESS_OR_EQUAL_10.source"),
				get("X_LESS_OR_EQUAL_10.tree")
			);
	}

	@Test public void X_OP_NOTGREATER_10() {
		helper.compileAndVerify(
				get("X_OP_NOTGREATER_10.source"),
				get("X_OP_NOTGREATER_10.tree")
			);
	}

	@Test public void $10_GREATER_10() {
		helper.compileAndVerify(
				get("10_GREATER_10.source"),
				get("10_GREATER_10.tree")
			);
	}

	@Test public void $10_OP_GREATER_10() {
		helper.compileAndVerify(
				get("10_OP_GREATER_10.source"),
				get("10_OP_GREATER_10.tree")
			);
	}

	@Test public void $10_LESS_10() {
		helper.compileAndVerify(
				get("10_LESS_10.source"),
				get("10_LESS_10.tree")
			);
	}

	@Test public void $10_OP_LESS_10() {
		helper.compileAndVerify(
				get("10_OP_LESS_10.source"),
				get("10_OP_LESS_10.tree")
			);
	}

	@Test public void $10_EQUAL_10() {
		helper.compileAndVerify(
				get("10_EQUAL_10.source"),
				get("10_EQUAL_10.tree")
			);
	}

	@Test public void $10_OP_EQUAL_10() {
		helper.compileAndVerify(
				get("10_OP_EQUAL_10.source"),
				get("10_OP_EQUAL_10.tree")
			);
	}

	@Test public void $10_GREATER_OR_EQUAL_10() {
		helper.compileAndVerify(
				get("10_GREATER_OR_EQUAL_10.source"),
				get("10_GREATER_OR_EQUAL_10.tree")
			);
	}

	@Test public void $10_OP_NOTLESS_10() {
		helper.compileAndVerify(
				get("10_OP_NOTLESS_10.source"),
				get("10_OP_NOTLESS_10.tree")
			);
	}

	@Test public void $10_LESS_OR_EQUAL_10() {
		helper.compileAndVerify(
				get("10_LESS_OR_EQUAL_10.source"),
				get("10_LESS_OR_EQUAL_10.tree")
			);
	}

	@Test public void $10_OP_NOTGREATER_10() {
		helper.compileAndVerify(
				get("10_OP_NOTGREATER_10.source"),
				get("10_OP_NOTGREATER_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_GREATER_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_10.source"),
				get("X_OP_PLUS_1_GREATER_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_OP_GREATER_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_GREATER_10.source"),
				get("X_OP_PLUS_1_OP_GREATER_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_LESS_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_10.source"),
				get("X_OP_PLUS_1_LESS_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_OP_LESS_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_LESS_10.source"),
				get("X_OP_PLUS_1_OP_LESS_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_EQUAL_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_EQUAL_10.source"),
				get("X_OP_PLUS_1_EQUAL_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_OP_EQUAL_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_EQUAL_10.source"),
				get("X_OP_PLUS_1_OP_EQUAL_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_GREATER_OR_EQUAL_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_10.source"),
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_OP_NOTLESS_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTLESS_10.source"),
				get("X_OP_PLUS_1_OP_NOTLESS_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_LESS_OR_EQUAL_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_OR_EQUAL_10.source"),
				get("X_OP_PLUS_1_LESS_OR_EQUAL_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_OP_NOTGREATER_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTGREATER_10.source"),
				get("X_OP_PLUS_1_OP_NOTGREATER_10.tree")
			);
	}

	@Test public void X_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_GREATER_X_OP_PLUS_1.source"),
				get("X_GREATER_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_GREATER_X_OP_PLUS_1.source"),
				get("X_OP_GREATER_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_LESS_X_OP_PLUS_1.source"),
				get("X_LESS_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_LESS_X_OP_PLUS_1.source"),
				get("X_OP_LESS_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_EQUAL_X_OP_PLUS_1.source"),
				get("X_EQUAL_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_EQUAL_X_OP_PLUS_1.source"),
				get("X_OP_EQUAL_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_GREATER_OR_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_GREATER_OR_EQUAL_X_OP_PLUS_1.source"),
				get("X_GREATER_OR_EQUAL_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_NOTLESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_NOTLESS_X_OP_PLUS_1.source"),
				get("X_OP_NOTLESS_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_LESS_OR_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_LESS_OR_EQUAL_X_OP_PLUS_1.source"),
				get("X_LESS_OR_EQUAL_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_NOTGREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_NOTGREATER_X_OP_PLUS_1.source"),
				get("X_OP_NOTGREATER_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_GREATER_X_OP_PLUS_1.source"),
				get("10_GREATER_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_OP_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_OP_GREATER_X_OP_PLUS_1.source"),
				get("10_OP_GREATER_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_LESS_X_OP_PLUS_1.source"),
				get("10_LESS_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_OP_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_OP_LESS_X_OP_PLUS_1.source"),
				get("10_OP_LESS_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_EQUAL_X_OP_PLUS_1.source"),
				get("10_EQUAL_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_OP_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_OP_EQUAL_X_OP_PLUS_1.source"),
				get("10_OP_EQUAL_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_GREATER_OR_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_GREATER_OR_EQUAL_X_OP_PLUS_1.source"),
				get("10_GREATER_OR_EQUAL_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_OP_NOTLESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_OP_NOTLESS_X_OP_PLUS_1.source"),
				get("10_OP_NOTLESS_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_LESS_OR_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_LESS_OR_EQUAL_X_OP_PLUS_1.source"),
				get("10_LESS_OR_EQUAL_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_OP_NOTGREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_OP_NOTGREATER_X_OP_PLUS_1.source"),
				get("10_OP_NOTGREATER_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_GREATER_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_OP_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_GREATER_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_OP_GREATER_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_LESS_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_OP_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_LESS_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_OP_LESS_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_EQUAL_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_EQUAL_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_OP_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_EQUAL_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_OP_EQUAL_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_GREATER_OR_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_OP_NOTLESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTLESS_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_OP_NOTLESS_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_LESS_OR_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_OR_EQUAL_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_LESS_OR_EQUAL_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_OP_NOTGREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTGREATER_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_OP_NOTGREATER_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_IS_NOT_GREATER_THAN_10() {
		helper.compileAndVerify(
				get("X_IS_NOT_GREATER_THAN_10.source"),
				get("X_IS_NOT_GREATER_THAN_10.tree")
			);
	}

	@Test public void X_IS_NOT_OP_GREATER_10() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_GREATER_10.source"),
				get("X_IS_NOT_OP_GREATER_10.tree")
			);
	}

	@Test public void X_IS_NOT_LESS_THAN_10() {
		helper.compileAndVerify(
				get("X_IS_NOT_LESS_THAN_10.source"),
				get("X_IS_NOT_LESS_THAN_10.tree")
			);
	}

	@Test public void X_IS_NOT_OP_LESS_10() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_LESS_10.source"),
				get("X_IS_NOT_OP_LESS_10.tree")
			);
	}

	@Test public void X_IS_NOT_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("X_IS_NOT_EQUAL_TO_10.source"),
				get("X_IS_NOT_EQUAL_TO_10.tree")
			);
	}

	@Test public void X_IS_NOT_OP_EQUAL_10() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_EQUAL_10.source"),
				get("X_IS_NOT_OP_EQUAL_10.tree")
			);
	}

	@Test public void X_IS_GREATER_THAN_OR_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_10.source"),
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_10.tree")
			);
	}

	@Test public void X_IS_LESS_THAN_OR_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("X_IS_LESS_THAN_OR_EQUAL_TO_10.source"),
				get("X_IS_LESS_THAN_OR_EQUAL_TO_10.tree")
			);
	}

	@Test public void $10_IS_NOT_GREATER_THAN_10() {
		helper.compileAndVerify(
				get("10_IS_NOT_GREATER_THAN_10.source"),
				get("10_IS_NOT_GREATER_THAN_10.tree")
			);
	}

	@Test public void $10_IS_NOT_OP_GREATER_10() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_GREATER_10.source"),
				get("10_IS_NOT_OP_GREATER_10.tree")
			);
	}

	@Test public void $10_IS_NOT_LESS_THAN_10() {
		helper.compileAndVerify(
				get("10_IS_NOT_LESS_THAN_10.source"),
				get("10_IS_NOT_LESS_THAN_10.tree")
			);
	}

	@Test public void $10_IS_NOT_OP_LESS_10() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_LESS_10.source"),
				get("10_IS_NOT_OP_LESS_10.tree")
			);
	}

	@Test public void $10_IS_NOT_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("10_IS_NOT_EQUAL_TO_10.source"),
				get("10_IS_NOT_EQUAL_TO_10.tree")
			);
	}

	@Test public void $10_IS_NOT_OP_EQUAL_10() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_EQUAL_10.source"),
				get("10_IS_NOT_OP_EQUAL_10.tree")
			);
	}

	@Test public void $10_IS_GREATER_THAN_OR_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_10.source"),
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_10.tree")
			);
	}

	@Test public void $10_IS_LESS_THAN_OR_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("10_IS_LESS_THAN_OR_EQUAL_TO_10.source"),
				get("10_IS_LESS_THAN_OR_EQUAL_TO_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_NOT_GREATER_THAN_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_10.source"),
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_NOT_OP_GREATER_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_10.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_NOT_LESS_THAN_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_10.source"),
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_NOT_OP_LESS_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_10.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_NOT_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_10.source"),
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_NOT_OP_EQUAL_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_10.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_10.source"),
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_10.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_10.source"),
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_10.tree")
			);
	}

	@Test public void X_IS_NOT_GREATER_THAN_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_GREATER_THAN_X_OP_PLUS_1.source"),
				get("X_IS_NOT_GREATER_THAN_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_IS_NOT_OP_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_GREATER_X_OP_PLUS_1.source"),
				get("X_IS_NOT_OP_GREATER_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_IS_NOT_LESS_THAN_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_LESS_THAN_X_OP_PLUS_1.source"),
				get("X_IS_NOT_LESS_THAN_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_IS_NOT_OP_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_LESS_X_OP_PLUS_1.source"),
				get("X_IS_NOT_OP_LESS_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_IS_NOT_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_EQUAL_TO_X_OP_PLUS_1.source"),
				get("X_IS_NOT_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_IS_NOT_OP_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_EQUAL_X_OP_PLUS_1.source"),
				get("X_IS_NOT_OP_EQUAL_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1.source"),
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1.source"),
				get("X_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_IS_NOT_GREATER_THAN_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_GREATER_THAN_X_OP_PLUS_1.source"),
				get("10_IS_NOT_GREATER_THAN_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_IS_NOT_OP_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_GREATER_X_OP_PLUS_1.source"),
				get("10_IS_NOT_OP_GREATER_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_IS_NOT_LESS_THAN_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_LESS_THAN_X_OP_PLUS_1.source"),
				get("10_IS_NOT_LESS_THAN_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_IS_NOT_OP_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_LESS_X_OP_PLUS_1.source"),
				get("10_IS_NOT_OP_LESS_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_IS_NOT_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_EQUAL_TO_X_OP_PLUS_1.source"),
				get("10_IS_NOT_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_IS_NOT_OP_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_EQUAL_X_OP_PLUS_1.source"),
				get("10_IS_NOT_OP_EQUAL_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1.source"),
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}

	@Test public void $10_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1.source"),
				get("10_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_NOT_GREATER_THAN_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_NOT_OP_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_NOT_LESS_THAN_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_NOT_OP_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_NOT_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_NOT_OP_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}

	@Test public void X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_GREATER_Y_AND_OP_GREATER_Y() {
		helper.compileAndVerify(
				get("X_GREATER_Y_AND_OP_GREATER_Y.source"),
				get("X_GREATER_Y_AND_OP_GREATER_Y.tree")
			);
	}
	
	@Test public void X_OP_GREATER_Y_OR_LESS_Y() {
		helper.compileAndVerify(
				get("X_OP_GREATER_Y_OR_LESS_Y.source"),
				get("X_OP_GREATER_Y_OR_LESS_Y.tree")
			);
	}
	
	@Test public void X_LESS_Y_AND_NOT_OP_LESS_Y() {
		helper.compileAndVerify(
				get("X_LESS_Y_AND_NOT_OP_LESS_Y.source"),
				get("X_LESS_Y_AND_NOT_OP_LESS_Y.tree")
			);
	}
	
	@Test public void X_OP_LESS_Y_OR_NOT_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_OP_LESS_Y_OR_NOT_EQUAL_Y.source"),
				get("X_OP_LESS_Y_OR_NOT_EQUAL_Y.tree")
			);
	}
	
	@Test public void X_EQUAL_Y_AND_OP_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_EQUAL_Y_AND_OP_EQUAL_Y.source"),
				get("X_EQUAL_Y_AND_OP_EQUAL_Y.tree")
			);
	}
	
	@Test public void X_OP_EQUAL_Y_OR_GREATER_OR_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_OP_EQUAL_Y_OR_GREATER_OR_EQUAL_Y.source"),
				get("X_OP_EQUAL_Y_OR_GREATER_OR_EQUAL_Y.tree")
			);
	}
	
	@Test public void X_GREATER_OR_EQUAL_Y_AND_NOT_OP_NOTLESS_Y() {
		helper.compileAndVerify(
				get("X_GREATER_OR_EQUAL_Y_AND_NOT_OP_NOTLESS_Y.source"),
				get("X_GREATER_OR_EQUAL_Y_AND_NOT_OP_NOTLESS_Y.tree")
			);
	}
	
	@Test public void X_OP_NOTLESS_Y_OR_NOT_LESS_OR_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_OP_NOTLESS_Y_OR_NOT_LESS_OR_EQUAL_Y.source"),
				get("X_OP_NOTLESS_Y_OR_NOT_LESS_OR_EQUAL_Y.tree")
			);
	}
	
	@Test public void X_LESS_OR_EQUAL_Y_AND_OP_NOTGREATER_Y() {
		helper.compileAndVerify(
				get("X_LESS_OR_EQUAL_Y_AND_OP_NOTGREATER_Y.source"),
				get("X_LESS_OR_EQUAL_Y_AND_OP_NOTGREATER_Y.tree")
			);
	}
	
	@Test public void X_OP_NOTGREATER_Y_OR_GREATER_Y() {
		helper.compileAndVerify(
				get("X_OP_NOTGREATER_Y_OR_GREATER_Y.source"),
				get("X_OP_NOTGREATER_Y_OR_GREATER_Y.tree")
			);
	}
	
	@Test public void $10_GREATER_Y_AND_NOT_OP_GREATER_Y() {
		helper.compileAndVerify(
				get("10_GREATER_Y_AND_NOT_OP_GREATER_Y.source"),
				get("10_GREATER_Y_AND_NOT_OP_GREATER_Y.tree")
			);
	}
	
	@Test public void $10_OP_GREATER_Y_OR_NOT_LESS_Y() {
		helper.compileAndVerify(
				get("10_OP_GREATER_Y_OR_NOT_LESS_Y.source"),
				get("10_OP_GREATER_Y_OR_NOT_LESS_Y.tree")
			);
	}
	
	@Test public void $10_LESS_Y_AND_OP_LESS_Y() {
		helper.compileAndVerify(
				get("10_LESS_Y_AND_OP_LESS_Y.source"),
				get("10_LESS_Y_AND_OP_LESS_Y.tree")
			);
	}
	
	@Test public void $10_OP_LESS_Y_OR_EQUAL_Y() {
		helper.compileAndVerify(
				get("10_OP_LESS_Y_OR_EQUAL_Y.source"),
				get("10_OP_LESS_Y_OR_EQUAL_Y.tree")
			);
	}
	
	@Test public void $10_EQUAL_Y_AND_NOT_OP_EQUAL_Y() {
		helper.compileAndVerify(
				get("10_EQUAL_Y_AND_NOT_OP_EQUAL_Y.source"),
				get("10_EQUAL_Y_AND_NOT_OP_EQUAL_Y.tree")
			);
	}
	
	@Test public void $10_OP_EQUAL_Y_OR_NOT_GREATER_OR_EQUAL_Y() {
		helper.compileAndVerify(
				get("10_OP_EQUAL_Y_OR_NOT_GREATER_OR_EQUAL_Y.source"),
				get("10_OP_EQUAL_Y_OR_NOT_GREATER_OR_EQUAL_Y.tree")
			);
	}
	
	@Test public void $10_GREATER_OR_EQUAL_Y_AND_OP_NOTLESS_Y() {
		helper.compileAndVerify(
				get("10_GREATER_OR_EQUAL_Y_AND_OP_NOTLESS_Y.source"),
				get("10_GREATER_OR_EQUAL_Y_AND_OP_NOTLESS_Y.tree")
			);
	}
	
	@Test public void $10_OP_NOTLESS_Y_OR_LESS_OR_EQUAL_Y() {
		helper.compileAndVerify(
				get("10_OP_NOTLESS_Y_OR_LESS_OR_EQUAL_Y.source"),
				get("10_OP_NOTLESS_Y_OR_LESS_OR_EQUAL_Y.tree")
			);
	}
	
	@Test public void $10_LESS_OR_EQUAL_Y_AND_NOT_OP_NOTGREATER_Y() {
		helper.compileAndVerify(
				get("10_LESS_OR_EQUAL_Y_AND_NOT_OP_NOTGREATER_Y.source"),
				get("10_LESS_OR_EQUAL_Y_AND_NOT_OP_NOTGREATER_Y.tree")
			);
	}
	
	@Test public void $10_OP_NOTGREATER_Y_OR_NOT_GREATER_Y() {
		helper.compileAndVerify(
				get("10_OP_NOTGREATER_Y_OR_NOT_GREATER_Y.source"),
				get("10_OP_NOTGREATER_Y_OR_NOT_GREATER_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_GREATER_Y_AND_OP_GREATER_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_Y_AND_OP_GREATER_Y.source"),
				get("X_OP_PLUS_1_GREATER_Y_AND_OP_GREATER_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_GREATER_Y_OR_LESS_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_GREATER_Y_OR_LESS_Y.source"),
				get("X_OP_PLUS_1_OP_GREATER_Y_OR_LESS_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_LESS_Y_AND_NOT_OP_LESS_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_Y_AND_NOT_OP_LESS_Y.source"),
				get("X_OP_PLUS_1_LESS_Y_AND_NOT_OP_LESS_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_LESS_Y_OR_NOT_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_LESS_Y_OR_NOT_EQUAL_Y.source"),
				get("X_OP_PLUS_1_OP_LESS_Y_OR_NOT_EQUAL_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_EQUAL_Y_AND_OP_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_EQUAL_Y_AND_OP_EQUAL_Y.source"),
				get("X_OP_PLUS_1_EQUAL_Y_AND_OP_EQUAL_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_EQUAL_Y_OR_GREATER_OR_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_EQUAL_Y_OR_GREATER_OR_EQUAL_Y.source"),
				get("X_OP_PLUS_1_OP_EQUAL_Y_OR_GREATER_OR_EQUAL_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_GREATER_OR_EQUAL_Y_AND_NOT_OP_NOTLESS_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_Y_AND_NOT_OP_NOTLESS_Y.source"),
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_Y_AND_NOT_OP_NOTLESS_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_NOTLESS_Y_OR_NOT_LESS_OR_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTLESS_Y_OR_NOT_LESS_OR_EQUAL_Y.source"),
				get("X_OP_PLUS_1_OP_NOTLESS_Y_OR_NOT_LESS_OR_EQUAL_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_LESS_OR_EQUAL_Y_AND_OP_NOTGREATER_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_OR_EQUAL_Y_AND_OP_NOTGREATER_Y.source"),
				get("X_OP_PLUS_1_LESS_OR_EQUAL_Y_AND_OP_NOTGREATER_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_NOTGREATER_Y_OR_GREATER_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTGREATER_Y_OR_GREATER_10.source"),
				get("X_OP_PLUS_1_OP_NOTGREATER_Y_OR_GREATER_10.tree")
			);
	}
	
	@Test public void X_GREATER_10_AND_NOT_OP_GREATER_10() {
		helper.compileAndVerify(
				get("X_GREATER_10_AND_NOT_OP_GREATER_10.source"),
				get("X_GREATER_10_AND_NOT_OP_GREATER_10.tree")
			);
	}
	
	@Test public void X_OP_GREATER_10_OR_NOT_LESS_10() {
		helper.compileAndVerify(
				get("X_OP_GREATER_10_OR_NOT_LESS_10.source"),
				get("X_OP_GREATER_10_OR_NOT_LESS_10.tree")
			);
	}
	
	@Test public void X_LESS_10_AND_OP_LESS_10() {
		helper.compileAndVerify(
				get("X_LESS_10_AND_OP_LESS_10.source"),
				get("X_LESS_10_AND_OP_LESS_10.tree")
			);
	}
	
	@Test public void X_OP_LESS_10_OR_EQUAL_10() {
		helper.compileAndVerify(
				get("X_OP_LESS_10_OR_EQUAL_10.source"),
				get("X_OP_LESS_10_OR_EQUAL_10.tree")
			);
	}
	
	@Test public void X_EQUAL_10_AND_NOT_OP_EQUAL_10() {
		helper.compileAndVerify(
				get("X_EQUAL_10_AND_NOT_OP_EQUAL_10.source"),
				get("X_EQUAL_10_AND_NOT_OP_EQUAL_10.tree")
			);
	}
	
	@Test public void X_OP_EQUAL_10_OR_NOT_GREATER_OR_EQUAL_10() {
		helper.compileAndVerify(
				get("X_OP_EQUAL_10_OR_NOT_GREATER_OR_EQUAL_10.source"),
				get("X_OP_EQUAL_10_OR_NOT_GREATER_OR_EQUAL_10.tree")
			);
	}
	
	@Test public void X_GREATER_OR_EQUAL_10_AND_OP_NOTLESS_10() {
		helper.compileAndVerify(
				get("X_GREATER_OR_EQUAL_10_AND_OP_NOTLESS_10.source"),
				get("X_GREATER_OR_EQUAL_10_AND_OP_NOTLESS_10.tree")
			);
	}
	
	@Test public void X_OP_NOTLESS_10_OR_LESS_OR_EQUAL_10() {
		helper.compileAndVerify(
				get("X_OP_NOTLESS_10_OR_LESS_OR_EQUAL_10.source"),
				get("X_OP_NOTLESS_10_OR_LESS_OR_EQUAL_10.tree")
			);
	}
	
	@Test public void X_LESS_OR_EQUAL_10_AND_NOT_OP_NOTGREATER_10() {
		helper.compileAndVerify(
				get("X_LESS_OR_EQUAL_10_AND_NOT_OP_NOTGREATER_10.source"),
				get("X_LESS_OR_EQUAL_10_AND_NOT_OP_NOTGREATER_10.tree")
			);
	}
	
	@Test public void X_OP_NOTGREATER_10_OR_NOT_GREATER_10() {
		helper.compileAndVerify(
				get("X_OP_NOTGREATER_10_OR_NOT_GREATER_10.source"),
				get("X_OP_NOTGREATER_10_OR_NOT_GREATER_10.tree")
			);
	}
	
	@Test public void $10_GREATER_10_AND_OP_GREATER_10() {
		helper.compileAndVerify(
				get("10_GREATER_10_AND_OP_GREATER_10.source"),
				get("10_GREATER_10_AND_OP_GREATER_10.tree")
			);
	}
	
	@Test public void $10_OP_GREATER_10_OR_LESS_10() {
		helper.compileAndVerify(
				get("10_OP_GREATER_10_OR_LESS_10.source"),
				get("10_OP_GREATER_10_OR_LESS_10.tree")
			);
	}
	
	@Test public void $10_LESS_10_AND_NOT_OP_LESS_10() {
		helper.compileAndVerify(
				get("10_LESS_10_AND_NOT_OP_LESS_10.source"),
				get("10_LESS_10_AND_NOT_OP_LESS_10.tree")
			);
	}
	
	@Test public void $10_OP_LESS_10_OR_NOT_EQUAL_10() {
		helper.compileAndVerify(
				get("10_OP_LESS_10_OR_NOT_EQUAL_10.source"),
				get("10_OP_LESS_10_OR_NOT_EQUAL_10.tree")
			);
	}
	
	@Test public void $10_EQUAL_10_AND_OP_EQUAL_10() {
		helper.compileAndVerify(
				get("10_EQUAL_10_AND_OP_EQUAL_10.source"),
				get("10_EQUAL_10_AND_OP_EQUAL_10.tree")
			);
	}
	
	@Test public void $10_OP_EQUAL_10_OR_GREATER_OR_EQUAL_10() {
		helper.compileAndVerify(
				get("10_OP_EQUAL_10_OR_GREATER_OR_EQUAL_10.source"),
				get("10_OP_EQUAL_10_OR_GREATER_OR_EQUAL_10.tree")
			);
	}
	
	@Test public void $10_GREATER_OR_EQUAL_10_AND_NOT_OP_NOTLESS_10() {
		helper.compileAndVerify(
				get("10_GREATER_OR_EQUAL_10_AND_NOT_OP_NOTLESS_10.source"),
				get("10_GREATER_OR_EQUAL_10_AND_NOT_OP_NOTLESS_10.tree")
			);
	}
	
	@Test public void $10_OP_NOTLESS_10_OR_NOT_LESS_OR_EQUAL_10() {
		helper.compileAndVerify(
				get("10_OP_NOTLESS_10_OR_NOT_LESS_OR_EQUAL_10.source"),
				get("10_OP_NOTLESS_10_OR_NOT_LESS_OR_EQUAL_10.tree")
			);
	}
	
	@Test public void $10_LESS_OR_EQUAL_10_AND_OP_NOTGREATER_10() {
		helper.compileAndVerify(
				get("10_LESS_OR_EQUAL_10_AND_OP_NOTGREATER_10.source"),
				get("10_LESS_OR_EQUAL_10_AND_OP_NOTGREATER_10.tree")
			);
	}
	
	@Test public void $10_OP_NOTGREATER_10_OR_GREATER_10() {
		helper.compileAndVerify(
				get("10_OP_NOTGREATER_10_OR_GREATER_10.source"),
				get("10_OP_NOTGREATER_10_OR_GREATER_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_GREATER_10_AND_NOT_OP_GREATER_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_10_AND_NOT_OP_GREATER_10.source"),
				get("X_OP_PLUS_1_GREATER_10_AND_NOT_OP_GREATER_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_GREATER_10_OR_NOT_LESS_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_GREATER_10_OR_NOT_LESS_10.source"),
				get("X_OP_PLUS_1_OP_GREATER_10_OR_NOT_LESS_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_LESS_10_AND_OP_LESS_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_10_AND_OP_LESS_10.source"),
				get("X_OP_PLUS_1_LESS_10_AND_OP_LESS_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_LESS_10_OR_EQUAL_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_LESS_10_OR_EQUAL_10.source"),
				get("X_OP_PLUS_1_OP_LESS_10_OR_EQUAL_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_EQUAL_10_AND_NOT_OP_EQUAL_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_EQUAL_10_AND_NOT_OP_EQUAL_10.source"),
				get("X_OP_PLUS_1_EQUAL_10_AND_NOT_OP_EQUAL_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_EQUAL_10_OR_NOT_GREATER_OR_EQUAL_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_EQUAL_10_OR_NOT_GREATER_OR_EQUAL_10.source"),
				get("X_OP_PLUS_1_OP_EQUAL_10_OR_NOT_GREATER_OR_EQUAL_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_GREATER_OR_EQUAL_10_AND_OP_NOTLESS_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_10_AND_OP_NOTLESS_10.source"),
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_10_AND_OP_NOTLESS_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_NOTLESS_10_OR_LESS_OR_EQUAL_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTLESS_10_OR_LESS_OR_EQUAL_10.source"),
				get("X_OP_PLUS_1_OP_NOTLESS_10_OR_LESS_OR_EQUAL_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_LESS_OR_EQUAL_10_AND_NOT_OP_NOTGREATER_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_OR_EQUAL_10_AND_NOT_OP_NOTGREATER_10.source"),
				get("X_OP_PLUS_1_LESS_OR_EQUAL_10_AND_NOT_OP_NOTGREATER_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_NOTGREATER_10_OR_NOT_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTGREATER_10_OR_NOT_GREATER_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_OP_NOTGREATER_10_OR_NOT_GREATER_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_GREATER_X_OP_PLUS_1_AND_OP_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_GREATER_X_OP_PLUS_1_AND_OP_GREATER_X_OP_PLUS_1.source"),
				get("X_GREATER_X_OP_PLUS_1_AND_OP_GREATER_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_GREATER_X_OP_PLUS_1_OR_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_GREATER_X_OP_PLUS_1_OR_LESS_X_OP_PLUS_1.source"),
				get("X_OP_GREATER_X_OP_PLUS_1_OR_LESS_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_LESS_X_OP_PLUS_1_AND_NOT_OP_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_LESS_X_OP_PLUS_1_AND_NOT_OP_LESS_X_OP_PLUS_1.source"),
				get("X_LESS_X_OP_PLUS_1_AND_NOT_OP_LESS_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_LESS_X_OP_PLUS_1_OR_NOT_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_LESS_X_OP_PLUS_1_OR_NOT_EQUAL_X_OP_PLUS_1.source"),
				get("X_OP_LESS_X_OP_PLUS_1_OR_NOT_EQUAL_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_EQUAL_X_OP_PLUS_1_AND_OP_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_EQUAL_X_OP_PLUS_1_AND_OP_EQUAL_X_OP_PLUS_1.source"),
				get("X_EQUAL_X_OP_PLUS_1_AND_OP_EQUAL_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_EQUAL_X_OP_PLUS_1_OR_GREATER_OR_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_EQUAL_X_OP_PLUS_1_OR_GREATER_OR_EQUAL_X_OP_PLUS_1.source"),
				get("X_OP_EQUAL_X_OP_PLUS_1_OR_GREATER_OR_EQUAL_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_NOT_OP_NOTLESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_NOT_OP_NOTLESS_X_OP_PLUS_1.source"),
				get("X_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_NOT_OP_NOTLESS_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_NOTLESS_X_OP_PLUS_1_OR_NOT_LESS_OR_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_NOTLESS_X_OP_PLUS_1_OR_NOT_LESS_OR_EQUAL_X_OP_PLUS_1.source"),
				get("X_OP_NOTLESS_X_OP_PLUS_1_OR_NOT_LESS_OR_EQUAL_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_LESS_OR_EQUAL_X_OP_PLUS_1_AND_OP_NOTGREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_LESS_OR_EQUAL_X_OP_PLUS_1_AND_OP_NOTGREATER_X_OP_PLUS_1.source"),
				get("X_LESS_OR_EQUAL_X_OP_PLUS_1_AND_OP_NOTGREATER_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_NOTGREATER_X_OP_PLUS_1_OR_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_NOTGREATER_X_OP_PLUS_1_OR_GREATER_X_OP_PLUS_1.source"),
				get("X_OP_NOTGREATER_X_OP_PLUS_1_OR_GREATER_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_GREATER_X_OP_PLUS_1_AND_NOT_OP_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_GREATER_X_OP_PLUS_1_AND_NOT_OP_GREATER_X_OP_PLUS_1.source"),
				get("10_GREATER_X_OP_PLUS_1_AND_NOT_OP_GREATER_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_OP_GREATER_X_OP_PLUS_1_OR_NOT_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_OP_GREATER_X_OP_PLUS_1_OR_NOT_LESS_X_OP_PLUS_1.source"),
				get("10_OP_GREATER_X_OP_PLUS_1_OR_NOT_LESS_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_LESS_X_OP_PLUS_1_AND_OP_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_LESS_X_OP_PLUS_1_AND_OP_LESS_X_OP_PLUS_1.source"),
				get("10_LESS_X_OP_PLUS_1_AND_OP_LESS_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_OP_LESS_X_OP_PLUS_1_OR_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_OP_LESS_X_OP_PLUS_1_OR_EQUAL_X_OP_PLUS_1.source"),
				get("10_OP_LESS_X_OP_PLUS_1_OR_EQUAL_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_EQUAL_X_OP_PLUS_1_AND_NOT_OP_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_EQUAL_X_OP_PLUS_1_AND_NOT_OP_EQUAL_X_OP_PLUS_1.source"),
				get("10_EQUAL_X_OP_PLUS_1_AND_NOT_OP_EQUAL_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_OP_EQUAL_X_OP_PLUS_1_OR_NOT_GREATER_OR_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_OP_EQUAL_X_OP_PLUS_1_OR_NOT_GREATER_OR_EQUAL_X_OP_PLUS_1.source"),
				get("10_OP_EQUAL_X_OP_PLUS_1_OR_NOT_GREATER_OR_EQUAL_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_OP_NOTLESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_OP_NOTLESS_X_OP_PLUS_1.source"),
				get("10_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_OP_NOTLESS_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_OP_NOTLESS_X_OP_PLUS_1_OR_LESS_OR_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_OP_NOTLESS_X_OP_PLUS_1_OR_LESS_OR_EQUAL_X_OP_PLUS_1.source"),
				get("10_OP_NOTLESS_X_OP_PLUS_1_OR_LESS_OR_EQUAL_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_LESS_OR_EQUAL_X_OP_PLUS_1_AND_NOT_OP_NOTGREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_LESS_OR_EQUAL_X_OP_PLUS_1_AND_NOT_OP_NOTGREATER_X_OP_PLUS_1.source"),
				get("10_LESS_OR_EQUAL_X_OP_PLUS_1_AND_NOT_OP_NOTGREATER_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_OP_NOTGREATER_X_OP_PLUS_1_OR_NOT_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_OP_NOTGREATER_X_OP_PLUS_1_OR_NOT_GREATER_X_OP_PLUS_1.source"),
				get("10_OP_NOTGREATER_X_OP_PLUS_1_OR_NOT_GREATER_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_GREATER_X_OP_PLUS_1_AND_OP_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_X_OP_PLUS_1_AND_OP_GREATER_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_GREATER_X_OP_PLUS_1_AND_OP_GREATER_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_GREATER_X_OP_PLUS_1_OR_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_GREATER_X_OP_PLUS_1_OR_LESS_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_OP_GREATER_X_OP_PLUS_1_OR_LESS_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_LESS_X_OP_PLUS_1_AND_NOT_OP_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_X_OP_PLUS_1_AND_NOT_OP_LESS_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_LESS_X_OP_PLUS_1_AND_NOT_OP_LESS_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_LESS_X_OP_PLUS_1_OR_NOT_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_LESS_X_OP_PLUS_1_OR_NOT_EQUAL_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_OP_LESS_X_OP_PLUS_1_OR_NOT_EQUAL_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_EQUAL_X_OP_PLUS_1_AND_OP_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_EQUAL_X_OP_PLUS_1_AND_OP_EQUAL_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_EQUAL_X_OP_PLUS_1_AND_OP_EQUAL_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_EQUAL_X_OP_PLUS_1_OR_GREATER_OR_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_EQUAL_X_OP_PLUS_1_OR_GREATER_OR_EQUAL_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_OP_EQUAL_X_OP_PLUS_1_OR_GREATER_OR_EQUAL_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_NOT_OP_NOTLESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_NOT_OP_NOTLESS_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_NOT_OP_NOTLESS_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_NOTLESS_X_OP_PLUS_1_OR_NOT_LESS_OR_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTLESS_X_OP_PLUS_1_OR_NOT_LESS_OR_EQUAL_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_OP_NOTLESS_X_OP_PLUS_1_OR_NOT_LESS_OR_EQUAL_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_LESS_OR_EQUAL_X_OP_PLUS_1_AND_OP_NOTGREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_OR_EQUAL_X_OP_PLUS_1_AND_OP_NOTGREATER_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_LESS_OR_EQUAL_X_OP_PLUS_1_AND_OP_NOTGREATER_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_NOTGREATER_X_OP_PLUS_1_OR_NOT_GREATER_THAN_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTGREATER_X_OP_PLUS_1_OR_NOT_GREATER_THAN_Y.source"),
				get("X_OP_PLUS_1_OP_NOTGREATER_X_OP_PLUS_1_OR_NOT_GREATER_THAN_Y.tree")
			);
	}
	
	@Test public void X_IS_NOT_GREATER_THAN_Y_AND_NOT_NOT_OP_GREATER_Y() {
		helper.compileAndVerify(
				get("X_IS_NOT_GREATER_THAN_Y_AND_NOT_NOT_OP_GREATER_Y.source"),
				get("X_IS_NOT_GREATER_THAN_Y_AND_NOT_NOT_OP_GREATER_Y.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_GREATER_Y_OR_NOT_NOT_LESS_THAN_Y() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_GREATER_Y_OR_NOT_NOT_LESS_THAN_Y.source"),
				get("X_IS_NOT_OP_GREATER_Y_OR_NOT_NOT_LESS_THAN_Y.tree")
			);
	}
	
	@Test public void X_IS_NOT_LESS_THAN_Y_AND_NOT_OP_LESS_Y() {
		helper.compileAndVerify(
				get("X_IS_NOT_LESS_THAN_Y_AND_NOT_OP_LESS_Y.source"),
				get("X_IS_NOT_LESS_THAN_Y_AND_NOT_OP_LESS_Y.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_LESS_Y_OR_NOT_EQUAL_TO_Y() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_LESS_Y_OR_NOT_EQUAL_TO_Y.source"),
				get("X_IS_NOT_OP_LESS_Y_OR_NOT_EQUAL_TO_Y.tree")
			);
	}
	
	@Test public void X_IS_NOT_EQUAL_TO_Y_AND_NOT_NOT_OP_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_IS_NOT_EQUAL_TO_Y_AND_NOT_NOT_OP_EQUAL_Y.source"),
				get("X_IS_NOT_EQUAL_TO_Y_AND_NOT_NOT_OP_EQUAL_Y.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_EQUAL_Y_OR_NOT_GREATER_THAN_OR_EQUAL_TO_Y() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_EQUAL_Y_OR_NOT_GREATER_THAN_OR_EQUAL_TO_Y.source"),
				get("X_IS_NOT_OP_EQUAL_Y_OR_NOT_GREATER_THAN_OR_EQUAL_TO_Y.tree")
			);
	}
	
	@Test public void X_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_LESS_THAN_OR_EQUAL_TO_Y() {
		helper.compileAndVerify(
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_LESS_THAN_OR_EQUAL_TO_Y.source"),
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_LESS_THAN_OR_EQUAL_TO_Y.tree")
			);
	}
	
	@Test public void X_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_NOT_GREATER_THAN_Y() {
		helper.compileAndVerify(
				get("X_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_NOT_GREATER_THAN_Y.source"),
				get("X_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_NOT_GREATER_THAN_Y.tree")
			);
	}
	
	@Test public void $10_IS_NOT_GREATER_THAN_Y_AND_NOT_NOT_OP_GREATER_Y() {
		helper.compileAndVerify(
				get("10_IS_NOT_GREATER_THAN_Y_AND_NOT_NOT_OP_GREATER_Y.source"),
				get("10_IS_NOT_GREATER_THAN_Y_AND_NOT_NOT_OP_GREATER_Y.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_GREATER_Y_OR_NOT_NOT_LESS_THAN_Y() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_GREATER_Y_OR_NOT_NOT_LESS_THAN_Y.source"),
				get("10_IS_NOT_OP_GREATER_Y_OR_NOT_NOT_LESS_THAN_Y.tree")
			);
	}
	
	@Test public void $10_IS_NOT_LESS_THAN_Y_AND_NOT_OP_LESS_Y() {
		helper.compileAndVerify(
				get("10_IS_NOT_LESS_THAN_Y_AND_NOT_OP_LESS_Y.source"),
				get("10_IS_NOT_LESS_THAN_Y_AND_NOT_OP_LESS_Y.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_LESS_Y_OR_NOT_EQUAL_TO_Y() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_LESS_Y_OR_NOT_EQUAL_TO_Y.source"),
				get("10_IS_NOT_OP_LESS_Y_OR_NOT_EQUAL_TO_Y.tree")
			);
	}
	
	@Test public void $10_IS_NOT_EQUAL_TO_Y_AND_NOT_NOT_OP_EQUAL_Y() {
		helper.compileAndVerify(
				get("10_IS_NOT_EQUAL_TO_Y_AND_NOT_NOT_OP_EQUAL_Y.source"),
				get("10_IS_NOT_EQUAL_TO_Y_AND_NOT_NOT_OP_EQUAL_Y.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_EQUAL_Y_OR_NOT_GREATER_THAN_OR_EQUAL_TO_Y() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_EQUAL_Y_OR_NOT_GREATER_THAN_OR_EQUAL_TO_Y.source"),
				get("10_IS_NOT_OP_EQUAL_Y_OR_NOT_GREATER_THAN_OR_EQUAL_TO_Y.tree")
			);
	}
	
	@Test public void $10_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_LESS_THAN_OR_EQUAL_TO_Y() {
		helper.compileAndVerify(
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_LESS_THAN_OR_EQUAL_TO_Y.source"),
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_LESS_THAN_OR_EQUAL_TO_Y.tree")
			);
	}
	
	@Test public void $10_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_NOT_GREATER_THAN_Y() {
		helper.compileAndVerify(
				get("10_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_NOT_GREATER_THAN_Y.source"),
				get("10_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_NOT_GREATER_THAN_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_GREATER_THAN_Y_AND_NOT_NOT_OP_GREATER_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_Y_AND_NOT_NOT_OP_GREATER_Y.source"),
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_Y_AND_NOT_NOT_OP_GREATER_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_GREATER_Y_OR_NOT_NOT_LESS_THAN_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_Y_OR_NOT_NOT_LESS_THAN_Y.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_Y_OR_NOT_NOT_LESS_THAN_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_LESS_THAN_Y_AND_NOT_OP_LESS_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_Y_AND_NOT_OP_LESS_Y.source"),
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_Y_AND_NOT_OP_LESS_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_LESS_Y_OR_NOT_EQUAL_TO_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_Y_OR_NOT_EQUAL_TO_Y.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_Y_OR_NOT_EQUAL_TO_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_EQUAL_TO_Y_AND_NOT_NOT_OP_EQUAL_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_Y_AND_NOT_NOT_OP_EQUAL_Y.source"),
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_Y_AND_NOT_NOT_OP_EQUAL_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_EQUAL_Y_OR_NOT_GREATER_THAN_OR_EQUAL_TO_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_Y_OR_NOT_GREATER_THAN_OR_EQUAL_TO_Y.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_Y_OR_NOT_GREATER_THAN_OR_EQUAL_TO_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_LESS_THAN_OR_EQUAL_TO_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_LESS_THAN_OR_EQUAL_TO_Y.source"),
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_LESS_THAN_OR_EQUAL_TO_Y.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_NOT_GREATER_THAN_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_NOT_GREATER_THAN_10.source"),
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_NOT_GREATER_THAN_10.tree")
			);
	}
	
	@Test public void X_IS_NOT_GREATER_THAN_10_AND_NOT_NOT_OP_GREATER_10() {
		helper.compileAndVerify(
				get("X_IS_NOT_GREATER_THAN_10_AND_NOT_NOT_OP_GREATER_10.source"),
				get("X_IS_NOT_GREATER_THAN_10_AND_NOT_NOT_OP_GREATER_10.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_GREATER_10_OR_NOT_NOT_LESS_THAN_10() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_GREATER_10_OR_NOT_NOT_LESS_THAN_10.source"),
				get("X_IS_NOT_OP_GREATER_10_OR_NOT_NOT_LESS_THAN_10.tree")
			);
	}
	
	@Test public void X_IS_NOT_LESS_THAN_10_AND_NOT_OP_LESS_10() {
		helper.compileAndVerify(
				get("X_IS_NOT_LESS_THAN_10_AND_NOT_OP_LESS_10.source"),
				get("X_IS_NOT_LESS_THAN_10_AND_NOT_OP_LESS_10.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_LESS_10_OR_NOT_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_LESS_10_OR_NOT_EQUAL_TO_10.source"),
				get("X_IS_NOT_OP_LESS_10_OR_NOT_EQUAL_TO_10.tree")
			);
	}
	
	@Test public void X_IS_NOT_EQUAL_TO_10_AND_NOT_NOT_OP_EQUAL_10() {
		helper.compileAndVerify(
				get("X_IS_NOT_EQUAL_TO_10_AND_NOT_NOT_OP_EQUAL_10.source"),
				get("X_IS_NOT_EQUAL_TO_10_AND_NOT_NOT_OP_EQUAL_10.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_EQUAL_10_OR_NOT_GREATER_THAN_OR_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_EQUAL_10_OR_NOT_GREATER_THAN_OR_EQUAL_TO_10.source"),
				get("X_IS_NOT_OP_EQUAL_10_OR_NOT_GREATER_THAN_OR_EQUAL_TO_10.tree")
			);
	}
	
	@Test public void X_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_LESS_THAN_OR_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_LESS_THAN_OR_EQUAL_TO_10.source"),
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_LESS_THAN_OR_EQUAL_TO_10.tree")
			);
	}
	
	@Test public void X_IS_LESS_THAN_OR_EQUAL_TO_10_OR_NOT_GREATER_THAN_10() {
		helper.compileAndVerify(
				get("X_IS_LESS_THAN_OR_EQUAL_TO_10_OR_NOT_GREATER_THAN_10.source"),
				get("X_IS_LESS_THAN_OR_EQUAL_TO_10_OR_NOT_GREATER_THAN_10.tree")
			);
	}
	
	@Test public void $10_IS_NOT_GREATER_THAN_10_AND_NOT_NOT_OP_GREATER_10() {
		helper.compileAndVerify(
				get("10_IS_NOT_GREATER_THAN_10_AND_NOT_NOT_OP_GREATER_10.source"),
				get("10_IS_NOT_GREATER_THAN_10_AND_NOT_NOT_OP_GREATER_10.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_GREATER_10_OR_NOT_NOT_LESS_THAN_10() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_GREATER_10_OR_NOT_NOT_LESS_THAN_10.source"),
				get("10_IS_NOT_OP_GREATER_10_OR_NOT_NOT_LESS_THAN_10.tree")
			);
	}
	
	@Test public void $10_IS_NOT_LESS_THAN_10_AND_NOT_OP_LESS_10() {
		helper.compileAndVerify(
				get("10_IS_NOT_LESS_THAN_10_AND_NOT_OP_LESS_10.source"),
				get("10_IS_NOT_LESS_THAN_10_AND_NOT_OP_LESS_10.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_LESS_10_OR_NOT_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_LESS_10_OR_NOT_EQUAL_TO_10.source"),
				get("10_IS_NOT_OP_LESS_10_OR_NOT_EQUAL_TO_10.tree")
			);
	}
	
	@Test public void $10_IS_NOT_EQUAL_TO_10_AND_NOT_NOT_OP_EQUAL_10() {
		helper.compileAndVerify(
				get("10_IS_NOT_EQUAL_TO_10_AND_NOT_NOT_OP_EQUAL_10.source"),
				get("10_IS_NOT_EQUAL_TO_10_AND_NOT_NOT_OP_EQUAL_10.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_EQUAL_10_OR_NOT_GREATER_THAN_OR_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_EQUAL_10_OR_NOT_GREATER_THAN_OR_EQUAL_TO_10.source"),
				get("10_IS_NOT_OP_EQUAL_10_OR_NOT_GREATER_THAN_OR_EQUAL_TO_10.tree")
			);
	}
	
	@Test public void $10_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_LESS_THAN_OR_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_LESS_THAN_OR_EQUAL_TO_10.source"),
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_LESS_THAN_OR_EQUAL_TO_10.tree")
			);
	}
	
	@Test public void $10_IS_LESS_THAN_OR_EQUAL_TO_10_OR_NOT_GREATER_THAN_10() {
		helper.compileAndVerify(
				get("10_IS_LESS_THAN_OR_EQUAL_TO_10_OR_NOT_GREATER_THAN_10.source"),
				get("10_IS_LESS_THAN_OR_EQUAL_TO_10_OR_NOT_GREATER_THAN_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_GREATER_THAN_10_AND_NOT_NOT_OP_GREATER_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_10_AND_NOT_NOT_OP_GREATER_10.source"),
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_10_AND_NOT_NOT_OP_GREATER_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_GREATER_10_OR_NOT_NOT_LESS_THAN_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_10_OR_NOT_NOT_LESS_THAN_10.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_10_OR_NOT_NOT_LESS_THAN_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_LESS_THAN_10_AND_NOT_OP_LESS_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_10_AND_NOT_OP_LESS_10.source"),
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_10_AND_NOT_OP_LESS_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_LESS_10_OR_NOT_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_10_OR_NOT_EQUAL_TO_10.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_10_OR_NOT_EQUAL_TO_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_EQUAL_TO_10_AND_NOT_NOT_OP_EQUAL_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_10_AND_NOT_NOT_OP_EQUAL_10.source"),
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_10_AND_NOT_NOT_OP_EQUAL_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_EQUAL_10_OR_NOT_GREATER_THAN_OR_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_10_OR_NOT_GREATER_THAN_OR_EQUAL_TO_10.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_10_OR_NOT_GREATER_THAN_OR_EQUAL_TO_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_LESS_THAN_OR_EQUAL_TO_10() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_LESS_THAN_OR_EQUAL_TO_10.source"),
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_LESS_THAN_OR_EQUAL_TO_10.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_10_OR_NOT_GREATER_THAN_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_10_OR_NOT_GREATER_THAN_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_10_OR_NOT_GREATER_THAN_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_NOT_OP_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_NOT_OP_GREATER_X_OP_PLUS_1.source"),
				get("X_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_NOT_OP_GREATER_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_NOT_LESS_THAN_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_NOT_LESS_THAN_X_OP_PLUS_1.source"),
				get("X_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_NOT_LESS_THAN_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_NOT_OP_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_NOT_OP_LESS_X_OP_PLUS_1.source"),
				get("X_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_NOT_OP_LESS_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_NOT_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_NOT_EQUAL_TO_X_OP_PLUS_1.source"),
				get("X_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_NOT_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_NOT_OP_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_NOT_OP_EQUAL_X_OP_PLUS_1.source"),
				get("X_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_NOT_OP_EQUAL_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_EQUAL_X_OP_PLUS_1_OR_NOT_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_EQUAL_X_OP_PLUS_1_OR_NOT_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1.source"),
				get("X_IS_NOT_OP_EQUAL_X_OP_PLUS_1_OR_NOT_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1.source"),
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_NOT_GREATER_THAN_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_NOT_GREATER_THAN_X_OP_PLUS_1.source"),
				get("X_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_NOT_GREATER_THAN_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_NOT_OP_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_NOT_OP_GREATER_X_OP_PLUS_1.source"),
				get("10_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_NOT_OP_GREATER_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_NOT_LESS_THAN_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_NOT_LESS_THAN_X_OP_PLUS_1.source"),
				get("10_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_NOT_LESS_THAN_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_NOT_OP_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_NOT_OP_LESS_X_OP_PLUS_1.source"),
				get("10_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_NOT_OP_LESS_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_NOT_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_NOT_EQUAL_TO_X_OP_PLUS_1.source"),
				get("10_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_NOT_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_NOT_OP_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_NOT_OP_EQUAL_X_OP_PLUS_1.source"),
				get("10_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_NOT_OP_EQUAL_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_EQUAL_X_OP_PLUS_1_OR_NOT_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_EQUAL_X_OP_PLUS_1_OR_NOT_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1.source"),
				get("10_IS_NOT_OP_EQUAL_X_OP_PLUS_1_OR_NOT_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1.source"),
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_NOT_GREATER_THAN_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_NOT_GREATER_THAN_X_OP_PLUS_1.source"),
				get("10_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_NOT_GREATER_THAN_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_NOT_OP_GREATER_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_NOT_OP_GREATER_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_NOT_OP_GREATER_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_NOT_LESS_THAN_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_NOT_LESS_THAN_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_NOT_LESS_THAN_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_NOT_OP_LESS_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_NOT_OP_LESS_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_NOT_OP_LESS_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_NOT_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_NOT_EQUAL_TO_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_NOT_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_NOT_OP_EQUAL_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_NOT_OP_EQUAL_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_NOT_OP_EQUAL_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_EQUAL_X_OP_PLUS_1_AND_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_X_OP_PLUS_1_AND_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_X_OP_PLUS_1_AND_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_NOT_GREATER_Y() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_NOT_GREATER_Y.source"),
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_NOT_GREATER_Y.tree")
			);
	}
	
	@Test public void X_GREATER_Y_AND_Z() {
		helper.compileAndVerify(
				get("X_GREATER_Y_AND_Z.source"),
				get("X_GREATER_Y_AND_Z.tree")
			);
	}
	
	@Test public void X_OP_GREATER_Y_OR_Z() {
		helper.compileAndVerify(
				get("X_OP_GREATER_Y_OR_Z.source"),
				get("X_OP_GREATER_Y_OR_Z.tree")
			);
	}
	
	@Test public void X_LESS_Y_AND_NOT_Z() {
		helper.compileAndVerify(
				get("X_LESS_Y_AND_NOT_Z.source"),
				get("X_LESS_Y_AND_NOT_Z.tree")
			);
	}
	
	@Test public void X_OP_LESS_Y_OR_NOT_Z() {
		helper.compileAndVerify(
				get("X_OP_LESS_Y_OR_NOT_Z.source"),
				get("X_OP_LESS_Y_OR_NOT_Z.tree")
			);
	}
	
	@Test public void X_EQUAL_Y_AND_Z() {
		helper.compileAndVerify(
				get("X_EQUAL_Y_AND_Z.source"),
				get("X_EQUAL_Y_AND_Z.tree")
			);
	}
	
	@Test public void X_OP_EQUAL_Y_OR_Z() {
		helper.compileAndVerify(
				get("X_OP_EQUAL_Y_OR_Z.source"),
				get("X_OP_EQUAL_Y_OR_Z.tree")
			);
	}
	
	@Test public void X_GREATER_OR_EQUAL_Y_AND_NOT_Z() {
		helper.compileAndVerify(
				get("X_GREATER_OR_EQUAL_Y_AND_NOT_Z.source"),
				get("X_GREATER_OR_EQUAL_Y_AND_NOT_Z.tree")
			);
	}
	
	@Test public void X_OP_NOTLESS_Y_OR_NOT_Z() {
		helper.compileAndVerify(
				get("X_OP_NOTLESS_Y_OR_NOT_Z.source"),
				get("X_OP_NOTLESS_Y_OR_NOT_Z.tree")
			);
	}
	
	@Test public void X_LESS_OR_EQUAL_Y_AND_Z() {
		helper.compileAndVerify(
				get("X_LESS_OR_EQUAL_Y_AND_Z.source"),
				get("X_LESS_OR_EQUAL_Y_AND_Z.tree")
			);
	}
	
	@Test public void X_OP_NOTGREATER_Y_OR_Z() {
		helper.compileAndVerify(
				get("X_OP_NOTGREATER_Y_OR_Z.source"),
				get("X_OP_NOTGREATER_Y_OR_Z.tree")
			);
	}
	
	@Test public void $10_GREATER_Y_AND_NOT_Z() {
		helper.compileAndVerify(
				get("10_GREATER_Y_AND_NOT_Z.source"),
				get("10_GREATER_Y_AND_NOT_Z.tree")
			);
	}
	
	@Test public void $10_OP_GREATER_Y_OR_NOT_Z() {
		helper.compileAndVerify(
				get("10_OP_GREATER_Y_OR_NOT_Z.source"),
				get("10_OP_GREATER_Y_OR_NOT_Z.tree")
			);
	}
	
	@Test public void $10_LESS_Y_AND_Z() {
		helper.compileAndVerify(
				get("10_LESS_Y_AND_Z.source"),
				get("10_LESS_Y_AND_Z.tree")
			);
	}
	
	@Test public void $10_OP_LESS_Y_OR_Z() {
		helper.compileAndVerify(
				get("10_OP_LESS_Y_OR_Z.source"),
				get("10_OP_LESS_Y_OR_Z.tree")
			);
	}
	
	@Test public void $10_EQUAL_Y_AND_NOT_Z() {
		helper.compileAndVerify(
				get("10_EQUAL_Y_AND_NOT_Z.source"),
				get("10_EQUAL_Y_AND_NOT_Z.tree")
			);
	}
	
	@Test public void $10_OP_EQUAL_Y_OR_NOT_Z() {
		helper.compileAndVerify(
				get("10_OP_EQUAL_Y_OR_NOT_Z.source"),
				get("10_OP_EQUAL_Y_OR_NOT_Z.tree")
			);
	}
	
	@Test public void $10_GREATER_OR_EQUAL_Y_AND_Z() {
		helper.compileAndVerify(
				get("10_GREATER_OR_EQUAL_Y_AND_Z.source"),
				get("10_GREATER_OR_EQUAL_Y_AND_Z.tree")
			);
	}
	
	@Test public void $10_OP_NOTLESS_Y_OR_Z() {
		helper.compileAndVerify(
				get("10_OP_NOTLESS_Y_OR_Z.source"),
				get("10_OP_NOTLESS_Y_OR_Z.tree")
			);
	}
	
	@Test public void $10_LESS_OR_EQUAL_Y_AND_NOT_Z() {
		helper.compileAndVerify(
				get("10_LESS_OR_EQUAL_Y_AND_NOT_Z.source"),
				get("10_LESS_OR_EQUAL_Y_AND_NOT_Z.tree")
			);
	}
	
	@Test public void $10_OP_NOTGREATER_Y_OR_NOT_Z() {
		helper.compileAndVerify(
				get("10_OP_NOTGREATER_Y_OR_NOT_Z.source"),
				get("10_OP_NOTGREATER_Y_OR_NOT_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_GREATER_Y_AND_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_Y_AND_Z.source"),
				get("X_OP_PLUS_1_GREATER_Y_AND_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_GREATER_Y_OR_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_GREATER_Y_OR_Z.source"),
				get("X_OP_PLUS_1_OP_GREATER_Y_OR_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_LESS_Y_AND_NOT_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_Y_AND_NOT_Z.source"),
				get("X_OP_PLUS_1_LESS_Y_AND_NOT_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_LESS_Y_OR_NOT_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_LESS_Y_OR_NOT_Z.source"),
				get("X_OP_PLUS_1_OP_LESS_Y_OR_NOT_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_EQUAL_Y_AND_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_EQUAL_Y_AND_Z.source"),
				get("X_OP_PLUS_1_EQUAL_Y_AND_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_EQUAL_Y_OR_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_EQUAL_Y_OR_Z.source"),
				get("X_OP_PLUS_1_OP_EQUAL_Y_OR_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_GREATER_OR_EQUAL_Y_AND_NOT_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_Y_AND_NOT_Z.source"),
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_Y_AND_NOT_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_NOTLESS_Y_OR_NOT_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTLESS_Y_OR_NOT_Z.source"),
				get("X_OP_PLUS_1_OP_NOTLESS_Y_OR_NOT_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_LESS_OR_EQUAL_Y_AND_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_OR_EQUAL_Y_AND_Z.source"),
				get("X_OP_PLUS_1_LESS_OR_EQUAL_Y_AND_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_NOTGREATER_Y_OR_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTGREATER_Y_OR_20.source"),
				get("X_OP_PLUS_1_OP_NOTGREATER_Y_OR_20.tree")
			);
	}
	
	@Test public void X_GREATER_10_AND_NOT_20() {
		helper.compileAndVerify(
				get("X_GREATER_10_AND_NOT_20.source"),
				get("X_GREATER_10_AND_NOT_20.tree")
			);
	}
	
	@Test public void X_OP_GREATER_10_OR_NOT_20() {
		helper.compileAndVerify(
				get("X_OP_GREATER_10_OR_NOT_20.source"),
				get("X_OP_GREATER_10_OR_NOT_20.tree")
			);
	}
	
	@Test public void X_LESS_10_AND_20() {
		helper.compileAndVerify(
				get("X_LESS_10_AND_20.source"),
				get("X_LESS_10_AND_20.tree")
			);
	}
	
	@Test public void X_OP_LESS_10_OR_20() {
		helper.compileAndVerify(
				get("X_OP_LESS_10_OR_20.source"),
				get("X_OP_LESS_10_OR_20.tree")
			);
	}
	
	@Test public void X_EQUAL_10_AND_NOT_20() {
		helper.compileAndVerify(
				get("X_EQUAL_10_AND_NOT_20.source"),
				get("X_EQUAL_10_AND_NOT_20.tree")
			);
	}
	
	@Test public void X_OP_EQUAL_10_OR_NOT_20() {
		helper.compileAndVerify(
				get("X_OP_EQUAL_10_OR_NOT_20.source"),
				get("X_OP_EQUAL_10_OR_NOT_20.tree")
			);
	}
	
	@Test public void X_GREATER_OR_EQUAL_10_AND_20() {
		helper.compileAndVerify(
				get("X_GREATER_OR_EQUAL_10_AND_20.source"),
				get("X_GREATER_OR_EQUAL_10_AND_20.tree")
			);
	}
	
	@Test public void X_OP_NOTLESS_10_OR_20() {
		helper.compileAndVerify(
				get("X_OP_NOTLESS_10_OR_20.source"),
				get("X_OP_NOTLESS_10_OR_20.tree")
			);
	}
	
	@Test public void X_LESS_OR_EQUAL_10_AND_NOT_20() {
		helper.compileAndVerify(
				get("X_LESS_OR_EQUAL_10_AND_NOT_20.source"),
				get("X_LESS_OR_EQUAL_10_AND_NOT_20.tree")
			);
	}
	
	@Test public void X_OP_NOTGREATER_10_OR_NOT_20() {
		helper.compileAndVerify(
				get("X_OP_NOTGREATER_10_OR_NOT_20.source"),
				get("X_OP_NOTGREATER_10_OR_NOT_20.tree")
			);
	}
	
	@Test public void $10_GREATER_10_AND_20() {
		helper.compileAndVerify(
				get("10_GREATER_10_AND_20.source"),
				get("10_GREATER_10_AND_20.tree")
			);
	}
	
	@Test public void $10_OP_GREATER_10_OR_20() {
		helper.compileAndVerify(
				get("10_OP_GREATER_10_OR_20.source"),
				get("10_OP_GREATER_10_OR_20.tree")
			);
	}
	
	@Test public void $10_LESS_10_AND_NOT_20() {
		helper.compileAndVerify(
				get("10_LESS_10_AND_NOT_20.source"),
				get("10_LESS_10_AND_NOT_20.tree")
			);
	}
	
	@Test public void $10_OP_LESS_10_OR_NOT_20() {
		helper.compileAndVerify(
				get("10_OP_LESS_10_OR_NOT_20.source"),
				get("10_OP_LESS_10_OR_NOT_20.tree")
			);
	}
	
	@Test public void $10_EQUAL_10_AND_20() {
		helper.compileAndVerify(
				get("10_EQUAL_10_AND_20.source"),
				get("10_EQUAL_10_AND_20.tree")
			);
	}
	
	@Test public void $10_OP_EQUAL_10_OR_20() {
		helper.compileAndVerify(
				get("10_OP_EQUAL_10_OR_20.source"),
				get("10_OP_EQUAL_10_OR_20.tree")
			);
	}
	
	@Test public void $10_GREATER_OR_EQUAL_10_AND_NOT_20() {
		helper.compileAndVerify(
				get("10_GREATER_OR_EQUAL_10_AND_NOT_20.source"),
				get("10_GREATER_OR_EQUAL_10_AND_NOT_20.tree")
			);
	}
	
	@Test public void $10_OP_NOTLESS_10_OR_NOT_20() {
		helper.compileAndVerify(
				get("10_OP_NOTLESS_10_OR_NOT_20.source"),
				get("10_OP_NOTLESS_10_OR_NOT_20.tree")
			);
	}
	
	@Test public void $10_LESS_OR_EQUAL_10_AND_20() {
		helper.compileAndVerify(
				get("10_LESS_OR_EQUAL_10_AND_20.source"),
				get("10_LESS_OR_EQUAL_10_AND_20.tree")
			);
	}
	
	@Test public void $10_OP_NOTGREATER_10_OR_20() {
		helper.compileAndVerify(
				get("10_OP_NOTGREATER_10_OR_20.source"),
				get("10_OP_NOTGREATER_10_OR_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_GREATER_10_AND_NOT_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_10_AND_NOT_20.source"),
				get("X_OP_PLUS_1_GREATER_10_AND_NOT_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_GREATER_10_OR_NOT_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_GREATER_10_OR_NOT_20.source"),
				get("X_OP_PLUS_1_OP_GREATER_10_OR_NOT_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_LESS_10_AND_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_10_AND_20.source"),
				get("X_OP_PLUS_1_LESS_10_AND_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_LESS_10_OR_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_LESS_10_OR_20.source"),
				get("X_OP_PLUS_1_OP_LESS_10_OR_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_EQUAL_10_AND_NOT_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_EQUAL_10_AND_NOT_20.source"),
				get("X_OP_PLUS_1_EQUAL_10_AND_NOT_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_EQUAL_10_OR_NOT_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_EQUAL_10_OR_NOT_20.source"),
				get("X_OP_PLUS_1_OP_EQUAL_10_OR_NOT_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_GREATER_OR_EQUAL_10_AND_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_10_AND_20.source"),
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_10_AND_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_NOTLESS_10_OR_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTLESS_10_OR_20.source"),
				get("X_OP_PLUS_1_OP_NOTLESS_10_OR_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_LESS_OR_EQUAL_10_AND_NOT_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_OR_EQUAL_10_AND_NOT_20.source"),
				get("X_OP_PLUS_1_LESS_OR_EQUAL_10_AND_NOT_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_NOTGREATER_10_OR_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTGREATER_10_OR_NOT_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_OP_NOTGREATER_10_OR_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_GREATER_X_OP_PLUS_1_AND_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_GREATER_X_OP_PLUS_1_AND_Z_OP_PLUS_1.source"),
				get("X_GREATER_X_OP_PLUS_1_AND_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_GREATER_X_OP_PLUS_1_OR_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_GREATER_X_OP_PLUS_1_OR_Z_OP_PLUS_1.source"),
				get("X_OP_GREATER_X_OP_PLUS_1_OR_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_LESS_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_LESS_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.source"),
				get("X_LESS_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_LESS_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_LESS_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.source"),
				get("X_OP_LESS_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1.source"),
				get("X_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_EQUAL_X_OP_PLUS_1_OR_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_EQUAL_X_OP_PLUS_1_OR_Z_OP_PLUS_1.source"),
				get("X_OP_EQUAL_X_OP_PLUS_1_OR_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.source"),
				get("X_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_NOTLESS_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_NOTLESS_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.source"),
				get("X_OP_NOTLESS_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_LESS_OR_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_LESS_OR_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1.source"),
				get("X_LESS_OR_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_NOTGREATER_X_OP_PLUS_1_OR_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_NOTGREATER_X_OP_PLUS_1_OR_Z_OP_PLUS_1.source"),
				get("X_OP_NOTGREATER_X_OP_PLUS_1_OR_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_GREATER_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_GREATER_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.source"),
				get("10_GREATER_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_OP_GREATER_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_OP_GREATER_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.source"),
				get("10_OP_GREATER_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_LESS_X_OP_PLUS_1_AND_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_LESS_X_OP_PLUS_1_AND_Z_OP_PLUS_1.source"),
				get("10_LESS_X_OP_PLUS_1_AND_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_OP_LESS_X_OP_PLUS_1_OR_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_OP_LESS_X_OP_PLUS_1_OR_Z_OP_PLUS_1.source"),
				get("10_OP_LESS_X_OP_PLUS_1_OR_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_EQUAL_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_EQUAL_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.source"),
				get("10_EQUAL_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_OP_EQUAL_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_OP_EQUAL_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.source"),
				get("10_OP_EQUAL_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1.source"),
				get("10_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_OP_NOTLESS_X_OP_PLUS_1_OR_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_OP_NOTLESS_X_OP_PLUS_1_OR_Z_OP_PLUS_1.source"),
				get("10_OP_NOTLESS_X_OP_PLUS_1_OR_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_LESS_OR_EQUAL_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_LESS_OR_EQUAL_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.source"),
				get("10_LESS_OR_EQUAL_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_OP_NOTGREATER_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_OP_NOTGREATER_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.source"),
				get("10_OP_NOTGREATER_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_GREATER_X_OP_PLUS_1_AND_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_X_OP_PLUS_1_AND_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_GREATER_X_OP_PLUS_1_AND_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_GREATER_X_OP_PLUS_1_OR_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_GREATER_X_OP_PLUS_1_OR_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_OP_GREATER_X_OP_PLUS_1_OR_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_LESS_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_LESS_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_LESS_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_LESS_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_OP_LESS_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_EQUAL_X_OP_PLUS_1_OR_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_EQUAL_X_OP_PLUS_1_OR_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_OP_EQUAL_X_OP_PLUS_1_OR_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_GREATER_OR_EQUAL_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_NOTLESS_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTLESS_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_OP_NOTLESS_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_LESS_OR_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_LESS_OR_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_LESS_OR_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_OP_NOTGREATER_X_OP_PLUS_1_OR_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_OP_NOTGREATER_X_OP_PLUS_1_OR_Z.source"),
				get("X_OP_PLUS_1_OP_NOTGREATER_X_OP_PLUS_1_OR_Z.tree")
			);
	}
	
	@Test public void X_IS_NOT_GREATER_THAN_Y_AND_NOT_Z() {
		helper.compileAndVerify(
				get("X_IS_NOT_GREATER_THAN_Y_AND_NOT_Z.source"),
				get("X_IS_NOT_GREATER_THAN_Y_AND_NOT_Z.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_GREATER_Y_OR_NOT_Z() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_GREATER_Y_OR_NOT_Z.source"),
				get("X_IS_NOT_OP_GREATER_Y_OR_NOT_Z.tree")
			);
	}
	
	@Test public void X_IS_NOT_LESS_THAN_Y_AND_Z() {
		helper.compileAndVerify(
				get("X_IS_NOT_LESS_THAN_Y_AND_Z.source"),
				get("X_IS_NOT_LESS_THAN_Y_AND_Z.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_LESS_Y_OR_Z() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_LESS_Y_OR_Z.source"),
				get("X_IS_NOT_OP_LESS_Y_OR_Z.tree")
			);
	}
	
	@Test public void X_IS_NOT_EQUAL_TO_Y_AND_NOT_Z() {
		helper.compileAndVerify(
				get("X_IS_NOT_EQUAL_TO_Y_AND_NOT_Z.source"),
				get("X_IS_NOT_EQUAL_TO_Y_AND_NOT_Z.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_EQUAL_Y_OR_NOT_Z() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_EQUAL_Y_OR_NOT_Z.source"),
				get("X_IS_NOT_OP_EQUAL_Y_OR_NOT_Z.tree")
			);
	}
	
	@Test public void X_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_Z() {
		helper.compileAndVerify(
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_Z.source"),
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_Z.tree")
			);
	}
	
	@Test public void X_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_Z() {
		helper.compileAndVerify(
				get("X_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_Z.source"),
				get("X_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_Z.tree")
			);
	}
	
	@Test public void $10_IS_NOT_GREATER_THAN_Y_AND_NOT_Z() {
		helper.compileAndVerify(
				get("10_IS_NOT_GREATER_THAN_Y_AND_NOT_Z.source"),
				get("10_IS_NOT_GREATER_THAN_Y_AND_NOT_Z.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_GREATER_Y_OR_NOT_Z() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_GREATER_Y_OR_NOT_Z.source"),
				get("10_IS_NOT_OP_GREATER_Y_OR_NOT_Z.tree")
			);
	}
	
	@Test public void $10_IS_NOT_LESS_THAN_Y_AND_Z() {
		helper.compileAndVerify(
				get("10_IS_NOT_LESS_THAN_Y_AND_Z.source"),
				get("10_IS_NOT_LESS_THAN_Y_AND_Z.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_LESS_Y_OR_Z() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_LESS_Y_OR_Z.source"),
				get("10_IS_NOT_OP_LESS_Y_OR_Z.tree")
			);
	}
	
	@Test public void $10_IS_NOT_EQUAL_TO_Y_AND_NOT_Z() {
		helper.compileAndVerify(
				get("10_IS_NOT_EQUAL_TO_Y_AND_NOT_Z.source"),
				get("10_IS_NOT_EQUAL_TO_Y_AND_NOT_Z.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_EQUAL_Y_OR_NOT_Z() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_EQUAL_Y_OR_NOT_Z.source"),
				get("10_IS_NOT_OP_EQUAL_Y_OR_NOT_Z.tree")
			);
	}
	
	@Test public void $10_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_Z() {
		helper.compileAndVerify(
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_Z.source"),
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_Z.tree")
			);
	}
	
	@Test public void $10_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_Z() {
		helper.compileAndVerify(
				get("10_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_Z.source"),
				get("10_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_GREATER_THAN_Y_AND_NOT_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_Y_AND_NOT_Z.source"),
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_Y_AND_NOT_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_GREATER_Y_OR_NOT_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_Y_OR_NOT_Z.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_Y_OR_NOT_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_LESS_THAN_Y_AND_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_Y_AND_Z.source"),
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_Y_AND_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_LESS_Y_OR_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_Y_OR_Z.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_Y_OR_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_EQUAL_TO_Y_AND_NOT_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_Y_AND_NOT_Z.source"),
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_Y_AND_NOT_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_EQUAL_Y_OR_NOT_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_Y_OR_NOT_Z.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_Y_OR_NOT_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_Z.source"),
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_Y_AND_Z.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_20.source"),
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_Y_OR_20.tree")
			);
	}
	
	@Test public void X_IS_NOT_GREATER_THAN_10_AND_NOT_20() {
		helper.compileAndVerify(
				get("X_IS_NOT_GREATER_THAN_10_AND_NOT_20.source"),
				get("X_IS_NOT_GREATER_THAN_10_AND_NOT_20.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_GREATER_10_OR_NOT_20() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_GREATER_10_OR_NOT_20.source"),
				get("X_IS_NOT_OP_GREATER_10_OR_NOT_20.tree")
			);
	}
	
	@Test public void X_IS_NOT_LESS_THAN_10_AND_20() {
		helper.compileAndVerify(
				get("X_IS_NOT_LESS_THAN_10_AND_20.source"),
				get("X_IS_NOT_LESS_THAN_10_AND_20.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_LESS_10_OR_20() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_LESS_10_OR_20.source"),
				get("X_IS_NOT_OP_LESS_10_OR_20.tree")
			);
	}
	
	@Test public void X_IS_NOT_EQUAL_TO_10_AND_NOT_20() {
		helper.compileAndVerify(
				get("X_IS_NOT_EQUAL_TO_10_AND_NOT_20.source"),
				get("X_IS_NOT_EQUAL_TO_10_AND_NOT_20.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_EQUAL_10_OR_NOT_20() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_EQUAL_10_OR_NOT_20.source"),
				get("X_IS_NOT_OP_EQUAL_10_OR_NOT_20.tree")
			);
	}
	
	@Test public void X_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_20() {
		helper.compileAndVerify(
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_20.source"),
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_20.tree")
			);
	}
	
	@Test public void X_IS_LESS_THAN_OR_EQUAL_TO_10_OR_20() {
		helper.compileAndVerify(
				get("X_IS_LESS_THAN_OR_EQUAL_TO_10_OR_20.source"),
				get("X_IS_LESS_THAN_OR_EQUAL_TO_10_OR_20.tree")
			);
	}
	
	@Test public void $10_IS_NOT_GREATER_THAN_10_AND_NOT_20() {
		helper.compileAndVerify(
				get("10_IS_NOT_GREATER_THAN_10_AND_NOT_20.source"),
				get("10_IS_NOT_GREATER_THAN_10_AND_NOT_20.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_GREATER_10_OR_NOT_20() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_GREATER_10_OR_NOT_20.source"),
				get("10_IS_NOT_OP_GREATER_10_OR_NOT_20.tree")
			);
	}
	
	@Test public void $10_IS_NOT_LESS_THAN_10_AND_20() {
		helper.compileAndVerify(
				get("10_IS_NOT_LESS_THAN_10_AND_20.source"),
				get("10_IS_NOT_LESS_THAN_10_AND_20.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_LESS_10_OR_20() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_LESS_10_OR_20.source"),
				get("10_IS_NOT_OP_LESS_10_OR_20.tree")
			);
	}
	
	@Test public void $10_IS_NOT_EQUAL_TO_10_AND_NOT_20() {
		helper.compileAndVerify(
				get("10_IS_NOT_EQUAL_TO_10_AND_NOT_20.source"),
				get("10_IS_NOT_EQUAL_TO_10_AND_NOT_20.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_EQUAL_10_OR_NOT_20() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_EQUAL_10_OR_NOT_20.source"),
				get("10_IS_NOT_OP_EQUAL_10_OR_NOT_20.tree")
			);
	}
	
	@Test public void $10_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_20() {
		helper.compileAndVerify(
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_20.source"),
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_20.tree")
			);
	}
	
	@Test public void $10_IS_LESS_THAN_OR_EQUAL_TO_10_OR_20() {
		helper.compileAndVerify(
				get("10_IS_LESS_THAN_OR_EQUAL_TO_10_OR_20.source"),
				get("10_IS_LESS_THAN_OR_EQUAL_TO_10_OR_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_GREATER_THAN_10_AND_NOT_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_10_AND_NOT_20.source"),
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_10_AND_NOT_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_GREATER_10_OR_NOT_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_10_OR_NOT_20.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_10_OR_NOT_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_LESS_THAN_10_AND_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_10_AND_20.source"),
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_10_AND_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_LESS_10_OR_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_10_OR_20.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_10_OR_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_EQUAL_TO_10_AND_NOT_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_10_AND_NOT_20.source"),
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_10_AND_NOT_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_EQUAL_10_OR_NOT_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_10_OR_NOT_20.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_10_OR_NOT_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_20() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_20.source"),
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_10_AND_20.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_10_OR_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_10_OR_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_10_OR_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.source"),
				get("X_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.source"),
				get("X_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_Z_OP_PLUS_1.source"),
				get("X_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_Z_OP_PLUS_1.source"),
				get("X_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.source"),
				get("X_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_NOT_OP_EQUAL_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_NOT_OP_EQUAL_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.source"),
				get("X_IS_NOT_OP_EQUAL_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_Z_OP_PLUS_1.source"),
				get("X_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_Z_OP_PLUS_1.source"),
				get("X_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.source"),
				get("10_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.source"),
				get("10_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_Z_OP_PLUS_1.source"),
				get("10_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_Z_OP_PLUS_1.source"),
				get("10_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.source"),
				get("10_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_NOT_OP_EQUAL_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_NOT_OP_EQUAL_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.source"),
				get("10_IS_NOT_OP_EQUAL_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_Z_OP_PLUS_1.source"),
				get("10_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void $10_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("10_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_Z_OP_PLUS_1.source"),
				get("10_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_GREATER_THAN_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_GREATER_X_OP_PLUS_1_OR_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_LESS_THAN_X_OP_PLUS_1_AND_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_LESS_X_OP_PLUS_1_OR_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_EQUAL_TO_X_OP_PLUS_1_AND_NOT_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_NOT_OP_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_NOT_OP_EQUAL_X_OP_PLUS_1_AND_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_Z_OP_PLUS_1() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_Z_OP_PLUS_1.source"),
				get("X_OP_PLUS_1_IS_GREATER_THAN_OR_EQUAL_TO_X_OP_PLUS_1_OR_Z_OP_PLUS_1.tree")
			);
	}
	
	@Test public void X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_NOT_Z() {
		helper.compileAndVerify(
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_NOT_Z.source"),
				get("X_OP_PLUS_1_IS_LESS_THAN_OR_EQUAL_TO_X_OP_PLUS_1_AND_NOT_Z.tree")
			);
	}
	
	@Test public void ADDRESS_OF_X_EQUAL_ADDRESS_OF_Y() {
		helper.compileAndVerify(
				get("ADDRESS_OF_X_EQUAL_ADDRESS_OF_Y.source"),
				get("ADDRESS_OF_X_EQUAL_ADDRESS_OF_Y.tree")
			);
	}
	
	@Test public void X_EQUAL_ADDRESS_OF_Y() {
		helper.compileAndVerify(
				get("X_EQUAL_ADDRESS_OF_Y.source"),
				get("X_EQUAL_ADDRESS_OF_Y.tree")
			);
	}
	
	@Test public void ADDRESS_OF_X_EQUAL_Y() {
		helper.compileAndVerify(
				get("ADDRESS_OF_X_EQUAL_Y.source"),
				get("ADDRESS_OF_X_EQUAL_Y.tree")
			);
	}
	
	@Test public void NULL_EQUAL_ADDRESS_OF_Y() {
		helper.compileAndVerify(
				get("NULL_EQUAL_ADDRESS_OF_Y.source"),
				get("NULL_EQUAL_ADDRESS_OF_Y.tree")
			);
	}
	
	@Test public void ADDRESS_OF_X_EQUAL_NULL() {
		helper.compileAndVerify(
				get("ADDRESS_OF_X_EQUAL_NULL.source"),
				get("ADDRESS_OF_X_EQUAL_NULL.tree")
			);
	}
	
	@Test public void ADDRESS_OF_X_IS_NOT_EQUAL_TO_ADDRESS_OF_Y() {
		helper.compileAndVerify(
				get("ADDRESS_OF_X_IS_NOT_EQUAL_TO_ADDRESS_OF_Y.source"),
				get("ADDRESS_OF_X_IS_NOT_EQUAL_TO_ADDRESS_OF_Y.tree")
			);
	}
	
	@Test public void X_IS_NOT_EQUAL_TO_ADDRESS_OF_Y() {
		helper.compileAndVerify(
				get("X_IS_NOT_EQUAL_TO_ADDRESS_OF_Y.source"),
				get("X_IS_NOT_EQUAL_TO_ADDRESS_OF_Y.tree")
			);
	}
	
	@Test public void ADDRESS_OF_X_IS_NOT_EQUAL_TO_Y() {
		helper.compileAndVerify(
				get("ADDRESS_OF_X_IS_NOT_EQUAL_TO_Y.source"),
				get("ADDRESS_OF_X_IS_NOT_EQUAL_TO_Y.tree")
			);
	}
	
	@Test public void NULL_IS_NOT_EQUAL_TO_ADDRESS_OF_Y() {
		helper.compileAndVerify(
				get("NULL_IS_NOT_EQUAL_TO_ADDRESS_OF_Y.source"),
				get("NULL_IS_NOT_EQUAL_TO_ADDRESS_OF_Y.tree")
			);
	}
	
	@Test public void ADDRESS_OF_X_IS_NOT_EQUAL_TO_NULL() {
		helper.compileAndVerify(
				get("ADDRESS_OF_X_IS_NOT_EQUAL_TO_NULL.source"),
				get("ADDRESS_OF_X_IS_NOT_EQUAL_TO_NULL.tree")
			);
	}
	
	@Test public void ADDRESS_OF_X_NOT_OP_EQUAL_ADDRESS_OF_Y() {
		helper.compileAndVerify(
				get("ADDRESS_OF_X_NOT_OP_EQUAL_ADDRESS_OF_Y.source"),
				get("ADDRESS_OF_X_NOT_OP_EQUAL_ADDRESS_OF_Y.tree")
			);
	}
	
	@Test public void X_NOT_OP_EQUAL_ADDRESS_OF_Y() {
		helper.compileAndVerify(
				get("X_NOT_OP_EQUAL_ADDRESS_OF_Y.source"),
				get("X_NOT_OP_EQUAL_ADDRESS_OF_Y.tree")
			);
	}
	
	@Test public void ADDRESS_OF_X_NOT_OP_EQUAL_Y() {
		helper.compileAndVerify(
				get("ADDRESS_OF_X_NOT_OP_EQUAL_Y.source"),
				get("ADDRESS_OF_X_NOT_OP_EQUAL_Y.tree")
			);
	}
	
	@Test public void NULL_NOT_OP_EQUAL_ADDRESS_OF_Y() {
		helper.compileAndVerify(
				get("NULL_NOT_OP_EQUAL_ADDRESS_OF_Y.source"),
				get("NULL_NOT_OP_EQUAL_ADDRESS_OF_Y.tree")
			);
	}
	
	@Test public void ADDRESS_OF_X_NOT_OP_EQUAL_NULL() {
		helper.compileAndVerify(
				get("ADDRESS_OF_X_NOT_OP_EQUAL_NULL.source"),
				get("ADDRESS_OF_X_NOT_OP_EQUAL_NULL.tree")
			);
	}
	
	@Test public void X_EQUAL_SELF() {
		helper.compileAndVerify(
				get("X_EQUAL_SELF.source"),
				get("X_EQUAL_SELF.tree")
			);
	}
	
	@Test public void SELF_EQUAL_Y() {
		helper.compileAndVerify(
				get("SELF_EQUAL_Y.source"),
				get("SELF_EQUAL_Y.tree")
			);
	}
	
	@Test public void SELF_EQUAL_NULL() {
		helper.compileAndVerify(
				get("SELF_EQUAL_NULL.source"),
				get("SELF_EQUAL_NULL.tree")
			);
	}
	
	@Test public void NULL_EQUAL_SELF() {
		helper.compileAndVerify(
				get("NULL_EQUAL_SELF.source"),
				get("NULL_EQUAL_SELF.tree")
			);
	}
	
	@Test public void X_IS_NOT_EQUAL_TO_SELF() {
		helper.compileAndVerify(
				get("X_IS_NOT_EQUAL_TO_SELF.source"),
				get("X_IS_NOT_EQUAL_TO_SELF.tree")
			);
	}
	
	@Test public void SELF_IS_NOT_EQUAL_TO_Y() {
		helper.compileAndVerify(
				get("SELF_IS_NOT_EQUAL_TO_Y.source"),
				get("SELF_IS_NOT_EQUAL_TO_Y.tree")
			);
	}
	
	@Test public void SELF_IS_NOT_EQUAL_TO_NULL() {
		helper.compileAndVerify(
				get("SELF_IS_NOT_EQUAL_TO_NULL.source"),
				get("SELF_IS_NOT_EQUAL_TO_NULL.tree")
			);
	}
	
	@Test public void NULL_IS_NOT_EQUAL_TO_SELF() {
		helper.compileAndVerify(
				get("NULL_IS_NOT_EQUAL_TO_SELF.source"),
				get("NULL_IS_NOT_EQUAL_TO_SELF.tree")
			);
	}
	
	@Test public void X_NOT_OP_EQUAL_SELF() {
		helper.compileAndVerify(
				get("X_NOT_OP_EQUAL_SELF.source"),
				get("X_NOT_OP_EQUAL_SELF.tree")
			);
	}
	
	@Test public void SELF_NOT_OP_EQUAL_Y() {
		helper.compileAndVerify(
				get("SELF_NOT_OP_EQUAL_Y.source"),
				get("SELF_NOT_OP_EQUAL_Y.tree")
			);
	}
	
	@Test public void SELF_NOT_OP_EQUAL_NULL() {
		helper.compileAndVerify(
				get("SELF_NOT_OP_EQUAL_NULL.source"),
				get("SELF_NOT_OP_EQUAL_NULL.tree")
			);
	}

	@Test public void NULL_NOT_OP_EQUAL_SELF() {
		helper.compileAndVerify(
				get("NULL_NOT_OP_EQUAL_SELF.source"),
				get("NULL_NOT_OP_EQUAL_SELF.tree")
			);
	}

	@Test public void X_POSITIVE() {
		helper.compileAndVerify(
				get("X_POSITIVE.source"),
				get("X_POSITIVE.tree")
			);
	}

	@Test public void X_NEGATIVE() {
		helper.compileAndVerify(
				get("X_NEGATIVE.source"),
				get("X_NEGATIVE.tree")
			);
	}

	@Test public void X_ZERO() {
		helper.compileAndVerify(
				get("X_ZERO.source"),
				get("X_ZERO.tree")
			);
	}

	@Test public void X_IS_POSITIVE() {
		helper.compileAndVerify(
				get("X_IS_POSITIVE.source"),
				get("X_IS_POSITIVE.tree")
			);
	}

	@Test public void X_IS_NEGATIVE() {
		helper.compileAndVerify(
				get("X_IS_NEGATIVE.source"),
				get("X_IS_NEGATIVE.tree")
			);
	}

	@Test public void X_IS_ZERO() {
		helper.compileAndVerify(
				get("X_IS_ZERO.source"),
				get("X_IS_ZERO.tree")
			);
	}

	@Test public void X_IS_NOT_POSITIVE() {
		helper.compileAndVerify(
				get("X_IS_NOT_POSITIVE.source"),
				get("X_IS_NOT_POSITIVE.tree")
			);
	}

	@Test public void X_IS_NOT_NEGATIVE() {
		helper.compileAndVerify(
				get("X_IS_NOT_NEGATIVE.source"),
				get("X_IS_NOT_NEGATIVE.tree")
			);
	}

	@Test public void X_IS_NOT_ZERO() {
		helper.compileAndVerify(
				get("X_IS_NOT_ZERO.source"),
				get("X_IS_NOT_ZERO.tree")
			);
	}

	@Test public void X_NOT_POSITIVE() {
		helper.compileAndVerify(
				get("X_NOT_POSITIVE.source"),
				get("X_NOT_POSITIVE.tree")
			);
	}

	@Test public void X_NOT_NEGATIVE() {
		helper.compileAndVerify(
				get("X_NOT_NEGATIVE.source"),
				get("X_NOT_NEGATIVE.tree")
			);
	}

	@Test public void X_NOT_ZERO() {
		helper.compileAndVerify(
				get("X_NOT_ZERO.source"),
				get("X_NOT_ZERO.tree")
			);
	}

	@Test public void X_OP_MINUS_POSITIVE() {
		helper.compileAndVerify(
				get("X_OP_MINUS_POSITIVE.source"),
				get("X_OP_MINUS_POSITIVE.tree")
			);
	}

	@Test public void X_OP_MINUS_NEGATIVE() {
		helper.compileAndVerify(
				get("X_OP_MINUS_NEGATIVE.source"),
				get("X_OP_MINUS_NEGATIVE.tree")
			);
	}

	@Test public void X_OP_MINUS_ZERO() {
		helper.compileAndVerify(
				get("X_OP_MINUS_ZERO.source"),
				get("X_OP_MINUS_ZERO.tree")
			);
	}

	@Test public void NOT_X_NUMERIC() {
		helper.compileAndVerify(
				get("NOT_X_NUMERIC.source"),
				get("NOT_X_NUMERIC.tree")
			);
	}

	@Test public void NOT_CONDITION() {
		helper.compileAndVerify(
				get("NOT_CONDITION.source"),
				get("NOT_CONDITION.tree")
			);
	}

	@Test public void NOT_X_GREATER_Y() {
		helper.compileAndVerify(
				get("NOT_X_GREATER_Y.source"),
				get("NOT_X_GREATER_Y.tree")
			);
	}

	@Test public void NOT_X_POSITIVE() {
		helper.compileAndVerify(
				get("NOT_X_POSITIVE.source"),
				get("NOT_X_POSITIVE.tree")
			);
	}

	@Test public void X_NUMERIC_AND_CONDITION() {
		helper.compileAndVerify(
				get("X_NUMERIC_AND_CONDITION.source"),
				get("X_NUMERIC_AND_CONDITION.tree")
			);
	}

	@Test public void CONDITION_AND_X_GREATER_Y() {
		helper.compileAndVerify(
				get("CONDITION_AND_X_GREATER_Y.source"),
				get("CONDITION_AND_X_GREATER_Y.tree")
			);
	}

	@Test public void X_GREATER_Y_AND_X_POSITIVE() {
		helper.compileAndVerify(
				get("X_GREATER_Y_AND_X_POSITIVE.source"),
				get("X_GREATER_Y_AND_X_POSITIVE.tree")
			);
	}

	@Test public void X_POSITIVE_AND_X_NUMERIC() {
		helper.compileAndVerify(
				get("X_POSITIVE_AND_X_NUMERIC.source"),
				get("X_POSITIVE_AND_X_NUMERIC.tree")
			);
	}

	@Test public void NOT_X_NUMERIC_OR_CONDITION() {
		helper.compileAndVerify(
				get("NOT_X_NUMERIC_OR_CONDITION.source"),
				get("NOT_X_NUMERIC_OR_CONDITION.tree")
			);
	}

	@Test public void NOT_CONDITION_OR_X_GREATER_Y() {
		helper.compileAndVerify(
				get("NOT_CONDITION_OR_X_GREATER_Y.source"),
				get("NOT_CONDITION_OR_X_GREATER_Y.tree")
			);
	}

	@Test public void NOT_X_GREATER_Y_OR_X_POSITIVE() {
		helper.compileAndVerify(
				get("NOT_X_GREATER_Y_OR_X_POSITIVE.source"),
				get("NOT_X_GREATER_Y_OR_X_POSITIVE.tree")
			);
	}

	@Test public void NOT_X_POSITIVE_OR_X_NUMERIC() {
		helper.compileAndVerify(
				get("NOT_X_POSITIVE_OR_X_NUMERIC.source"),
				get("NOT_X_POSITIVE_OR_X_NUMERIC.tree")
			);
	}
}
