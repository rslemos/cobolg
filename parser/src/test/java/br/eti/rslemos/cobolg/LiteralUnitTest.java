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

import static br.eti.rslemos.cobolg.PostProcessingCompiler.parserForFixedFormat;

import java.io.IOException;
import java.io.Reader;

import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.LiteralContext;

public class LiteralUnitTest {
	private static CompilerHelper<LiteralContext> helper = new CompilerHelper<LiteralContext>() {
		@Override protected LiteralContext parsePart() { return parser.literal(); }
		@Override public void compileAndVerify(String source, String expectedTree) {
			super.compileAndVerify(source, "(literal " + expectedTree + ")");
		}
	};

	// alphanumeric tests are in fixed format
	private static CompilerHelper<LiteralContext> fixedHelper = new CompilerHelper<LiteralContext>() {
		@Override protected LiteralContext parsePart() { return parser.literal(); }
		@Override protected PostProcessingCompiler createCompiler(Reader source) throws IOException { return parserForFixedFormat(source); }
		@Override public void compileAndVerify(String source, String expectedTree) {
			super.compileAndVerify(source, "(literal " + expectedTree + ")");
		}
	};

	@Test public void SINGLY_QUOTED_STRING() {
		fixedHelper.compileAndVerify(
				AlphanumericLiteralUnitTest.get("SINGLY_QUOTED_STRING.source"), 
				AlphanumericLiteralUnitTest.get("SINGLY_QUOTED_STRING.tree")
			);
	}
	
	@Test public void DOUBLY_QUOTED_STRING() {
		fixedHelper.compileAndVerify(
				AlphanumericLiteralUnitTest.get("DOUBLY_QUOTED_STRING.source"), 
				AlphanumericLiteralUnitTest.get("DOUBLY_QUOTED_STRING.tree")
			);
	}

	@Test public void SINGLY_QUOTED_HEXSTRING() {
		fixedHelper.compileAndVerify(
				AlphanumericLiteralUnitTest.get("SINGLY_QUOTED_HEXSTRING.source"), 
				AlphanumericLiteralUnitTest.get("SINGLY_QUOTED_HEXSTRING.tree")
			);
	}
	
	@Test public void DOUBLY_QUOTED_HEXSTRING() {
		fixedHelper.compileAndVerify(
				AlphanumericLiteralUnitTest.get("DOUBLY_QUOTED_HEXSTRING.source"), 
				AlphanumericLiteralUnitTest.get("DOUBLY_QUOTED_HEXSTRING.tree")
			);
	}

	@Test public void SIGNED_POSITIVE_INTEGER() {
		helper.compileAndVerify(
				NumericLiteralUnitTest.get("SIGNED_POSITIVE_INTEGER.source"), 
				NumericLiteralUnitTest.get("SIGNED_POSITIVE_INTEGER.tree")
			);
	}
	
	@Test public void POSITIVE_INTEGER() {
		helper.compileAndVerify(
				NumericLiteralUnitTest.get("POSITIVE_INTEGER.source"), 
				NumericLiteralUnitTest.get("POSITIVE_INTEGER.tree")
			);
	}
	
	@Test public void NEGATIVE_INTEGER() {
		helper.compileAndVerify(
				NumericLiteralUnitTest.get("NEGATIVE_INTEGER.source"), 
				NumericLiteralUnitTest.get("NEGATIVE_INTEGER.tree")
			);
	}
	
	@Test public void SIGNED_POSITIVE_FIXEDPOINT() {
		helper.compileAndVerify(
				NumericLiteralUnitTest.get("SIGNED_POSITIVE_FIXEDPOINT.source"),
				NumericLiteralUnitTest.get("SIGNED_POSITIVE_FIXEDPOINT.tree")
			);
	}

	@Test public void POSITIVE_FIXEDPOINT() {
		helper.compileAndVerify(
				NumericLiteralUnitTest.get("POSITIVE_FIXEDPOINT.source"),
				NumericLiteralUnitTest.get("POSITIVE_FIXEDPOINT.tree")
			);
	}

	@Test public void NEGATIVE_FIXEDPOINT() {
		helper.compileAndVerify(
				NumericLiteralUnitTest.get("NEGATIVE_FIXEDPOINT.source"),
				NumericLiteralUnitTest.get("NEGATIVE_FIXEDPOINT.tree")
			);
	}

	@Test public void SIGNED_POSITIVE_FLOATINGPOINT() {
		helper.compileAndVerify(
				NumericLiteralUnitTest.get("SIGNED_POSITIVE_FLOATINGPOINT.source"),
				NumericLiteralUnitTest.get("SIGNED_POSITIVE_FLOATINGPOINT.tree")
			);
	}

	@Test public void POSITIVE_FLOATINGPOINT() {
		helper.compileAndVerify(
				NumericLiteralUnitTest.get("POSITIVE_FLOATINGPOINT.source"),
				NumericLiteralUnitTest.get("POSITIVE_FLOATINGPOINT.tree")
			);
	}

	@Test public void NEGATIVE_FLOATINGPOINT() {
		helper.compileAndVerify(
				NumericLiteralUnitTest.get("NEGATIVE_FLOATINGPOINT.source"),
				NumericLiteralUnitTest.get("NEGATIVE_FLOATINGPOINT.tree")
			);
	}

	@Test public void SIGNED_POSITIVE_FLOATINGPOINT_COMMA() {
		helper.compileAndVerify(
				NumericLiteralUnitTest.get("SIGNED_POSITIVE_FLOATINGPOINT_COMMA.source"),
				NumericLiteralUnitTest.get("SIGNED_POSITIVE_FLOATINGPOINT_COMMA.tree")
			);
	}

	@Test public void POSITIVE_FLOATINGPOINT_COMMA() {
		helper.compileAndVerify(
				NumericLiteralUnitTest.get("POSITIVE_FLOATINGPOINT_COMMA.source"),
				NumericLiteralUnitTest.get("POSITIVE_FLOATINGPOINT_COMMA.tree")
			);
	}

	@Test public void NEGATIVE_FLOATINGPOINT_COMMA() {
		helper.compileAndVerify(
				NumericLiteralUnitTest.get("NEGATIVE_FLOATINGPOINT_COMMA.source"),
				NumericLiteralUnitTest.get("NEGATIVE_FLOATINGPOINT_COMMA.tree")
			);
	}
	
	@Test public void ZERO() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("ZERO.source"),
				FigurativeConstantUnitTest.get("ZERO.tree")
			);
	}

	@Test public void ZEROS() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("ZEROS.source"),
				FigurativeConstantUnitTest.get("ZEROS.tree")
			);
	}

	@Test public void ZEROES() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("ZEROES.source"),
				FigurativeConstantUnitTest.get("ZEROES.tree")
			);
	}

	@Test public void SPACE() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("SPACE.source"),
				FigurativeConstantUnitTest.get("SPACE.tree")
			);
	}

	@Test public void SPACES() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("SPACES.source"),
				FigurativeConstantUnitTest.get("SPACES.tree")
			);
	}

	@Test public void HIGH_VALUE() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("HIGH_VALUE.source"),
				FigurativeConstantUnitTest.get("HIGH_VALUE.tree")
			);
	}

	@Test public void HIGH_VALUES() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("HIGH_VALUES.source"),
				FigurativeConstantUnitTest.get("HIGH_VALUES.tree")
			);
	}

	@Test public void LOW_VALUE() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("LOW_VALUE.source"),
				FigurativeConstantUnitTest.get("LOW_VALUE.tree")
			);
	}

	@Test public void LOW_VALUES() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("LOW_VALUES.source"),
				FigurativeConstantUnitTest.get("LOW_VALUES.tree")
			);
	}

	@Test public void QUOTE() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("QUOTE.source"),
				FigurativeConstantUnitTest.get("QUOTE.tree")
			);
	}

	@Test public void QUOTES() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("QUOTES.source"),
				FigurativeConstantUnitTest.get("QUOTES.tree")
			);
	}

	@Test public void NULL() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("NULL.source"),
				FigurativeConstantUnitTest.get("NULL.tree")
			);
	}

	@Test public void NULLS() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("NULLS.source"),
				FigurativeConstantUnitTest.get("NULLS.tree")
			);
	}

	@Test public void ALL_0() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("ALL_0.source"),
				FigurativeConstantUnitTest.get("ALL_0.tree")
			);
	}

	@Test public void ALL_X() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("ALL_X.source"),
				FigurativeConstantUnitTest.get("ALL_X.tree")
			);
	}

	@Test public void ALL_SPACES() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("ALL_SPACES.source"),
				FigurativeConstantUnitTest.get("ALL_SPACES.tree")
			);
	}

	@Test public void ALL_NULLS() {
		helper.compileAndVerify(
				FigurativeConstantUnitTest.get("ALL_NULLS.source"),
				FigurativeConstantUnitTest.get("ALL_NULLS.tree")
			);
	}
}
