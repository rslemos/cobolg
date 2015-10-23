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

import br.eti.rslemos.cobolg.COBOLParser.PictureClauseContext;

public class PictureClauseUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.pictureClause");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<PictureClauseContext> helper = new CompilerHelper<PictureClauseContext>() {
		@Override protected PictureClauseContext parsePart() { return parser.pictureClause(); }
	};

	@Test public void PIC_99999c999() {
		helper.compileAndVerify(
				get("PIC_99999c999.source"),
				get("PIC_99999c999.tree")
			);
	}

	@Test public void PIC_99999c999p() {
		helper.compileAndVerify(
				get("PIC_99999c999p.source"),
				get("PIC_99999c999p.tree")
			);
	}

	@Test public void PIC_99999c999m() {
		helper.compileAndVerify(
				get("PIC_99999c999m.source"),
				get("PIC_99999c999m.tree")
			);
	}

	@Test public void PIC_p9999c999() {
		helper.compileAndVerify(
				get("PIC_p9999c999.source"),
				get("PIC_p9999c999.tree")
			);
	}

	@Test public void PIC_m9999c999() {
		helper.compileAndVerify(
				get("PIC_m9999c999.source"),
				get("PIC_m9999c999.tree")
			);
	}

	@Test public void PIC_pppp9c999() {
		helper.compileAndVerify(
				get("PIC_pppp9c999.source"),
				get("PIC_pppp9c999.tree")
			);
	}

	@Test public void PIC_mmmm9c999() {
		helper.compileAndVerify(
				get("PIC_mmmm9c999.source"),
				get("PIC_mmmm9c999.tree")
			);
	}

	@Test public void PIC_ZZZZ9c999() {
		helper.compileAndVerify(
				get("PIC_ZZZZ9c999.source"),
				get("PIC_ZZZZ9c999.tree")
			);
	}

	@Test public void PIC_pZZZ9c999() {
		helper.compileAndVerify(
				get("PIC_pZZZ9c999.source"),
				get("PIC_pZZZ9c999.tree")
			);
	}

	@Test public void PIC_BBBB9c999() {
		helper.compileAndVerify(
				get("PIC_BBBB9c999.source"),
				get("PIC_BBBB9c999.tree")
			);
	}

	@Test public void PIC_BZZZ9c999() {
		helper.compileAndVerify(
				get("PIC_BZZZ9c999.source"),
				get("PIC_BZZZ9c999.tree")
			);
	}

	@Test public void PIC_pBZZ9c999() {
		helper.compileAndVerify(
				get("PIC_pBZZ9c999.source"),
				get("PIC_pBZZ9c999.tree")
			);
	}

	@Test public void PIC_pBBZ9c999() {
		helper.compileAndVerify(
				get("PIC_pBBZ9c999.source"),
				get("PIC_pBBZ9c999.tree")
			);
	}

	@Test public void PIC_d9999c999() {
		helper.compileAndVerify(
				get("PIC_d9999c999.source"),
				get("PIC_d9999c999.tree")
			);
	}

	@Test public void PIC_dddd9c999() {
		helper.compileAndVerify(
				get("PIC_dddd9c999.source"),
				get("PIC_dddd9c999.tree")
			);
	}

	@Test public void PIC_dZZZ9c999() {
		helper.compileAndVerify(
				get("PIC_dZZZ9c999.source"),
				get("PIC_dZZZ9c999.tree")
			);
	}

	@Test public void PIC_BBdd9c999() {
		helper.compileAndVerify(
				get("PIC_BBdd9c999.source"),
				get("PIC_BBdd9c999.tree")
			);
	}

	@Test public void PIC_S9_5_A_10_() {
		helper.compileAndVerify(
				get("PIC_S9_5_A_10_.source"),
				get("PIC_S9_5_A_10_.tree")
			);
	}

	@Test public void PIC_pd_dZ9c99() {
		helper.compileAndVerify(
				get("PIC_pd_dZ9c99.source"),
				get("PIC_pd_dZ9c99.tree")
			);
	}

	@Test public void PIC_ppZZ9c999() {
		helper.compileAndVerify(
				get("PIC_ppZZ9c999.source"),
				get("PIC_ppZZ9c999.tree")
			);
	}

	@Test public void PIC_999_999_999_99() {
		helper.compileAndVerify(
				get("PIC_999_999_999_99.source"),
				get("PIC_999_999_999_99.tree")
			);
	}

	@Test public void PIC_XXX_XXX_XXX_XX() {
		helper.compileAndVerify(
				get("PIC_XXX_XXX_XXX_XX.source"),
				get("PIC_XXX_XXX_XXX_XX.tree")
			);
	}

}
