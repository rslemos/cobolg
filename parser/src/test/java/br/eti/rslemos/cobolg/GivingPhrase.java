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

import br.eti.rslemos.cobolg.COBOLParser.GivingPhraseContext;

public class GivingPhrase {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.givingPhrase");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<GivingPhraseContext> helper = new CompilerHelper<GivingPhraseContext>() {
		@Override protected GivingPhraseContext parsePart() { return parser.givingPhrase(); }
	};
	
	@Test public void GIVING_X() {
		helper.compileAndVerify(
				get("GIVING_X.source"),
				get("GIVING_X.tree")
			);
	}
	
	@Test public void GIVING_X_ROUNDED() {
		helper.compileAndVerify(
				get("GIVING_X_ROUNDED.source"),
				get("GIVING_X_ROUNDED.tree")
			);
	}
	
	@Test public void GIVING_X_ROUNDED_Y() {
		helper.compileAndVerify(
				get("GIVING_X_ROUNDED_Y.source"),
				get("GIVING_X_ROUNDED_Y.tree")
			);
	}
	
	@Test public void GIVING_X_Y_ROUNDED() {
		helper.compileAndVerify(
				get("GIVING_X_Y_ROUNDED.source"),
				get("GIVING_X_Y_ROUNDED.tree")
			);
	}
}
