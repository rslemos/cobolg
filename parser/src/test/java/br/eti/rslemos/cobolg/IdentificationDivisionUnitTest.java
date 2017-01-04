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

import br.eti.rslemos.cobolg.COBOLParser.IdentificationDivisionContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
public class IdentificationDivisionUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.identificationDivision");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<IdentificationDivisionContext> helper = new CompilerHelper<IdentificationDivisionContext>() {
		@Override protected IdentificationDivisionContext parsePart() { return parser.identificationDivision(); }
	};

	@Test public void BASIC() {
		helper.compileAndVerify(
				get("BASIC.source"),
				get("BASIC.tree")
			);
	}

	@Test public void OMIT_OPTIONAL_PERIODS() {
		helper.compileAndVerify(
				get("OMIT_OPTIONAL_PERIODS.source"),
				get("OMIT_OPTIONAL_PERIODS.tree")
			);
	}

	@Test public void IS_INITIAL_PROGRAM() {
		helper.compileAndVerify(
				get("IS_INITIAL_PROGRAM.source"),
				get("IS_INITIAL_PROGRAM.tree")
			);
	}

	@Test public void WITH_OPTIONAL_AUTHOR() {
		helper.compileAndVerify(
				get("WITH_OPTIONAL_AUTHOR.source"),
				get("WITH_OPTIONAL_AUTHOR.tree")
			);
	}
}
