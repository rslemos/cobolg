/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2013  Rodrigo Lemos
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

import static br.eti.rslemos.cobolg.DataDescriptionEntryData.source;
import static br.eti.rslemos.cobolg.DataDescriptionEntryData.tree;
import static br.eti.rslemos.cobolg.DataDescriptionEntryData.DataDescriptionEntryClause.OCCURS;
import static br.eti.rslemos.cobolg.DataDescriptionEntryData.DataDescriptionEntryClause.PICTURE;
import static br.eti.rslemos.cobolg.DataDescriptionEntryData.DataDescriptionEntryClause.USAGE;
import static br.eti.rslemos.cobolg.DataDescriptionEntryData.DataDescriptionEntryClause.VALUE;

import java.util.ResourceBundle;

import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.DataDescriptionEntryContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

public class DataDescriptionEntryUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.dataDescriptionEntry");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<DataDescriptionEntryContext> helper = new CompilerHelper<DataDescriptionEntryContext>() {
		@Override protected DataDescriptionEntryContext parsePart() { return parser.dataDescriptionEntry(); }
	};

	@Test public void EMPTYDECLARATION() {
		helper.compileAndVerify(
				get("EMPTYDECLARATION.source"),
				get("EMPTYDECLARATION.tree")
			);
	}

	@Test public void FILLERDECLARATION() {
		helper.compileAndVerify(
				get("FILLERDECLARATION.source"),
				get("FILLERDECLARATION.tree")
			);
	}

	@Test public void ANONYMOUSDECLARATION() {
		helper.compileAndVerify(
				get("ANONYMOUSDECLARATION.source"),
				get("ANONYMOUSDECLARATION.tree")
			);
	}

	@Test public void PICDECLARATION() {
		helper.compileAndVerify(
				get("PICDECLARATION.source"),
				get("PICDECLARATION.tree")
			);
	}

	@Test public void USAGECLAUSE() {
		helper.compileAndVerify(
				get("USAGECLAUSE.source"),
				get("USAGECLAUSE.tree")
			);
	}

	@Test public void VALUECLAUSE() {
		helper.compileAndVerify(
				get("VALUECLAUSE.source"),
				get("VALUECLAUSE.tree")
			);
	}

	@Test public void VALUECLAUSEWITHFIGURATIVECONSTANT() {
		helper.compileAndVerify(
				get("VALUECLAUSEWITHFIGURATIVECONSTANT.source"),
				get("VALUECLAUSEWITHFIGURATIVECONSTANT.tree")
			);
	}

	@Test public void OCCURSCLAUSE() {
		helper.compileAndVerify(
				get("OCCURSCLAUSE.source"),
				get("OCCURSCLAUSE.tree")
			);
	}

	@Test public void REDEFINESCLAUSE() {
		helper.compileAndVerify(
				get("REDEFINESCLAUSE.source"),
				get("REDEFINESCLAUSE.tree")
			);
	}

	/* all 65 permutations (http://oeis.org/A000522(4)) of 4 clauses */

	// 0
	@Test public void DECL_X() {
		helper.compileAndVerify(
				source(),
				tree  ()
			);
	}

	// 1
	@Test public void DECL_X_PICTURE() {
		helper.compileAndVerify(
				source(PICTURE),
				tree  (PICTURE)
			);
	}

	@Test public void DECL_X_USAGE() {
		helper.compileAndVerify(
				source(USAGE),
				tree  (USAGE)
			);
	}

	@Test public void DECL_X_VALUE() {
		helper.compileAndVerify(
				source(VALUE),
				tree  (VALUE)
			);
	}

	@Test public void DECL_X_OCCURS() {
		helper.compileAndVerify(
				source(OCCURS),
				tree  (OCCURS)
			);
	}

	// 2
	@Test public void DECL_X_PICTURE_USAGE() {
		helper.compileAndVerify(
				source(PICTURE, USAGE),
				tree  (PICTURE, USAGE)
			);
	}

	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DECL_X_PICTURE_VALUE() {
		helper.compileAndVerify(
				source(PICTURE, VALUE),
				tree  (PICTURE, VALUE)
			);
	}

	@Test public void DECL_X_PICTURE_OCCURS() {
		helper.compileAndVerify(
				source(PICTURE, OCCURS),
				tree  (PICTURE, OCCURS)
			);
	}

	@Test public void DECL_X_USAGE_PICTURE() {
		helper.compileAndVerify(
				source(USAGE, PICTURE),
				tree  (USAGE, PICTURE)
			);
	}

	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DECL_X_USAGE_VALUE() {
		helper.compileAndVerify(
				source(USAGE, VALUE),
				tree  (USAGE, VALUE)
			);
	}

	@Test public void DECL_X_USAGE_OCCURS() {
		helper.compileAndVerify(
				source(USAGE, OCCURS),
				tree  (USAGE, OCCURS)
			);
	}

	@Test public void DECL_X_VALUE_PICTURE() {
		helper.compileAndVerify(
				source(VALUE, PICTURE),
				tree  (VALUE, PICTURE)
			);
	}

	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DECL_X_VALUE_USAGE() {
		helper.compileAndVerify(
				source(VALUE, USAGE),
				tree  (VALUE, USAGE)
			);
	}

	@Test public void DECL_X_VALUE_OCCURS() {
		helper.compileAndVerify(
				source(VALUE, OCCURS),
				tree  (VALUE, OCCURS)
			);
	}

	@Test public void DECL_X_OCCURS_PICTURE() {
		helper.compileAndVerify(
				source(OCCURS, PICTURE),
				tree  (OCCURS, PICTURE)
			);
	}

	@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void DECL_X_OCCURS_USAGE() {
		helper.compileAndVerify(
				source(OCCURS, USAGE),
				tree  (OCCURS, USAGE)
			);
	}

	@Test public void DECL_X_OCCURS_VALUE() {
		helper.compileAndVerify(
				source(OCCURS, VALUE),
				tree  (OCCURS, VALUE)
			);
	}

	// 3
	@Test public void DECL_X_PICTURE_USAGE_VALUE() {
		helper.compileAndVerify(
				source(PICTURE, USAGE, VALUE),
				tree  (PICTURE, USAGE, VALUE)
			);
	}

	@Test public void DECL_X_PICTURE_USAGE_OCCURS() {
		helper.compileAndVerify(
				source(PICTURE, USAGE, OCCURS),
				tree  (PICTURE, USAGE, OCCURS)
			);
	}

	@Test public void DECL_X_PICTURE_VALUE_USAGE() {
		helper.compileAndVerify(
				source(PICTURE, VALUE, USAGE),
				tree  (PICTURE, VALUE, USAGE)
			);
	}

	@Test public void DECL_X_PICTURE_VALUE_OCCURS() {
		helper.compileAndVerify(
				source(PICTURE, VALUE, OCCURS),
				tree  (PICTURE, VALUE, OCCURS)
			);
	}

	@Test public void DECL_X_PICTURE_OCCURS_USAGE() {
		helper.compileAndVerify(
				source(PICTURE, OCCURS, USAGE),
				tree  (PICTURE, OCCURS, USAGE)
			);
	}

	@Test public void DECL_X_PICTURE_OCCURS_VALUE() {
		helper.compileAndVerify(
				source(PICTURE, OCCURS, VALUE),
				tree  (PICTURE, OCCURS, VALUE)
			);
	}

	@Test public void DECL_X_USAGE_PICTURE_VALUE() {
		helper.compileAndVerify(
				source(USAGE, PICTURE, VALUE),
				tree  (USAGE, PICTURE, VALUE)
			);
	}

	@Test public void DECL_X_USAGE_PICTURE_OCCURS() {
		helper.compileAndVerify(
				source(USAGE, PICTURE, OCCURS),
				tree  (USAGE, PICTURE, OCCURS)
			);
	}

	@Test public void DECL_X_USAGE_VALUE_PICTURE() {
		helper.compileAndVerify(
				source(USAGE, VALUE, PICTURE),
				tree  (USAGE, VALUE, PICTURE)
			);
	}

	@Test public void DECL_X_USAGE_VALUE_OCCURS() {
		helper.compileAndVerify(
				source(USAGE, VALUE, OCCURS),
				tree  (USAGE, VALUE, OCCURS)
			);
	}

	@Test public void DECL_X_USAGE_OCCURS_PICTURE() {
		helper.compileAndVerify(
				source(USAGE, OCCURS, PICTURE),
				tree  (USAGE, OCCURS, PICTURE)
			);
	}

	@Test public void DECL_X_USAGE_OCCURS_VALUE() {
		helper.compileAndVerify(
				source(USAGE, OCCURS, VALUE),
				tree  (USAGE, OCCURS, VALUE)
			);
	}

	@Test public void DECL_X_VALUE_PICTURE_USAGE() {
		helper.compileAndVerify(
				source(VALUE, PICTURE, USAGE),
				tree  (VALUE, PICTURE, USAGE)
			);
	}

	@Test public void DECL_X_VALUE_PICTURE_OCCURS() {
		helper.compileAndVerify(
				source(VALUE, PICTURE, OCCURS),
				tree  (VALUE, PICTURE, OCCURS)
			);
	}

	@Test public void DECL_X_VALUE_USAGE_PICTURE() {
		helper.compileAndVerify(
				source(VALUE, USAGE, PICTURE),
				tree  (VALUE, USAGE, PICTURE)
			);
	}

	@Test public void DECL_X_VALUE_USAGE_OCCURS() {
		helper.compileAndVerify(
				source(VALUE, USAGE, OCCURS),
				tree  (VALUE, USAGE, OCCURS)
			);
	}

	@Test public void DECL_X_VALUE_OCCURS_PICTURE() {
		helper.compileAndVerify(
				source(VALUE, OCCURS, PICTURE),
				tree  (VALUE, OCCURS, PICTURE)
			);
	}

	@Test public void DECL_X_VALUE_OCCURS_USAGE() {
		helper.compileAndVerify(
				source(VALUE, OCCURS, USAGE),
				tree  (VALUE, OCCURS, USAGE)
			);
	}

	@Test public void DECL_X_OCCURS_PICTURE_USAGE() {
		helper.compileAndVerify(
				source(OCCURS, PICTURE, USAGE),
				tree  (OCCURS, PICTURE, USAGE)
			);
	}

	@Test public void DECL_X_OCCURS_PICTURE_VALUE() {
		helper.compileAndVerify(
				source(OCCURS, PICTURE, VALUE),
				tree  (OCCURS, PICTURE, VALUE)
			);
	}

	@Test public void DECL_X_OCCURS_USAGE_PICTURE() {
		helper.compileAndVerify(
				source(OCCURS, USAGE, PICTURE),
				tree  (OCCURS, USAGE, PICTURE)
			);
	}

	@Test public void DECL_X_OCCURS_USAGE_VALUE() {
		helper.compileAndVerify(
				source(OCCURS, USAGE, VALUE),
				tree  (OCCURS, USAGE, VALUE)
			);
	}

	@Test public void DECL_X_OCCURS_VALUE_PICTURE() {
		helper.compileAndVerify(
				source(OCCURS, VALUE, PICTURE),
				tree  (OCCURS, VALUE, PICTURE)
			);
	}

	@Test public void DECL_X_OCCURS_VALUE_USAGE() {
		helper.compileAndVerify(
				source(OCCURS, VALUE, USAGE),
				tree  (OCCURS, VALUE, USAGE)
			);
	}

	// 4
	@Test public void DECL_X_PICTURE_USAGE_VALUE_OCCURS() {
		helper.compileAndVerify(
				source(PICTURE, USAGE, VALUE, OCCURS),
				tree  (PICTURE, USAGE, VALUE, OCCURS)
			);
	}

	@Test public void DECL_X_PICTURE_USAGE_OCCURS_VALUE() {
		helper.compileAndVerify(
				source(PICTURE, USAGE, OCCURS, VALUE),
				tree  (PICTURE, USAGE, OCCURS, VALUE)
			);
	}

	@Test public void DECL_X_PICTURE_VALUE_USAGE_OCCURS() {
		helper.compileAndVerify(
				source(PICTURE, VALUE, USAGE, OCCURS),
				tree  (PICTURE, VALUE, USAGE, OCCURS)
			);
	}

	@Test public void DECL_X_PICTURE_VALUE_OCCURS_USAGE() {
		helper.compileAndVerify(
				source(PICTURE, VALUE, OCCURS, USAGE),
				tree  (PICTURE, VALUE, OCCURS, USAGE)
			);
	}

	@Test public void DECL_X_PICTURE_OCCURS_USAGE_VALUE() {
		helper.compileAndVerify(
				source(PICTURE, OCCURS, USAGE, VALUE),
				tree  (PICTURE, OCCURS, USAGE, VALUE)
			);
	}

	@Test public void DECL_X_PICTURE_OCCURS_VALUE_USAGE() {
		helper.compileAndVerify(
				source(PICTURE, OCCURS, VALUE, USAGE),
				tree  (PICTURE, OCCURS, VALUE, USAGE)
			);
	}

	@Test public void DECL_X_USAGE_PICTURE_VALUE_OCCURS() {
		helper.compileAndVerify(
				source(USAGE, PICTURE, VALUE, OCCURS),
				tree  (USAGE, PICTURE, VALUE, OCCURS)
			);
	}

	@Test public void DECL_X_USAGE_PICTURE_OCCURS_VALUE() {
		helper.compileAndVerify(
				source(USAGE, PICTURE, OCCURS, VALUE),
				tree  (USAGE, PICTURE, OCCURS, VALUE)
			);
	}

	@Test public void DECL_X_USAGE_VALUE_PICTURE_OCCURS() {
		helper.compileAndVerify(
				source(USAGE, VALUE, PICTURE, OCCURS),
				tree  (USAGE, VALUE, PICTURE, OCCURS)
			);
	}

	@Test public void DECL_X_USAGE_VALUE_OCCURS_PICTURE() {
		helper.compileAndVerify(
				source(USAGE, VALUE, OCCURS, PICTURE),
				tree  (USAGE, VALUE, OCCURS, PICTURE)
			);
	}

	@Test public void DECL_X_USAGE_OCCURS_PICTURE_VALUE() {
		helper.compileAndVerify(
				source(USAGE, OCCURS, PICTURE, VALUE),
				tree  (USAGE, OCCURS, PICTURE, VALUE)
			);
	}

	@Test public void DECL_X_USAGE_OCCURS_VALUE_PICTURE() {
		helper.compileAndVerify(
				source(USAGE, OCCURS, VALUE, PICTURE),
				tree  (USAGE, OCCURS, VALUE, PICTURE)
			);
	}

	@Test public void DECL_X_VALUE_PICTURE_USAGE_OCCURS() {
		helper.compileAndVerify(
				source(VALUE, PICTURE, USAGE, OCCURS),
				tree  (VALUE, PICTURE, USAGE, OCCURS)
			);
	}

	@Test public void DECL_X_VALUE_PICTURE_OCCURS_USAGE() {
		helper.compileAndVerify(
				source(VALUE, PICTURE, OCCURS, USAGE),
				tree  (VALUE, PICTURE, OCCURS, USAGE)
			);
	}

	@Test public void DECL_X_VALUE_USAGE_PICTURE_OCCURS() {
		helper.compileAndVerify(
				source(VALUE, USAGE, PICTURE, OCCURS),
				tree  (VALUE, USAGE, PICTURE, OCCURS)
			);
	}

	@Test public void DECL_X_VALUE_USAGE_OCCURS_PICTURE() {
		helper.compileAndVerify(
				source(VALUE, USAGE, OCCURS, PICTURE),
				tree  (VALUE, USAGE, OCCURS, PICTURE)
			);
	}

	@Test public void DECL_X_VALUE_OCCURS_PICTURE_USAGE() {
		helper.compileAndVerify(
				source(VALUE, OCCURS, PICTURE, USAGE),
				tree  (VALUE, OCCURS, PICTURE, USAGE)
			);
	}

	@Test public void DECL_X_VALUE_OCCURS_USAGE_PICTURE() {
		helper.compileAndVerify(
				source(VALUE, OCCURS, USAGE, PICTURE),
				tree  (VALUE, OCCURS, USAGE, PICTURE)
			);
	}

	@Test public void DECL_X_OCCURS_PICTURE_USAGE_VALUE() {
		helper.compileAndVerify(
				source(OCCURS, PICTURE, USAGE, VALUE),
				tree  (OCCURS, PICTURE, USAGE, VALUE)
			);
	}

	@Test public void DECL_X_OCCURS_PICTURE_VALUE_USAGE() {
		helper.compileAndVerify(
				source(OCCURS, PICTURE, VALUE, USAGE),
				tree  (OCCURS, PICTURE, VALUE, USAGE)
			);
	}

	@Test public void DECL_X_OCCURS_USAGE_PICTURE_VALUE() {
		helper.compileAndVerify(
				source(OCCURS, USAGE, PICTURE, VALUE),
				tree  (OCCURS, USAGE, PICTURE, VALUE)
			);
	}

	@Test public void DECL_X_OCCURS_USAGE_VALUE_PICTURE() {
		helper.compileAndVerify(
				source(OCCURS, USAGE, VALUE, PICTURE),
				tree  (OCCURS, USAGE, VALUE, PICTURE)
			);
	}

	@Test public void DECL_X_OCCURS_VALUE_PICTURE_USAGE() {
		helper.compileAndVerify(
				source(OCCURS, VALUE, PICTURE, USAGE),
				tree  (OCCURS, VALUE, PICTURE, USAGE)
			);
	}

	@Test public void DECL_X_OCCURS_VALUE_USAGE_PICTURE() {
		helper.compileAndVerify(
				source(OCCURS, VALUE, USAGE, PICTURE),
				tree  (OCCURS, VALUE, USAGE, PICTURE)
			);
	}
}
