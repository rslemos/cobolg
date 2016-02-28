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
import static br.eti.rslemos.cobolg.DataDescriptionEntryData.DataDescriptionEntryClause.*;

import java.util.ResourceBundle;

import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.DataDescriptionEntryContext;

public class DataDescriptionEntryUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.dataDescriptionEntry");
	public static String get(String key) { return TEST_DATA.getString(key); }

	static CompilerHelper<DataDescriptionEntryContext> helper = new CompilerHelper<DataDescriptionEntryContext>() {
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

	@Test public void DATA_ENTRY_PIC_X() {
		helper.compileAndVerify(
				get("DATA_ENTRY_PIC_X.source"),
				get("DATA_ENTRY_PIC_X.tree")
			);
	}

	@Test public void DATA_ENTRY_PIC_9() {
		helper.compileAndVerify(
				get("DATA_ENTRY_PIC_9.source"),
				get("DATA_ENTRY_PIC_9.tree")
			);
	}

	@Test public void DATA_ENTRY_PIC_99V99() {
		helper.compileAndVerify(
				get("DATA_ENTRY_PIC_99V99.source"),
				get("DATA_ENTRY_PIC_99V99.tree")
			);
	}

	@Test public void DATA_ENTRY_PIC_ZZZZZZZZZZ999V999999() {
		helper.compileAndVerify(
				get("DATA_ENTRY_PIC_ZZZZZZZZZZ999V999999.source"),
				get("DATA_ENTRY_PIC_ZZZZZZZZZZ999V999999.tree")
			);
	}

	@Test public void DATA_ENTRY_PIC_Z$ABX09PPAAAVS____() {
		helper.compileAndVerify(
				get("DATA_ENTRY_PIC_Z$ABX09PPAAAVS____.source"),
				get("DATA_ENTRY_PIC_Z$ABX09PPAAAVS____.tree")
			);
	}

	@Test public void DATA_ENTRY_PIC___ABEGNPSVXZCRDB90___$__012345678_9() {
		helper.compileAndVerify(
				get("DATA_ENTRY_PIC___ABEGNPSVXZCRDB90___$__012345678_9.source"),
				get("DATA_ENTRY_PIC___ABEGNPSVXZCRDB90___$__012345678_9.tree")
			);
	}

	@Test public void DATA_ENTRY_PIC_____AABBEEGGNNPPSSVVXXZZCCRRDDBB9900______$$____001122334455667788__99() {
		helper.compileAndVerify(
				get("DATA_ENTRY_PIC_____AABBEEGGNNPPSSVVXXZZCCRRDDBB9900______$$____001122334455667788__99.source"),
				get("DATA_ENTRY_PIC_____AABBEEGGNNPPSSVVXXZZCCRRDDBB9900______$$____001122334455667788__99.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_BINARY() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_BINARY.source"),
				get("DATA_ENTRY_USAGE_IS_BINARY.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMP() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMP.source"),
				get("DATA_ENTRY_USAGE_IS_COMP.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMP_1() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMP_1.source"),
				get("DATA_ENTRY_USAGE_IS_COMP_1.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMP_2() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMP_2.source"),
				get("DATA_ENTRY_USAGE_IS_COMP_2.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMP_3() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMP_3.source"),
				get("DATA_ENTRY_USAGE_IS_COMP_3.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMP_4() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMP_4.source"),
				get("DATA_ENTRY_USAGE_IS_COMP_4.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMP_5() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMP_5.source"),
				get("DATA_ENTRY_USAGE_IS_COMP_5.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMPUTATIONAL() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL.source"),
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMPUTATIONAL_1() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_1.source"),
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_1.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMPUTATIONAL_2() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_2.source"),
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_2.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMPUTATIONAL_3() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_3.source"),
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_3.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMPUTATIONAL_4() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_4.source"),
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_4.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMPUTATIONAL_5() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_5.source"),
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_5.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_DISPLAY() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_DISPLAY.source"),
				get("DATA_ENTRY_USAGE_IS_DISPLAY.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_DISPLAY_1() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_DISPLAY_1.source"),
				get("DATA_ENTRY_USAGE_IS_DISPLAY_1.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_INDEX() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_INDEX.source"),
				get("DATA_ENTRY_USAGE_IS_INDEX.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_NATIONAL() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_NATIONAL.source"),
				get("DATA_ENTRY_USAGE_IS_NATIONAL.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_OBJECT_REFERENCE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_OBJECT_REFERENCE.source"),
				get("DATA_ENTRY_USAGE_IS_OBJECT_REFERENCE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_PACKED_DECIMAL() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_PACKED_DECIMAL.source"),
				get("DATA_ENTRY_USAGE_IS_PACKED_DECIMAL.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_POINTER() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_POINTER.source"),
				get("DATA_ENTRY_USAGE_IS_POINTER.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_PROCEDURE_POINTER() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_PROCEDURE_POINTER.source"),
				get("DATA_ENTRY_USAGE_IS_PROCEDURE_POINTER.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_FUNCTION_POINTER() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_FUNCTION_POINTER.source"),
				get("DATA_ENTRY_USAGE_IS_FUNCTION_POINTER.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_BINARY_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_BINARY_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_BINARY_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMP_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMP_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_COMP_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMP_1_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMP_1_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_COMP_1_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMP_2_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMP_2_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_COMP_2_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMP_3_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMP_3_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_COMP_3_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMP_4_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMP_4_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_COMP_4_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMP_5_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMP_5_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_COMP_5_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMPUTATIONAL_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMPUTATIONAL_1_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_1_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_1_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMPUTATIONAL_2_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_2_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_2_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMPUTATIONAL_3_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_3_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_3_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMPUTATIONAL_4_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_4_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_4_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_COMPUTATIONAL_5_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_5_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_COMPUTATIONAL_5_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_DISPLAY_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_DISPLAY_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_DISPLAY_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_DISPLAY_1_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_DISPLAY_1_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_DISPLAY_1_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_NATIONAL_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_NATIONAL_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_NATIONAL_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_OBJECT_REFERENCE_CLASS_X() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_OBJECT_REFERENCE_CLASS_X.source"),
				get("DATA_ENTRY_USAGE_IS_OBJECT_REFERENCE_CLASS_X.tree")
			);
	}

	@Test public void DATA_ENTRY_USAGE_IS_PACKED_DECIMAL_NATIVE() {
		helper.compileAndVerify(
				get("DATA_ENTRY_USAGE_IS_PACKED_DECIMAL_NATIVE.source"),
				get("DATA_ENTRY_USAGE_IS_PACKED_DECIMAL_NATIVE.tree")
			);
	}

	@Test public void DATA_ENTRY_VALUE_IS_0() {
		helper.compileAndVerify(
				get("DATA_ENTRY_VALUE_IS_0.source"),
				get("DATA_ENTRY_VALUE_IS_0.tree")
			);
	}

	@Test public void DATA_ENTRY_VALUE_IS_ZERO() {
		helper.compileAndVerify(
				get("DATA_ENTRY_VALUE_IS_ZERO.source"),
				get("DATA_ENTRY_VALUE_IS_ZERO.tree")
			);
	}

	@Test public void DATA_ENTRY_VALUE_IS_ABC() {
		helper.compileAndVerify(
				get("DATA_ENTRY_VALUE_IS_ABC.source"),
				get("DATA_ENTRY_VALUE_IS_ABC.tree")
			);
	}

	@Test public void DATA_ENTRY_VALUES_ARE_0_1_2_3() {
		helper.compileAndVerify(
				get("DATA_ENTRY_VALUES_ARE_0_1_2_3.source"),
				get("DATA_ENTRY_VALUES_ARE_0_1_2_3.tree")
			);
	}

	@Test public void DATA_ENTRY_VALUES_ARE_0_THROUGH_3() {
		helper.compileAndVerify(
				get("DATA_ENTRY_VALUES_ARE_0_THROUGH_3.source"),
				get("DATA_ENTRY_VALUES_ARE_0_THROUGH_3.tree")
			);
	}

	@Test public void DATA_ENTRY_VALUES_ARE_0_THROUGH_3_ABC() {
		helper.compileAndVerify(
				get("DATA_ENTRY_VALUES_ARE_0_THROUGH_3_ABC.source"),
				get("DATA_ENTRY_VALUES_ARE_0_THROUGH_3_ABC.tree")
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
	@Test public void DECL_X_REDEFINES() {
		helper.compileAndVerify(
				source(REDEFINES),
				tree  (REDEFINES)
			);
	}

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
