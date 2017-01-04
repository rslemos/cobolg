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

import static br.eti.rslemos.cobolg.RecordDescriptionEntryData.flatten;
import static br.eti.rslemos.cobolg.RecordDescriptionEntryData.source;
import static br.eti.rslemos.cobolg.RecordDescriptionEntryData.tree;

import java.util.ResourceBundle;

import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.WorkingStorageSectionContext;

public class RecordDescriptionEntryUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.recordDescriptionEntry");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<WorkingStorageSectionContext> helper = new CompilerHelper<WorkingStorageSectionContext>() {
		@Override protected WorkingStorageSectionContext parsePart() {
			return parser.workingStorageSection();
		}
	};
	
	@Test public void ITEM77() {
		helper.compileAndVerify(
				get("ITEM77.source"),
				get("ITEM77.tree")
			);
	}

	@Test public void SHALLOWITEMGROUP() {
		helper.compileAndVerify(
				get("SHALLOWITEMGROUP.source"),
				get("SHALLOWITEMGROUP.tree")
			);
	}

	@Test public void ITEMGROUP() {
		helper.compileAndVerify(
				get("ITEMGROUP.source"),
				get("ITEMGROUP.tree")
			);
	}

	@Test public void WSS_01_01_05() {
		helper.compileAndVerify(
				source      (01, 01, 05),
				flatten(tree(01, 01, 05))
			);
	}

	@Test public void WSS_01_01_01_01_01_01_01_01_01_01_01_01() {
		helper.compileAndVerify(
				source      (01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01),
				flatten(tree(01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01))
			);
	}

	@Test public void WSS_05_05_05_05_05_05_05_05_05_05_05_05() {
		helper.compileAndVerify(
				source      (05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05),
				flatten(tree(05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05))
			);
	}

	@Test public void WSS_01_01_01_01_01_01_01_01_01_01_01_05() {
		helper.compileAndVerify(
				source      (01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 05),
				flatten(tree(01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 05))
			);
	}

	@Test public void WSS_01_01_01_01_01_01_01_01_01_01_05_10() {
		helper.compileAndVerify(
				source      (01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 05, 10),
				flatten(tree(01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 05, 10))
			);
	}

	@Test public void WSS_01_01_01_01_01_01_01_01_01_05_10_20() {
		helper.compileAndVerify(
				source      (01, 01, 01, 01, 01, 01, 01, 01, 01, 05, 10, 20),
				flatten(tree(01, 01, 01, 01, 01, 01, 01, 01, 01, 05, 10, 20))
			);
	}

	@Test public void WSS_01_05_01_01_01_01_01_01_01_01_01_01() {
		helper.compileAndVerify(
				source      (01, 05, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01),
				flatten(tree(01, 05, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01))
			);
	}

	@Test public void WSS_01_05_10_01_01_01_01_01_01_01_01_01() {
		helper.compileAndVerify(
				source      (01, 05, 10, 01, 01, 01, 01, 01, 01, 01, 01, 01),
				flatten(tree(01, 05, 10, 01, 01, 01, 01, 01, 01, 01, 01, 01))
			);
	}

	@Test public void WSS_01_05_10_20_01_01_01_01_01_01_01_01() {
		helper.compileAndVerify(
				source      (01, 05, 10, 20, 01, 01, 01, 01, 01, 01, 01, 01),
				flatten(tree(01, 05, 10, 20, 01, 01, 01, 01, 01, 01, 01, 01))
			);
	}

	@Test public void WSS_01_05_05_05_05_05_05_05_05_05_05_05() {
		helper.compileAndVerify(
				source      (01, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05),
				flatten(tree(01, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05))
			);
	}

	@Test public void WSS_01_05_10_10_10_10_10_10_10_10_10_10() {
		helper.compileAndVerify(
				source      (01, 05, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10),
				flatten(tree(01, 05, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
			);
	}

	@Test public void WSS_01_05_10_20_20_20_20_20_20_20_20_20() {
		helper.compileAndVerify(
				source      (01, 05, 10, 20, 20, 20, 20, 20, 20, 20, 20, 20),
				flatten(tree(01, 05, 10, 20, 20, 20, 20, 20, 20, 20, 20, 20))
			);
	}

	@Test public void WSS_01_05_05_05_05_05_01_05_05_05_05_05() {
		helper.compileAndVerify(
				source      (01, 05, 05, 05, 05, 05, 01, 05, 05, 05, 05, 05),
				flatten(tree(01, 05, 05, 05, 05, 05, 01, 05, 05, 05, 05, 05))
			);
	}

	@Test public void WSS_01_05_10_10_10_10_01_05_10_10_10_10() {
		helper.compileAndVerify(
				source      (01, 05, 10, 10, 10, 10, 01, 05, 10, 10, 10, 10),
				flatten(tree(01, 05, 10, 10, 10, 10, 01, 05, 10, 10, 10, 10))
			);
	}

	@Test public void WSS_01_05_10_20_20_20_01_05_10_20_20_20() {
		helper.compileAndVerify(
				source      (01, 05, 10, 20, 20, 20, 01, 05, 10, 20, 20, 20),
				flatten(tree(01, 05, 10, 20, 20, 20, 01, 05, 10, 20, 20, 20))
			);
	}

	@Test public void WSS_01_05_05_05_01_05_05_05_01_05_05_05() {
		helper.compileAndVerify(
				source      (01, 05, 05, 05, 01, 05, 05, 05, 01, 05, 05, 05),
				flatten(tree(01, 05, 05, 05, 01, 05, 05, 05, 01, 05, 05, 05))
			);
	}

	@Test public void WSS_01_05_10_10_01_05_10_10_01_05_10_10() {
		helper.compileAndVerify(
				source      (01, 05, 10, 10, 01, 05, 10, 10, 01, 05, 10, 10),
				flatten(tree(01, 05, 10, 10, 01, 05, 10, 10, 01, 05, 10, 10))
			);
	}

	@Test public void WSS_01_05_10_20_01_05_10_20_01_05_10_20() {
		helper.compileAndVerify(
				source      (01, 05, 10, 20, 01, 05, 10, 20, 01, 05, 10, 20),
				flatten(tree(01, 05, 10, 20, 01, 05, 10, 20, 01, 05, 10, 20))
			);
	}

	@Test public void WSS_01_05_05_01_05_05_01_05_05_01_05_05() {
		helper.compileAndVerify(
				source      (01, 05, 05, 01, 05, 05, 01, 05, 05, 01, 05, 05),
				flatten(tree(01, 05, 05, 01, 05, 05, 01, 05, 05, 01, 05, 05))
			);
	}

	@Test public void WSS_01_05_10_01_05_10_01_05_10_01_05_10() {
		helper.compileAndVerify(
				source      (01, 05, 10, 01, 05, 10, 01, 05, 10, 01, 05, 10),
				flatten(tree(01, 05, 10, 01, 05, 10, 01, 05, 10, 01, 05, 10))
			);
	}

	@Test public void WSS_01_05_01_05_01_05_01_05_01_05_01_05() {
		helper.compileAndVerify(
				source      (01, 05, 01, 05, 01, 05, 01, 05, 01, 05, 01, 05),
				flatten(tree(01, 05, 01, 05, 01, 05, 01, 05, 01, 05, 01, 05))
			);
	}

	@Test public void WSS_01_05_10_05_01_05_10_05_01_05_10_05() {
		helper.compileAndVerify(
				source      (01, 05, 10, 05, 01, 05, 10, 05, 01, 05, 10, 05),
				flatten(tree(01, 05, 10, 05, 01, 05, 10, 05, 01, 05, 10, 05))
			);
	}

	@Test public void WSS_01_05_10_05_10_05_01_05_10_05_10_05() {
		helper.compileAndVerify(
				source      (01, 05, 10, 05, 10, 05, 01, 05, 10, 05, 10, 05),
				flatten(tree(01, 05, 10, 05, 10, 05, 01, 05, 10, 05, 10, 05))
			);
	}

}
