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

import br.eti.rslemos.cobolg.COBOLParser.IoControlParagraphContext;

public class IOControlParagraphUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.ioControlParagraph");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<IoControlParagraphContext> helper = new CompilerHelper<IoControlParagraphContext>() {
		@Override protected IoControlParagraphContext parsePart() { return parser.ioControlParagraph(); }
	};

	@Test public void I_O_CONTROL_RERUN_ON_LOCAL_NAME() {
		helper.compileAndVerify(
				get("I_O_CONTROL_RERUN_ON_LOCAL_NAME.source"),
				get("I_O_CONTROL_RERUN_ON_LOCAL_NAME.tree")
			);
	}

	@Test public void I_O_CONTROL_RERUN_ON_LOCAL_NAME_EVERY_128_RECORDS_OF_LOCAL_NAME() {
		helper.compileAndVerify(
				get("I_O_CONTROL_RERUN_ON_LOCAL_NAME_EVERY_128_RECORDS_OF_LOCAL_NAME.source"),
				get("I_O_CONTROL_RERUN_ON_LOCAL_NAME_EVERY_128_RECORDS_OF_LOCAL_NAME.tree")
			);
	}

	@Test public void I_O_CONTROL_RERUN_ON_LOCAL_NAME_EVERY_END_OF_REEL_OF_LOCAL_NAME() {
		helper.compileAndVerify(
				get("I_O_CONTROL_RERUN_ON_LOCAL_NAME_EVERY_END_OF_REEL_OF_LOCAL_NAME.source"),
				get("I_O_CONTROL_RERUN_ON_LOCAL_NAME_EVERY_END_OF_REEL_OF_LOCAL_NAME.tree")
			);
	}

	@Test public void I_O_CONTROL_RERUN_ON_LOCAL_NAME_EVERY_END_OF_UNIT_OF_LOCAL_NAME() {
		helper.compileAndVerify(
				get("I_O_CONTROL_RERUN_ON_LOCAL_NAME_EVERY_END_OF_UNIT_OF_LOCAL_NAME.source"),
				get("I_O_CONTROL_RERUN_ON_LOCAL_NAME_EVERY_END_OF_UNIT_OF_LOCAL_NAME.tree")
			);
	}

	@Test public void I_O_CONTROL_SAME_AREA_FOR_FILE_1_FILE_2() {
		helper.compileAndVerify(
				get("I_O_CONTROL_SAME_AREA_FOR_FILE_1_FILE_2.source"),
				get("I_O_CONTROL_SAME_AREA_FOR_FILE_1_FILE_2.tree")
			);
	}

	@Test public void I_O_CONTROL_SAME_RECORD_AREA_FOR_FILE_1_FILE_2() {
		helper.compileAndVerify(
				get("I_O_CONTROL_SAME_RECORD_AREA_FOR_FILE_1_FILE_2.source"),
				get("I_O_CONTROL_SAME_RECORD_AREA_FOR_FILE_1_FILE_2.tree")
			);
	}

	@Test public void I_O_CONTROL_SAME_SORT_AREA_FOR_FILE_1_FILE_2() {
		helper.compileAndVerify(
				get("I_O_CONTROL_SAME_SORT_AREA_FOR_FILE_1_FILE_2.source"),
				get("I_O_CONTROL_SAME_SORT_AREA_FOR_FILE_1_FILE_2.tree")
			);
	}

	@Test public void I_O_CONTROL_SAME_SORT_MERGE_AREA_FOR_FILE_1_FILE_2() {
		helper.compileAndVerify(
				get("I_O_CONTROL_SAME_SORT_MERGE_AREA_FOR_FILE_1_FILE_2.source"),
				get("I_O_CONTROL_SAME_SORT_MERGE_AREA_FOR_FILE_1_FILE_2.tree")
			);
	}

	@Test public void I_O_CONTROL_MULTIPLE_FILE_TAPE_CONTAINS_FILE_1_POSITION_10_FILE_2() {
		helper.compileAndVerify(
				get("I_O_CONTROL_MULTIPLE_FILE_TAPE_CONTAINS_FILE_1_POSITION_10_FILE_2.source"),
				get("I_O_CONTROL_MULTIPLE_FILE_TAPE_CONTAINS_FILE_1_POSITION_10_FILE_2.tree")
			);
	}

	@Test public void I_O_CONTROL_APPLY_WRITE_ONLY_ON_FILE_1_FILE_2() {
		helper.compileAndVerify(
				get("I_O_CONTROL_APPLY_WRITE_ONLY_ON_FILE_1_FILE_2.source"),
				get("I_O_CONTROL_APPLY_WRITE_ONLY_ON_FILE_1_FILE_2.tree")
			);
	}
}
