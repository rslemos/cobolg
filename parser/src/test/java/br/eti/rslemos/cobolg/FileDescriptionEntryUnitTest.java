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

import br.eti.rslemos.cobolg.COBOLParser.FileDescriptionEntryContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

public class FileDescriptionEntryUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.fileDescriptionEntry");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<FileDescriptionEntryContext> helper = new CompilerHelper<FileDescriptionEntryContext>() {
		@Override protected FileDescriptionEntryContext parsePart() { return parser.fileDescriptionEntry(); }
	};

	@Test public void FD_FILE_NAME() {
		helper.compileAndVerify(
				get("FD_FILE_NAME.source"),
				get("FD_FILE_NAME.tree")
			);
	}

	@Test public void FD_FILE_NAME_IS_EXTERNAL() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_IS_EXTERNAL.source"),
				get("FD_FILE_NAME_IS_EXTERNAL.tree")
			);
	}

	@Test public void FD_FILE_NAME_IS_GLOBAL() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_IS_GLOBAL.source"),
				get("FD_FILE_NAME_IS_GLOBAL.tree")
			);
	}

	@Test public void FD_FILE_NAME_IS_EXTERNAL_IS_GLOBAL() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_IS_EXTERNAL_IS_GLOBAL.source"),
				get("FD_FILE_NAME_IS_EXTERNAL_IS_GLOBAL.tree")
			);
	}

	@Test public void FD_FILE_NAME_IS_GLOBAL_IS_EXTERNAL() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_IS_GLOBAL_IS_EXTERNAL.source"),
				get("FD_FILE_NAME_IS_GLOBAL_IS_EXTERNAL.tree")
			);
	}

	@Test public void FD_FILE_NAME_IS_GLOBAL_IS_EXTERNAL_BLOCK_CONTAINS_5_CHARACTERS() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_IS_GLOBAL_IS_EXTERNAL_BLOCK_CONTAINS_5_CHARACTERS.source"),
				get("FD_FILE_NAME_IS_GLOBAL_IS_EXTERNAL_BLOCK_CONTAINS_5_CHARACTERS.tree")
			);
	}

	@Test public void FD_FILE_NAME_BLOCK_CONTAINS_5_CHARACTERS() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_BLOCK_CONTAINS_5_CHARACTERS.source"),
				get("FD_FILE_NAME_BLOCK_CONTAINS_5_CHARACTERS.tree")
			);
	}

	@Test public void FD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_CHARACTERS() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_CHARACTERS.source"),
				get("FD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_CHARACTERS.tree")
			);
	}

	@Test public void FD_FILE_NAME_BLOCK_CONTAINS_5_RECORDS() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_BLOCK_CONTAINS_5_RECORDS.source"),
				get("FD_FILE_NAME_BLOCK_CONTAINS_5_RECORDS.tree")
			);
	}

	@Test public void FD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_RECORDS() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_RECORDS.source"),
				get("FD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_RECORDS.tree")
			);
	}

	@Test public void FD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_RECORDS_RECORD_CONTAINS_80_CHARACTERS() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_RECORDS_RECORD_CONTAINS_80_CHARACTERS.source"),
				get("FD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_RECORDS_RECORD_CONTAINS_80_CHARACTERS.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORD_CONTAINS_80_CHARACTERS() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORD_CONTAINS_80_CHARACTERS.source"),
				get("FD_FILE_NAME_RECORD_CONTAINS_80_CHARACTERS.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORD_CONTAINS_80_TO_120_CHARACTERS() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORD_CONTAINS_80_TO_120_CHARACTERS.source"),
				get("FD_FILE_NAME_RECORD_CONTAINS_80_TO_120_CHARACTERS.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_TO_120_CHARACTERS_DEPENDING_ON_REC_LEN() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_TO_120_CHARACTERS_DEPENDING_ON_REC_LEN.source"),
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_TO_120_CHARACTERS_DEPENDING_ON_REC_LEN.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_TO_120_CHARACTERS() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_TO_120_CHARACTERS.source"),
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_TO_120_CHARACTERS.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_TO_120_CHARACTERS_DEPENDING_ON_REC_LEN() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_TO_120_CHARACTERS_DEPENDING_ON_REC_LEN.source"),
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_TO_120_CHARACTERS_DEPENDING_ON_REC_LEN.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_TO_120_CHARACTERS() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_TO_120_CHARACTERS.source"),
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_TO_120_CHARACTERS.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_CHARACTERS_DEPENDING_ON_REC_LEN() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_CHARACTERS_DEPENDING_ON_REC_LEN.source"),
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_CHARACTERS_DEPENDING_ON_REC_LEN.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_CHARACTERS() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_CHARACTERS.source"),
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_CHARACTERS.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS_DEPENDING_ON_REC_LEN() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS_DEPENDING_ON_REC_LEN.source"),
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS_DEPENDING_ON_REC_LEN.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS.source"),
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS_LABEL_RECORD_IS_STANDARD() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS_LABEL_RECORD_IS_STANDARD.source"),
				get("FD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS_LABEL_RECORD_IS_STANDARD.tree")
			);
	}

	@Test public void FD_FILE_NAME_LABEL_RECORD_IS_STANDARD() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LABEL_RECORD_IS_STANDARD.source"),
				get("FD_FILE_NAME_LABEL_RECORD_IS_STANDARD.tree")
			);
	}

	@Test public void FD_FILE_NAME_LABEL_RECORD_IS_OMITTED() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LABEL_RECORD_IS_OMITTED.source"),
				get("FD_FILE_NAME_LABEL_RECORD_IS_OMITTED.tree")
			);
	}

	@Test public void FD_FILE_NAME_LABEL_RECORD_IS_LABEL_1_LABEL_2() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LABEL_RECORD_IS_LABEL_1_LABEL_2.source"),
				get("FD_FILE_NAME_LABEL_RECORD_IS_LABEL_1_LABEL_2.tree")
			);
	}

	@Test public void FD_FILE_NAME_LABEL_RECORD_IS_LABEL_1() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LABEL_RECORD_IS_LABEL_1.source"),
				get("FD_FILE_NAME_LABEL_RECORD_IS_LABEL_1.tree")
			);
	}

	@Waive({CompilationError.SYNTAX_ERROR})
	@Test public void FD_FILE_NAME_LABEL_RECORD_IS() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LABEL_RECORD_IS.source"),
				get("FD_FILE_NAME_LABEL_RECORD_IS.tree")
			);
	}

	@Test public void FD_FILE_NAME_LABEL_RECORDS_ARE_STANDARD() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LABEL_RECORDS_ARE_STANDARD.source"),
				get("FD_FILE_NAME_LABEL_RECORDS_ARE_STANDARD.tree")
			);
	}

	@Test public void FD_FILE_NAME_LABEL_RECORDS_ARE_OMITTED() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LABEL_RECORDS_ARE_OMITTED.source"),
				get("FD_FILE_NAME_LABEL_RECORDS_ARE_OMITTED.tree")
			);
	}

	@Test public void FD_FILE_NAME_LABEL_RECORDS_ARE_LABEL_1_LABEL_2() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LABEL_RECORDS_ARE_LABEL_1_LABEL_2.source"),
				get("FD_FILE_NAME_LABEL_RECORDS_ARE_LABEL_1_LABEL_2.tree")
			);
	}

	@Test public void FD_FILE_NAME_LABEL_RECORDS_ARE_LABEL_1() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LABEL_RECORDS_ARE_LABEL_1.source"),
				get("FD_FILE_NAME_LABEL_RECORDS_ARE_LABEL_1.tree")
			);
	}

	@Waive({CompilationError.SYNTAX_ERROR})
	@Test public void FD_FILE_NAME_LABEL_RECORDS_ARE() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LABEL_RECORDS_ARE.source"),
				get("FD_FILE_NAME_LABEL_RECORDS_ARE.tree")
			);
	}

	@Test public void FD_FILE_NAME_LABEL_RECORDS_ARE_VALUE_OF_THIS_NAME_IS_THAT_VALUE_TWELVE_IS_12_ABC_IS_ABC() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LABEL_RECORDS_ARE_VALUE_OF_THIS_NAME_IS_THAT_VALUE_TWELVE_IS_12_ABC_IS_ABC.source"),
				get("FD_FILE_NAME_LABEL_RECORDS_ARE_VALUE_OF_THIS_NAME_IS_THAT_VALUE_TWELVE_IS_12_ABC_IS_ABC.tree")
			);
	}

	@Test public void FD_FILE_NAME_VALUE_OF_THIS_NAME_IS_THAT_VALUE_TWELVE_IS_12_ABC_IS_ABC() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_VALUE_OF_THIS_NAME_IS_THAT_VALUE_TWELVE_IS_12_ABC_IS_ABC.source"),
				get("FD_FILE_NAME_VALUE_OF_THIS_NAME_IS_THAT_VALUE_TWELVE_IS_12_ABC_IS_ABC.tree")
			);
	}

	@Test public void FD_FILE_NAME_VALUE_OF_THIS_NAME_IS_THAT_VALUE() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_VALUE_OF_THIS_NAME_IS_THAT_VALUE.source"),
				get("FD_FILE_NAME_VALUE_OF_THIS_NAME_IS_THAT_VALUE.tree")
			);
	}

	@Test public void FD_FILE_NAME_VALUE_OF_TWELVE_IS_12() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_VALUE_OF_TWELVE_IS_12.source"),
				get("FD_FILE_NAME_VALUE_OF_TWELVE_IS_12.tree")
			);
	}

	@Test public void FD_FILE_NAME_VALUE_OF_ABC_IS_ABC() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_VALUE_OF_ABC_IS_ABC.source"),
				get("FD_FILE_NAME_VALUE_OF_ABC_IS_ABC.tree")
			);
	}

	@Test public void FD_FILE_NAME_VALUE_OF_ABC_IS_ABC_DATA_RECORD_IS_REC_1() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_VALUE_OF_ABC_IS_ABC_DATA_RECORD_IS_REC_1.source"),
				get("FD_FILE_NAME_VALUE_OF_ABC_IS_ABC_DATA_RECORD_IS_REC_1.tree")
			);
	}

	@Test public void FD_FILE_NAME_DATA_RECORD_IS_REC_1() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_DATA_RECORD_IS_REC_1.source"),
				get("FD_FILE_NAME_DATA_RECORD_IS_REC_1.tree")
			);
	}

	@Test public void FD_FILE_NAME_DATA_RECORDS_ARE_REC_1_REC_2() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_DATA_RECORDS_ARE_REC_1_REC_2.source"),
				get("FD_FILE_NAME_DATA_RECORDS_ARE_REC_1_REC_2.tree")
			);
	}

	@Test public void FD_FILE_NAME_DATA_RECORDS_ARE_REC_1_REC_2_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2_LINES_AT_BOTTOM_2() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_DATA_RECORDS_ARE_REC_1_REC_2_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2_LINES_AT_BOTTOM_2.source"),
				get("FD_FILE_NAME_DATA_RECORDS_ARE_REC_1_REC_2_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2_LINES_AT_BOTTOM_2.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2_LINES_AT_BOTTOM_2() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2_LINES_AT_BOTTOM_2.source"),
				get("FD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2_LINES_AT_BOTTOM_2.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2.source"),
				get("FD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_BOTTOM_2() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_BOTTOM_2.source"),
				get("FD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_BOTTOM_2.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_TOP_2_LINES_AT_BOTTOM_2() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_TOP_2_LINES_AT_BOTTOM_2.source"),
				get("FD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_TOP_2_LINES_AT_BOTTOM_2.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50.source"),
				get("FD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_BOTTOM_2() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_BOTTOM_2.source"),
				get("FD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_BOTTOM_2.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_TOP_2() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_TOP_2.source"),
				get("FD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_TOP_2.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_60_LINES() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_60_LINES.source"),
				get("FD_FILE_NAME_LINAGE_IS_60_LINES.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_TOP_LINES_AT_TOP_LINES_AT_BOTTOM_LINES_AT_BOTTOM() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_TOP_LINES_AT_TOP_LINES_AT_BOTTOM_LINES_AT_BOTTOM.source"),
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_TOP_LINES_AT_TOP_LINES_AT_BOTTOM_LINES_AT_BOTTOM.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_TOP_LINES_AT_TOP() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_TOP_LINES_AT_TOP.source"),
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_TOP_LINES_AT_TOP.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_BOTTOM_LINES_AT_BOTTOM() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_BOTTOM_LINES_AT_BOTTOM.source"),
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_BOTTOM_LINES_AT_BOTTOM.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_TOP_LINES_AT_TOP_LINES_AT_BOTTOM_LINES_AT_BOTTOM() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_TOP_LINES_AT_TOP_LINES_AT_BOTTOM_LINES_AT_BOTTOM.source"),
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_TOP_LINES_AT_TOP_LINES_AT_BOTTOM_LINES_AT_BOTTOM.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES.source"),
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_BOTTOM_LINES_AT_BOTTOM() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_BOTTOM_LINES_AT_BOTTOM.source"),
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_BOTTOM_LINES_AT_BOTTOM.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_TOP_LINES_AT_TOP() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_TOP_LINES_AT_TOP.source"),
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_TOP_LINES_AT_TOP.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES.source"),
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES.tree")
			);
	}

	@Test public void FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_RECORDING_MODE_IS_F() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_RECORDING_MODE_IS_F.source"),
				get("FD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_RECORDING_MODE_IS_F.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORDING_MODE_IS_F() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORDING_MODE_IS_F.source"),
				get("FD_FILE_NAME_RECORDING_MODE_IS_F.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORDING_MODE_IS_V() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORDING_MODE_IS_V.source"),
				get("FD_FILE_NAME_RECORDING_MODE_IS_V.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORDING_MODE_IS_U() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORDING_MODE_IS_U.source"),
				get("FD_FILE_NAME_RECORDING_MODE_IS_U.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORDING_MODE_IS_S() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORDING_MODE_IS_S.source"),
				get("FD_FILE_NAME_RECORDING_MODE_IS_S.tree")
			);
	}

	@Test public void FD_FILE_NAME_RECORDING_MODE_IS_S_CODE_SET_IS_ASCII() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_RECORDING_MODE_IS_S_CODE_SET_IS_ASCII.source"),
				get("FD_FILE_NAME_RECORDING_MODE_IS_S_CODE_SET_IS_ASCII.tree")
			);
	}

	@Test public void FD_FILE_NAME_CODE_SET_IS_ASCII() {
		helper.compileAndVerify(
				get("FD_FILE_NAME_CODE_SET_IS_ASCII.source"),
				get("FD_FILE_NAME_CODE_SET_IS_ASCII.tree")
			);
	}

	@Test public void SD_FILE_NAME() {
		helper.compileAndVerify(
				get("SD_FILE_NAME.source"),
				get("SD_FILE_NAME.tree")
			);
	}

	@Test public void SD_FILE_NAME_BLOCK_CONTAINS_5_CHARACTERS() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_BLOCK_CONTAINS_5_CHARACTERS.source"),
				get("SD_FILE_NAME_BLOCK_CONTAINS_5_CHARACTERS.tree")
			);
	}

	@Test public void SD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_CHARACTERS() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_CHARACTERS.source"),
				get("SD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_CHARACTERS.tree")
			);
	}

	@Test public void SD_FILE_NAME_BLOCK_CONTAINS_5_RECORDS() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_BLOCK_CONTAINS_5_RECORDS.source"),
				get("SD_FILE_NAME_BLOCK_CONTAINS_5_RECORDS.tree")
			);
	}

	@Test public void SD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_RECORDS() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_RECORDS.source"),
				get("SD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_RECORDS.tree")
			);
	}

	@Test public void SD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_RECORDS_RECORD_CONTAINS_80_CHARACTERS() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_RECORDS_RECORD_CONTAINS_80_CHARACTERS.source"),
				get("SD_FILE_NAME_BLOCK_CONTAINS_5_TO_10_RECORDS_RECORD_CONTAINS_80_CHARACTERS.tree")
			);
	}

	@Test public void SD_FILE_NAME_RECORD_CONTAINS_80_CHARACTERS() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_RECORD_CONTAINS_80_CHARACTERS.source"),
				get("SD_FILE_NAME_RECORD_CONTAINS_80_CHARACTERS.tree")
			);
	}

	@Test public void SD_FILE_NAME_RECORD_CONTAINS_80_TO_120_CHARACTERS() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_RECORD_CONTAINS_80_TO_120_CHARACTERS.source"),
				get("SD_FILE_NAME_RECORD_CONTAINS_80_TO_120_CHARACTERS.tree")
			);
	}

	@Test public void SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_TO_120_CHARACTERS_DEPENDING_ON_REC_LEN() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_TO_120_CHARACTERS_DEPENDING_ON_REC_LEN.source"),
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_TO_120_CHARACTERS_DEPENDING_ON_REC_LEN.tree")
			);
	}

	@Test public void SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_TO_120_CHARACTERS() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_TO_120_CHARACTERS.source"),
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_TO_120_CHARACTERS.tree")
			);
	}

	@Test public void SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_TO_120_CHARACTERS_DEPENDING_ON_REC_LEN() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_TO_120_CHARACTERS_DEPENDING_ON_REC_LEN.source"),
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_TO_120_CHARACTERS_DEPENDING_ON_REC_LEN.tree")
			);
	}

	@Test public void SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_TO_120_CHARACTERS() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_TO_120_CHARACTERS.source"),
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_TO_120_CHARACTERS.tree")
			);
	}

	@Test public void SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_CHARACTERS_DEPENDING_ON_REC_LEN() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_CHARACTERS_DEPENDING_ON_REC_LEN.source"),
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_CHARACTERS_DEPENDING_ON_REC_LEN.tree")
			);
	}

	@Test public void SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_CHARACTERS() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_CHARACTERS.source"),
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_FROM_80_CHARACTERS.tree")
			);
	}

	@Test public void SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS_DEPENDING_ON_REC_LEN() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS_DEPENDING_ON_REC_LEN.source"),
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS_DEPENDING_ON_REC_LEN.tree")
			);
	}

	@Test public void SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS.source"),
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS.tree")
			);
	}

	@Test public void SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS_LABEL_RECORD_IS_STANDARD() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS_LABEL_RECORD_IS_STANDARD.source"),
				get("SD_FILE_NAME_RECORD_IS_VARYING_IN_SIZE_CHARACTERS_LABEL_RECORD_IS_STANDARD.tree")
			);
	}

	@Test public void SD_FILE_NAME_LABEL_RECORD_IS_STANDARD() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LABEL_RECORD_IS_STANDARD.source"),
				get("SD_FILE_NAME_LABEL_RECORD_IS_STANDARD.tree")
			);
	}

	@Test public void SD_FILE_NAME_LABEL_RECORD_IS_OMITTED() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LABEL_RECORD_IS_OMITTED.source"),
				get("SD_FILE_NAME_LABEL_RECORD_IS_OMITTED.tree")
			);
	}

	@Test public void SD_FILE_NAME_LABEL_RECORD_IS_LABEL_1_LABEL_2() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LABEL_RECORD_IS_LABEL_1_LABEL_2.source"),
				get("SD_FILE_NAME_LABEL_RECORD_IS_LABEL_1_LABEL_2.tree")
			);
	}

	@Test public void SD_FILE_NAME_LABEL_RECORD_IS_LABEL_1() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LABEL_RECORD_IS_LABEL_1.source"),
				get("SD_FILE_NAME_LABEL_RECORD_IS_LABEL_1.tree")
			);
	}

	@Waive({CompilationError.SYNTAX_ERROR})
	@Test public void SD_FILE_NAME_LABEL_RECORD_IS() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LABEL_RECORD_IS.source"),
				get("SD_FILE_NAME_LABEL_RECORD_IS.tree")
			);
	}

	@Test public void SD_FILE_NAME_LABEL_RECORDS_ARE_STANDARD() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LABEL_RECORDS_ARE_STANDARD.source"),
				get("SD_FILE_NAME_LABEL_RECORDS_ARE_STANDARD.tree")
			);
	}

	@Test public void SD_FILE_NAME_LABEL_RECORDS_ARE_OMITTED() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LABEL_RECORDS_ARE_OMITTED.source"),
				get("SD_FILE_NAME_LABEL_RECORDS_ARE_OMITTED.tree")
			);
	}

	@Test public void SD_FILE_NAME_LABEL_RECORDS_ARE_LABEL_1_LABEL_2() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LABEL_RECORDS_ARE_LABEL_1_LABEL_2.source"),
				get("SD_FILE_NAME_LABEL_RECORDS_ARE_LABEL_1_LABEL_2.tree")
			);
	}

	@Test public void SD_FILE_NAME_LABEL_RECORDS_ARE_LABEL_1() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LABEL_RECORDS_ARE_LABEL_1.source"),
				get("SD_FILE_NAME_LABEL_RECORDS_ARE_LABEL_1.tree")
			);
	}

	@Waive({CompilationError.SYNTAX_ERROR})
	@Test public void SD_FILE_NAME_LABEL_RECORDS_ARE() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LABEL_RECORDS_ARE.source"),
				get("SD_FILE_NAME_LABEL_RECORDS_ARE.tree")
			);
	}

	@Test public void SD_FILE_NAME_LABEL_RECORDS_ARE_VALUE_OF_THIS_NAME_IS_THAT_VALUE_TWELVE_IS_12_ABC_IS_ABC() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LABEL_RECORDS_ARE_VALUE_OF_THIS_NAME_IS_THAT_VALUE_TWELVE_IS_12_ABC_IS_ABC.source"),
				get("SD_FILE_NAME_LABEL_RECORDS_ARE_VALUE_OF_THIS_NAME_IS_THAT_VALUE_TWELVE_IS_12_ABC_IS_ABC.tree")
			);
	}

	@Test public void SD_FILE_NAME_VALUE_OF_THIS_NAME_IS_THAT_VALUE_TWELVE_IS_12_ABC_IS_ABC() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_VALUE_OF_THIS_NAME_IS_THAT_VALUE_TWELVE_IS_12_ABC_IS_ABC.source"),
				get("SD_FILE_NAME_VALUE_OF_THIS_NAME_IS_THAT_VALUE_TWELVE_IS_12_ABC_IS_ABC.tree")
			);
	}

	@Test public void SD_FILE_NAME_VALUE_OF_THIS_NAME_IS_THAT_VALUE() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_VALUE_OF_THIS_NAME_IS_THAT_VALUE.source"),
				get("SD_FILE_NAME_VALUE_OF_THIS_NAME_IS_THAT_VALUE.tree")
			);
	}

	@Test public void SD_FILE_NAME_VALUE_OF_TWELVE_IS_12() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_VALUE_OF_TWELVE_IS_12.source"),
				get("SD_FILE_NAME_VALUE_OF_TWELVE_IS_12.tree")
			);
	}

	@Test public void SD_FILE_NAME_VALUE_OF_ABC_IS_ABC() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_VALUE_OF_ABC_IS_ABC.source"),
				get("SD_FILE_NAME_VALUE_OF_ABC_IS_ABC.tree")
			);
	}

	@Test public void SD_FILE_NAME_VALUE_OF_ABC_IS_ABC_DATA_RECORD_IS_REC_1() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_VALUE_OF_ABC_IS_ABC_DATA_RECORD_IS_REC_1.source"),
				get("SD_FILE_NAME_VALUE_OF_ABC_IS_ABC_DATA_RECORD_IS_REC_1.tree")
			);
	}

	@Test public void SD_FILE_NAME_DATA_RECORD_IS_REC_1() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_DATA_RECORD_IS_REC_1.source"),
				get("SD_FILE_NAME_DATA_RECORD_IS_REC_1.tree")
			);
	}

	@Test public void SD_FILE_NAME_DATA_RECORDS_ARE_REC_1_REC_2() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_DATA_RECORDS_ARE_REC_1_REC_2.source"),
				get("SD_FILE_NAME_DATA_RECORDS_ARE_REC_1_REC_2.tree")
			);
	}

	@Test public void SD_FILE_NAME_DATA_RECORDS_ARE_REC_1_REC_2_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2_LINES_AT_BOTTOM_2() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_DATA_RECORDS_ARE_REC_1_REC_2_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2_LINES_AT_BOTTOM_2.source"),
				get("SD_FILE_NAME_DATA_RECORDS_ARE_REC_1_REC_2_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2_LINES_AT_BOTTOM_2.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2_LINES_AT_BOTTOM_2() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2_LINES_AT_BOTTOM_2.source"),
				get("SD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2_LINES_AT_BOTTOM_2.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2.source"),
				get("SD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_TOP_2.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_BOTTOM_2() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_BOTTOM_2.source"),
				get("SD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50_LINES_AT_BOTTOM_2.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_TOP_2_LINES_AT_BOTTOM_2() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_TOP_2_LINES_AT_BOTTOM_2.source"),
				get("SD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_TOP_2_LINES_AT_BOTTOM_2.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50.source"),
				get("SD_FILE_NAME_LINAGE_IS_60_LINES_WITH_FOOTING_AT_50.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_BOTTOM_2() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_BOTTOM_2.source"),
				get("SD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_BOTTOM_2.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_TOP_2() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_TOP_2.source"),
				get("SD_FILE_NAME_LINAGE_IS_60_LINES_LINES_AT_TOP_2.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_60_LINES() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_60_LINES.source"),
				get("SD_FILE_NAME_LINAGE_IS_60_LINES.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_TOP_LINES_AT_TOP_LINES_AT_BOTTOM_LINES_AT_BOTTOM() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_TOP_LINES_AT_TOP_LINES_AT_BOTTOM_LINES_AT_BOTTOM.source"),
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_TOP_LINES_AT_TOP_LINES_AT_BOTTOM_LINES_AT_BOTTOM.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_TOP_LINES_AT_TOP() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_TOP_LINES_AT_TOP.source"),
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_TOP_LINES_AT_TOP.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_BOTTOM_LINES_AT_BOTTOM() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_BOTTOM_LINES_AT_BOTTOM.source"),
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES_AT_BOTTOM_LINES_AT_BOTTOM.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_TOP_LINES_AT_TOP_LINES_AT_BOTTOM_LINES_AT_BOTTOM() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_TOP_LINES_AT_TOP_LINES_AT_BOTTOM_LINES_AT_BOTTOM.source"),
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_TOP_LINES_AT_TOP_LINES_AT_BOTTOM_LINES_AT_BOTTOM.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES.source"),
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_WITH_FOOTING_AT_FOOTING_LINES.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_BOTTOM_LINES_AT_BOTTOM() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_BOTTOM_LINES_AT_BOTTOM.source"),
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_BOTTOM_LINES_AT_BOTTOM.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_TOP_LINES_AT_TOP() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_TOP_LINES_AT_TOP.source"),
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_LINES_AT_TOP_LINES_AT_TOP.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES.source"),
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES.tree")
			);
	}

	@Test public void SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_CODE_SET_IS_ASCII() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_CODE_SET_IS_ASCII.source"),
				get("SD_FILE_NAME_LINAGE_IS_V_LINAGE_LINES_CODE_SET_IS_ASCII.tree")
			);
	}

	@Test public void SD_FILE_NAME_CODE_SET_IS_ASCII() {
		helper.compileAndVerify(
				get("SD_FILE_NAME_CODE_SET_IS_ASCII.source"),
				get("SD_FILE_NAME_CODE_SET_IS_ASCII.tree")
			);
	}
}