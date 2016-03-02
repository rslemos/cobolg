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

import br.eti.rslemos.cobolg.COBOLParser.ConfigurationSectionContext;

public class ConfigurationSectionUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.configurationSection");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<ConfigurationSectionContext> helper = new CompilerHelper<ConfigurationSectionContext>() {
		@Override protected ConfigurationSectionContext parsePart() { return parser.configurationSection(); }
	};

	@Test public void EMPTY() {
		helper.compileAndVerify(
				get("EMPTY.source"),
				get("EMPTY.tree")
			);
	}

	@Test public void SOURCE_COMPUTER_XYZ() {
		helper.compileAndVerify(
				get("SOURCE_COMPUTER_XYZ.source"),
				get("SOURCE_COMPUTER_XYZ.tree")
			);
	}

	@Test public void OBJECT_COMPUTER() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER.source"),
				get("OBJECT_COMPUTER.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ.source"),
				get("OBJECT_COMPUTER_XYZ.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_MEMORY_SIZE_5000000_WORDS() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_MEMORY_SIZE_5000000_WORDS.source"),
				get("OBJECT_COMPUTER_XYZ_MEMORY_SIZE_5000000_WORDS.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_MEMORY_SIZE_5000000_CHARACTERS() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_MEMORY_SIZE_5000000_CHARACTERS.source"),
				get("OBJECT_COMPUTER_XYZ_MEMORY_SIZE_5000000_CHARACTERS.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_MEMORY_SIZE_5000000_MODULES() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_MEMORY_SIZE_5000000_MODULES.source"),
				get("OBJECT_COMPUTER_XYZ_MEMORY_SIZE_5000000_MODULES.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_MEMORY_5000000_WORDS() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_MEMORY_5000000_WORDS.source"),
				get("OBJECT_COMPUTER_XYZ_MEMORY_5000000_WORDS.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_MEMORY_5000000_CHARACTERS() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_MEMORY_5000000_CHARACTERS.source"),
				get("OBJECT_COMPUTER_XYZ_MEMORY_5000000_CHARACTERS.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_MEMORY_5000000_MODULES() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_MEMORY_5000000_MODULES.source"),
				get("OBJECT_COMPUTER_XYZ_MEMORY_5000000_MODULES.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_PROGRAM_COLLATING_SEQUENCE_IS_ISO88591() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_PROGRAM_COLLATING_SEQUENCE_IS_ISO88591.source"),
				get("OBJECT_COMPUTER_XYZ_PROGRAM_COLLATING_SEQUENCE_IS_ISO88591.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_PROGRAM_COLLATING_SEQUENCE_ISO88591() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_PROGRAM_COLLATING_SEQUENCE_ISO88591.source"),
				get("OBJECT_COMPUTER_XYZ_PROGRAM_COLLATING_SEQUENCE_ISO88591.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_PROGRAM_SEQUENCE_IS_ISO88591() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_PROGRAM_SEQUENCE_IS_ISO88591.source"),
				get("OBJECT_COMPUTER_XYZ_PROGRAM_SEQUENCE_IS_ISO88591.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_COLLATING_SEQUENCE_IS_ISO88591() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_COLLATING_SEQUENCE_IS_ISO88591.source"),
				get("OBJECT_COMPUTER_XYZ_COLLATING_SEQUENCE_IS_ISO88591.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_PROGRAM_SEQUENCE_ISO88591() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_PROGRAM_SEQUENCE_ISO88591.source"),
				get("OBJECT_COMPUTER_XYZ_PROGRAM_SEQUENCE_ISO88591.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_COLLATING_SEQUENCE_ISO88591() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_COLLATING_SEQUENCE_ISO88591.source"),
				get("OBJECT_COMPUTER_XYZ_COLLATING_SEQUENCE_ISO88591.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_SEQUENCE_IS_ISO88591() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_SEQUENCE_IS_ISO88591.source"),
				get("OBJECT_COMPUTER_XYZ_SEQUENCE_IS_ISO88591.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_SEQUENCE_ISO88591() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_SEQUENCE_ISO88591.source"),
				get("OBJECT_COMPUTER_XYZ_SEQUENCE_ISO88591.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_SEGMENT_LIMIT_IS_30() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_SEGMENT_LIMIT_IS_30.source"),
				get("OBJECT_COMPUTER_XYZ_SEGMENT_LIMIT_IS_30.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_SEGMENT_LIMIT_30() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_SEGMENT_LIMIT_30.source"),
				get("OBJECT_COMPUTER_XYZ_SEGMENT_LIMIT_30.tree")
			);
	}

	@Test public void OBJECT_COMPUTER_XYZ_MEMORY_SIZE_5000000_WORDS_PROGRAM_COLLATING_SEQUENCE_IS_ISO88591_SEGMENT_LIMIT_IS_30() {
		helper.compileAndVerify(
				get("OBJECT_COMPUTER_XYZ_MEMORY_SIZE_5000000_WORDS_PROGRAM_COLLATING_SEQUENCE_IS_ISO88591_SEGMENT_LIMIT_IS_30.source"),
				get("OBJECT_COMPUTER_XYZ_MEMORY_SIZE_5000000_WORDS_PROGRAM_COLLATING_SEQUENCE_IS_ISO88591_SEGMENT_LIMIT_IS_30.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ABC_IS_XYZ() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ABC_IS_XYZ.source"),
				get("SPECIAL_NAMES_ABC_IS_XYZ.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ABC_XYZ() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ABC_XYZ.source"),
				get("SPECIAL_NAMES_ABC_XYZ.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ABC_IS_XYZ_ON_STATUS_IS_CONDITION_A() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ABC_IS_XYZ_ON_STATUS_IS_CONDITION_A.source"),
				get("SPECIAL_NAMES_ABC_IS_XYZ_ON_STATUS_IS_CONDITION_A.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ABC_IS_XYZ_ON_STATUS_IS_CONDITION_A_OFF_STATUS_IS_CONDITION_B() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ABC_IS_XYZ_ON_STATUS_IS_CONDITION_A_OFF_STATUS_IS_CONDITION_B.source"),
				get("SPECIAL_NAMES_ABC_IS_XYZ_ON_STATUS_IS_CONDITION_A_OFF_STATUS_IS_CONDITION_B.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ABC_IS_XYZ_OFF_STATUS_IS_CONDITION_A() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ABC_IS_XYZ_OFF_STATUS_IS_CONDITION_A.source"),
				get("SPECIAL_NAMES_ABC_IS_XYZ_OFF_STATUS_IS_CONDITION_A.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ABC_IS_XYZ_OFF_STATUS_IS_CONDITION_A_ON_STATUS_IS_CONDITION_B() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ABC_IS_XYZ_OFF_STATUS_IS_CONDITION_A_ON_STATUS_IS_CONDITION_B.source"),
				get("SPECIAL_NAMES_ABC_IS_XYZ_OFF_STATUS_IS_CONDITION_A_ON_STATUS_IS_CONDITION_B.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ABC_ON_STATUS_IS_CONDITION_A() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ABC_ON_STATUS_IS_CONDITION_A.source"),
				get("SPECIAL_NAMES_ABC_ON_STATUS_IS_CONDITION_A.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ABC_ON_STATUS_IS_CONDITION_A_OFF_STATUS_IS_CONDITION_B() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ABC_ON_STATUS_IS_CONDITION_A_OFF_STATUS_IS_CONDITION_B.source"),
				get("SPECIAL_NAMES_ABC_ON_STATUS_IS_CONDITION_A_OFF_STATUS_IS_CONDITION_B.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ABC_OFF_STATUS_IS_CONDITION_A() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ABC_OFF_STATUS_IS_CONDITION_A.source"),
				get("SPECIAL_NAMES_ABC_OFF_STATUS_IS_CONDITION_A.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ABC_OFF_STATUS_IS_CONDITION_A_ON_STATUS_IS_CONDITION_B() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ABC_OFF_STATUS_IS_CONDITION_A_ON_STATUS_IS_CONDITION_B.source"),
				get("SPECIAL_NAMES_ABC_OFF_STATUS_IS_CONDITION_A_ON_STATUS_IS_CONDITION_B.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ALPHABET_ABC_IS_NATIVE() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ALPHABET_ABC_IS_NATIVE.source"),
				get("SPECIAL_NAMES_ALPHABET_ABC_IS_NATIVE.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ALPHABET_ABC_IS_A_THROUGH_C() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ALPHABET_ABC_IS_A_THROUGH_C.source"),
				get("SPECIAL_NAMES_ALPHABET_ABC_IS_A_THROUGH_C.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ALPHABET_ABC_IS_A_THROUGH_C_A_ALSO_a_B_ALSO_b() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ALPHABET_ABC_IS_A_THROUGH_C_A_ALSO_a_B_ALSO_b.source"),
				get("SPECIAL_NAMES_ALPHABET_ABC_IS_A_THROUGH_C_A_ALSO_a_B_ALSO_b.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ALPHABET_AZ_IS_A_THROUGH_Z_DOT_ALSO_COMMA_ALSO_COLON_ALSO_SEMICOLON() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ALPHABET_AZ_IS_A_THROUGH_Z_DOT_ALSO_COMMA_ALSO_COLON_ALSO_SEMICOLON.source"),
				get("SPECIAL_NAMES_ALPHABET_AZ_IS_A_THROUGH_Z_DOT_ALSO_COMMA_ALSO_COLON_ALSO_SEMICOLON.tree")
			);
	}

	@Test public void SPECIAL_NAMES_ALPHABET_AZ_IS_A_THROUGH_Z_DOT_ALSO_COMMA_0_THRU_9() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_ALPHABET_AZ_IS_A_THROUGH_Z_DOT_ALSO_COMMA_0_THRU_9.source"),
				get("SPECIAL_NAMES_ALPHABET_AZ_IS_A_THROUGH_Z_DOT_ALSO_COMMA_0_THRU_9.tree")
			);
	}

	@Test public void SPECIAL_NAMES_SYMBOLIC_CHARACTERS_ABC_ARE_65_66_67_IN_ASCII() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_SYMBOLIC_CHARACTERS_ABC_ARE_65_66_67_IN_ASCII.source"),
				get("SPECIAL_NAMES_SYMBOLIC_CHARACTERS_ABC_ARE_65_66_67_IN_ASCII.tree")
			);
	}

	@Test public void SPECIAL_NAMES_SYMBOLIC_CHARACTERS_A_IS_65_B_IS_66_C_IS_67() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_SYMBOLIC_CHARACTERS_A_IS_65_B_IS_66_C_IS_67.source"),
				get("SPECIAL_NAMES_SYMBOLIC_CHARACTERS_A_IS_65_B_IS_66_C_IS_67.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CLASS_ABCD_IS_A_THROUGH_D() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CLASS_ABCD_IS_A_THROUGH_D.source"),
				get("SPECIAL_NAMES_CLASS_ABCD_IS_A_THROUGH_D.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CLASS_ABCD_IS_A_THRU_D() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CLASS_ABCD_IS_A_THRU_D.source"),
				get("SPECIAL_NAMES_CLASS_ABCD_IS_A_THRU_D.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CLASS_ABCD_IS_ABCD() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CLASS_ABCD_IS_ABCD.source"),
				get("SPECIAL_NAMES_CLASS_ABCD_IS_ABCD.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CLASS_ABCD_IS_A_B_C_D() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CLASS_ABCD_IS_A_B_C_D.source"),
				get("SPECIAL_NAMES_CLASS_ABCD_IS_A_B_C_D.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CLASS_ABCD_A_THROUGH_D() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CLASS_ABCD_A_THROUGH_D.source"),
				get("SPECIAL_NAMES_CLASS_ABCD_A_THROUGH_D.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CLASS_ABCD_A_THRU_D() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CLASS_ABCD_A_THRU_D.source"),
				get("SPECIAL_NAMES_CLASS_ABCD_A_THRU_D.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CLASS_ABCD_ABCD() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CLASS_ABCD_ABCD.source"),
				get("SPECIAL_NAMES_CLASS_ABCD_ABCD.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CLASS_ABCD_A_B_C_D() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CLASS_ABCD_A_B_C_D.source"),
				get("SPECIAL_NAMES_CLASS_ABCD_A_B_C_D.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CURRENCY_SIGN_IS_US$_WITH_PICTURE_SYMBOL_$() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CURRENCY_SIGN_IS_US$_WITH_PICTURE_SYMBOL_$.source"),
				get("SPECIAL_NAMES_CURRENCY_SIGN_IS_US$_WITH_PICTURE_SYMBOL_$.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CURRENCY_SIGN_IS_US$_PICTURE_SYMBOL_$() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CURRENCY_SIGN_IS_US$_PICTURE_SYMBOL_$.source"),
				get("SPECIAL_NAMES_CURRENCY_SIGN_IS_US$_PICTURE_SYMBOL_$.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CURRENCY_SIGN_IS_$() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CURRENCY_SIGN_IS_$.source"),
				get("SPECIAL_NAMES_CURRENCY_SIGN_IS_$.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CURRENCY_SIGN_US$_WITH_PICTURE_SYMBOL_$() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CURRENCY_SIGN_US$_WITH_PICTURE_SYMBOL_$.source"),
				get("SPECIAL_NAMES_CURRENCY_SIGN_US$_WITH_PICTURE_SYMBOL_$.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CURRENCY_SIGN_US$_PICTURE_SYMBOL_$() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CURRENCY_SIGN_US$_PICTURE_SYMBOL_$.source"),
				get("SPECIAL_NAMES_CURRENCY_SIGN_US$_PICTURE_SYMBOL_$.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CURRENCY_SIGN_$() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CURRENCY_SIGN_$.source"),
				get("SPECIAL_NAMES_CURRENCY_SIGN_$.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CURRENCY_IS_US$_WITH_PICTURE_SYMBOL_$() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CURRENCY_IS_US$_WITH_PICTURE_SYMBOL_$.source"),
				get("SPECIAL_NAMES_CURRENCY_IS_US$_WITH_PICTURE_SYMBOL_$.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CURRENCY_IS_US$_PICTURE_SYMBOL_$() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CURRENCY_IS_US$_PICTURE_SYMBOL_$.source"),
				get("SPECIAL_NAMES_CURRENCY_IS_US$_PICTURE_SYMBOL_$.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CURRENCY_IS_$() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CURRENCY_IS_$.source"),
				get("SPECIAL_NAMES_CURRENCY_IS_$.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CURRENCY_US$_WITH_PICTURE_SYMBOL_$() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CURRENCY_US$_WITH_PICTURE_SYMBOL_$.source"),
				get("SPECIAL_NAMES_CURRENCY_US$_WITH_PICTURE_SYMBOL_$.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CURRENCY_US$_PICTURE_SYMBOL_$() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CURRENCY_US$_PICTURE_SYMBOL_$.source"),
				get("SPECIAL_NAMES_CURRENCY_US$_PICTURE_SYMBOL_$.tree")
			);
	}

	@Test public void SPECIAL_NAMES_CURRENCY_$() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_CURRENCY_$.source"),
				get("SPECIAL_NAMES_CURRENCY_$.tree")
			);
	}

	@Test public void SPECIAL_NAMES_DECIMAL_POINT_IS_COMMA() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_DECIMAL_POINT_IS_COMMA.source"),
				get("SPECIAL_NAMES_DECIMAL_POINT_IS_COMMA.tree")
			);
	}

	@Test public void SPECIAL_NAMES_DECIMAL_POINT_COMMA() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_DECIMAL_POINT_COMMA.source"),
				get("SPECIAL_NAMES_DECIMAL_POINT_COMMA.tree")
			);
	}

	@Test public void SPECIAL_NAMES_XML_SCHEMA_XHTML_IS_DDNAME() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_XML_SCHEMA_XHTML_IS_DDNAME.source"),
				get("SPECIAL_NAMES_XML_SCHEMA_XHTML_IS_DDNAME.tree")
			);
	}

	@Test public void SPECIAL_NAMES_XML_SCHEMA_XHTML_IS_URL() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_XML_SCHEMA_XHTML_IS_URL.source"),
				get("SPECIAL_NAMES_XML_SCHEMA_XHTML_IS_URL.tree")
			);
	}

	@Test public void SPECIAL_NAMES_XML_SCHEMA_XHTML_DDNAME() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_XML_SCHEMA_XHTML_DDNAME.source"),
				get("SPECIAL_NAMES_XML_SCHEMA_XHTML_DDNAME.tree")
			);
	}

	@Test public void SPECIAL_NAMES_XML_SCHEMA_XHTML_URL() {
		helper.compileAndVerify(
				get("SPECIAL_NAMES_XML_SCHEMA_XHTML_URL.source"),
				get("SPECIAL_NAMES_XML_SCHEMA_XHTML_URL.tree")
			);
	}

	@Test public void REPOSITORY_CLASS_JAVAEXCEPTION() {
		helper.compileAndVerify(
				get("REPOSITORY_CLASS_JAVAEXCEPTION.source"),
				get("REPOSITORY_CLASS_JAVAEXCEPTION.tree")
			);
	}

	@Test public void REPOSITORY_CLASS_JAVAEXCEPTION_IS_JAVA_LANG_EXCEPTION() {
		helper.compileAndVerify(
				get("REPOSITORY_CLASS_JAVAEXCEPTION_IS_JAVA_LANG_EXCEPTION.source"),
				get("REPOSITORY_CLASS_JAVAEXCEPTION_IS_JAVA_LANG_EXCEPTION.tree")
			);
	}

	@Test public void REPOSITORY_CLASS_MYCLASS_CLASS_JAVAEXCEPTION_IS_JAVA_LANG_EXCEPTION() {
		helper.compileAndVerify(
				get("REPOSITORY_CLASS_MYCLASS_CLASS_JAVAEXCEPTION_IS_JAVA_LANG_EXCEPTION.source"),
				get("REPOSITORY_CLASS_MYCLASS_CLASS_JAVAEXCEPTION_IS_JAVA_LANG_EXCEPTION.tree")
			);
	}
}
