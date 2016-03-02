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

import br.eti.rslemos.cobolg.COBOLParser.SpecialRegisterContext;

public class SpecialRegisterUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.specialRegister");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<SpecialRegisterContext> helper = new CompilerHelper<SpecialRegisterContext>() {
		@Override protected SpecialRegisterContext parsePart() { return parser.specialRegister(); }
	};
	
	@Test public void DEBUG_CONTENTS() {
		helper.compileAndVerify(
				get("DEBUG_CONTENTS.source"),
				get("DEBUG_CONTENTS.tree")
			);
	}

	@Test public void DEBUG_ITEM() {
		helper.compileAndVerify(
				get("DEBUG_ITEM.source"),
				get("DEBUG_ITEM.tree")
			);
	}

	@Test public void DEBUG_LINE() {
		helper.compileAndVerify(
				get("DEBUG_LINE.source"),
				get("DEBUG_LINE.tree")
			);
	}

	@Test public void DEBUG_NAME() {
		helper.compileAndVerify(
				get("DEBUG_NAME.source"),
				get("DEBUG_NAME.tree")
			);
	}

	@Test public void DEBUG_SUB_1() {
		helper.compileAndVerify(
				get("DEBUG_SUB_1.source"),
				get("DEBUG_SUB_1.tree")
			);
	}

	@Test public void DEBUG_SUB_2() {
		helper.compileAndVerify(
				get("DEBUG_SUB_2.source"),
				get("DEBUG_SUB_2.tree")
			);
	}

	@Test public void DEBUG_SUB_3() {
		helper.compileAndVerify(
				get("DEBUG_SUB_3.source"),
				get("DEBUG_SUB_3.tree")
			);
	}

	@Test public void JNIENVPTR() {
		helper.compileAndVerify(
				get("JNIENVPTR.source"),
				get("JNIENVPTR.tree")
			);
	}

	@Test public void LINAGE_COUNTER() {
		helper.compileAndVerify(
				get("LINAGE_COUNTER.source"),
				get("LINAGE_COUNTER.tree")
			);
	}

	@Test public void RETURN_CODE() {
		helper.compileAndVerify(
				get("RETURN_CODE.source"),
				get("RETURN_CODE.tree")
			);
	}

	@Test public void SHIFT_IN() {
		helper.compileAndVerify(
				get("SHIFT_IN.source"),
				get("SHIFT_IN.tree")
			);
	}

	@Test public void SHIFT_OUT() {
		helper.compileAndVerify(
				get("SHIFT_OUT.source"),
				get("SHIFT_OUT.tree")
			);
	}

	@Test public void SORT_CONTROL() {
		helper.compileAndVerify(
				get("SORT_CONTROL.source"),
				get("SORT_CONTROL.tree")
			);
	}

	@Test public void SORT_CORE_SIZE() {
		helper.compileAndVerify(
				get("SORT_CORE_SIZE.source"),
				get("SORT_CORE_SIZE.tree")
			);
	}

	@Test public void SORT_FILE_SIZE() {
		helper.compileAndVerify(
				get("SORT_FILE_SIZE.source"),
				get("SORT_FILE_SIZE.tree")
			);
	}

	@Test public void SORT_MESSAGE() {
		helper.compileAndVerify(
				get("SORT_MESSAGE.source"),
				get("SORT_MESSAGE.tree")
			);
	}

	@Test public void SORT_MODE_SIZE() {
		helper.compileAndVerify(
				get("SORT_MODE_SIZE.source"),
				get("SORT_MODE_SIZE.tree")
			);
	}

	@Test public void SORT_RETURN() {
		helper.compileAndVerify(
				get("SORT_RETURN.source"),
				get("SORT_RETURN.tree")
			);
	}

	@Test public void TALLY() {
		helper.compileAndVerify(
				get("TALLY.source"),
				get("TALLY.tree")
			);
	}

	@Test public void WHEN_COMPILED() {
		helper.compileAndVerify(
				get("WHEN_COMPILED.source"),
				get("WHEN_COMPILED.tree")
			);
	}

	@Test public void XML_CODE() {
		helper.compileAndVerify(
				get("XML_CODE.source"),
				get("XML_CODE.tree")
			);
	}

	@Test public void XML_EVENT() {
		helper.compileAndVerify(
				get("XML_EVENT.source"),
				get("XML_EVENT.tree")
			);
	}

	@Test public void XML_INFORMATION() {
		helper.compileAndVerify(
				get("XML_INFORMATION.source"),
				get("XML_INFORMATION.tree")
			);
	}

	@Test public void XML_NAMESPACE() {
		helper.compileAndVerify(
				get("XML_NAMESPACE.source"),
				get("XML_NAMESPACE.tree")
			);
	}

	@Test public void XML_NAMESPACE_PREFIX() {
		helper.compileAndVerify(
				get("XML_NAMESPACE_PREFIX.source"),
				get("XML_NAMESPACE_PREFIX.tree")
			);
	}

	@Test public void XML_NNAMESPACE() {
		helper.compileAndVerify(
				get("XML_NNAMESPACE.source"),
				get("XML_NNAMESPACE.tree")
			);
	}

	@Test public void XML_NNAMESPACE_PREFIX() {
		helper.compileAndVerify(
				get("XML_NNAMESPACE_PREFIX.source"),
				get("XML_NNAMESPACE_PREFIX.tree")
			);
	}

	@Test public void XML_NTEXT() {
		helper.compileAndVerify(
				get("XML_NTEXT.source"),
				get("XML_NTEXT.tree")
			);
	}

	@Test public void XML_TEXT() {
		helper.compileAndVerify(
				get("XML_TEXT.source"),
				get("XML_TEXT.tree")
			);
	}

}
