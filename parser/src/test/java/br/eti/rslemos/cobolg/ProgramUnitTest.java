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

import br.eti.rslemos.cobolg.COBOLParser.ProgramContext;

public class ProgramUnitTest {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.program");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<ProgramContext> helper = new CompilerHelper<ProgramContext>() {
		@Override protected ProgramContext parsePart() { return parser.program(); }
	};
	
	@Test public void ID_DIVISION__PROGRAM_ID__PROGNAME_() {
		helper.compileAndVerify(
				get("ID_DIVISION__PROGRAM_ID__PROGNAME_.source"),
				get("ID_DIVISION__PROGRAM_ID__PROGNAME_.tree")
			);
	}
	
	@Test public void ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION_() {
		helper.compileAndVerify(
				get("ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION_.source"),
				get("ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION_.tree")
			);
	}
	
	@Test public void ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION__DATA_DIVISION_() {
		helper.compileAndVerify(
				get("ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION__DATA_DIVISION_.source"),
				get("ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION__DATA_DIVISION_.tree")
			);
	}
	
	@Test public void ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION__DATA_DIVISION__PROCEDURE_DIVISION__STOP_RUN_() {
		helper.compileAndVerify(
				get("ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION__DATA_DIVISION__PROCEDURE_DIVISION__STOP_RUN_.source"),
				get("ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION__DATA_DIVISION__PROCEDURE_DIVISION__STOP_RUN_.tree")
			);
	}
	
	@Test public void ID_DIVISION__PROGRAM_ID__PROGNAME__END_PROGRAM_PROGNAME_() {
		helper.compileAndVerify(
				get("ID_DIVISION__PROGRAM_ID__PROGNAME__END_PROGRAM_PROGNAME_.source"),
				get("ID_DIVISION__PROGRAM_ID__PROGNAME__END_PROGRAM_PROGNAME_.tree")
			);
	}
	
	@Test public void ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION__END_PROGRAM_PROGNAME_() {
		helper.compileAndVerify(
				get("ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION__END_PROGRAM_PROGNAME_.source"),
				get("ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION__END_PROGRAM_PROGNAME_.tree")
			);
	}
	
	@Test public void ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION__DATA_DIVISION__END_PROGRAM_PROGNAME_() {
		helper.compileAndVerify(
				get("ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION__DATA_DIVISION__END_PROGRAM_PROGNAME_.source"),
				get("ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION__DATA_DIVISION__END_PROGRAM_PROGNAME_.tree")
			);
	}
	
	@Test public void ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION__DATA_DIVISION__PROCEDURE_DIVISION__STOP_RUN__END_PROGRAM_PROGNAME_() {
		helper.compileAndVerify(
				get("ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION__DATA_DIVISION__PROCEDURE_DIVISION__STOP_RUN__END_PROGRAM_PROGNAME_.source"),
				get("ID_DIVISION__PROGRAM_ID__PROGNAME__ENVIRONMENT_DIVISION__DATA_DIVISION__PROCEDURE_DIVISION__STOP_RUN__END_PROGRAM_PROGNAME_.tree")
			);
	}
}
