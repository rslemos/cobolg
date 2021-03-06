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

import br.eti.rslemos.cobolg.COBOLParser.StmtXMLPARSEContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

@Waive({CompilationError.EXACT_AMBIGUITY, CompilationError.FULL_CONTEXT_ATTEMPT})
public class StmtXMLPARSE {
	private static final ResourceBundle TEST_DATA = ResourceBundle.getBundle("br.eti.rslemos.cobolg.stmtXMLPARSE");
	public static String get(String key) { return TEST_DATA.getString(key); }

	private static CompilerHelper<StmtXMLPARSEContext> helper = new CompilerHelper<StmtXMLPARSEContext>() {
		@Override protected StmtXMLPARSEContext parsePart() { return parser.stmtXMLPARSE(true); }
	};
	
	@Test public void XML_PARSE_XML_1_PROCESSING_PROCEDURE_IS_SAX_0() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_PROCESSING_PROCEDURE_IS_SAX_0.source"),
				get("XML_PARSE_XML_1_PROCESSING_PROCEDURE_IS_SAX_0.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_CDPG_PROCESSING_PROCEDURE_SAX_0() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_PROCESSING_PROCEDURE_SAX_0.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_PROCESSING_PROCEDURE_SAX_0.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_ENCODING_1200_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_1200_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1.source"),
				get("XML_PARSE_XML_1_ENCODING_1200_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1.source"),
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1.source"),
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1.source"),
				get("XML_PARSE_XML_1_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1.source"),
				get("XML_PARSE_XML_1_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_CDPG_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_ENCODING_CDPG_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_CDPG_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0.source"),
				get("XML_PARSE_XML_1_ENCODING_CDPG_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_ENCODING_1200_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_1200_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1.source"),
				get("XML_PARSE_XML_1_ENCODING_1200_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_1200_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1.source"),
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1.source"),
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1.source"),
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0.source"),
				get("XML_PARSE_XML_1_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0.tree")
			);
	}
	
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_CDPG_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_ENCODING_1200_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_1200_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_ENCODING_1200_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_CDPG_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_ENCODING_CDPG_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_CDPG_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_ENCODING_CDPG_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_ENCODING_1200_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_1200_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_ENCODING_1200_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_1200_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_PROCESSING_PROCEDURE_IS_SAX_0_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_PROCESSING_PROCEDURE_IS_SAX_0_END_XML.source"),
				get("XML_PARSE_XML_1_PROCESSING_PROCEDURE_IS_SAX_0_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_CDPG_PROCESSING_PROCEDURE_SAX_0_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_PROCESSING_PROCEDURE_SAX_0_END_XML.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_PROCESSING_PROCEDURE_SAX_0_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_ENCODING_1200_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_1200_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_END_XML.source"),
				get("XML_PARSE_XML_1_ENCODING_1200_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_END_XML.source"),
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_END_XML.source"),
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_END_XML.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_END_XML.source"),
				get("XML_PARSE_XML_1_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_END_XML.source"),
				get("XML_PARSE_XML_1_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_CDPG_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_END_XML.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_ENCODING_CDPG_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_CDPG_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_END_XML.source"),
				get("XML_PARSE_XML_1_ENCODING_CDPG_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_ENCODING_1200_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_1200_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_END_XML.source"),
				get("XML_PARSE_XML_1_ENCODING_1200_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_1200_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_END_XML.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_END_XML.source"),
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_END_XML.source"),
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_END_XML.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_END_XML.source"),
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_END_XML.source"),
				get("XML_PARSE_XML_1_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_END_XML.tree")
			);
	}
	
	@Waive(CompilationError.CONTEXT_SENSITIVITY)
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_END_XML.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_CDPG_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_ENCODING_1200_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_1200_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_ENCODING_1200_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_CDPG_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_ENCODING_CDPG_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_CDPG_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_ENCODING_CDPG_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_ENCODING_1200_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_1200_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_ENCODING_1200_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_1200_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THRU_SAX_1_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_WITH_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_ENCODING_CDPG_RETURNING_NATIONAL_VALIDATING_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_THROUGH_SAX_1_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_VLDTR_1_PROCESSING_PROCEDURE_SAX_0_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
	
	@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
	@Test public void XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN_END_XML() {
		helper.compileAndVerify(
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN_END_XML.source"),
				get("XML_PARSE_XML_1_WITH_ENCODING_1200_RETURNING_NATIONAL_VALIDATING_WITH_FILE_SCHEMA_1_PROCESSING_PROCEDURE_IS_SAX_0_EXCEPTION_STOP_RUN_END_XML.tree")
			);
	}
}
