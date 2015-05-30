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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.DiagnosticErrorListener;
import org.antlr.v4.runtime.RuleContext;

import br.eti.rslemos.cobolg.COBOLParser.DataDescriptionClausesContext;
import br.eti.rslemos.cobolg.COBOLParser.DataDescriptionParagraphContext;
import br.eti.rslemos.cobolg.Compiler.FreeFormatCompiler;

public class DataDescriptionUnitTest extends TestCase {
	public void testEmptyDeclaration () {
		assertThat(compile("01  EMPTY-DECLARATION."), is(equalTo("(dataDescriptionParagraph "
				+ "(levelNumber 01) "
				+ "(dataName EMPTY-DECLARATION) "
			+ ".)")));
	}

	public void testFillerDeclaration () {
		assertThat(compile("01  FILLER."), is(equalTo("(dataDescriptionParagraph "
				+ "(levelNumber 01) "
				+ "FILLER "
			+ ".)")));
	}

	public void testAnonymousDeclaration () {
		assertThat(compile("01  ."), is(equalTo("(dataDescriptionParagraph "
				+ "(levelNumber 01) "
			+ ".)")));
	}

	public void testPICDeclaration () {
		// the PICTURESTRING is invalid on purpose
		// this is a bold statement that whatever PICTURESTRING our lexer gave us
		// should be validated elsewhere (a specialized lexer perhaps?)
		assertThat(compile("01  PIC-DECLARATION PIC Z$ABX09PPAAAVS,.,,."), is(equalTo("(dataDescriptionParagraph "
				+ "(levelNumber 01) "
				+ "(dataName PIC-DECLARATION) "
				+ "(dataDescriptionClauses "
					+ "(pictureClause PIC Z$ABX09PPAAAVS,.,,)"
				+ ") "
			+ ".)")));
	}
	
	public void testUsageClause () {
		assertThat(compile("01  USAGE-DECLARATION USAGE IS BINARY."), is(equalTo("(dataDescriptionParagraph "
				+ "(levelNumber 01) "
				+ "(dataName USAGE-DECLARATION) "
				+ "(dataDescriptionClauses "
					+ "(usageClause USAGE IS (usage BINARY))"
				+ ") "
			+ ".)")));
	}

	public void testValueClause () {
		assertThat(compile("01  VALUE-DECLARATION VALUE IS 0."), is(equalTo("(dataDescriptionParagraph "
				+ "(levelNumber 01) "
				+ "(dataName VALUE-DECLARATION) "
				+ "(dataDescriptionClauses "
					+ "(valueClause VALUE IS (literal (numericLiteral 0)))"
				+ ") "
			+ ".)")));
	}

	public void testValueClauseWithFigurativeConstant () {
		assertThat(compile("01  VALUE-DECLARATION VALUE IS ZERO."), is(equalTo("(dataDescriptionParagraph "
				+ "(levelNumber 01) "
				+ "(dataName VALUE-DECLARATION) "
				+ "(dataDescriptionClauses "
					+ "(valueClause VALUE IS (literal (figurativeConstant ZERO)))"
				+ ") "
			+ ".)")));
	}

	public void testOccursClause () {
		assertThat(compile("01  OCCURS-DECLARATION OCCURS 10 TIMES."), is(equalTo("(dataDescriptionParagraph "
				+ "(levelNumber 01) "
				+ "(dataName OCCURS-DECLARATION) "
				+ "(dataDescriptionClauses "
					+ "(occursClause OCCURS 10 TIMES)"
				+ ") "
			+ ".)")));
	}

	public void testRedefinesClause () {
		assertThat(compile("01  REDEFINITION-DECLARATION REDEFINES REDEFINED-DECLARATION."), is(equalTo("(dataDescriptionParagraph "
				+ "(levelNumber 01) "
				+ "(dataName REDEFINITION-DECLARATION) "
				+ "(redefinesClause REDEFINES (dataName REDEFINED-DECLARATION)) "
			+ ".)")));
	}
	
	public void testPIC_USAGE_VALUE () {
		assertThat(compile("01  DECL PIC XXXXX USAGE COMP-3 VALUE IS QUOTES."), is(equalTo("(dataDescriptionParagraph "
				+ "(levelNumber 01) "
				+ "(dataName DECL) "
				+ "(dataDescriptionClauses "
					+ "(pictureClause PIC XXXXX) "
					+ "(usageClause USAGE (usage COMP-3)) "
					+ "(valueClause VALUE IS (literal (figurativeConstant QUOTES)))"
				+ ") "
			+ ".)")));
	}

	public void testPIC_VALUE_USAGE () {
		assertThat(compile("01  DECL PIC XXXXX VALUE IS QUOTES USAGE COMP-3."), is(equalTo("(dataDescriptionParagraph "
				+ "(levelNumber 01) "
				+ "(dataName DECL) "
				+ "(dataDescriptionClauses "
					+ "(pictureClause PIC XXXXX) "
					+ "(valueClause VALUE IS (literal (figurativeConstant QUOTES))) "
					+ "(usageClause USAGE (usage COMP-3))"
				+ ") "
			+ ".)")));
	}

	private static FreeFormatCompiler compiler;

	private static String compile(String source) {
		return compileAsContext(source).toStringTree(compiler.mainParser);
	}

	private static DataDescriptionParagraphContext compileAsContext(String source) {
		try {
			compiler = new FreeFormatCompiler(new StringReader(source));
			
			ErrorDetector detector = new ErrorDetector();
			//compiler.addErrorListener(new DiagnosticErrorListener(true));
			compiler.addErrorListener(ConsoleErrorListener.INSTANCE);
			compiler.addErrorListener(detector);
			
			DataDescriptionParagraphContext tree = compiler.mainParser.dataDescriptionParagraph();
			
			assertThat(tree, is(not(nullValue(DataDescriptionParagraphContext.class))));
			
			//detector.check();
			
			return tree;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
	
	public static TestSuite suite() {
		TestSuite suite = new TestSuite(DataDescriptionUnitTest.class);
		suite.addTest(FullPermutationTestCase.suite());
		
		return suite;
	}

	public static class FullPermutationTestCase extends TestCase {

		private static String[] ELEMENTS = {"PIC XXXX ", "USAGE COMP-3 ", "VALUE IS QUOTES ", "OCCURS 10 TIMES "};
		
		@Override
		protected void runTest() throws Throwable {
			DataDescriptionClausesContext clauses = compileAsContext(getName()).dataDescriptionClauses();
			
			if (permutation.size() == 0)
				assertThat(clauses, is(nullValue(DataDescriptionClausesContext.class)));
			else {
				checkClause(clauses.pictureClause(), permutation.contains(0) ? ELEMENTS[0] : null);
				checkClause(clauses.usageClause(),   permutation.contains(1) ? ELEMENTS[1] : null);
				checkClause(clauses.valueClause(),   permutation.contains(2) ? ELEMENTS[2] : null);
				checkClause(clauses.occursClause(),  permutation.contains(3) ? ELEMENTS[3] : null);
			}
		}

		private void checkClause(RuleContext clause, String expected) {
			if (expected != null) {
				// whitespace go to HIDDEN channel (so getText() will not return them)
				expected = expected.replaceAll(" ", "");
				assertThat(clause.getText(), is(equalTo(expected)));
			} else
				assertThat(clause, is(nullValue(RuleContext.class)));
		}

		public static TestSuite suite() {
			TestSuite suite = new TestSuite(FullPermutationTestCase.class.getName());
			
			// http://oeis.org/A000522(4)
			List<ArrayList<Integer>> permutations = new ArrayList<ArrayList<Integer>>(65);
			
			// start with empty permutation
			permutations.add(new ArrayList<Integer>());
			
			for (int i = 0; i < ELEMENTS.length; i++) {
				ListIterator<ArrayList<Integer>> it = permutations.listIterator();
				while(it.hasNext()) {
					// new permutations based on existing ones
					ArrayList<Integer> existing = it.next();
					
					for (int j = 0; j < ELEMENTS.length; j++)
						// avoid repeated element
						if (!existing.contains(j)) {
							ArrayList<Integer> neww = new ArrayList<Integer>(existing);
							neww.add(j);
							
							// both fors make a square, but only half is needed (a triangle)
							if (!permutations.contains(neww))
								it.add(neww);
						}
				}
			}
			
			for (ArrayList<Integer> permutation : permutations)
				suite.addTest(new FullPermutationTestCase(permutation));
			
			return suite;
		}
		
		private final List<Integer> permutation;

		public FullPermutationTestCase(List<Integer> permutation) {
			super(buildDecl(permutation));
			this.permutation = permutation;
		}

		private static String buildDecl(List<Integer> permutation) {
			StringBuilder decl = new StringBuilder();
			decl.append("77  DECL-");
			decl.append(permutation.toString().replaceAll("[^0-9]", ""));
			decl.append("-X ");
			
			for (Integer i : permutation)
				decl.append(ELEMENTS[i]);
			
			decl.setLength(decl.length() - 1);
			decl.append('.');
			
			return decl.toString();
		}

	}
}
