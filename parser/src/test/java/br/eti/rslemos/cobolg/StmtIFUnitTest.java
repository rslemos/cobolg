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

import static br.eti.rslemos.cobolg.StmtIFData.IF;
import static br.eti.rslemos.cobolg.StmtIFData.IFELSE;
import static br.eti.rslemos.cobolg.StmtIFData.IFELSEENDIF;
import static br.eti.rslemos.cobolg.StmtIFData.IFENDIF;
import static br.eti.rslemos.cobolg.StmtIFData.flatten;

import org.junit.Ignore;
import org.junit.Test;

import br.eti.rslemos.cobolg.COBOLParser.ProceduralSentenceContext;
import br.eti.rslemos.cobolg.Waive.CompilationError;

@Waive({CompilationError.CONTEXT_SENSITIVITY, CompilationError.FULL_CONTEXT_ATTEMPT})
public class StmtIFUnitTest {
	private static CompilerHelper<ProceduralSentenceContext> helper = new CompilerHelper<ProceduralSentenceContext>() {
		@Override protected ProceduralSentenceContext parsePart() { return parser.proceduralSentence(); }
	};


	@Test public void IF1() {
		helper.compileAndVerify(
				IF.source(1), 
				flatten(IF.tree(1))
			);
	}

	@Test public void IFENDIF1() {
		helper.compileAndVerify(
				IFENDIF.source(1), 
				flatten(IFENDIF.tree(1))
			);
	}

	@Test public void IFELSE1() {
		helper.compileAndVerify(
				IFELSE.source(1), 
				flatten(IFELSE.tree(1))
			);
	}

	@Test public void IFELSEENDIF1() {
		helper.compileAndVerify(
				IFELSEENDIF.source(1), 
				flatten(IFELSEENDIF.tree(1))
			);
	}
	@Test public void IF2() {
		helper.compileAndVerify(
				IF.source(2), 
				flatten(IF.tree(2))
			);
	}

	@Test public void IFENDIF2() {
		helper.compileAndVerify(
				IFENDIF.source(2), 
				flatten(IFENDIF.tree(2))
			);
	}

	@Test public void IFELSE2() {
		helper.compileAndVerify(
				IFELSE.source(2), 
				flatten(IFELSE.tree(2))
			);
	}

	@Test public void IFELSEENDIF2() {
		helper.compileAndVerify(
				IFELSEENDIF.source(2), 
				flatten(IFELSEENDIF.tree(2))
			);
	}

	@Test public void IF3() {
		helper.compileAndVerify(
				IF.source(3), 
				flatten(IF.tree(3))
			);
	}

	@Test public void IFENDIF3() {
		helper.compileAndVerify(
				IFENDIF.source(3), 
				flatten(IFENDIF.tree(3))
			);
	}

	@Test public void IFELSE3() {
		helper.compileAndVerify(
				IFELSE.source(3), 
				flatten(IFELSE.tree(3))
			);
	}

	@Test public void IFELSEENDIF3() {
		helper.compileAndVerify(
				IFELSEENDIF.source(3), 
				flatten(IFELSEENDIF.tree(3))
			);
	}

	@Test public void IF4() {
		helper.compileAndVerify(
				IF.source(4), 
				flatten(IF.tree(4))
			);
	}

	@Test public void IFENDIF4() {
		helper.compileAndVerify(
				IFENDIF.source(4), 
				flatten(IFENDIF.tree(4))
			);
	}

	@Test public void IFELSE4() {
		helper.compileAndVerify(
				IFELSE.source(4), 
				flatten(IFELSE.tree(4))
			);
	}

	@Test public void IFELSEENDIF4() {
		helper.compileAndVerify(
				IFELSEENDIF.source(4), 
				flatten(IFELSEENDIF.tree(4))
			);
	}

	@Ignore
	@Test public void IF5() {
		helper.compileAndVerify(
				IF.source(5), 
				flatten(IF.tree(5))
			);
	}

	@Ignore
	@Test public void IFENDIF5() {
		helper.compileAndVerify(
				IFENDIF.source(5), 
				flatten(IFENDIF.tree(5))
			);
	}

	@Ignore
	@Test public void IFELSE5() {
		helper.compileAndVerify(
				IFELSE.source(5), 
				flatten(IFELSE.tree(5))
			);
	}

	@Ignore
	@Test public void IFELSEENDIF5() {
		helper.compileAndVerify(
				IFELSEENDIF.source(5), 
				flatten(IFELSEENDIF.tree(5))
			);
	}

	@Ignore
	@Test public void IF6() {
		helper.compileAndVerify(
				IF.source(6), 
				flatten(IF.tree(6))
			);
	}

	@Ignore
	@Test public void IFENDIF6() {
		helper.compileAndVerify(
				IFENDIF.source(6), 
				flatten(IFENDIF.tree(6))
			);
	}

	@Ignore
	@Test public void IFELSE6() {
		helper.compileAndVerify(
				IFELSE.source(6), 
				flatten(IFELSE.tree(6))
			);
	}

	@Ignore
	@Test public void IFELSEENDIF6() {
		helper.compileAndVerify(
				IFELSEENDIF.source(6), 
				flatten(IFELSEENDIF.tree(6))
			);
	}

	@Ignore
	@Test public void IF7() {
		helper.compileAndVerify(
				IF.source(7), 
				flatten(IF.tree(7))
			);
	}

	@Ignore
	@Test public void IFENDIF7() {
		helper.compileAndVerify(
				IFENDIF.source(7), 
				flatten(IFENDIF.tree(7))
			);
	}

	@Ignore
	@Test public void IFELSE7() {
		helper.compileAndVerify(
				IFELSE.source(7), 
				flatten(IFELSE.tree(7))
			);
	}

	@Ignore
	@Test public void IFELSEENDIF7() {
		helper.compileAndVerify(
				IFELSEENDIF.source(7), 
				flatten(IFELSEENDIF.tree(7))
			);
	}

	@Ignore
	@Test public void IF8() {
		helper.compileAndVerify(
				IF.source(8), 
				flatten(IF.tree(8))
			);
	}

	@Ignore
	@Test public void IFENDIF8() {
		helper.compileAndVerify(
				IFENDIF.source(8), 
				flatten(IFENDIF.tree(8))
			);
	}

	@Ignore
	@Test public void IFELSE8() {
		helper.compileAndVerify(
				IFELSE.source(8), 
				flatten(IFELSE.tree(8))
			);
	}

	@Ignore
	@Test public void IFELSEENDIF8() {
		helper.compileAndVerify(
				IFELSEENDIF.source(8), 
				flatten(IFELSEENDIF.tree(8))
			);
	}

	@Ignore
	@Test public void IF9() {
		helper.compileAndVerify(
				IF.source(9), 
				flatten(IF.tree(9))
			);
	}

	@Ignore
	@Test public void IFENDIF9() {
		helper.compileAndVerify(
				IFENDIF.source(9), 
				flatten(IFENDIF.tree(9))
			);
	}

	@Ignore
	@Test public void IFELSE9() {
		helper.compileAndVerify(
				IFELSE.source(9), 
				flatten(IFELSE.tree(9))
			);
	}

	@Ignore
	@Test public void IFELSEENDIF9() {
		helper.compileAndVerify(
				IFELSEENDIF.source(9), 
				flatten(IFELSEENDIF.tree(9))
			);
	}
}
