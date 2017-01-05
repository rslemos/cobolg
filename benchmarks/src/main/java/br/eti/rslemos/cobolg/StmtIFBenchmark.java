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

import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;

@State(Scope.Benchmark)
public class StmtIFBenchmark {

	public static enum StmtIfSample {

		// 1
		IF1         (IF.source(1)         ),
		IFENDIF1    (IFENDIF.source(1)    ),
		IFELSE1     (IFELSE.source(1)     ),
		IFELSEENDIF1(IFELSEENDIF.source(1)),
		
		// 2
		IF2         (IF.source(2)         ),
		IFENDIF2    (IFENDIF.source(2)    ),
		IFELSE2     (IFELSE.source(2)     ),
		IFELSEENDIF2(IFELSEENDIF.source(2)),
		
		// 3
		IF3         (IF.source(3)         ),
		IFENDIF3    (IFENDIF.source(3)    ),
		IFELSE3     (IFELSE.source(3)     ),
		IFELSEENDIF3(IFELSEENDIF.source(3)),
		
		// 4
		IF4         (IF.source(4)         ),
		IFENDIF4    (IFENDIF.source(4)    ),
		IFELSE4     (IFELSE.source(4)     ),
		IFELSEENDIF4(IFELSEENDIF.source(4)),
		
//		// 5
//		IF5         (IF.source(5)         ),
//		IFENDIF5    (IFENDIF.source(5)    ),
//		IFELSE5     (IFELSE.source(5)     ),
//		IFELSEENDIF5(IFELSEENDIF.source(5)),
//		
//		// 6
//		IF6         (IF.source(6)         ),
//		IFENDIF6    (IFENDIF.source(6)    ),
//		IFELSE6     (IFELSE.source(6)     ),
//		IFELSEENDIF6(IFELSEENDIF.source(6)),
//		
//		// 7
//		IF7         (IF.source(7)         ),
//		IFENDIF7    (IFENDIF.source(7)    ),
//		IFELSE7     (IFELSE.source(7)     ),
//		IFELSEENDIF7(IFELSEENDIF.source(7)),
//		
//		// 8
//		IF8         (IF.source(8)         ),
//		IFENDIF8    (IFENDIF.source(8)    ),
//		IFELSE8     (IFELSE.source(8)     ),
//		IFELSEENDIF8(IFELSEENDIF.source(8)),
//		
//		// 9
//		IF9         (IF.source(9)         ),
//		IFENDIF9    (IFENDIF.source(9)    ),
//		IFELSE9     (IFELSE.source(9)     ),
//		IFELSEENDIF9(IFELSEENDIF.source(9)),
		;


		private final String source;
		
		private StmtIfSample(String source) { this.source = source; }
	}
	
	@Param
	public StmtIfSample parameter;
	
	@Benchmark
	@BenchmarkMode({Mode.AverageTime/*, Mode.SampleTime, Mode.Throughput*/})
	@OutputTimeUnit(TimeUnit.MILLISECONDS)
	@Warmup(iterations = 15)
	public Object compile() {
		return StmtIFUnitTest.helper.compile(parameter.source);
	}
}
