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

import static br.eti.rslemos.cobolg.DataDescriptionEntryData.source;
import static br.eti.rslemos.cobolg.DataDescriptionEntryData.DataDescriptionEntryClause.*;

import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Timeout;
import org.openjdk.jmh.annotations.Warmup;

import br.eti.rslemos.cobolg.COBOLParser.DataDescriptionEntryContext;

@State(Scope.Benchmark)
public class DataDescriptionEntryBenchmark {
	public enum DataDescriptionEntrySample {
		// none
		DECL_X(source()),
		
		// each
		DECL_X_REDEFINES(source(REDEFINES)),
		DECL_X_OCCURS(source(OCCURS)),
		DECL_X_PICTURE(source(PICTURE)),
		DECL_X_USAGE(source(USAGE)),
		DECL_X_VALUE(source(VALUE)),
		
		// all
		DECL_X_ALL(source(REDEFINES, OCCURS, PICTURE, USAGE, VALUE)),
		;

		public final String source;

		private DataDescriptionEntrySample(String source) {
			this.source = source;
		}
	}

	@Param
	public DataDescriptionEntrySample sample;
	
	@Benchmark
	@BenchmarkMode(Mode.AverageTime)
	@Fork(2)
	@Measurement(iterations = 20)
	@Warmup(iterations = 20)
	@OutputTimeUnit(TimeUnit.MICROSECONDS)
	public DataDescriptionEntryContext compile() {
		return DataDescriptionEntryUnitTest.helper.compile(sample.source);
	}
}
