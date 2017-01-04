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

import static br.eti.rslemos.cobolg.RecordDescriptionEntryData.source;

import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;

@State(Scope.Benchmark)
public class RecordDescriptionEntryBenchmark {

	public static enum RecordDescriptionEntrySample {
		// top-level only
		WSS_01_01_01_01_01_01_01_01_01_01_01_01(01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01),
		WSS_05_05_05_05_05_05_05_05_05_05_05_05(05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05),

		// mostly top-level (rise at end)
		WSS_01_01_01_01_01_01_01_01_01_01_01_05(01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 05),
		WSS_01_01_01_01_01_01_01_01_01_01_05_10(01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 05, 10),
		WSS_01_01_01_01_01_01_01_01_01_05_10_20(01, 01, 01, 01, 01, 01, 01, 01, 01, 05, 10, 20),

		// mostly top-level (rise at start,then drop)
		WSS_01_05_01_01_01_01_01_01_01_01_01_01(01, 05, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01),
		WSS_01_05_10_01_01_01_01_01_01_01_01_01(01, 05, 10, 01, 01, 01, 01, 01, 01, 01, 01, 01),
		WSS_01_05_10_20_01_01_01_01_01_01_01_01(01, 05, 10, 20, 01, 01, 01, 01, 01, 01, 01, 01),

		// 1 top-level,then rise (1 or more),then keep
		WSS_01_05_05_05_05_05_05_05_05_05_05_05(01, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05, 05),
		WSS_01_05_10_10_10_10_10_10_10_10_10_10(01, 05, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10),
		WSS_01_05_10_20_20_20_20_20_20_20_20_20(01, 05, 10, 20, 20, 20, 20, 20, 20, 20, 20, 20),

		// 2 top-levels (by middle)
		WSS_01_05_05_05_05_05_01_05_05_05_05_05(01, 05, 05, 05, 05, 05, 01, 05, 05, 05, 05, 05),
		WSS_01_05_10_10_10_10_01_05_10_10_10_10(01, 05, 10, 10, 10, 10, 01, 05, 10, 10, 10, 10),
		WSS_01_05_10_20_20_20_01_05_10_20_20_20(01, 05, 10, 20, 20, 20, 01, 05, 10, 20, 20, 20),

		// 3 top-levels (by thirds)
		WSS_01_05_05_05_01_05_05_05_01_05_05_05(01, 05, 05, 05, 01, 05, 05, 05, 01, 05, 05, 05),
		WSS_01_05_10_10_01_05_10_10_01_05_10_10(01, 05, 10, 10, 01, 05, 10, 10, 01, 05, 10, 10),
		WSS_01_05_10_20_01_05_10_20_01_05_10_20(01, 05, 10, 20, 01, 05, 10, 20, 01, 05, 10, 20),

		// 4 top-levels (by fourths)
		WSS_01_05_05_01_05_05_01_05_05_01_05_05(01, 05, 05, 01, 05, 05, 01, 05, 05, 01, 05, 05),
		WSS_01_05_10_01_05_10_01_05_10_01_05_10(01, 05, 10, 01, 05, 10, 01, 05, 10, 01, 05, 10),

		// 6 top-levels (alternating)
		WSS_01_05_01_05_01_05_01_05_01_05_01_05(01, 05, 01, 05, 01, 05, 01, 05, 01, 05, 01, 05),

		// swinging
		WSS_01_05_10_05_01_05_10_05_01_05_10_05(01, 05, 10, 05, 01, 05, 10, 05, 01, 05, 10, 05),
		WSS_01_05_10_05_10_05_01_05_10_05_10_05(01, 05, 10, 05, 10, 05, 01, 05, 10, 05, 10, 05),

		// rising
		WSS_01_05_10_15_20_25_30_35_40_45_46_47(01, 05, 10, 15, 20, 25, 30, 35, 40, 45, 46, 47),

		;

		private final String source;
		private RecordDescriptionEntrySample(int... split) { this.source = source(split); }
	}
	
	@Param
	public RecordDescriptionEntrySample parameter;
	
	@Benchmark
	@BenchmarkMode({Mode.AverageTime/*, Mode.SampleTime, Mode.Throughput*/})
	@OutputTimeUnit(TimeUnit.MILLISECONDS)
	@Warmup(iterations = 40)
	@Fork(1)
	public Object compile() {
		return RecordDescriptionEntryUnitTest.helper.compile(parameter.source);
	}
}
