/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2017  Rodrigo Lemos
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
import static org.junit.Assert.assertThat;

import org.junit.Test;

public class SegmentationStrategyUnitTest {
	@Test public void fullLine() {
		String[] segments = SegmentationStrategy.FIXED_FORMAT.segment(
				"000006 IDENTIFICATION DIVISION.                                         TRAILER \n"
			);
		
		assertThat(segments.length, is(equalTo(5)));
		assertThat(segments[0], is(equalTo("000006")));
		assertThat(segments[1], is(equalTo(" ")));
		assertThat(segments[2], is(equalTo("IDENTIFICATION DIVISION.                                         ")));
		assertThat(segments[3], is(equalTo("TRAILER ")));
		assertThat(segments[4], is(equalTo("\n")));
	}
	
	@Test public void shortOnTrailer() {
		String[] segments = SegmentationStrategy.FIXED_FORMAT.segment(
				"000006 IDENTIFICATION DIVISION.                                         T\n"
			);
		
		assertThat(segments.length, is(equalTo(5)));
		assertThat(segments[0], is(equalTo("000006")));
		assertThat(segments[1], is(equalTo(" ")));
		assertThat(segments[2], is(equalTo("IDENTIFICATION DIVISION.                                         ")));
		assertThat(segments[3], is(equalTo("T")));
		assertThat(segments[4], is(equalTo("\n")));
	}
	
	@Test public void shortOnSource() {
		String[] segments = SegmentationStrategy.FIXED_FORMAT.segment(
				"000006 IDENTIFICATION DIVISION.                                       \n"
			);
		
		assertThat(segments.length, is(equalTo(5)));
		assertThat(segments[0], is(equalTo("000006")));
		assertThat(segments[1], is(equalTo(" ")));
		assertThat(segments[2], is(equalTo("IDENTIFICATION DIVISION.                                       ")));
		assertThat(segments[4], is(equalTo("\n")));
	}

	@Test public void shortOnIndicator() {
		String[] segments = SegmentationStrategy.FIXED_FORMAT.segment(
				"000006\n"
			);
		
		assertThat(segments.length, is(equalTo(5)));
		assertThat(segments[0], is(equalTo("000006")));
		assertThat(segments[4], is(equalTo("\n")));
	}

	@Test public void shortOnHeader() {
		String[] segments = SegmentationStrategy.FIXED_FORMAT.segment(
				"00005\n"
			);
		
		assertThat(segments.length, is(equalTo(5)));
		assertThat(segments[0], is(equalTo("00005")));
		assertThat(segments[4], is(equalTo("\n")));
	}
}
