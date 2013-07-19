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

import static org.junit.Assert.fail;

import org.junit.Test;

public class FreeFormatUnitTest {
	@Test
	public void testSimpleHelloWorld() {
		final String SOURCE = join(
				"IDENTIFICATION DIVISION.",
				"PROGRAM-ID. HELLO-WORLD.",
				"PROCEDURE DIVISION.",
				"    DISPLAY 'Hello, world'.",
				"    STOP RUN."
			);
		
		compile(SOURCE);
	}
	
	private void compile(String contents) {
		fail("I don't know how to parse contents");
	}
	
	private static String join(String... lines) {
		StringBuilder builder = new StringBuilder();
		
		for (String line : lines) {
			builder.append(line).append('\n');
		}
		
		if (lines.length > 0)
			builder.setLength(builder.length() - 1);
		
		return builder.toString();
	}
}
