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

public enum SegmentationStrategy {

	FIXED_FORMAT() {
		public String[] segment(char[] line) {
			String[] result = new String[5];

			int n = findEOL(line);
			
			result[0] = extract(line, n, 0, 6); 
			result[1] = extract(line, n, 6, 7);
			result[2] = extract(line, n, 7, 72);
			result[3] = extract(line, n, 72, n);
			result[4] = extract(line, line.length, n, line.length);

			return result;
		}
	},
	
	FREE_FORMAT() {
		public String[] segment(char[] line) {
			String[] result = new String[5];

			int n = findEOL(line);

			result[0] = ""; 
			result[1] = "";
			result[2] = extract(line, n, 0, n);
			result[3] = "";
			result[4] = extract(line, line.length, n, line.length);

			return result;
		}
	},
	;
	
	int findEOL(char[] line) {
		for(int eol = 0; eol < line.length; eol++)
			if (line[eol] == '\r' || line[eol] == '\n')
				return eol;
		
		return line.length;
	}

	String extract(char[] line, int length, int from, int to) {
		return from < length ? new String(line, from, Math.min(to, length) - from) : "";
	}

	/**
	 * Segment the given line into the 5 areas of COBOL source code.
	 *
	 * Areas are (by index):
	 * 0 - prefix area
	 * 1 - indicator area
	 * 2 - code area
	 * 3 - suffix area
	 * 4 - end of line
	 */
	public abstract String[] segment(char[] line);
	
	public String[] segment(String line) {
		return segment(line.toCharArray());
	}
}
