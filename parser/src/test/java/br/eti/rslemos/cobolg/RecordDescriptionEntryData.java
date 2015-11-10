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

import java.util.Arrays;

public class RecordDescriptionEntryData {
	private static final String INDENT = "%1$s";
	private static final String TAB = "\t";
	private static final String NEWLINE = "\n";

	private static final String FMT_GROUP_SOURCE = INDENT + "%2$02d LEVEL-%2$02d-%3$03d-GROUP.\n%4$s";
	private static final String FMT_ITEM_SOURCE = INDENT + "%1$s%2$02d LEVEL-%2$02d-%3$03d PIC X.\n";
	
	private static final String FMT_GROUP_TREE = NEWLINE +
			TAB + "(recordDescriptionEntry " + NEWLINE +
			TAB + TAB + "(dataDescriptionEntry " + NEWLINE +
			TAB + TAB + TAB + "(levelNumber %2$02d) (dataName LEVEL-%2$02d-%3$03d-GROUP) " + NEWLINE +
			TAB + TAB + TAB + "dataDescriptionClauses " + NEWLINE +
			TAB + TAB + ".)" + NEWLINE +
			TAB + ") " + NEWLINE +
			TAB + "%4$s ";
	
	private static final String FMT_ITEM_TREE = NEWLINE +
			TAB + "(recordDescriptionEntry " + NEWLINE +
			TAB + TAB + "(dataDescriptionEntry " + NEWLINE +
			TAB + TAB + TAB + "(levelNumber %2$02d) (dataName LEVEL-%2$02d-%3$03d) " + NEWLINE +
			TAB + TAB + TAB + "(dataDescriptionClauses (dataDescriptionClause (pictureClause PIC X))) " + NEWLINE +
			TAB + TAB + ".)" + NEWLINE +
			TAB + ") ";

	public static String source(int... split) {
		return "WORKING-STORAGE SECTION.\n" + build0("", FMT_GROUP_SOURCE, FMT_ITEM_SOURCE, new int[] {1}, split);
	}

	public static String tree(int... split) {
		return "(workingStorageSection WORKING-STORAGE SECTION . " + build0("", FMT_GROUP_TREE, FMT_ITEM_TREE, new int[] {1}, split) + "\n)";
	}

	private static String build0(String indent, String formatGroup, String formatItem, int[] j, int... split) {
		StringBuilder result = new StringBuilder();
		
		for (int i = 0; i < split.length; ) {
			int i0 = i++;
			
			for (; i < split.length && split[i] > split[i0]; i++);
			if (i - i0 == 1) {
				result.append(String.format(formatItem, indent, split[i0], j[0]++));
			} else
				result.append(String.format(formatGroup, indent, split[i0], j[0]++, build0(indent + "\t", formatGroup, formatItem, j, Arrays.copyOfRange(split, i0+1, i))));

		}
		
		// actually just for TREE
		if (result.charAt(result.length() - 1) == ' ')
			result.setLength(result.length() - 1);
		
		return result.toString();
	}

	public static String flatten(String pretty) {
		return pretty.replaceAll("\n[ \t]*", "");
	}
	

}
