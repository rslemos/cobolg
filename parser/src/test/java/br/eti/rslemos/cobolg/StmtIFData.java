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

public enum StmtIFData {
	IF         (Source.FMT_IF         , Tree.FMT_IF         , false),
	IFENDIF    (Source.FMT_IFENDIF    , Tree.FMT_IFENDIF    , false),
	IFELSE     (Source.FMT_IFELSE     , Tree.FMT_IFELSE     , true ),
	IFELSEENDIF(Source.FMT_IFELSEENDIF, Tree.FMT_IFELSEENDIF, true ),
	;
	
	private final String sourceFormat;
	private final String treeFormat;
	private boolean twoBranches;

	private StmtIFData(String sourceFormat, String treeFormat, boolean twoBranches) {
		this.sourceFormat = sourceFormat;
		this.treeFormat = treeFormat;
		this.twoBranches = twoBranches;
	}
	
	public String source(int levels) {
		return build(sourceFormat, "", Source.FMT_LEAF, levels, new int[] {0}) + ".";
	}
	
	public String tree(int levels) {
		return "(proceduralSentence " + Tree.NEWLINE + build(treeFormat, "", Tree.FMT_LEAF, levels, new int[] {0}) + " \n.)";
	}
	
	private String build(String format, String indent, String leaf, int levels, int[] i) {
		if (levels == 0) return String.format(leaf, indent);
		
		int i0 = i[0]++;
		String branch0 = build(format, indent + TAB, leaf, levels - 1, i);
		if (twoBranches) {
			String branch1 = build(format, indent + TAB, leaf, levels - 1, i);
			return String.format(format, indent, i0, branch0, branch1);
		} else {
			return String.format(format, indent, i0, branch0);
		}
	}

	private static final String TAB = "\t";

	private static class Source {
		// format building blocks for input
		private static final String INDENT = "%1$s";
		private static final String NEWLINE = " \n";
		
		private static final String FMT_IFTHEN = INDENT + "IF X%2$04d THEN" + NEWLINE + "%3$s";
		private static final String FMT_ELSE   = INDENT + "ELSE" + NEWLINE + "%4$s";
		private static final String FMT_ENDIF  = INDENT + "END-IF";
		private static final String FMT_LEAF   = INDENT + "NEXT SENTENCE";

		private static final String FMT_IF          = FMT_IFTHEN;
		private static final String FMT_IFENDIF     = FMT_IFTHEN + NEWLINE + FMT_ENDIF;
		private static final String FMT_IFELSE      = FMT_IFTHEN + NEWLINE + FMT_ELSE;
		private static final String FMT_IFELSEENDIF = FMT_IFTHEN + NEWLINE + FMT_ELSE + NEWLINE + FMT_ENDIF;
	}
	
	private static class Tree {
		private static final String INDENT = "%1$s%1$s%1$s%1$s";
		private static final String PINDENT = "%%1$s%%1$s%%1$s%%1$s";
		private static final String NEWLINE = "\n" + TAB;
		
		// format building blocks for output
		private static final String FMT_STMTIF0 = NEWLINE +
				PINDENT + TAB + TAB + "IF (conditionalExpression (conditionNameCondition (conditionName X%%2$04d))) " + NEWLINE + 
				PINDENT + TAB + TAB + "THEN " + NEWLINE + 
				"%%3$s%s";
		private static final String FMT_STMTIF = 
				PINDENT + "(proceduralStatement " + NEWLINE + 
				PINDENT + TAB + "(stmtIF %s" + NEWLINE + 
				PINDENT + TAB + ")" + NEWLINE + 
				PINDENT + ")";
		private static final String FMT_STMTIFENDIF = 
				PINDENT + "(proceduralStatement " + NEWLINE + 
				PINDENT + TAB + "(stmtIFdelimitedScope " + NEWLINE + 
				PINDENT + TAB + TAB + "(stmtIF %s" + NEWLINE + 
				PINDENT + TAB + TAB + ") " + NEWLINE + 
				PINDENT + TAB + "END-IF)" + NEWLINE + 
				PINDENT + ")";
		private static final String FMT_ELSE = " " + NEWLINE + 
				INDENT + TAB + TAB + "ELSE " + NEWLINE + 
				"%4$s";
		
		private static final String FMT_LEAF = INDENT + "NEXT SENTENCE";

		private static final String FMT_IF          = String.format(FMT_STMTIF,      String.format(FMT_STMTIF0, ""      ));
		private static final String FMT_IFENDIF     = String.format(FMT_STMTIFENDIF, String.format(FMT_STMTIF0, ""      ));
		private static final String FMT_IFELSE      = String.format(FMT_STMTIF,      String.format(FMT_STMTIF0, FMT_ELSE));
		private static final String FMT_IFELSEENDIF = String.format(FMT_STMTIFENDIF, String.format(FMT_STMTIF0, FMT_ELSE));
	}

	public static String flatten(String pretty) {
		return pretty.replaceAll("\n[ \t]*", "");
	}
}
