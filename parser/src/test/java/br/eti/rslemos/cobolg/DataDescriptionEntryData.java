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

public class DataDescriptionEntryData {
	public static enum DataDescriptionEntryClause {
		PICTURE("PIC XXXX"       , "(pictureClause PIC XXXX)"                                    ),
		USAGE  ("USAGE COMP-3"   , "(usageClause USAGE (usage COMP-3))"                          ),
		VALUE  ("VALUE IS QUOTES", "(valueClause VALUE IS (literal (figurativeConstant QUOTES)))"),
		OCCURS ("OCCURS 10 TIMES", "(occursClause OCCURS 10 TIMES)"                              ),
		;
	
		private final String source, tree;
		
		private DataDescriptionEntryClause(String source, String tree) {
			this.source = source;
			this.tree = tree;
		}
	
		public String getSource() { return source; }
		public String getTree() { return tree; }
		
	}

	public static String source(DataDescriptionEntryClause... clauses) {
		return "77  DECL-X " + TextHelper.join0(" ", getSources(clauses)) + ".";
	}

	public static String tree(DataDescriptionEntryClause... clauses) {
		return "(dataDescriptionEntry (levelNumber 77) (dataName DECL-X) " + 
					(clauses.length > 0 
							? "(dataDescriptionClauses " + TextHelper.join0(" ", getTrees(clauses)) + ") " 
							: ""
					) + 
				".)";
	}

	// these could only be in better shape by lambda calculus
	
	private static String[] getSources(DataDescriptionEntryClause... clauses) {
		String[] inputs = new String[clauses.length];
		
		for (int i = 0; i < clauses.length; i++)
			inputs[i] = clauses[i].getSource();
		
		return inputs;
	}

	private static String[] getTrees(DataDescriptionEntryClause... clauses) {
		String[] outputs = new String[clauses.length];
		
		for (int i = 0; i < clauses.length; i++)
			outputs[i] = clauses[i].getTree();
		
		return outputs;
	}
}