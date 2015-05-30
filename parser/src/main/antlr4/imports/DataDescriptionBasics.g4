/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2015  Rodrigo Lemos
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
parser grammar DataDescriptionBasics;
import Basics;

options { tokenVocab = COBOLLexer; }

dataDescriptionParagraph :
		levelNumber (dataName | FILLER)?
		redefinesClause?
		dataDescriptionClauses?
		PERIOD
	;

// Waiting for correction or workaround to https://github.com/antlr/antlr4/issues/867 or http://stackoverflow.com/questions/30021472/antlr4-semantic-predicates-mess-with-error-recovery-why
// This rather small permutation set covers all "variations" of 4 distinct
// objects and was found by trial and error (after some algorithmic foundation).
// (see also http://oeis.org/A007526)
dataDescriptionClauses :
		pictureClause usageClause    valueClause    occursClause
	|	pictureClause usageClause    occursClause   valueClause?
	|	pictureClause valueClause    usageClause    occursClause
	|	pictureClause valueClause    occursClause?  usageClause?
	|	pictureClause occursClause   valueClause    usageClause
	|	pictureClause occursClause?  usageClause?   valueClause?
	|	usageClause   pictureClause  valueClause    occursClause
	|	usageClause   pictureClause  occursClause   valueClause?
	|	usageClause   valueClause    pictureClause  occursClause
	|	usageClause   valueClause    occursClause?  pictureClause?
	|	usageClause   occursClause   valueClause    pictureClause
	|	usageClause   occursClause?  pictureClause? valueClause?
	|	valueClause   pictureClause  usageClause    occursClause
	|	valueClause   pictureClause  occursClause   usageClause?
	|	valueClause   usageClause    pictureClause  occursClause
	|	valueClause   usageClause    occursClause?  pictureClause?
	|	valueClause   occursClause   usageClause    pictureClause
	|	valueClause   occursClause?  pictureClause? usageClause?
	|	occursClause  pictureClause  usageClause    valueClause
	|	occursClause  pictureClause  valueClause    usageClause?
	|	occursClause  usageClause    pictureClause  valueClause
	|	occursClause  usageClause    valueClause?   pictureClause?
	|	occursClause  valueClause    usageClause    pictureClause
	|	occursClause  valueClause?   pictureClause? usageClause?
	;

redefinesClause :
		REDEFINES dataName
	;

pictureClause :
		(PICTURE | PIC) IS? PICTURESTRING
	;

usageClause :
		(USAGE IS?)? usage
	;

usage :
		BINARY NATIVE?
	|	(COMP   | COMPUTATIONAL  ) NATIVE?
	|	(COMP_1 | COMPUTATIONAL_1) NATIVE?
	|	(COMP_2 | COMPUTATIONAL_2) NATIVE?
	|	(COMP_3 | COMPUTATIONAL_3) NATIVE?
	|	(COMP_4 | COMPUTATIONAL_4) NATIVE?
	|	(COMP_5 | COMPUTATIONAL_5) NATIVE?
	|	DISPLAY NATIVE?
	|	DISPLAY_1 NATIVE?
	|	INDEX
	|	NATIONAL NATIVE?
//	|	OBJECT REFERENCE className
	|	PACKED_DECIMAL NATIVE?
	|	POINTER
	|	PROCEDURE_POINTER
	|	FUNCTION_POINTER
	;

valueClause :
		VALUE IS? literal
	;

occursClause :
		OCCURS INTEGER TIMES?
//		((ASCENDING | DESCENDING) KEY? IS? dataName+)*
		(INDEXED BY? indexName+)?
	;

levelNumber :
		INTEGER { $INTEGER.text.matches("^[0-9][0-9]$") }?
	;

indexName :
		USERDEFINEDWORD
	;
