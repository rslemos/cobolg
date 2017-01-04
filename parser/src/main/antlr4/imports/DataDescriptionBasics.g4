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

/**
 * Record-description-entry.
 * 
 * - is a set of data description entries (in the sense that a group data description entry encompasses a set of entries);
 * - more than one record description entry can be specified; each is an alternative description of the same record storage area.
 * 
 * Data description entry is a single line of description.
 * Record description entry is a data description entry and its children
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=179&zoom=auto,-100,700
 */
recordDescriptionEntry :
		dataDescriptionEntry
	;

/**
 * Data description entry.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=209&zoom=auto,-100,730
 */
dataDescriptionEntry :
	levelNumber
	(dataName | FILLER)? redefinesClause? dataDescriptionClauses
	PERIOD
	;

dataDescriptionClauses :
	dataDescriptionClause*
	;

dataDescriptionClause :
		blankWhenZeroClause
	|	externalClause
	|	globalClause
	|	groupUsageClause
	|	justifiedClause
	|	occursClause
	|	pictureClause
	|	signClause
	|	usageClause
	|	valueClause
	;

/**
 * Blank when zero clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=212&zoom=auto,-40,735
 */
blankWhenZeroClause : BLANK WHEN (ZERO | ZEROS | ZEROES);

// externalClause and globalClause are doubly defined in FILE SECTION and in data descriptors.
// fortunately both definitions are equal.

/**
 * External clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=198&zoom=auto,-100,330
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=212&zoom=auto,-40,450
 */
externalClause : IS? EXTERNAL;

/**
 * Global clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=199&zoom=auto,-100,675
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=213&zoom=auto,-40,735
 */
globalClause : IS? GLOBAL;

/**
 * Group-usage clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=214&zoom=auto,-40,510
 */
groupUsageClause : GROUP_USAGE IS? NATIONAL;

/**
 * Justified clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=213&zoom=auto,-40,390
 */
justifiedClause : (JUSTIFIED | JUST) RIGHT?;

/**
 * Occurs clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=215&zoom=auto,-40,335
 */
occursClause :
		OCCURS INTEGER TIMES? keyIsPhrase* indexedByPhrase?
	|	OCCURS (INTEGER TO)? (INTEGER | UNBOUNDED) TIMES? dependingOnPhrase keyIsPhrase* indexedByPhrase?
	;

/**
 * (Ascending/descending) key is phrase.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=217&zoom=auto,-40,740
 */
keyIsPhrase : (ASCENDING | DESCENDING) KEY? IS? dataName+;

/**
 * Indexed by phrase.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=218&zoom=auto,-40,400
 */
indexedByPhrase : INDEXED BY indexName+;

/**
 * Depending on phrase (clause).
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=220&zoom=auto,-40,330
 */
dependingOnPhrase : DEPENDING ON? dataName;

/**
 * Picture clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=222&zoom=auto,-40,140
 */
pictureClause : (PICTURE | PIC) IS? PICTURESTRING;

/**
 * Redefines clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=240&zoom=auto,-40,735
 */
redefinesClause : REDEFINES dataName;

/**
 * Sign clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=245&zoom=auto,-40,220
 */
signClause : (SIGN IS?)? (LEADING | TRAILING) (SEPARATE CHARACTER?)?;

/**
 * Usage clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=252&zoom=auto,-40,455
 */
usageClause : (USAGE IS?)? usage; 

usage :
		BINARY          NATIVE?
	|	COMP            NATIVE?
	|	COMP_1          NATIVE?
	|	COMP_2          NATIVE?
	|	COMP_3          NATIVE?
	|	COMP_4          NATIVE?
	|	COMP_5          NATIVE?
	|	COMPUTATIONAL   NATIVE?
	|	COMPUTATIONAL_1 NATIVE?
	|	COMPUTATIONAL_2 NATIVE?
	|	COMPUTATIONAL_3 NATIVE?
	|	COMPUTATIONAL_4 NATIVE?
	|	COMPUTATIONAL_5 NATIVE?
	|	DISPLAY         NATIVE?
	|	DISPLAY_1       NATIVE?
	|	INDEX
	|	NATIONAL        NATIVE?
	|	OBJECT REFERENCE className?
	|	PACKED_DECIMAL  NATIVE?
	|	POINTER
	|	PROCEDURE_POINTER
	|	FUNCTION_POINTER
	;

/**
 * Value clause.
 * 
 * This embodies also the value clause format for level 88 (list of literals
 * and/or literal ranges).
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=261&zoom=auto,-40,735
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=263&zoom=auto,-40,430
 */
valueClause :
		(VALUE IS? | VALUES ARE?) (literal ((THROUGH | THRU) literal)?)+
	;

