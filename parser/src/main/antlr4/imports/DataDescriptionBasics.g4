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

@parser::members {
	public int nextTokenAsIntOr(int ifNot) {
		try {
			return Integer.parseInt(getCurrentToken().getText());
		} catch (NumberFormatException e) {
			return ifNot;
		}
	}
}

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
		(
			{ nextTokenAsIntOr(-1) > $dataDescriptionEntry.level && 77 != nextTokenAsIntOr(-1) }?
			recordDescriptionEntry
		)*
		{ !(nextTokenAsIntOr(-1) > $dataDescriptionEntry.level && 77 != nextTokenAsIntOr(-1)) }?
	;

/**
 * Data description entry.
 * 
 * Not all accepted inputs (by grammar definition) are to be accepted.
 * 
 * renamesClause, valueClause (format for level 88) and redefinesClause +
 * dataDescriptionClauses are all mutually exclusive.
 * 
 * Also FILLER cannot be used with neither renamesClause nor valueClause88.
 * These cases also do not support anonymous data entry. These requirements
 * could be conveyed in the grammar itself, were not for ANTLR issue #687
 * {@link https://github.com/antlr/antlr4/issues/867}: these constructions
 * would trigger another way of predicting the viable alternative, a way that
 * is incompatible with error recovery by missing token inject, which is
 * crucial for the COPY compiler statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=209&zoom=auto,-100,730
 */
dataDescriptionEntry returns[int level] :
	levelNumber { $level = $levelNumber.value; }
	(dataName | FILLER)? redefinesClause? renamesClause? dataDescriptionClauses
	PERIOD
	;

dataDescriptionClauses returns [
	BlankWhenZeroClauseContext blankWhenZeroClause_,
	ExternalClauseContext      externalClause_,
	GlobalClauseContext        globalClause_,
	GroupUsageClauseContext    groupUsageClause_,
	JustifiedClauseContext     justifiedClause_,
	OccursClauseContext        occursClause_,
	PictureClauseContext       pictureClause_,
	SignClauseContext          signClause_,
	SynchronizedClauseContext  synchronizedClause_,
	UsageClauseContext         usageClause_,
	ValueClauseContext         valueClause_,
	VolatileClauseContext      volatileClause_
] :
	dataDescriptionClause*
	;

dataDescriptionClause :
		blankWhenZeroClause { $dataDescriptionClauses::blankWhenZeroClause_ == null }?<fail={"Repeated BLANK WHEN ZERO clause"}> {$dataDescriptionClauses::blankWhenZeroClause_ = $blankWhenZeroClause.ctx;}
	|	externalClause      { $dataDescriptionClauses::externalClause_      == null }?<fail={"Repeated EXTERNAL clause"       }> {$dataDescriptionClauses::externalClause_      = $externalClause.ctx;     }
	|	globalClause        { $dataDescriptionClauses::globalClause_        == null }?<fail={"Repeated GLOBAL clause"         }> {$dataDescriptionClauses::globalClause_        = $globalClause.ctx;       }
	|	groupUsageClause    { $dataDescriptionClauses::groupUsageClause_    == null }?<fail={"Repeated GROUP USAGE clause"    }> {$dataDescriptionClauses::groupUsageClause_    = $groupUsageClause.ctx;   }
	|	justifiedClause     { $dataDescriptionClauses::justifiedClause_     == null }?<fail={"Repeated JUSTIFIED clause"      }> {$dataDescriptionClauses::justifiedClause_     = $justifiedClause.ctx;    }
	|	occursClause        { $dataDescriptionClauses::occursClause_        == null }?<fail={"Repeated OCCURS clause"         }> {$dataDescriptionClauses::occursClause_        = $occursClause.ctx;       }
	|	pictureClause       { $dataDescriptionClauses::pictureClause_       == null }?<fail={"Repeated PICTURE clause"        }> {$dataDescriptionClauses::pictureClause_       = $pictureClause.ctx;      }
	|	signClause          { $dataDescriptionClauses::signClause_          == null }?<fail={"Repeated SIGN clause"           }> {$dataDescriptionClauses::signClause_          = $signClause.ctx;         }
	|	synchronizedClause  { $dataDescriptionClauses::synchronizedClause_  == null }?<fail={"Repeated SYNCHRONIZED clause"   }> {$dataDescriptionClauses::synchronizedClause_  = $synchronizedClause.ctx; }
	|	usageClause         { $dataDescriptionClauses::usageClause_         == null }?<fail={"Repeated USAGE clause"          }> {$dataDescriptionClauses::usageClause_         = $usageClause.ctx;        }
	|	valueClause         { $dataDescriptionClauses::valueClause_         == null }?<fail={"Repeated VALUE clause"          }> {$dataDescriptionClauses::valueClause_         = $valueClause.ctx;        }
	|	volatileClause      { $dataDescriptionClauses::volatileClause_      == null }?<fail={"Repeated VOLATILE clause"       }> {$dataDescriptionClauses::volatileClause_      = $volatileClause.ctx;     }
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
 * Renames clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=243&zoom=auto,-40,290
 */
renamesClause : RENAMES dataName ((THROUGH | THRU) dataName)?;

/**
 * Sign clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=245&zoom=auto,-40,220
 */
signClause : (SIGN IS?)? (LEADING | TRAILING) (SEPARATE CHARACTER?)?;

/**
 * Synchronized clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=247&zoom=auto,-40,735
 */
synchronizedClause : (SYNCHRONIZED | SYNC) (LEFT | RIGHT)?;

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

/**
 * Volatile clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=266&zoom=auto,-40,265
 */
volatileClause: VOLATILE;
