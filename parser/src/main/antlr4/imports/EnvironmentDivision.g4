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
parser grammar EnvironmentDivision;
import Basics;

options { tokenVocab = COBOLLexer; }

environmentDivision :
		ENVIRONMENT DIVISION PERIOD
		configurationSection?
		inputOutputSection?
	;

configurationSection :
		CONFIGURATION SECTION PERIOD
		sourceComputerParagraph?
		objectComputerParagraph?
		specialNamesParagraph?
	;

/**
 * Source-computer paragraph.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=134&zoom=auto,-40,670
 */
sourceComputerParagraph : 
		SOURCE_COMPUTER PERIOD (computerName (WITH? DEBUGGING MODE)? PERIOD)?
	;

/**
 * Object-computer paragraph.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=134&zoom=auto,-40,135
 */
objectComputerParagraph :
		OBJECT_COMPUTER PERIOD (
			computerName
			(MEMORY SIZE? INTEGER (WORDS | CHARACTERS | MODULES))?
			(PROGRAM? COLLATING? SEQUENCE IS? alphabetName)?
			(SEGMENT_LIMIT IS? priorityNumber)?
			PERIOD
		)?
	;

specialNamesParagraph :
		SPECIAL_NAMES PERIOD
		specialNamesClause*
		PERIOD?
	;

specialNamesClause :
		environmentAssignmentClause
	|	alphabetClause
	|	symbolicCharactersClause
	;

environmentAssignmentClause :
		environmentName IS? mnemonicName environmentStatusPhrase?
	|	environmentName environmentStatusPhrase
	;

environmentStatusPhrase :
		ON  STATUS? IS? conditionName (OFF STATUS? IS? conditionName)?
	|	OFF STATUS? IS? conditionName (ON  STATUS? IS? conditionName)?
	;

/**
 * Alphabet clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=139&zoom=auto,-40,280
 */
alphabetClause :
		ALPHABET alphabetName IS? 
		(STANDARD_1 | STANDARD_2 | NATIVE | EBCDIC | (literal ((THROUGH | THRU) literal | (ALSO literal)+)?)+)
	;

/**
 * Symbolic characters clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=141&zoom=auto,-40,360
 */
symbolicCharactersClause :
		SYMBOLIC CHARACTERS? (symbolicCharacter+ (ARE | IS)? numericLiteral+)+ (IN alphabetName)?
	;

inputOutputSection :
		INPUT_OUTPUT SECTION PERIOD
		fileControlParagraph?
	;

fileControlParagraph :
		FILE_CONTROL PERIOD
		selectFileSentence+
	;

selectFileSentence :
		SELECT OPTIONAL? USERDEFINEDWORD ASSIGN TO? USERDEFINEDWORD
		(fileOrganizationIndexed)?
		PERIOD
	;

// TODO: may appear in any order, but at most once
// (though this may be not a syntatic concern, but rather semantic one)
fileOrganizationIndexed :
		RECORD KEY? IS? USERDEFINEDWORD
		(ACCESS MODE? IS? SEQUENTIAL)?	// other modes also apply (but not now)
		(STATUS IS? USERDEFINEDWORD)?	// this clause belongs to general selectFileSentence
		ORGANIZATION IS? INDEXED
	;

