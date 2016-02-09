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

/**
 * Configuration section.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=133&zoom=auto,-40,710
 */
configurationSection :
		CONFIGURATION SECTION PERIOD
		sourceComputerParagraph?
		objectComputerParagraph?
		specialNamesParagraph?
		repositoryParagraph?
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

/**
 * Special-names paragraph.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=136&zoom=auto,-40,620
 */
specialNamesParagraph :
		SPECIAL_NAMES PERIOD
		specialNamesClause*
		PERIOD?
	;

specialNamesClause :
		environmentAssignmentClause
	|	alphabetClause
	|	symbolicCharactersClause
	|	classClause
	|	currencySignClause
	|	decimalPointClause
	|	xmlSchemaClause
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

/**
 * Class clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=142&zoom=auto,-40,670
 */
classClause :
		CLASS className IS? (literal ((THROUGH | THRU) literal)?)+
	;

/**
 * Currency sign clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=142&zoom=auto,-40,210
 */
currencySignClause :
		CURRENCY SIGN? IS? literal (WITH? PICTURE SYMBOL literal)?
	;

/**
 * Decimal-point is comma clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=144&zoom=auto,-40,680
 */
decimalPointClause :
		DECIMAL_POINT IS? COMMA
	;

/**
 * XML-schema clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=144&zoom=auto,-40,615
 */
xmlSchemaClause :
		XML_SCHEMA xmlSchemaName IS? (externalFileId | literal)
	;
/**
 * Repository paragraph.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=145&zoom=auto,-40,275
 *
 * The final PERIOD is not in reference manual. But samples collected on the 
 * Internet show it (needs further testing in a COBOL compiler).
 */
repositoryParagraph :
		REPOSITORY PERIOD
		repositoryClassSentence*
		PERIOD?
	;

repositoryClassSentence :
		CLASS dataClassName (IS? alphanumericLiteral)?
	;

inputOutputSection :
		INPUT_OUTPUT SECTION PERIOD
		fileControlParagraph?
	;

fileControlParagraph :
		FILE_CONTROL PERIOD
		selectEntry+
	;

selectEntry :
		sequentialFileControlEntry
	|	indexedFileControlEntry
	;

/**
 * Select clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=154&zoom=auto,-40,730
 */
selectClause :
		SELECT OPTIONAL? fileName
	;

/**
 * Assign clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=154&zoom=auto,-40,460
 */
assignClause :
		ASSIGN TO? assignmentName+
	;

/**
 * Sequential file control entry.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=151&zoom=auto,-40,730
 */
sequentialFileControlEntry :
		selectClause assignClause
		PERIOD
	;

/**
 * Indexed file control entry.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=152&zoom=auto,-40,730
 */
// TODO: may appear in any order, but at most once
// (though this may be not a syntatic concern, but rather semantic one)
indexedFileControlEntry :
		selectClause assignClause
		RECORD KEY? IS? USERDEFINEDWORD
		(ACCESS MODE? IS? SEQUENTIAL)?	// other modes also apply (but not now)
		(STATUS IS? USERDEFINEDWORD)?	// this clause belongs to general selectFileSentence
		ORGANIZATION IS? INDEXED
		PERIOD
	;

