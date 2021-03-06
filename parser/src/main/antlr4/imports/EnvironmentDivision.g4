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

/**
 * Environment division.
 * 
 * Implicitly defined.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=131&zoom=auto,-40,710
 */
environmentDivision :
		ENVIRONMENT DIVISION PERIOD
		environmentDivisionContent
	;

environmentDivisionContent :
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

/**
 * Input-output section.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=149&zoom=auto,-40,700
 */
inputOutputSection :
		INPUT_OUTPUT SECTION PERIOD
		fileControlParagraph?
		ioControlParagraph?
	;

/**
 * File-control paragraph.
 * 
 * Here defined somewhat differently than in reference manual, for the sake of
 * uniformity: sentences start at area B.
 * 
 * The File-control paragraph itself does not appear to be constrained to area
 * A, although that is not clear in the reference manual.
 * 
 * Entries clauses may appear in any order, though only once. This restriction
 * would be too painful to state in ANTLR, so it is postponed to a later
 * semantic analysis phase.
 */
fileControlParagraph :
		FILE_CONTROL PERIOD
		selectEntry+
	;

selectEntry :
		sequentialFileControlEntry
	|	indexedFileControlEntry
	|	relativeFileControlEntry
	|	lineSequentialFileControlEntry
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
		sequentialFileControlEntryClause*
		(
			organizationIsSequential
			sequentialFileControlEntryClause*
		)?
		PERIOD
	;

sequentialFileControlEntryClause :
		reserveClause
	|	paddingCharacterClause
	|	recordDelimiterClause
	|	passwordClause
	|	fileStatusClause
	|	accessModeClause[0x100]
	;

/**
 * Indexed file control entry.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=152&zoom=auto,-40,730
 */
indexedFileControlEntry :
		selectClause assignClause
		indexedFileControlEntryClause*
		organizationIsIndexed
		indexedFileControlEntryClause*
		PERIOD
	;

indexedFileControlEntryClause :
		reserveClause
	|	recordKeyClause passwordClause?
	|	alternateRecordKeyClause passwordClause?
	|	fileStatusClause
	|	accessModeClause[0x0100 | 0x0200 | 0x0400]
	;

/**
 * Relative file control entry.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=153&zoom=auto,-40,730
 */
relativeFileControlEntry :
		selectClause assignClause
		relativeFileControlEntryClause*
		organizationIsRelative
		relativeFileControlEntryClause*
		PERIOD
	;

relativeFileControlEntryClause :
		reserveClause
	|	passwordClause
	|	fileStatusClause
	|	accessModeClause[0x0100 | 0x0800 | 0x1000 | 0x2000]
	;

/**
 * Line sequential file control entry.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=153&zoom=auto,-40,320
 */
lineSequentialFileControlEntry :
		selectClause assignClause
		lineSequentialFileControlEntryClause*
		organizationIsLineSequential
		lineSequentialFileControlEntryClause*
		PERIOD
	;

lineSequentialFileControlEntryClause :
		fileStatusClause
	|	accessModeClause[0x0100]
	;

/*
 * Organization clauses.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=159&zoom=auto,-40,550
 */
 
organizationIsSequential :
		(ORGANIZATION IS?)? SEQUENTIAL
	;

organizationIsIndexed :
		(ORGANIZATION IS?)? INDEXED
	;

organizationIsRelative :
		(ORGANIZATION IS?)? RELATIVE
	;

organizationIsLineSequential :
		(ORGANIZATION IS?)? LINE SEQUENTIAL
	;

/**
 * Reserve clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=159&zoom=auto,-40,730
 */
reserveClause :
		RESERVE INTEGER (AREA | AREAS)
	;

/**
 * Padding character clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=161&zoom=auto,-40,240
 */
paddingCharacterClause :
		PADDING CHARACTER? IS? (refDataName | literal)
	;

/**
 * Record delimiter clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=162&zoom=auto,-40,700
 */
recordDelimiterClause :
		RECORD DELIMITER IS? (STANDARD_1 | assignmentName)
	;

/**
 * Access mode clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=162&zoom=auto,-40,520
 */
accessModeClause[int flags] :
		ACCESS MODE? IS? accessMode[$flags]
	;

accessMode[int flags] :
		{$flags << ~8 < 0}? SEQUENTIAL
	|	{$flags << ~9 < 0}? RANDOM
	|	{$flags << ~10 < 0}? DYNAMIC
	|	{$flags << ~11 < 0}? SEQUENTIAL relativeKeyClause
	|	{$flags << ~12 < 0}? RANDOM relativeKeyClause
	|	{$flags << ~13 < 0}? DYNAMIC relativeKeyClause
	;

/**
 * Record key clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=164&zoom=auto,-40,400
 */
recordKeyClause :
		RECORD KEY? IS? refDataName
	;

/**
 * Alternate record key clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=165&zoom=auto,-40,500
 */
alternateRecordKeyClause :
		ALTERNATE RECORD? KEY? IS? refDataName (WITH? DUPLICATES)?
	;

/**
 * Password clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=167&zoom=auto,-40,730
 */
passwordClause :
		PASSWORD IS? refDataName
	;

/**
 * Relative key clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=166&zoom=auto,-40,400
 */
relativeKeyClause :
		RELATIVE KEY? IS? refDataName
	;

/**
 * File status clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=167&zoom=auto,-40,280
 */
fileStatusClause :
		FILE? STATUS IS? refDataName refDataName?
	;

/**
 * I-O-Control paragraph.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=168&zoom=auto,-40,250
 */
ioControlParagraph :
		I_O_CONTROL PERIOD
		ioControlEntry+
		PERIOD
	;

ioControlEntry :
		rerunClause
	|	sameAreaClause
	|	multipleFileTapeClause
	|	applyWriteOnlyClause
	;

/**
 * Rerun clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=170&zoom=auto,-40,300
 * 
 * Note: reference documentation defines rerun clause over assignmentName and 
 * fileName. But ANTLR won't be able to decide which one to take (because both 
 * are defined the same), unless we keep track of which files were defined on 
 * the FILE-CONTROL paragraph.
 */
rerunClause :
		RERUN ON? (/*assignmentName | */fileName) (EVERY? (INTEGER RECORDS | END OF? (REEL | UNIT)) OF? fileName)?
	;

/**
 * Same area clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=171&zoom=auto,-40,150
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=172&zoom=auto,-40,470 (RECORD)
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=173&zoom=auto,-40,660 (SORT)
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=173&zoom=auto,-40,330 (SORT-MERGE)
 */
sameAreaClause :
		SAME (RECORD | SORT | SORT_MERGE)? AREA? FOR? fileName+
	;

/**
 * Multiple file tape clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=173&zoom=auto,-40,240
 */
multipleFileTapeClause :
		MULTIPLE FILE TAPE? CONTAINS? (fileName (POSITION INTEGER)?)+
	;

/**
 * Apply write-only clause.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=173&zoom=auto,-40,140
 */
applyWriteOnlyClause :
		APPLY WRITE_ONLY ON? fileName+
	;
