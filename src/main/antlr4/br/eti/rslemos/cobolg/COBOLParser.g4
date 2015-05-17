/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2013  Rodrigo Lemos
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
parser grammar COBOLParser;

options { tokenVocab = COBOLFixedFormatLexer; }

program :
		identificationDivision
		environmentDivision?
		dataDivision?
		procedureDivision
	;

/* divisions */

identificationDivision :
		IDENTIFICATION DIVISION PERIOD
		PROGRAM_ID PERIOD programName PERIOD
	;

environmentDivision :
		ENVIRONMENT DIVISION PERIOD
		configurationSection?
		inputOutputSection?
	;

dataDivision :
		DATA DIVISION PERIOD
		fileSection?
		workingStorageSection?
		linkageSection?
	;

procedureDivision :
		PROCEDURE DIVISION usingClause? PERIOD
		( unnamedProceduralSection namedProceduralSection* | namedProceduralSection+ )
	;

/* sections */

configurationSection :
		CONFIGURATION SECTION PERIOD
		objectComputerParagraph?
		specialNamesParagraph?
	;

inputOutputSection :
		INPUT_OUTPUT SECTION PERIOD
		fileControlParagraph?
	;

fileSection :
		FILE SECTION PERIOD
		fileDescriptionParagraph*
	;

workingStorageSection :
		WORKING_STORAGE SECTION PERIOD
		dataDescriptionParagraph*
	;

linkageSection :
		LINKAGE SECTION PERIOD
		dataDescriptionParagraph*
	;

unnamedProceduralSection :
		( unnamedProceduralParagraph namedProceduralParagraph* | namedProceduralParagraph+ )
	;

namedProceduralSection :
		sectionName SECTION PERIOD
		( unnamedProceduralParagraph namedProceduralParagraph* | namedProceduralParagraph+ )
	;

/* paragraphs */
objectComputerParagraph :
		OBJECT_COMPUTER PERIOD
		ID
		PERIOD
	;

specialNamesParagraph :
		SPECIAL_NAMES PERIOD
		specialNamesSentence+
		PERIOD
	;

fileControlParagraph :
		FILE_CONTROL PERIOD
		selectFileSentence+
	;

fileDescriptionParagraph :
		FD fileName
		fdIsClauses?
		fdBlockClause?
		fdRecordClause?
		fdLabelRecordClause?
		fdValueOfClause?
		fdDataRecordClause?
		fdLinageClause?
		fdRecordingModeClause?
		fdCodeSetClause?
		PERIOD
	;

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

unnamedProceduralParagraph :
		proceduralStatement+
	;

namedProceduralParagraph :
		paragraphName PERIOD
		proceduralStatement+
	;

/* sentences */
specialNamesSentence :
		ID IS? ID
	;

selectFileSentence :
		SELECT OPTIONAL? ID ASSIGN TO? ID
		(fileOrganizationIndexed)?
		PERIOD
	;

// TODO: may appear in any order, but at most once
// (though this may be not a syntatic concern, but rather semantic one)
fileOrganizationIndexed :
		RECORD KEY? IS? ID
		(ACCESS MODE? IS? SEQUENTIAL)?	// other modes also apply (but not now)
		(STATUS IS? ID)?				// this clause belongs to general selectFileSentence
		ORGANIZATION IS? INDEXED
	;

/* statements */

proceduralStatement :
		DISPLAY literal PERIOD
	|	STOP RUN PERIOD
	;

/* other elements */

programName :
		ID
	;

sectionName :
		ID
	;

paragraphName :
		ID
	;

fileName :
		ID
	;

systemName :
		ID
	;

alphabetName :
		ID
	;

dataName :
		ID
	;

indexName :
		ID
	;

fdIsClauses :
		IS? EXTERNAL (IS? GLOBAL)?
	|	IS? GLOBAL
	;

fdBlockClause :
		BLOCK CONTAINS? (from=INTEGER TO)? to=INTEGER (CHARACTERS | RECORDS)
	;

fdRecordClause :
		RECORD CONTAINS? (from=INTEGER TO)? to=INTEGER CHARACTERS?
	|	RECORD IS? VARYING IN? SIZE? (FROM? from=INTEGER)? (TO to=INTEGER)? CHARACTERS? (DEPENDING ON? dependingOn=dataName)?
	;

fdLabelRecordClause :
		LABEL (RECORD IS? | RECORDS ARE?) (STANDARD | OMITTED | dataName*) // why not dataName+?
	;

fdValueOfClause :
		VALUE OF (systemName IS? (dataName | literal))+
	;

fdDataRecordClause :
		DATA (RECORD IS? | RECORDS ARE?) dataName+
	;

fdLinageClause :
		LINAGE IS? (dataName | INTEGER) LINES?
		(WITH? FOOTING AT? footingAt)? 
		(LINES? AT? TOP linesAtTop)?
		(LINES? AT? BOTTOM linesAtBottom)?
	;

footingAt :
		(dataName | INTEGER)
	;

linesAtTop :
		(dataName | INTEGER)
	;

linesAtBottom :
		(dataName | INTEGER)
	;

fdRecordingModeClause :
		RECORDING REC_MODE? REC_IS? (F | V | U | S)
	;

fdCodeSetClause :
		CODE_SET IS? alphabetName
	;

redefinesClause :
		REDEFINES dataName
	;

pictureClause :
		PICTURE IS? PICTURESTRING
	;

usageClause :
		(USAGE IS?)? usage
	;

usage :
		BINARY NATIVE?
	|	COMPUTATIONAL NATIVE?
	|	COMPUTATIONAL_1 NATIVE?
	|	COMPUTATIONAL_2 NATIVE?
	|	COMPUTATIONAL_3 NATIVE?
	|	COMPUTATIONAL_4 NATIVE?
	|	COMPUTATIONAL_5 NATIVE?
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

usingClause :
		USING ((BY? (REFERENCE|VALUE))? dataName)+
	;

literal :
		numericLiteral
	|	alphanumericLiteral
	|	figurativeConstant
	;

numericLiteral :
		INTEGER
	|	FIXEDPOINT
	|	HEXINTEGER
	;

alphanumericLiteral :
		quotedString
	|	HEXSTRING
	;

figurativeConstant :
		ZERO
	|	SPACE
	|	HIGH_VALUE
	|	LOW_VALUE
	|	QUOTE
	|	ALL literal
	|	NULL
//	|	symbolicCharacter
	;

quotedString :
		DOUBLEQUOTEDSTRING
	|	SINGLEQUOTEDSTRING
	|	DOUBLEQUOTEDSTRING_START DOUBLEQUOTEDSTRING_MID* DOUBLEQUOTEDSTRING_END
	|	SINGLEQUOTEDSTRING_START SINGLEQUOTEDSTRING_MID* SINGLEQUOTEDSTRING_END
	;
	
levelNumber :
		INTEGER { $INTEGER.text.matches("^[0-9][0-9]$") }?
	;

/* compiler statements */

compilerStatements :
		compilerStatement*
	;

compilerStatement :
		EJECT
	|	COPY COPY_ID COPY_PERIOD
	;