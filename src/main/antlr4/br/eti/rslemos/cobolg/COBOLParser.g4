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
		PROCEDURE DIVISION PERIOD
		userDefinedProcedureSection*
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
	;

workingStorageSection :
		WORKING_STORAGE SECTION PERIOD
		dataDescriptionParagraph*
	;

linkageSection :
		LINKAGE SECTION PERIOD
		dataDescriptionParagraph*
	;

userDefinedProcedureSection :
		( sectionName SECTION PERIOD )?
		( paragraphName PERIOD )?
		proceduralStatement+
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

dataDescriptionParagraph
locals [
	PictureClauseContext pictureClause_ = null,
	UsageClauseContext usageClause_ = null,
	ValueClauseContext valueClause_ = null,
	OccursClauseContext occursClause_ = null
] :
		levelNumber (dataName | FILLER)?
		redefinesClause?
		dataDescriptionClauses*
		PERIOD
	;

dataDescriptionClauses :
		{ $dataDescriptionParagraph::pictureClause_ == null }? pictureClause { $dataDescriptionParagraph::pictureClause_ = $pictureClause; }
	|	{ $dataDescriptionParagraph::usageClause_ == null }? usageClause { $dataDescriptionParagraph::usageClause_ = $usageClause; }
	|	{ $dataDescriptionParagraph::valueClause_ == null }? valueClause { $dataDescriptionParagraph::valueClause_ = $valueClause; }
	|	{ $dataDescriptionParagraph::occursClause_ == null }? occursClause { $dataDescriptionParagraph::occursClause_ = $occursClause; }
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

dataName :
		ID
	;

indexName :
		ID
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
