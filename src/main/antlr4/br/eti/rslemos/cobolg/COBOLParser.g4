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

dataDescriptionParagraph :
		levelNumber dataName
		PERIOD
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

literal :
		numericLiteral
	|	alphanumericLiteral
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

quotedString :
		DOUBLEQUOTEDSTRING
	|	SINGLEQUOTEDSTRING
	|	DOUBLEQUOTEDSTRING_START DOUBLEQUOTEDSTRING_MID* DOUBLEQUOTEDSTRING_END
	|	SINGLEQUOTEDSTRING_START SINGLEQUOTEDSTRING_MID* SINGLEQUOTEDSTRING_END
	;
	
levelNumber :
		INTEGER { $INTEGER.text.matches("^[0-9][0-9]$") }?
	;
