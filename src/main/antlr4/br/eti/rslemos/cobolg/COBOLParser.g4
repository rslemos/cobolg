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

options { tokenVocab = COBOLFreeFormatLexer; }

program :
		identificationDivision
		environmentDivision?
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

/* sentences */
specialNamesSentence :
		ID IS ID
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
		QUOTEDSTRING
	|	HEXSTRING
	;
