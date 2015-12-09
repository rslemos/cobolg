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

objectComputerParagraph :
		OBJECT_COMPUTER PERIOD
		USERDEFINEDWORD
		PERIOD
	;

specialNamesParagraph :
		SPECIAL_NAMES PERIOD
		specialNamesSentence+
		PERIOD
	;

specialNamesSentence :
		USERDEFINEDWORD IS? USERDEFINEDWORD
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

