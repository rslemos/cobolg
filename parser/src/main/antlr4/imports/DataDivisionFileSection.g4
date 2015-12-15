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
parser grammar DataDivisionFileSection;
import Basics, DataDescriptionBasics;

options { tokenVocab = COBOLLexer; }

fileSection :
		FILE SECTION PERIOD
		fileDescriptionParagraph*
	;

fileDescriptionParagraph :
		fileDescriptionEntry PERIOD
		recordDescriptionEntry* // the reference manual states recordDescriptionEntry+
	;

/**
 * File description entry.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=193&zoom=auto,-40,730
 */
fileDescriptionEntry :
		FD fileName fileDescriptionEntryClauses
	|	SD fileName sortDescriptionEntryClauses
	;

fileDescriptionEntryClauses :
		fileDescriptionEntryClause*
	;

fileDescriptionEntryClause :
		externalClause
	|	globalClause
	|	blockContainsClause
	|	fdRecordClause
	|	fdLabelRecordClause
	|	fdValueOfClause
	|	fdDataRecordClause
	|	fdLinageClause
	|	fdRecordingModeClause
	|	fdCodeSetClause
	;

sortDescriptionEntryClauses :
		sortDescriptionEntryClause*
	;

sortDescriptionEntryClause :
		fdRecordClause
	|	fdDataRecordClause
	|	blockContainsClause
	|	fdLabelRecordClause
	|	fdValueOfClause
	|	fdLinageClause
	|	fdCodeSetClause
	;

/**
 * Block contains.
 * 
 * Main syntax diagram indicates that either CHARACTERS or RECORDS should be 
 * used. But reference text says that "the CHARACTERS phrase is the default".
 * Moreover sources were found where none of them is specified (look at NIST 
 * COBOL-85 Test Suite's IX(IX216A), IX(IX108A)).
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=199&zoom=auto,-100,390
 */
blockContainsClause :
		BLOCK CONTAINS?
		(INTEGER TO)?
		INTEGER
		(CHARACTERS | RECORDS)?
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

systemName :
		USERDEFINEDWORD
	;

