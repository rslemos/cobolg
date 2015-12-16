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
parser grammar Statements;
import Basics;

options { tokenVocab = COBOLLexer; }

/**
 * Procedural statements.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=302&zoom=auto,-40,185
 */
proceduralStatement :
		imperativeStatement
	;

/**
 * Imperative statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=302&zoom=auto,-40,120
 */
imperativeStatement :
		/* unknown statements */
		stmtSTOPRUN
		/* arithmetic (without the ON SIZE ERROR or the NOT ON SIZE ERROR phrase) */
	|	stmtADDimperative
	|	stmtCOMPUTEimperative
	|	stmtDIVIDEimperative
	|	stmtMULTIPLYimperative
	|	stmtSUBTRACTimperative
//	|	ACCEPT // format 2
	|	stmtINITIALIZE
	|	stmtINSPECT
	|	stmtMOVE
		/* input-output (without the INVALID KEY or the NOT INVALID KEY phrase or the AT END or NOT AT END, and INVALID KEY or NOT INVALID or the INVALID KEY or NOT INVALID KEY, and END-OF-PAGE or NOT END-OF-PAGE phrases) */
	|	stmtACCEPT // format 1
	|	stmtCLOSE
	|	stmtDELETEimperative
	|	stmtDISPLAY
	|	stmtOPEN
	|	stmtSequentialREADimperative
	|	stmtRandomREADimperative
	|	stmtREWRITEimperative
	|	stmtSTARTimperative
	|	stmtSTOP
	|	stmtPageWRITEimperative
	|	stmtSequentialWRITEimperative
	;

/**
 * CORRESPONDING phrase.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=307&zoom=auto,-40,290
 */
correspondingPhrase : (CORR | CORRESPONDING);

/**
 * ROUNDED phrase.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=309&zoom=auto,-40,735
 */
roundedPhrase : identifier ROUNDED?;

/**
 * GIVING phrase.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=308&zoom=auto,-40,140
 */
givingPhrase : GIVING roundedPhrase+;

/* here come the actual statements (all prefixed by stmt) */

/**
 * ACCEPT statement.
 * 
 * Format 1:
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=322&zoom=auto,-40,735
 * 
 * Format 2:
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=323&zoom=auto,-40,105
 */
stmtACCEPT :
		ACCEPT identifier (FROM (mnemonicName | environmentName))?
	|	ACCEPT identifier FROM (DATE YYYYMMDD?| DAY YYYYDDD? | DAY_OF_WEEK | TIME)
	;

/**
 * ADD statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=326&zoom=auto,-40,735
 */
stmtADDimperative :
		ADD (identifier | literal)+ TO roundedPhrase+
	|	ADD (identifier | literal)+ TO? (identifier | literal) givingPhrase
	|	ADD correspondingPhrase identifier TO roundedPhrase
	;

/**
 * CLOSE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=341&zoom=auto,-40,735
 */
stmtCLOSE : CLOSE (fileName ((REEL | UNIT) (FOR? REMOVAL | WITH NO REWIND) | WITH? (NO REWIND | LOCK))?)+;

/**
 * COMPUTE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=345&zoom=auto,-40,735
 */
stmtCOMPUTEimperative : COMPUTE roundedPhrase+ (EQUAL | OP_EQUAL) arithmeticExpression;

/**
 * DELETE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=348&zoom=auto,-40,735
 */
stmtDELETEimperative : DELETE fileName RECORD?;

/**
 * DISPLAY statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=350&zoom=auto,-40,735
 */
stmtDISPLAY : DISPLAY (identifier | literal)+ (UPON (mnemonicName | environmentName))? (WITH? NO ADVANCING)?;

/**
 * DIVIDE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=353&zoom=auto,-40,735
 */
stmtDIVIDEimperative :
		DIVIDE (identifier | literal) INTO roundedPhrase+
	|	DIVIDE (identifier | literal) (INTO | BY) (identifier | literal) givingPhrase
	|	DIVIDE (identifier | literal) (INTO | BY) (identifier | literal) GIVING roundedPhrase REMAINDER identifier
	;

/**
 * INITIALIZE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=372&zoom=auto,-40,735
 */
stmtINITIALIZE : INITIALIZE identifier+ (REPLACING ((ALPHABETIC | ALPHANUMERIC | ALPHANUMERIC_EDITED | NATIONAL | NATIONAL_EDITED | NUMERIC | NUMERIC_EDITED | DBCS | EGCS) DATA? BY (identifier | literal))+)?;

/**
 * INSPECT statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=375&zoom=auto,-40,735
 */
stmtINSPECT :
		INSPECT identifier TALLYING (identifier FOR inspectTallyingFor+)+
	|	INSPECT identifier REPLACING inspectReplacingObject+
	|	INSPECT identifier TALLYING (identifier FOR inspectTallyingFor+)+ REPLACING inspectReplacingObject+
	|	INSPECT identifier CONVERTING (identifier | literal) TO (identifier | literal) ((BEFORE | AFTER) INITIAL? (identifier | literal))+
	;

inspectTallyingFor :
		CHARACTERS ((BEFORE | AFTER) INITIAL? (identifier | literal))*
	|	(ALL | LEADING) ((identifier | literal) ((BEFORE | AFTER) INITIAL? (identifier | literal))*)+
	;

inspectReplacingObject :
		CHARACTERS BY (identifier | literal) ((BEFORE | AFTER) INITIAL? (identifier | literal))*
	|	(ALL | LEADING | FIRST) ((identifier | literal) BY (identifier | literal) ((BEFORE | AFTER) INITIAL? (identifier | literal))*)+
	;

/**
 * MOVE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=398&zoom=auto,-40,735
 */
stmtMOVE :
		MOVE (identifier | literal) TO identifier+
	|	MOVE (CORRESPONDING | CORR) identifier TO identifier
	;

/**
 * MULTIPLY statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=405&zoom=auto,-40,735
 */
stmtMULTIPLYimperative :
		MULTIPLY (identifier | literal) BY roundedPhrase+
	|	MULTIPLY (identifier | literal) BY (identifier | literal) givingPhrase
	;

/**
 * OPEN statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=408&zoom=auto,-40,735
 */
stmtOPEN : OPEN openObject+;

openObject :
		INPUT (fileName (REVERSED | WITH? NO REWIND)?)+
	|	OUTPUT (fileName (WITH? NO REWIND)?)+
	|	I_O (fileName)+
	|	EXTEND (fileName)+
	;

/**
 * READ statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=424&zoom=auto,-40,735
 */
stmtSequentialREADimperative : READ fileName NEXT? RECORD? (INTO identifier)?;

stmtRandomREADimperative : READ fileName RECORD? (INTO identifier)? (KEY IS? dataName);

/**
 * REWRITE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=435&zoom=auto,-40,735
 */
stmtREWRITEimperative : REWRITE recordName (FROM identifier);

/**
 * START statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=462&zoom=auto,-40,735
 */
stmtSTARTimperative : START fileName (KEY IS? (EQUAL TO? | OP_EQUAL | GREATER THAN? | OP_GREATER | NOT LESS THAN? | NOT OP_LESS | GREATER THAN? OR EQUAL TO? | OP_NOTLESS) dataName)?;

/**
 * STOP statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=465&zoom=auto,-40,735
 */
stmtSTOP : STOP literal;

stmtSTOPRUN : STOP RUN;

/**
 * SUBTRACT statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=471&zoom=auto,-40,735
 */
stmtSUBTRACTimperative :
		SUBTRACT (identifier | literal)+ FROM roundedPhrase+
	|	SUBTRACT (identifier | literal)+ FROM (identifier | literal) givingPhrase
	|	SUBTRACT correspondingPhrase identifier FROM roundedPhrase
	;

/**
 * WRITE statement.
 * 
 * Reference manual does not mention 'LINE' (only 'LINES').
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=482&zoom=auto,-40,735
 */
stmtPageWRITEimperative : WRITE recordName (FROM identifier)? ((BEFORE | AFTER) ADVANCING? ((identifier | literal) (LINE | LINES) | mnemonicName | PAGE))?;

stmtSequentialWRITEimperative : WRITE recordName (FROM identifier)?;
