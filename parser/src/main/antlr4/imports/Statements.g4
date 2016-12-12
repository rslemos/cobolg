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
	|	conditionalStatement
	;

/**
 * Imperative statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=302&zoom=auto,-40,120
 */
imperativeStatement :
		/* arithmetic (without the ON SIZE ERROR or the NOT ON SIZE ERROR phrase) */
		stmtADDimperative
	|	stmtCOMPUTEimperative
	|	stmtDIVIDEimperative
	|	stmtMULTIPLYimperative
	|	stmtSUBTRACTimperative
		/* data movement (without the ON OVERFLOW or the NOT ON OVERFLOW phrase or the ON EXCEPTION or NOT ON EXCEPTION phrase) */
//	|	ACCEPT // format 2
	|	stmtINITIALIZE
	|	stmtINSPECT
	|	stmtMOVE
	|	stmtSET
	|	stmtSTRINGimperative
	|	stmtUNSTRINGimperative
	|	stmtXMLGENERATEimperative
	|	stmtXMLPARSEimperative
		/* ending */
	|	stmtEXIT
	|	stmtSTOPRUN
	|	stmtGOBACK
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
		/* ordering (without the AT END or NOT AT END phrase) */
	|	stmtMERGE
	|	stmtRELEASE
	|	stmtRETURNimperative
	|	stmtSORT
		/* procedure-branching */
	|	stmtALTER
	|	stmtGOTO
	|	stmtPERFORMimperative
	|	stmtCONTINUE
		/* program or method linkage (without the ON OVERFLOW phrase, and without the ON EXCEPTION or NOT ON EXCEPTION phrase) */
	|	stmtCALLimperative
	|	stmtCANCEL
	|	stmtINVOKEimperative
	;

/**
 * Conditional statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=304&zoom=auto,-40,280
 */
conditionalStatement :
		stmtDELETEconditional
	|	stmtSequentialREADconditional
	|	stmtRandomREADconditional
	|	stmtREWRITEconditional
	|	stmtSTARTconditional
	|	stmtPageWRITEconditional
	|	stmtSequentialWRITEconditional
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

/**
 * SIZE ERROR phrases.
 * 
 * (should allow any order?)
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=309&zoom=auto,-40,340
 */
sizeErrorPhrases : onSizeErrorPhrase? notOnSizeErrorPhrase?;
onSizeErrorPhrase    :     ON? SIZE ERROR imperativeStatement;
notOnSizeErrorPhrase : NOT ON? SIZE ERROR imperativeStatement;

/**
 * EXCEPTION phrases.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=337&zoom=auto,-40,410
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=337&zoom=auto,-40,230
 */
exceptionPhrases : onExceptionPhrase? notOnExceptionPhrase?;
onExceptionPhrase    :     ON? EXCEPTION imperativeStatement;
notOnExceptionPhrase : NOT ON? EXCEPTION imperativeStatement;

/**
 * OVERFLOW phrases.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=338&zoom=auto,-40,700
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=468&zoom=auto,-40,630
 */
overflowPhrases : onOverflowPhrase? notOnOverflowPhrase?;
onOverflowPhrase    :     ON? OVERFLOW imperativeStatement;
notOnOverflowPhrase : NOT ON? OVERFLOW imperativeStatement;

/**
 * INVALID KEY phrases.
 * 
 */
invalidKeyPhrases : invalidKeyPhrase? notInvalidKeyPhrase?;
invalidKeyPhrase    :     INVALID KEY? imperativeStatement;
notInvalidKeyPhrase : NOT INVALID KEY? imperativeStatement;

/**
 * AT END phrases.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=425&zoom=auto,-40,190
 */
atEndPhrases : atEndPhrase? notAtEndPhrase?;
atEndPhrase    :     AT? END imperativeStatement;
notAtEndPhrase : NOT AT? END imperativeStatement;

/**
 * AT END-OF-PAGE phrases.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=485&zoom=auto,-40,335
 */
atEndOfPagePhrases : atEndOfPagePhrase? notAtEndOfPagePhrase?;
atEndOfPagePhrase    :     AT? (END_OF_PAGE | EOP) imperativeStatement;
notAtEndOfPagePhrase : NOT AT? (END_OF_PAGE | EOP) imperativeStatement;

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
 * ALTER statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=329&zoom=auto,-40,735
 */
stmtALTER : ALTER (procedureName TO (PROCEED TO)? procedureName)+;

/**
 * CALL statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=331&zoom=auto,-40,735
 */
stmtCALLimperative : CALL (identifier | literal /* | procedurePointer | functionPointer */) (USING callUsing+)? (RETURNING identifier)?;

callUsing :
		(BY? REFERENCE)? ((ADDRESS OF)? identifier /* | fileName */| OMITTED)+
	|	BY? CONTENT (((ADDRESS|LENGTH) OF)? identifier | literal | OMITTED)+
	|	BY? VALUE (((ADDRESS|LENGTH) OF)? identifier | literal)+
	;

/**
 * CANCEL statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=339&zoom=auto,-40,735
 */
stmtCANCEL : CANCEL (identifier | literal)+;

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
 * CONTINUE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=347&zoom=auto,-40,735
 */
stmtCONTINUE : CONTINUE;

/**
 * DELETE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=348&zoom=auto,-40,735
 */
stmtDELETEimperative : DELETE fileName RECORD?;

stmtDELETEconditional : stmtDELETEimperative invalidKeyPhrases;

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
 * EXIT statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=363&zoom=auto,-40,735
 */
stmtEXIT :
		EXIT (PROGRAM | METHOD | PARAGRAPH | SECTION | PERFORM CYCLE?)?
	;

/**
 * GO TO statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=368&zoom=auto,-40,735
 */
stmtGOTO :
		GO TO?
	|	GO TO? procedureName
	|	GO TO? procedureName+ DEPENDING ON? identifier
	;

/**
 * GOBACK statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=367&zoom=auto,-40,735
 */
stmtGOBACK : GOBACK;

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
 * INVOKE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=385&zoom=auto,-40,735
 */
stmtINVOKEimperative :
		INVOKE (identifier | className | SELF | SUPER) (literal | identifier | NEW)
		(USING (BY? VALUE ((LENGTH OF)? identifier | literal)+)+)?
		(RETURNING identifier)?
	;

/**
 * MERGE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=393&zoom=auto,-40,735
 */
stmtMERGE :
		MERGE fileName (ON? (ASCENDING | DESCENDING) KEY? dataName+)+
		(COLLATING? SEQUENCE IS? alphabetName)?
		USING fileName fileName+
		(OUTPUT PROCEDURE IS? procedureName ((THROUGH | THRU) procedureName)? | GIVING fileName+)
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
 * PERFORM statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=413&zoom=auto,-40,735
 */
stmtPERFORMimperative : PERFORM procedureName ((THROUGH | THRU) procedureName)? (performTimes | performUntil | performVarying performVaryingAfterPhrase*)?;

performTimes : (identifier | INTEGER) TIMES;

performUntil : (WITH? TEST (BEFORE|AFTER))? UNTIL conditionalExpression;

performVarying : (WITH? TEST (BEFORE|AFTER))? VARYING (identifier | indexName) FROM (identifier | indexName | literal) BY (identifier | literal) UNTIL conditionalExpression;

performVaryingAfterPhrase: AFTER (identifier | indexName) FROM (identifier | indexName | literal) BY (identifier | literal) UNTIL conditionalExpression;

/**
 * READ statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=424&zoom=auto,-40,735
 */
stmtSequentialREADimperative : READ fileName NEXT? RECORD? (INTO identifier)?;

stmtRandomREADimperative : READ fileName RECORD? (INTO identifier)? (KEY IS? dataName);

stmtSequentialREADconditional : stmtSequentialREADimperative atEndPhrases;

stmtRandomREADconditional : stmtRandomREADimperative invalidKeyPhrases;

/**
 * RELEASE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=431&zoom=auto,-40,735
 */
stmtRELEASE : RELEASE recordName (FROM identifier)?;

/**
 * RETURN statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=433&zoom=auto,-40,735
 */
stmtRETURNimperative : RETURN fileName RECORD? (INTO identifier)?;

/**
 * REWRITE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=435&zoom=auto,-40,735
 */
stmtREWRITEimperative : REWRITE recordName (FROM identifier);

stmtREWRITEconditional : stmtREWRITEimperative invalidKeyPhrases;

/**
 * SET statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=445&zoom=auto,-40,735
 */
stmtSET :
		SET (indexName | identifier)+ TO (indexName | identifier | INTEGER)
	|	SET indexName+ (UP | DOWN) BY (identifier | INTEGER)
	|	SET (mnemonicName+ TO (ON | OFF))+
	|	SET conditionName+ TO TRUE
	|	SET (identifier | ADDRESS OF identifier)+ TO (identifier | ADDRESS OF identifier | NULL | NULLS)
//	|	SET (procedurePointer | functionPointer)+ TO (procedurePointer | functionPointer | ENTRY (identifier | literal) | NULL | NULLS | pointerDataItem)
//	|	SET objectReference TO (objectReference | SELF | NULL)
	;

/**
 * SORT statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=452&zoom=auto,-40,735
 */
stmtSORT :
		SORT dataName (ON? (ASCENDING | DESCENDING) KEY? dataName+)* (WITH? DUPLICATES IN? ORDER?)? (COLLATING? SEQUENCE IS? alphabetName)?
	|	SORT fileName (ON? (ASCENDING | DESCENDING) KEY? dataName+)+ (WITH? DUPLICATES IN? ORDER?)? (COLLATING? SEQUENCE IS? alphabetName)?
		(USING fileName+ | INPUT PROCEDURE IS? procedureName ((THROUGH | THRU) procedureName)?)
		(GIVING fileName+ | OUTPUT PROCEDURE IS? procedureName ((THROUGH | THRU) procedureName)?)
	;

/**
 * START statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=462&zoom=auto,-40,735
 */
stmtSTARTimperative : START fileName (KEY IS? (EQUAL TO? | OP_EQUAL | GREATER THAN? | OP_GREATER | NOT LESS THAN? | NOT OP_LESS | GREATER THAN? OR EQUAL TO? | OP_NOTLESS) dataName)?;

stmtSTARTconditional : stmtSTARTimperative invalidKeyPhrases;

/**
 * STOP statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=465&zoom=auto,-40,735
 */
stmtSTOP : STOP literal;

stmtSTOPRUN : STOP RUN;

/**
 * STRING statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=466&zoom=auto,-40,735
 */
stmtSTRINGimperative : STRING ((identifier | literal)+ DELIMITED BY? (identifier | literal | SIZE))+ INTO identifier (WITH? POINTER identifier)?;

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
 * UNSTRING statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=474&zoom=auto,-40,735
 */
stmtUNSTRINGimperative :
		UNSTRING identifier
		(DELIMITED BY? ALL? (identifier | literal) (OR ALL? (identifier | literal))*)?
		INTO (identifier (DELIMITER IN? identifier)? (COUNT IN? identifier))+
		(WITH? POINTER identifier)?
		(TALLYING IN? identifier)?
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

stmtPageWRITEconditional : stmtPageWRITEimperative atEndOfPagePhrases;

stmtSequentialWRITEconditional : stmtSequentialWRITEimperative invalidKeyPhrases;

/**
 * XML GENERATE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=490&zoom=auto,-40,735
 */
stmtXMLGENERATEimperative :
		XML GENERATE identifier FROM identifier
		(COUNT IN? identifier)?
		(WITH? ENCODING (identifier | literal))?
		(WITH? XML_DECLARATION)?
		(WITH? ATTRIBUTES)?
		(NAMESPACE IS? (identifier | literal) (NAMESPACE_PREFIX IS? (identifier | literal))?)?
		(NAME OF? (identifier IS? literal)+)?
		(TYPE OF? (identifier IS? (ATTRIBUTE | ELEMENT | CONTENT))+)?
		(SUPPRESS (identifier xmlGenerateWhenPhrase? | genericSupressionPhrase)+)?
	;

xmlGenerateWhenPhrase :
		WHEN (ZERO | ZEROS | ZEROES | SPACE | SPACES | HIGH_VALUE | HIGH_VALUES | LOW_VALUE | LOW_VALUES)
		(OR? (ZERO | ZEROS | ZEROES | SPACE | SPACES | HIGH_VALUE | HIGH_VALUES | LOW_VALUE | LOW_VALUES))*
	;

genericSupressionPhrase : (EVERY ((NUMERIC | NONNUMERIC)? (ATTRIBUTE | CONTENT | ELEMENT) | NUMERIC | NONNUMERIC))? xmlGenerateWhenPhrase;

/**
 * XML PARSE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=502&zoom=auto,-40,735
 */
stmtXMLPARSEimperative :
		XML PARSE identifier
		(WITH? ENCODING (identifier | literal))?
		(RETURNING NATIONAL)?
		(VALIDATING WITH? (identifier | FILE xmlSchemaName))?
		PROCESSING PROCEDURE IS? procedureName ((THROUGH | THRU) procedureName)?
	;
