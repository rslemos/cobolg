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
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=302&zoom=auto,-40,120
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=304&zoom=auto,-40,280
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=306&zoom=auto,-40,670
 */
proceduralStatement[boolean conditionalAllowed] :
		/* arithmetic */
		stmtADD[$conditionalAllowed]
	|	stmtCOMPUTE[$conditionalAllowed]
	|	stmtDIVIDE[$conditionalAllowed]
	|	stmtMULTIPLY[$conditionalAllowed]
	|	stmtSUBTRACT[$conditionalAllowed]
		/* data movement */
//	|	ACCEPT // format 2
	|	stmtINITIALIZE
	|	stmtINSPECT
	|	stmtMOVE
	|	stmtSET
	|	stmtSTRING[$conditionalAllowed]
	|	stmtUNSTRING[$conditionalAllowed]
	|	stmtXMLGENERATE[$conditionalAllowed]
	|	stmtXMLPARSE[$conditionalAllowed]
		/* decision */
	|	stmtEVALUATE[$conditionalAllowed]
	|	stmtIF[$conditionalAllowed]
		/* ending */
	|	stmtEXIT
	|	stmtSTOPRUN
	|	stmtGOBACK
		/* input-output */
	|	stmtACCEPT // format 1
	|	stmtCLOSE
	|	stmtDELETE[$conditionalAllowed]
	|	stmtDISPLAY
	|	stmtOPEN
	|	stmtSequentialREAD[$conditionalAllowed]
	|	stmtRandomREAD[$conditionalAllowed]
	|	stmtREWRITE[$conditionalAllowed]
	|	stmtSTART[$conditionalAllowed]
	|	stmtSTOP
	|	stmtPageWRITE[$conditionalAllowed]
	|	stmtSequentialWRITE[$conditionalAllowed]
		/* ordering */
	|	stmtMERGE
	|	stmtRELEASE
	|	stmtRETURN[$conditionalAllowed]
	|	stmtSORT
		/* procedure-branching */
	|	stmtALTER
	|	stmtGOTO
	|	stmtPERFORM
	|	stmtCONTINUE
		/* program or method linkage */
	|	stmtCANCEL
	|	stmtCALL[$conditionalAllowed]
	|	stmtINVOKE[$conditionalAllowed]
		/* table-handling */
	|	stmtSEARCH[$conditionalAllowed]
	;

/**
 * Using phrase.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=273&zoom=auto,-40,470
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=274&zoom=auto,-40,410
 */
usingPhrase : USING (byReferencePhrase | byValuePhrase)+;

byReferencePhrase : (BY? REFERENCE)? dataName+;
byValuePhrase : BY? VALUE dataName+;

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
sizeErrorPhrases : onSizeErrorPhrase notOnSizeErrorPhrase? | notOnSizeErrorPhrase;
onSizeErrorPhrase    :     ON? SIZE ERROR proceduralStatement[false];
notOnSizeErrorPhrase : NOT ON? SIZE ERROR proceduralStatement[false];

/**
 * EXCEPTION phrases.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=337&zoom=auto,-40,410
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=337&zoom=auto,-40,230
 */
exceptionPhrases : onExceptionPhrase notOnExceptionPhrase? | notOnExceptionPhrase;
onExceptionPhrase    :     ON? EXCEPTION proceduralStatement[false];
notOnExceptionPhrase : NOT ON? EXCEPTION proceduralStatement[false];

/**
 * OVERFLOW phrases.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=338&zoom=auto,-40,700
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=468&zoom=auto,-40,630
 */
overflowPhrases : onOverflowPhrase notOnOverflowPhrase? | notOnOverflowPhrase;
onOverflowPhrase    :     ON? OVERFLOW proceduralStatement[false];
notOnOverflowPhrase : NOT ON? OVERFLOW proceduralStatement[false];

/**
 * INVALID KEY phrases.
 * 
 */
invalidKeyPhrases : invalidKeyPhrase notInvalidKeyPhrase? | notInvalidKeyPhrase;
invalidKeyPhrase    :     INVALID KEY? proceduralStatement[false];
notInvalidKeyPhrase : NOT INVALID KEY? proceduralStatement[false];

/**
 * AT END phrases.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=425&zoom=auto,-40,190
 */
atEndPhrases : atEndPhrase notAtEndPhrase? | notAtEndPhrase;
atEndPhrase    :     AT? END proceduralStatement[false];
notAtEndPhrase : NOT AT? END proceduralStatement[false];

/**
 * AT END-OF-PAGE phrases.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=485&zoom=auto,-40,335
 */
atEndOfPagePhrases : atEndOfPagePhrase notAtEndOfPagePhrase? | notAtEndOfPagePhrase;
atEndOfPagePhrase    :     AT? (END_OF_PAGE | EOP) proceduralStatement[false];
notAtEndOfPagePhrase : NOT AT? (END_OF_PAGE | EOP) proceduralStatement[false];

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
stmtADD[boolean conditionalAllowed] :
		ADD (identifier | literal)+ TO roundedPhrase+ stmtADDtail[$conditionalAllowed]
	|	ADD (identifier | literal)+ TO? (identifier | literal) givingPhrase stmtADDtail[$conditionalAllowed]
	|	ADD correspondingPhrase identifier TO roundedPhrase stmtADDtail[$conditionalAllowed]
	;

stmtADDtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? sizeErrorPhrases
	|	sizeErrorPhrases? END_ADD
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
stmtCALL[boolean conditionalAllowed] :
		CALL (identifier | literal /* | procedurePointer | functionPointer */) (USING callUsing+)? (RETURNING identifier)? stmtCALLtail[$conditionalAllowed]
	;

callUsing :
		(BY? REFERENCE)? ((ADDRESS OF)? identifier /* | fileName */| OMITTED)+
	|	BY? CONTENT (((ADDRESS|LENGTH) OF)? identifier | literal | OMITTED)+
	|	BY? VALUE (((ADDRESS|LENGTH) OF)? identifier | literal)+
	;

stmtCALLtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? (exceptionPhrases | onOverflowPhrase)
	|	(exceptionPhrases | onOverflowPhrase)? END_CALL
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
stmtCOMPUTE[boolean conditionalAllowed] :
		COMPUTE roundedPhrase+ (EQUAL | OP_EQUAL) arithmeticExpression stmtCOMPUTEtail[$conditionalAllowed]
	;

stmtCOMPUTEtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? sizeErrorPhrases
	|	sizeErrorPhrases? END_COMPUTE
	;

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
stmtDELETE[boolean conditionalAllowed] :
		DELETE fileName RECORD? stmtDELETEtail[$conditionalAllowed]
	;

stmtDELETEtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? invalidKeyPhrases
	|	invalidKeyPhrases? END_DELETE
	;

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
stmtDIVIDE[boolean conditionalAllowed] :
		DIVIDE (identifier | literal) INTO roundedPhrase+ stmtDIVIDEtail[$conditionalAllowed]
	|	DIVIDE (identifier | literal) (INTO | BY) (identifier | literal) givingPhrase stmtDIVIDEtail[$conditionalAllowed]
	|	DIVIDE (identifier | literal) (INTO | BY) (identifier | literal) GIVING roundedPhrase REMAINDER identifier stmtDIVIDEtail[$conditionalAllowed]
	;

stmtDIVIDEtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? sizeErrorPhrases
	|	sizeErrorPhrases? END_DIVIDE
	;

/**
 * EVALUATE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=359&zoom=auto,-40,735
 */
stmtEVALUATE[boolean conditionalAllowed] :
		EVALUATE
		      (identifier | literal | arithmeticExpression | TRUE | FALSE)
		(ALSO (identifier | literal | arithmeticExpression | TRUE | FALSE))*
		(WHEN evaluateWhenPhrase (ALSO evaluateWhenPhrase)* proceduralStatement[false])+
		(WHEN OTHER proceduralStatement[false])?
		stmtEVALUATEtail[$conditionalAllowed]
	;

evaluateWhenPhrase : (NOT? (identifier | literal | arithmeticExpression) ((THRU | THROUGH) (identifier | literal | arithmeticExpression))? | ANY | conditionalExpression | TRUE | FALSE );

stmtEVALUATEtail[boolean conditionalAllowed] :
		{$conditionalAllowed}?
	|	END_EVALUATE
	;

/**
 * ENTRY statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=358&zoom=auto,-40,735
 */
stmtENTRY : ENTRY alphanumericLiteral usingPhrase?;

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
 * IF statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=370&zoom=auto,-40,735
 */
stmtIF[boolean conditionalAllowed] :
		IF conditionalExpression THEN? (proceduralStatement[true]+ | NEXT SENTENCE) (ELSE (proceduralStatement[true]+ | NEXT SENTENCE))? stmtIFtail[$conditionalAllowed]
	;

stmtIFtail[boolean conditionalAllowed] :
		{$conditionalAllowed}?
	|	END_IF
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
 * INVOKE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=385&zoom=auto,-40,735
 */
stmtINVOKE[boolean conditionalAllowed] :
		INVOKE (identifier | className | SELF | SUPER) (literal | identifier | NEW)
		(USING (BY? VALUE ((LENGTH OF)? identifier | literal)+)+)?
		(RETURNING identifier)?
		stmtINVOKEtail[$conditionalAllowed]
	;

stmtINVOKEtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? exceptionPhrases
	|	exceptionPhrases? END_INVOKE
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
stmtMULTIPLY[boolean conditionalAllowed] :
		MULTIPLY (identifier | literal) BY roundedPhrase+ stmtMULTIPLYtail[$conditionalAllowed]
	|	MULTIPLY (identifier | literal) BY (identifier | literal) givingPhrase stmtMULTIPLYtail[$conditionalAllowed]
	;

stmtMULTIPLYtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? sizeErrorPhrases
	|	sizeErrorPhrases? END_MULTIPLY
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
stmtPERFORM :
		PERFORM procedureName ((THROUGH | THRU) procedureName)? (performTimes | performUntil | performVarying performVaryingAfterPhrase*)?
	|	PERFORM (performTimes | performUntil | performVarying)? proceduralStatement[true]+ END_PERFORM
	;

performTimes : (identifier | INTEGER) TIMES;

performUntil : (WITH? TEST (BEFORE|AFTER))? UNTIL conditionalExpression;

performVarying : (WITH? TEST (BEFORE|AFTER))? VARYING (identifier | indexName) FROM (identifier | indexName | literal) BY (identifier | literal) UNTIL conditionalExpression;

performVaryingAfterPhrase: AFTER (identifier | indexName) FROM (identifier | indexName | literal) BY (identifier | literal) UNTIL conditionalExpression;

/**
 * READ statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=424&zoom=auto,-40,735
 */
stmtSequentialREAD[boolean conditionalAllowed] :
		READ fileName NEXT? RECORD? (INTO identifier)? stmtSequentialREADtail[$conditionalAllowed]
	;

stmtSequentialREADtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? atEndPhrases
	|	atEndPhrases? END_READ
	;

stmtRandomREAD[boolean conditionalAllowed] :
		READ fileName RECORD? (INTO identifier)? (KEY IS? dataName) stmtRandomREADtail[$conditionalAllowed]
	;

stmtRandomREADtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? invalidKeyPhrases
	|	invalidKeyPhrases? END_READ
	;

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
stmtRETURN[boolean conditionalAllowed] :
		RETURN fileName RECORD? (INTO identifier)? stmtRETURNtail[$conditionalAllowed]
	;

stmtRETURNtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? atEndPhrases
	|	atEndPhrases? END_RETURN
	;

/**
 * REWRITE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=435&zoom=auto,-40,735
 */
stmtREWRITE[boolean conditionalAllowed] :
		REWRITE recordName (FROM identifier) stmtREWRITEtail[$conditionalAllowed]
	;

stmtREWRITEtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? invalidKeyPhrases
	|	invalidKeyPhrases? END_REWRITE
	;

/**
 * SEARCH statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=438&zoom=auto,-40,735
 */
stmtSEARCH[boolean conditionalAllowed] :
		SEARCH identifier (VARYING (identifier | indexName)) atEndPhrase? (WHEN conditionalExpression (proceduralStatement[false] | NEXT SENTENCE))+ stmtSEARCHtail[$conditionalAllowed]
	|	SEARCH ALL identifier atEndPhrase? WHEN searchWhenPhrase (AND searchWhenPhrase)* (proceduralStatement[false] | NEXT SENTENCE) stmtSEARCHtail[$conditionalAllowed]
	;

searchWhenPhrase :
		dataName IS? (EQUAL TO? | OP_EQUAL) (identifier | literal | arithmeticExpression)
	|	conditionalExpression
	;

stmtSEARCHtail[boolean conditionalAllowed] :
		{$conditionalAllowed}?
	|	END_SEARCH
	;

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
stmtSTART[boolean conditionalAllowed] :
		START fileName (KEY IS? (EQUAL TO? | OP_EQUAL | GREATER THAN? | OP_GREATER | NOT LESS THAN? | NOT OP_LESS | GREATER THAN? OR EQUAL TO? | OP_NOTLESS) dataName)? stmtSTARTtail[$conditionalAllowed]
	;

stmtSTARTtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? invalidKeyPhrases
	|	invalidKeyPhrases? END_START
	;

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
stmtSTRING[boolean conditionalAllowed] :
		STRING ((identifier | literal)+ DELIMITED BY? (identifier | literal | SIZE))+ INTO identifier (WITH? POINTER identifier)? stmtSTRINGtail[$conditionalAllowed]
	;

stmtSTRINGtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? overflowPhrases
	|	overflowPhrases? END_STRING
	;

/**
 * SUBTRACT statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=471&zoom=auto,-40,735
 */
stmtSUBTRACT[boolean conditionalAllowed] :
		SUBTRACT (identifier | literal)+ FROM roundedPhrase+ stmtSUBTRACTtail[$conditionalAllowed]
	|	SUBTRACT (identifier | literal)+ FROM (identifier | literal) givingPhrase stmtSUBTRACTtail[$conditionalAllowed]
	|	SUBTRACT correspondingPhrase identifier FROM roundedPhrase stmtSUBTRACTtail[$conditionalAllowed]
	;

stmtSUBTRACTtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? sizeErrorPhrases
	|	sizeErrorPhrases? END_SUBTRACT
	;

/**
 * UNSTRING statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=474&zoom=auto,-40,735
 */
stmtUNSTRING[boolean conditionalAllowed] :
		UNSTRING identifier
		(DELIMITED BY? ALL? (identifier | literal) (OR ALL? (identifier | literal))*)?
		INTO (identifier (DELIMITER IN? identifier)? (COUNT IN? identifier))+
		(WITH? POINTER identifier)?
		(TALLYING IN? identifier)?
		stmtUNSTRINGtail[$conditionalAllowed]
	;

stmtUNSTRINGtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? overflowPhrases
	|	overflowPhrases? END_UNSTRING
	;

/**
 * WRITE statement.
 * 
 * Reference manual does not mention 'LINE' (only 'LINES').
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=482&zoom=auto,-40,735
 */
stmtPageWRITE[boolean conditionalAllowed] :
		WRITE recordName (FROM identifier)? ((BEFORE | AFTER) ADVANCING? ((identifier | literal) (LINE | LINES) | mnemonicName | PAGE))? stmtPageWRITEtail[$conditionalAllowed]
	;

stmtPageWRITEtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? atEndOfPagePhrases
	|	atEndOfPagePhrases? END_WRITE
	;

stmtSequentialWRITE[boolean conditionalAllowed] :
		WRITE recordName (FROM identifier)? stmtSequentialWRITEtail[$conditionalAllowed]
	;

stmtSequentialWRITEtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? invalidKeyPhrases
	|	invalidKeyPhrases? END_WRITE
	;

/**
 * XML GENERATE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=490&zoom=auto,-40,735
 */
stmtXMLGENERATE[boolean conditionalAllowed] :
		XML GENERATE identifier FROM identifier
		(COUNT IN? identifier)?
		(WITH? ENCODING (identifier | literal))?
		(WITH? XML_DECLARATION)?
		(WITH? ATTRIBUTES)?
		(NAMESPACE IS? (identifier | literal) (NAMESPACE_PREFIX IS? (identifier | literal))?)?
		(NAME OF? (identifier IS? literal)+)?
		(TYPE OF? (identifier IS? (ATTRIBUTE | ELEMENT | CONTENT))+)?
		(SUPPRESS (identifier xmlGenerateWhenPhrase? | genericSupressionPhrase)+)?
		stmtXMLGENERATEtail[$conditionalAllowed]
	;

xmlGenerateWhenPhrase :
		WHEN (ZERO | ZEROS | ZEROES | SPACE | SPACES | HIGH_VALUE | HIGH_VALUES | LOW_VALUE | LOW_VALUES)
		(OR? (ZERO | ZEROS | ZEROES | SPACE | SPACES | HIGH_VALUE | HIGH_VALUES | LOW_VALUE | LOW_VALUES))*
	;

genericSupressionPhrase : (EVERY ((NUMERIC | NONNUMERIC)? (ATTRIBUTE | CONTENT | ELEMENT) | NUMERIC | NONNUMERIC))? xmlGenerateWhenPhrase;

stmtXMLGENERATEtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? exceptionPhrases
	|	exceptionPhrases? END_XML
	;

/**
 * XML PARSE statement.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=502&zoom=auto,-40,735
 */
stmtXMLPARSE[boolean conditionalAllowed] :
		XML PARSE identifier
		(WITH? ENCODING (identifier | literal))?
		(RETURNING NATIONAL)?
		(VALIDATING WITH? (identifier | FILE xmlSchemaName))?
		PROCESSING PROCEDURE IS? procedureName ((THROUGH | THRU) procedureName)?
		stmtXMLPARSEtail[$conditionalAllowed]
	;

stmtXMLPARSEtail[boolean conditionalAllowed] :
	|	{$conditionalAllowed}? exceptionPhrases
	|	exceptionPhrases? END_XML
	;

