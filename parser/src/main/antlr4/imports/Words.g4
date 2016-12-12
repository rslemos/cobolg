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
/**
 * This grammar is based on Enterprise COBOL for z/OS Language Reference Version 5.2
 * (SC14-7381-03).
 * 
 * This COBOLKeywords lexer lists all available reserved words for COBOL. It
 * includes the potential reserved words.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=621
 */
lexer grammar Words;

PERIOD                : '.';

/* categories given in http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=34&zoom=auto,-100,275 */

/* KEYWORDS */

ACCEPT                : 'ACCEPT';
ACCESS                : 'ACCESS';
ADD                   : 'ADD';
ADDRESS               : 'ADDRESS';
ADVANCING             : 'ADVANCING';
AFTER                 : 'AFTER';
ALL                   : 'ALL';
ALPHABET              : 'ALPHABET';
ALPHABETIC            : 'ALPHABETIC';
ALPHABETIC_LOWER      : 'ALPHABETIC-LOWER';
ALPHABETIC_UPPER      : 'ALPHABETIC-UPPER';
ALPHANUMERIC          : 'ALPHANUMERIC';
ALPHANUMERIC_EDITED   : 'ALPHANUMERIC-EDITED';
ALSO                  : 'ALSO';
ALTER                 : 'ALTER';
ALTERNATE             : 'ALTERNATE';
AND                   : 'AND';
ANY                   : 'ANY';
APPLY                 : 'APPLY';
ARE                   : 'ARE';
AREA                  : 'AREA';
AREAS                 : 'AREAS';
ASCENDING             : 'ASCENDING';
ASSIGN                : 'ASSIGN';
AT                    : 'AT';
AUTHOR                : 'AUTHOR';
BEFORE                : 'BEFORE';
BINARY                : 'BINARY';
BLANK                 : 'BLANK';
BLOCK                 : 'BLOCK';
BOTTOM                : 'BOTTOM';
BY                    : 'BY';
CALL                  : 'CALL';
CANCEL                : 'CANCEL';
CHARACTER             : 'CHARACTER';
CHARACTERS            : 'CHARACTERS';
CLASS                 : 'CLASS';
CLOSE                 : 'CLOSE';
CODE_SET              : 'CODE-SET';
COLLATING             : 'COLLATING';
COMMA                 : 'COMMA';
COMP                  : 'COMP';
COMP_1                : 'COMP-1';
COMP_2                : 'COMP-2';
COMP_3                : 'COMP-3';
COMP_4                : 'COMP-4';
COMP_5                : 'COMP-5';
COMPUTATIONAL         : 'COMPUTATIONAL';
COMPUTATIONAL_1       : 'COMPUTATIONAL-1';
COMPUTATIONAL_2       : 'COMPUTATIONAL-2';
COMPUTATIONAL_3       : 'COMPUTATIONAL-3';
COMPUTATIONAL_4       : 'COMPUTATIONAL-4';
COMPUTATIONAL_5       : 'COMPUTATIONAL-5';
COMPUTE               : 'COMPUTE';
CONFIGURATION         : 'CONFIGURATION';
CONTAINS              : 'CONTAINS';
CONTENT               : 'CONTENT';
CONTINUE              : 'CONTINUE';
CONVERTING            : 'CONVERTING';
CORR                  : 'CORR';
CORRESPONDING         : 'CORRESPONDING';
COUNT                 : 'COUNT';
CURRENCY              : 'CURRENCY';
DATA                  : 'DATA';
DATE                  : 'DATE';
DATE_COMPILED         : 'DATE-COMPILED';
DATE_WRITTEN          : 'DATE-WRITTEN';
DAY                   : 'DAY';
DAY_OF_WEEK           : 'DAY-OF-WEEK';
DBCS                  : 'DBCS';
DEBUGGING             : 'DEBUGGING';
DECIMAL_POINT         : 'DECIMAL-POINT';
DELETE                : 'DELETE';
DELIMITED             : 'DELIMITED';
DELIMITER             : 'DELIMITER';
DEPENDING             : 'DEPENDING';
DESCENDING            : 'DESCENDING';
DISPLAY               : 'DISPLAY';
DISPLAY_1             : 'DISPLAY-1';
DIVIDE                : 'DIVIDE';
DIVISION              : 'DIVISION';
DOWN                  : 'DOWN';
DUPLICATES            : 'DUPLICATES';
DYNAMIC               : 'DYNAMIC';
EGCS                  : 'EGCS';
ELSE                  : 'ELSE';
END                   : 'END';
END_ADD               : 'END-ADD';
END_CALL              : 'END-CALL';
END_COMPUTE           : 'END-COMPUTE';
END_DELETE            : 'END-DELETE';
END_DIVIDE            : 'END-DIVIDE';
END_EVALUATE          : 'END-EVALUATE';
END_IF                : 'END-IF';
END_INVOKE            : 'END-INVOKE';
END_MULTIPLY          : 'END-MULTIPLY';
END_OF_PAGE           : 'END-OF-PAGE';
END_PERFORM           : 'END-PERFORM';
END_READ              : 'END-READ';
END_REWRITE           : 'END-REWRITE';
END_START             : 'END-START';
END_STRING            : 'END-STRING';
END_SUBTRACT          : 'END-SUBTRACT';
END_UNSTRING          : 'END-UNSTRING';
END_WRITE             : 'END-WRITE';
END_XML               : 'END-XML';
ENTRY                 : 'ENTRY';
ENVIRONMENT           : 'ENVIRONMENT';
EOP                   : 'EOP';
EQUAL                 : 'EQUAL';
ERROR                 : 'ERROR';
EVALUATE              : 'EVALUATE';
EVERY                 : 'EVERY';
EXCEPTION             : 'EXCEPTION';
EXIT                  : 'EXIT';
EXTEND                : 'EXTEND';
EXTERNAL              : 'EXTERNAL';
FALSE                 : 'FALSE';
FD                    : 'FD';
FILE                  : 'FILE';
FILE_CONTROL          : 'FILE-CONTROL';
FILLER                : 'FILLER';
FIRST                 : 'FIRST';
FOOTING               : 'FOOTING';
FOR                   : 'FOR';
FROM                  : 'FROM';
FUNCTION_POINTER      : 'FUNCTION-POINTER';
GENERATE              : 'GENERATE';
GIVING                : 'GIVING';
GLOBAL                : 'GLOBAL';
GO                    : 'GO';
GOBACK                : 'GOBACK';
GREATER               : 'GREATER';
GROUP_USAGE           : 'GROUP-USAGE';
I_O                   : 'I-O';
I_O_CONTROL           : 'I-O-CONTROL';
ID                    : 'ID';
IDENTIFICATION        : 'IDENTIFICATION';
IF                    : 'IF';
IN                    : 'IN';
INDEX                 : 'INDEX';
INDEXED               : 'INDEXED';
INITIAL               : 'INITIAL';
INITIALIZE            : 'INITIALIZE';
INPUT                 : 'INPUT';
INPUT_OUTPUT          : 'INPUT-OUTPUT';
INSPECT               : 'INSPECT';
INSTALLATION          : 'INSTALLATION';
INTO                  : 'INTO';
INVALID               : 'INVALID';
INVOKE                : 'INVOKE';
IS                    : 'IS';
JUST                  : 'JUST';
JUSTIFIED             : 'JUSTIFIED';
KEY                   : 'KEY';
LABEL                 : 'LABEL';
LEADING               : 'LEADING';
LEFT                  : 'LEFT';
LENGTH                : 'LENGTH';
LESS                  : 'LESS';
LINAGE                : 'LINAGE';
LINE                  : 'LINE';
LINES                 : 'LINES';
LINKAGE               : 'LINKAGE';
LOCAL_STORAGE         : 'LOCAL-STORAGE';
LOCK                  : 'LOCK';
MEMORY                : 'MEMORY';
MERGE                 : 'MERGE';
METHOD                : 'METHOD';
MODE                  : 'MODE';
MODULES               : 'MODULES';
MOVE                  : 'MOVE';
MULTIPLE              : 'MULTIPLE';
MULTIPLY              : 'MULTIPLY';
NATIONAL              : 'NATIONAL';
NATIONAL_EDITED       : 'NATIONAL-EDITED';
NATIVE                : 'NATIVE';
NEGATIVE              : 'NEGATIVE';
NEXT                  : 'NEXT';
NO                    : 'NO';
NOT                   : 'NOT';
NUMERIC               : 'NUMERIC';
NUMERIC_EDITED        : 'NUMERIC-EDITED';
OBJECT                : 'OBJECT';
OBJECT_COMPUTER       : 'OBJECT-COMPUTER';
OCCURS                : 'OCCURS';
OF                    : 'OF';
OFF                   : 'OFF';
OMITTED               : 'OMITTED';
ON                    : 'ON';
OPEN                  : 'OPEN';
OPTIONAL              : 'OPTIONAL';
OR                    : 'OR';
ORDER                 : 'ORDER';
ORGANIZATION          : 'ORGANIZATION';
OTHER                 : 'OTHER';
OUTPUT                : 'OUTPUT';
OVERFLOW              : 'OVERFLOW';
PACKED_DECIMAL        : 'PACKED-DECIMAL';
PADDING               : 'PADDING';
PAGE                  : 'PAGE';
PASSWORD              : 'PASSWORD';
PERFORM               : 'PERFORM';
PIC                   : 'PIC';     // will be overridden on the main lexer
PICTURE               : 'PICTURE'; // will be overridden on the main lexer
POINTER               : 'POINTER';
POSITION              : 'POSITION';
POSITIVE              : 'POSITIVE';
PROCEDURE             : 'PROCEDURE';
PROCEDURE_POINTER     : 'PROCEDURE-POINTER';
PROCEED               : 'PROCEED';
PROCESSING            : 'PROCESSING';
PROGRAM               : 'PROGRAM';
PROGRAM_ID            : 'PROGRAM-ID';
RANDOM                : 'RANDOM';
READ                  : 'READ';
RECORD                : 'RECORD';
RECORDS               : 'RECORDS';
RECURSIVE             : 'RECURSIVE';
REDEFINES             : 'REDEFINES';
REEL                  : 'REEL';
REFERENCE             : 'REFERENCE';
RELATIVE              : 'RELATIVE';
RELEASE               : 'RELEASE';
REMAINDER             : 'REMAINDER';
REMOVAL               : 'REMOVAL';
RENAMES               : 'RENAMES';
REPLACING             : 'REPLACING';
REPOSITORY            : 'REPOSITORY';
RERUN                 : 'RERUN';
RESERVE               : 'RESERVE';
RETURN                : 'RETURN';
RETURNING             : 'RETURNING';
REVERSED              : 'REVERSED';
REWIND                : 'REWIND';
REWRITE               : 'REWRITE';
RIGHT                 : 'RIGHT';
ROUNDED               : 'ROUNDED';
RUN                   : 'RUN';
SAME                  : 'SAME';
SD                    : 'SD';
SEARCH                : 'SEARCH';
SECTION               : 'SECTION';
SECURITY              : 'SECURITY';
SEGMENT_LIMIT         : 'SEGMENT-LIMIT';
SELECT                : 'SELECT';
SENTENCE              : 'SENTENCE';
SEPARATE              : 'SEPARATE';
SEQUENCE              : 'SEQUENCE';
SEQUENTIAL            : 'SEQUENTIAL';
SET                   : 'SET';
SIGN                  : 'SIGN';
SIZE                  : 'SIZE';
SORT                  : 'SORT';
SORT_MERGE            : 'SORT-MERGE';
SOURCE_COMPUTER       : 'SOURCE-COMPUTER';
SPECIAL_NAMES         : 'SPECIAL-NAMES';
STANDARD              : 'STANDARD';
STANDARD_1            : 'STANDARD-1';
STANDARD_2            : 'STANDARD-2';
START                 : 'START';
STATUS                : 'STATUS';
STOP                  : 'STOP';
STRING                : 'STRING';
SUBTRACT              : 'SUBTRACT';
SUPPRESS              : 'SUPPRESS';
SYMBOLIC              : 'SYMBOLIC';
SYNC                  : 'SYNC';
SYNCHRONIZED          : 'SYNCHRONIZED';
TALLYING              : 'TALLYING';
TAPE                  : 'TAPE';
TEST                  : 'TEST';
THAN                  : 'THAN';
THEN                  : 'THEN';
THROUGH               : 'THROUGH';
THRU                  : 'THRU';
TIME                  : 'TIME';
TIMES                 : 'TIMES';
TO                    : 'TO';
TOP                   : 'TOP';
TRAILING              : 'TRAILING';
TRUE                  : 'TRUE';
TYPE                  : 'TYPE';
UNBOUNDED             : 'UNBOUNDED';
UNIT                  : 'UNIT';
UNSTRING              : 'UNSTRING';
UNTIL                 : 'UNTIL';
UP                    : 'UP';
UPON                  : 'UPON';
USAGE                 : 'USAGE';
USING                 : 'USING';
VALUE                 : 'VALUE';
VALUES                : 'VALUES';
VARYING               : 'VARYING';
VOLATILE              : 'VOLATILE';
WHEN                  : 'WHEN';
WITH                  : 'WITH';
WORDS                 : 'WORDS';
WORKING_STORAGE       : 'WORKING-STORAGE';
WRITE                 : 'WRITE';
WRITE_ONLY            : 'WRITE-ONLY';
XML                   : 'XML';
XML_SCHEMA            : 'XML-SCHEMA';

/* FIGURATIVE CONSTANTS */
/* http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=35&zoom=auto,-100,160 */

// ALL literal        : 'ALL' + ...;
HIGH_VALUE            : 'HIGH-VALUE';
HIGH_VALUES           : 'HIGH-VALUES';
LOW_VALUE             : 'LOW-VALUE';
LOW_VALUES            : 'LOW-VALUES';
NULL                  : 'NULL';
NULLS                 : 'NULLS';
QUOTE                 : 'QUOTE';
QUOTES                : 'QUOTES';
SPACE                 : 'SPACE';
SPACES                : 'SPACES';
ZERO                  : 'ZERO';
ZEROES                : 'ZEROES';
ZEROS                 : 'ZEROS';

/* SPECIAL CHARACTER WORDS */

/* arithmetic operators */
/* http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=279&zoom=auto,-100,185 */
OP_PLUS               : '+';  // unary plus or addition
OP_MINUS              : '-';  // unary minus or subtraction
OP_STAR               : '*';  // multiplication
OP_SLASH              : '/';  // division
OP_STARSTAR           : '**'; // exponentiation

/* relational operators */
/* http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=282&zoom=auto,-100,730 */
OP_GREATER            : '>';  // greater than
OP_LESS               : '<';  // less than
OP_EQUAL              : '=';  // equal and assignment operator in COMPUTE
OP_NOTLESS            : '>='; // greater than or equal
OP_NOTGREATER         : '<='; // less than or equal

/* SPECIAL OBJECT IDENTIFIERS */

SELF                  : 'SELF';
SUPER                 : 'SUPER';

/* SPECIAL REGISTERS */
/* http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=38&zoom=auto,-100,610 */

// ADDRESS_OF         : 'ADDRESS OF';
DEBUG_CONTENTS        : 'DEBUG-CONTENTS';
DEBUG_ITEM            : 'DEBUG-ITEM';
DEBUG_LINE            : 'DEBUG-LINE';
DEBUG_NAME            : 'DEBUG-NAME';
DEBUG_SUB_1           : 'DEBUG-SUB-1';
DEBUG_SUB_2           : 'DEBUG-SUB-2';
DEBUG_SUB_3           : 'DEBUG-SUB-3';
JNIENVPTR             : 'JNIENVPTR';
// LENGTH_OF          : 'LENGTH OF';
LINAGE_COUNTER        : 'LINAGE-COUNTER';
RETURN_CODE           : 'RETURN-CODE';
SHIFT_IN              : 'SHIFT-IN';
SHIFT_OUT             : 'SHIFT-OUT';
SORT_CONTROL          : 'SORT-CONTROL';
SORT_CORE_SIZE        : 'SORT-CORE-SIZE';
SORT_FILE_SIZE        : 'SORT-FILE-SIZE';
SORT_MESSAGE          : 'SORT-MESSAGE';
SORT_MODE_SIZE        : 'SORT-MODE-SIZE';
SORT_RETURN           : 'SORT-RETURN';
TALLY                 : 'TALLY';
WHEN_COMPILED         : 'WHEN-COMPILED';
XML_CODE              : 'XML-CODE';
XML_EVENT             : 'XML-EVENT';
XML_INFORMATION       : 'XML-INFORMATION';
XML_NAMESPACE         : 'XML-NAMESPACE';
XML_NAMESPACE_PREFIX  : 'XML-NAMESPACE-PREFIX';
XML_NNAMESPACE        : 'XML-NNAMESPACE';
XML_NNAMESPACE_PREFIX : 'XML-NNAMESPACE-PREFIX';
XML_NTEXT             : 'XML-NTEXT';
XML_TEXT              : 'XML-TEXT';

/* the following keywords are missing from reference */
// perhaps they are not reserved words; maybe they are allowed as identifiers
// for backward compatibility
ATTRIBUTE             : 'ATTRIBUTE';
ATTRIBUTES            : 'ATTRIBUTES';
CYCLE                 : 'CYCLE';
EBCDIC                : 'EBCDIC';
ELEMENT               : 'ELEMENT';
ENCODING              : 'ENCODING';
NAME                  : 'NAME';
NAMESPACE             : 'NAMESPACE';
NAMESPACE_PREFIX      : 'NAMESPACE-PREFIX';
NEW                   : 'NEW';
NONNUMERIC            : 'NONNUMERIC';
PARAGRAPH             : 'PARAGRAPH';
PARSE                 : 'PARSE';
//SYMBOL comes after PICTURE, so it should be treated specially inside PICTURE_MODE on main lexer
//SYMBOL                : 'SYMBOL';
VALIDATING            : 'VALIDATING';
XML_DECLARATION       : 'XML-DECLARATION';
YYYYDDD               : 'YYYYDDD';
YYYYMMDD              : 'YYYYMMDD';

/* COMPILER DIRECTING STATEMENTS */
/* http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=561&zoom=auto,,770 */
COPY                  : 'COPY';
EJECT                 : 'EJECT';

// not exactly a keyword
/**
 * User defined words.
 * 
 * - Latin uppercase letters A through Z (A-Z)
 * - Latin lowercase letters a through z (a-z)
 * - digits 0 through 9 (0-9)
 * - hyphen (-)
 * - underscore (_)
 * - the hyphen cannot appear as the first or last character
 * - the underscore cannot appear as the first character
 * - must contain at least one alphabetic character
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=31&zoom=auto,-100,370
 */
USERDEFINEDWORD	:
		([A-Za-z0-9][-_A-Za-z0-9]*)? [A-Za-z] ([-_A-Za-z0-9]*[_A-Za-z0-9])?
	;

