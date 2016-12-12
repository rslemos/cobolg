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
ALSO                  : 'ALSO';
ALTERNATE             : 'ALTERNATE';
AND                   : 'AND';
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
CONFIGURATION         : 'CONFIGURATION';
CONTAINS              : 'CONTAINS';
CORR                  : 'CORR';
CORRESPONDING         : 'CORRESPONDING';
CURRENCY              : 'CURRENCY';
DATA                  : 'DATA';
DATE_COMPILED         : 'DATE-COMPILED';
DATE_WRITTEN          : 'DATE-WRITTEN';
DEBUGGING             : 'DEBUGGING';
DECIMAL_POINT         : 'DECIMAL-POINT';
DELETE                : 'DELETE';
DELIMITER             : 'DELIMITER';
DEPENDING             : 'DEPENDING';
DESCENDING            : 'DESCENDING';
DISPLAY               : 'DISPLAY';
DISPLAY_1             : 'DISPLAY-1';
DIVISION              : 'DIVISION';
DUPLICATES            : 'DUPLICATES';
DYNAMIC               : 'DYNAMIC';
END                   : 'END';
ENVIRONMENT           : 'ENVIRONMENT';
EQUAL                 : 'EQUAL';
EVERY                 : 'EVERY';
EXTEND                : 'EXTEND';
EXTERNAL              : 'EXTERNAL';
FD                    : 'FD';
FILE                  : 'FILE';
FILE_CONTROL          : 'FILE-CONTROL';
FILLER                : 'FILLER';
FOOTING               : 'FOOTING';
FOR                   : 'FOR';
FROM                  : 'FROM';
FUNCTION_POINTER      : 'FUNCTION-POINTER';
GIVING                : 'GIVING';
GLOBAL                : 'GLOBAL';
GREATER               : 'GREATER';
GROUP_USAGE           : 'GROUP-USAGE';
I_O                   : 'I-O';
I_O_CONTROL           : 'I-O-CONTROL';
ID                    : 'ID';
IDENTIFICATION        : 'IDENTIFICATION';
IN                    : 'IN';
INDEX                 : 'INDEX';
INDEXED               : 'INDEXED';
INITIAL               : 'INITIAL';
INPUT                 : 'INPUT';
INPUT_OUTPUT          : 'INPUT-OUTPUT';
INSTALLATION          : 'INSTALLATION';
INTO                  : 'INTO';
IS                    : 'IS';
JUST                  : 'JUST';
JUSTIFIED             : 'JUSTIFIED';
KEY                   : 'KEY';
LABEL                 : 'LABEL';
LEADING               : 'LEADING';
LEFT                  : 'LEFT';
LESS                  : 'LESS';
LINAGE                : 'LINAGE';
LINE                  : 'LINE';
LINES                 : 'LINES';
LINKAGE               : 'LINKAGE';
LOCAL_STORAGE         : 'LOCAL-STORAGE';
LOCK                  : 'LOCK';
MEMORY                : 'MEMORY';
MODE                  : 'MODE';
MODULES               : 'MODULES';
MULTIPLE              : 'MULTIPLE';
NATIONAL              : 'NATIONAL';
NATIVE                : 'NATIVE';
NEGATIVE              : 'NEGATIVE';
NEXT                  : 'NEXT';
NO                    : 'NO';
NOT                   : 'NOT';
NUMERIC               : 'NUMERIC';
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
ORGANIZATION          : 'ORGANIZATION';
OUTPUT                : 'OUTPUT';
PACKED_DECIMAL        : 'PACKED-DECIMAL';
PADDING               : 'PADDING';
PAGE                  : 'PAGE';
PASSWORD              : 'PASSWORD';
PIC                   : 'PIC';     // will be overridden on the main lexer
PICTURE               : 'PICTURE'; // will be overridden on the main lexer
POINTER               : 'POINTER';
POSITION              : 'POSITION';
POSITIVE              : 'POSITIVE';
PROCEDURE             : 'PROCEDURE';
PROCEDURE_POINTER     : 'PROCEDURE-POINTER';
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
REMOVAL               : 'REMOVAL';
RENAMES               : 'RENAMES';
REPOSITORY            : 'REPOSITORY';
RERUN                 : 'RERUN';
RESERVE               : 'RESERVE';
RETURNING             : 'RETURNING';
REVERSED              : 'REVERSED';
REWIND                : 'REWIND';
REWRITE               : 'REWRITE';
RIGHT                 : 'RIGHT';
ROUNDED               : 'ROUNDED';
RUN                   : 'RUN';
SAME                  : 'SAME';
SD                    : 'SD';
SECTION               : 'SECTION';
SECURITY              : 'SECURITY';
SEGMENT_LIMIT         : 'SEGMENT-LIMIT';
SELECT                : 'SELECT';
SEPARATE              : 'SEPARATE';
SEQUENCE              : 'SEQUENCE';
SEQUENTIAL            : 'SEQUENTIAL';
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
SYMBOLIC              : 'SYMBOLIC';
SYNC                  : 'SYNC';
SYNCHRONIZED          : 'SYNCHRONIZED';
TAPE                  : 'TAPE';
THAN                  : 'THAN';
THROUGH               : 'THROUGH';
THRU                  : 'THRU';
TIMES                 : 'TIMES';
TO                    : 'TO';
TOP                   : 'TOP';
TRAILING              : 'TRAILING';
UNBOUNDED             : 'UNBOUNDED';
UNIT                  : 'UNIT';
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

EBCDIC                : 'EBCDIC';
//SYMBOL comes after PICTURE, so it should be treated specially inside PICTURE_MODE on main lexer
//SYMBOL                : 'SYMBOL';

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

