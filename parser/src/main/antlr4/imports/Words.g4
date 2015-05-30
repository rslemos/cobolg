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
lexer grammar Words;

PERIOD                : '.';

/* categories given in http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=34&zoom=auto,-100,275 */

/* KEYWORDS */

ACCESS                : 'ACCESS';
ARE                   : 'ARE';
ASSIGN                : 'ASSIGN';
AT                    : 'AT';
BINARY                : 'BINARY';
BLOCK                 : 'BLOCK';
BOTTOM                : 'BOTTOM';
BY                    : 'BY';
CHARACTERS            : 'CHARACTERS';
CODE_SET              : 'CODE-SET';
COMPUTATIONAL         : 'COMPUTATIONAL' | 'COMP';
COMPUTATIONAL_1       : 'COMPUTATIONAL-1' | 'COMP-1';
COMPUTATIONAL_2       : 'COMPUTATIONAL-2' | 'COMP-2';
COMPUTATIONAL_3       : 'COMPUTATIONAL-3' | 'COMP-3';
COMPUTATIONAL_4       : 'COMPUTATIONAL-4' | 'COMP-4';
COMPUTATIONAL_5       : 'COMPUTATIONAL-5' | 'COMP-5';
CONFIGURATION         : 'CONFIGURATION';
CONTAINS              : 'CONTAINS';
DATA                  : 'DATA';
DEPENDING             : 'DEPENDING';
DISPLAY               : 'DISPLAY';
DISPLAY_1             : 'DISPLAY-1';
DIVISION              : 'DIVISION';
ENVIRONMENT           : 'ENVIRONMENT';
EXTERNAL              : 'EXTERNAL';
FD                    : 'FD';
FILE                  : 'FILE';
FILE_CONTROL          : 'FILE-CONTROL';
FILLER                : 'FILLER';
FOOTING               : 'FOOTING';
FROM                  : 'FROM';
FUNCTION_POINTER      : 'FUNCTION-POINTER';
GLOBAL                : 'GLOBAL';
IDENTIFICATION        : 'ID' ('ENTIFICATION')?;
IN                    : 'IN';
INDEX                 : 'INDEX';
INDEXED               : 'INDEXED';
INPUT_OUTPUT          : 'INPUT-OUTPUT';
IS                    : 'IS';
KEY                   : 'KEY';
LABEL                 : 'LABEL';
LINAGE                : 'LINAGE';
LINES                 : 'LINES';
LINKAGE               : 'LINKAGE';
MODE                  : 'MODE';
NATIONAL              : 'NATIONAL';
NATIVE                : 'NATIVE';
// OBJECT                : 'OBJECT';
OBJECT_COMPUTER       : 'OBJECT-COMPUTER';
OCCURS                : 'OCCURS';
OF                    : 'OF';
OMITTED               : 'OMITTED';
ON                    : 'ON';
OPTIONAL              : 'OPTIONAL';
ORGANIZATION          : 'ORGANIZATION';
PACKED_DECIMAL        : 'PACKED-DECIMAL';
POINTER               : 'POINTER';
PROCEDURE             : 'PROCEDURE';
PROCEDURE_POINTER     : 'PROCEDURE-POINTER';
PROGRAM_ID            : 'PROGRAM-ID';
RECORD                : 'RECORD';
RECORDS               : 'RECORDS';
REDEFINES             : 'REDEFINES';
REFERENCE             : 'REFERENCE';
RUN                   : 'RUN';
SECTION               : 'SECTION';
SELECT                : 'SELECT';
SEQUENTIAL            : 'SEQUENTIAL';
SIZE                  : 'SIZE';
SPECIAL_NAMES         : 'SPECIAL-NAMES';
STANDARD              : 'STANDARD';
STATUS                : 'STATUS';
STOP                  : 'STOP';
TIMES                 : 'TIMES';
TO                    : 'TO';
TOP                   : 'TOP';
USAGE                 : 'USAGE';
USING                 : 'USING';
VALUE                 : 'VALUE';
VARYING               : 'VARYING';
WITH                  : 'WITH';
WORKING_STORAGE       : 'WORKING-STORAGE';

/* FIGURATIVE CONSTANTS */

ALL                   : 'ALL';
HIGH_VALUE            : 'HIGH-VALUES' | 'HIGH-VALUE';
LOW_VALUE             : 'LOW-VALUES' | 'LOW-VALUE';
NULL                  : 'NULLS' | 'NULL';
QUOTE                 : 'QUOTES' | 'QUOTE';
SPACE                 : 'SPACES' | 'SPACE';
ZERO                  : 'ZEROES' | 'ZEROS' | 'ZERO';

/* COMPILER DIRECTING STATEMENTS */
/* http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=561&zoom=auto,,770 */
COPY                  : 'COPY';
EJECT                 : 'EJECT';

// not exactly a keyword
ID	:
		[A-Za-z0-9]+
	|	[A-Za-z0-9][-A-Za-z0-9]*[A-Za-z0-9]
	;
