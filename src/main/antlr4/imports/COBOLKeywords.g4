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
lexer grammar COBOLKeywords;
@members {
	public static final int COMPILER_CHANNEL = 65000;
}

PERIOD			: '.';

// headers
DIVISION 		: 'DIVISION';
SECTION			: 'SECTION';

// divisions
IDENTIFICATION	: 'ID' ('ENTIFICATION')?;
ENVIRONMENT		: 'ENVIRONMENT';
DATA			: 'DATA';
PROCEDURE		: 'PROCEDURE';

// sections
CONFIGURATION	: 'CONFIGURATION';
INPUT_OUTPUT	: 'INPUT-OUTPUT';
FILE			: 'FILE';
WORKING_STORAGE : 'WORKING-STORAGE';
LINKAGE			: 'LINKAGE';

// paragraphs
OBJECT_COMPUTER	: 'OBJECT-COMPUTER';
SPECIAL_NAMES	: 'SPECIAL-NAMES';
FILE_CONTROL	: 'FILE-CONTROL';
FD				: 'FD';

// sentences
PROGRAM_ID		: 'PROGRAM-ID';
SELECT			: 'SELECT';
OPTIONAL		: 'OPTIONAL';
ASSIGN			: 'ASSIGN';
ORGANIZATION	: 'ORGANIZATION';
INDEXED			: 'INDEXED';
ACCESS			: 'ACCESS';
SEQUENTIAL		: 'SEQUENTIAL';
RECORD			: 'RECORD';
RECORDS			: 'RECORDS';
STATUS			: 'STATUS';
BLOCK			: 'BLOCK';
EXTERNAL		: 'EXTERNAL';
GLOBAL			: 'GLOBAL';
CHARACTERS		: 'CHARACTERS';
VARYING			: 'VARYING';
DEPENDING		: 'DEPENDING';
LABEL			: 'LABEL';
STANDARD		: 'STANDARD';
OMITTED			: 'OMITTED';
OF				: 'OF';
LINAGE			: 'LINAGE';
FOOTING			: 'FOOTING';
TOP				: 'TOP';
BOTTOM			: 'BOTTOM';
CODE_SET		: 'CODE-SET';
FILLER			: 'FILLER';
USAGE			: 'USAGE';
VALUE			: 'VALUE';
OCCURS			: 'OCCURS';
TIMES			: 'TIMES';
REDEFINES		: 'REDEFINES';
USING			: 'USING';
REFERENCE		: 'REFERENCE';

// syntatic sugar
BY				: 'BY';
IS				: 'IS';
ARE				: 'ARE';
FROM			: 'FROM';
TO				: 'TO';
IN				: 'IN';
ON				: 'ON';
SIZE			: 'SIZE';
KEY				: 'KEY';
MODE			: 'MODE';
NATIVE			: 'NATIVE';
CONTAINS		: 'CONTAINS';
WITH			: 'WITH';
LINES			: 'LINES';
AT				: 'AT';

// usages
BINARY			: 'BINARY';
COMPUTATIONAL	: 'COMPUTATIONAL' | 'COMP';
COMPUTATIONAL_1	: 'COMPUTATIONAL-1' | 'COMP-1';
COMPUTATIONAL_2	: 'COMPUTATIONAL-2' | 'COMP-2';
COMPUTATIONAL_3	: 'COMPUTATIONAL-3' | 'COMP-3';
COMPUTATIONAL_4	: 'COMPUTATIONAL-4' | 'COMP-4';
COMPUTATIONAL_5	: 'COMPUTATIONAL-5' | 'COMP-5';
DISPLAY			: 'DISPLAY';
DISPLAY_1		: 'DISPLAY-1';
INDEX			: 'INDEX';
NATIONAL		: 'NATIONAL';
// OBJECT		: 'OBJECT';
// REFERENCE	: 'REFERENCE';
PACKED_DECIMAL	: 'PACKED-DECIMAL';
POINTER			: 'POINTER';
PROCEDURE_POINTER : 'PROCEDURE-POINTER';
FUNCTION_POINTER  : 'FUNCTION-POINTER';

// figurative constants
ZERO			: 'ZEROES' | 'ZEROS' | 'ZERO';
SPACE			: 'SPACES' | 'SPACE';
HIGH_VALUE		: 'HIGH-VALUES' | 'HIGH-VALUE';
LOW_VALUE		: 'LOW-VALUES' | 'LOW-VALUE';
QUOTE			: 'QUOTES' | 'QUOTE';
ALL				: 'ALL';
NULL			: 'NULLS' | 'NULL';

// statements
// DISPLAY			: 'DISPLAY';  (also a usage mode)
STOP			: 'STOP';
RUN				: 'RUN';


