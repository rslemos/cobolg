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

// paragraphs
OBJECT_COMPUTER	: 'OBJECT-COMPUTER';
SPECIAL_NAMES	: 'SPECIAL-NAMES';
FILE_CONTROL	: 'FILE-CONTROL';

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
STATUS			: 'STATUS';
FILLER			: 'FILLER';

// syntatic sugar
IS				: 'IS';
TO				: 'TO';
KEY				: 'KEY';
MODE			: 'MODE';

// statements
DISPLAY			: 'DISPLAY';
STOP			: 'STOP';
RUN				: 'RUN';


