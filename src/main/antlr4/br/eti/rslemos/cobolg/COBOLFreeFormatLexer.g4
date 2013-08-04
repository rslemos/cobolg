/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2013  Rodrigo Lemos
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
lexer grammar COBOLFreeFormatLexer;

COMMENT			: ('*' | '/') ~('\n' | '\r')* '\r'? '\n'? 
				{ _tokenStartCharPositionInLine == 0 }?	-> channel(HIDDEN);

PERIOD			: '.';

DIVISION 		: 'DIVISION';
SECTION			: 'SECTION';

IDENTIFICATION	: 'ID' ('ENTIFICATION')?;
PROCEDURE		: 'PROCEDURE';

PROGRAM_ID		: 'PROGRAM-ID';

DISPLAY			: 'DISPLAY';
STOP			: 'STOP';
RUN				: 'RUN';

WS	: [ \n\r]											-> channel(HIDDEN);

ID	:
		[A-Za-z0-9]+
	|	[A-Za-z0-9][-A-Za-z0-9]*[A-Za-z0-9]
	;

INTEGER : '-'? [0-9]+
	;

FIXEDPOINT : [0-9]+ '.' [0-9]+
	;

HEXINTEGER :
		'H' '"' [0-9A-F]+ '"'
	|	'H' '\'' [0-9A-F]+ '\''
	;

QUOTEDSTRING :
		'"' .*? '"'			// TODO: ""
	|	'\'' .*? '\''		// TODO: ''
	;

HEXSTRING :
		'X' '"' ([0-9A-F][0-9A-F])+ '"'
	|	'X' '\'' ([0-9A-F][0-9A-F])+ '\''
	;
