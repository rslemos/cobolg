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
lexer grammar COBOLBasics;
import COBOLKeywords;

WS : ' '+
	-> channel(HIDDEN);

NEWLINE : ('\n' '\r'? | '\r' '\n'?)
	-> channel(HIDDEN);

INTEGER : '-'? [0-9]+
	;

FIXEDPOINT : [0-9]+ '.' [0-9]+
	;

ID	:
		[A-Za-z]
	|	[A-Za-z][-A-Za-z0-9]*[A-Za-z0-9]
	|	[A-Za-z0-9]*[A-Za-z][-A-Za-z0-9]*[A-Za-z0-9]
	|	[A-Za-z0-9][-A-Za-z0-9]*[A-Za-z]
	;

HEXINTEGER :
		'H' ["] [0-9A-F]+ ["]
	|	'H' ['] [0-9A-F]+ [']
	;

DOUBLEQUOTEDSTRING : ["] ( ~["\n\r] | ["] ["] )* ["]
	;

SINGLEQUOTEDSTRING : ['] ( ~['\n\r] | ['] ['] )* [']
	;

HEXSTRING :
		'X' ["] ([0-9A-F][0-9A-F])+ ["]
	|	'X' ['] ([0-9A-F][0-9A-F])+ [']
	;

fragment PICTURECHAR : [9X]
	;

fragment PICTURESTRING :
		PICTURECHAR+ '(' [0-9]+ ')'
	;

PICTURECLAUSE : PICTURE WS (IS WS)? PICTURESTRING
	;
