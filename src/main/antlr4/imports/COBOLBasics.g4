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

fragment PICTURE_REPETITION :
		'(' [0-9]+ ')'
	;

// Disp-Num: 999, 9(2), 9(3)9(4), S99, 99V99, S999(3)V9(3), SV999, V99
// Disp-Num: 99V, S9(10)V, S9(1)9(2)99(3)V (less used)
fragment PICTURE_DISPNUM : 
		'S'? PICTURE_DISPNUM_P* PICTURE_DISPNUM_9V9 
	|	'S'?                    PICTURE_DISPNUM_9V9 PICTURE_DISPNUM_P* 
	;

fragment PICTURE_DISPNUM_9V9 :
		PICTURE_DISPNUM_9* 'V'? PICTURE_DISPNUM_9+
	|	PICTURE_DISPNUM_9+ 'V'
	;

fragment PICTURE_DISPNUM_9 :
		'9' PICTURE_REPETITION?
	;

fragment PICTURE_DISPNUM_P :
		'P' PICTURE_REPETITION?
	;


// Display: X, XX, A, AA, XA, AX, X(02), A(03), X(10)A(10), X9, A9, 9(19)X
fragment PICTURE_DISPLAY :
		([9AX] PICTURE_REPETITION?)+
	;

// Disp-Num-Edit:
fragment PICTURE_DISPNUMEDIT_POINT :
		('+' | '-') '$'?? (PICTURE_DISPNUMEDIT_POINT_FLOATINGINSERTION | PICTURE_DISPNUMEDIT_POINT_ZEROSUPPRESSION)
	|	            '$'?? (PICTURE_DISPNUMEDIT_POINT_FLOATINGINSERTION | PICTURE_DISPNUMEDIT_POINT_ZEROSUPPRESSION) ('+' | '-' | 'CR' | 'DB')?
	;

fragment POINT_FLOATING_INSERTION :
		'$'                    ([$B0/,] PICTURE_REPETITION?)+
	|	'$' PICTURE_REPETITION ([$B0/,] PICTURE_REPETITION?)*
	|	'+'                    ([+B0/,] PICTURE_REPETITION?)+
	|	'+' PICTURE_REPETITION ([+B0/,] PICTURE_REPETITION?)*
	|	'-'                    ([-B0/,] PICTURE_REPETITION?)+
	|	'-' PICTURE_REPETITION ([-B0/,] PICTURE_REPETITION?)*
	;

fragment PICTURE_DISPNUMEDIT_POINT_FLOATINGINSERTION :
		POINT_FLOATING_INSERTION? PICTURE_DISPNUMEDIT_POINT_FIXEDINSERTION
	;

fragment POINT_ZEROSUPPRESSION :
		([ZB0/,] PICTURE_REPETITION?)+
	|	([*B0/,] PICTURE_REPETITION?)+
	;

fragment PICTURE_DISPNUMEDIT_POINT_ZEROSUPPRESSION :
		POINT_ZEROSUPPRESSION? PICTURE_DISPNUMEDIT_POINT_FIXEDINSERTION
	;

fragment PICTURE_DISPNUMEDIT_POINT_FIXEDINSERTION :
		PICTURE_DISPNUMEDIT_POINT_SIMPLEINSERTION* [V.]? PICTURE_DISPNUMEDIT_POINT_SIMPLEINSERTION+
	|	PICTURE_DISPNUMEDIT_POINT_SIMPLEINSERTION+ [V.]
	;

fragment PICTURE_DISPNUMEDIT_POINT_SIMPLEINSERTION :
		[9B0/,] PICTURE_REPETITION?		// though ",(02)" and "/(10)" are weird editions
	;

// Disp-Num-Edit (DECIMAL POINT IS COMMA):
fragment PICTURE_DISPNUMEDIT_COMMA :
		('+' | '-') '$'?? (PICTURE_DISPNUMEDIT_COMMA_FLOATINGINSERTION | PICTURE_DISPNUMEDIT_COMMA_ZEROSUPPRESSION)
	|	            '$'?? (PICTURE_DISPNUMEDIT_COMMA_FLOATINGINSERTION | PICTURE_DISPNUMEDIT_COMMA_ZEROSUPPRESSION) ('+' | '-' | 'CR' | 'DB')?
	; 

fragment COMMA_FLOATING_INSERTION :
		'$'                    ([$B0/.] PICTURE_REPETITION?)+
	|	'$' PICTURE_REPETITION ([$B0/.] PICTURE_REPETITION?)*
	|	'+'                    ([+B0/.] PICTURE_REPETITION?)+
	|	'+' PICTURE_REPETITION ([+B0/.] PICTURE_REPETITION?)*
	|	'-'                    ([-B0/.] PICTURE_REPETITION?)+
	|	'-' PICTURE_REPETITION ([-B0/.] PICTURE_REPETITION?)*
	;

fragment PICTURE_DISPNUMEDIT_COMMA_FLOATINGINSERTION :
		COMMA_FLOATING_INSERTION? PICTURE_DISPNUMEDIT_COMMA_FIXEDINSERTION
	;

fragment COMMA_ZEROSUPPRESSION :
		([ZB0/.] PICTURE_REPETITION?)+
	|	([*B0/.] PICTURE_REPETITION?)+
	;

fragment PICTURE_DISPNUMEDIT_COMMA_ZEROSUPPRESSION :
		COMMA_ZEROSUPPRESSION? PICTURE_DISPNUMEDIT_COMMA_FIXEDINSERTION
	;

fragment PICTURE_DISPNUMEDIT_COMMA_FIXEDINSERTION :
		PICTURE_DISPNUMEDIT_COMMA_SIMPLEINSERTION* [V,]? PICTURE_DISPNUMEDIT_COMMA_SIMPLEINSERTION+
	|	PICTURE_DISPNUMEDIT_COMMA_SIMPLEINSERTION+ [V,]
	;

fragment PICTURE_DISPNUMEDIT_COMMA_SIMPLEINSERTION :
		[9B0/.] PICTURE_REPETITION?		// though ".(02)" and "/(10)" are weird editions
	; 
			  
fragment PICTURESTRING :
		PICTURE_DISPNUM
	|	PICTURE_DISPLAY		  
	|	PICTURE_DISPNUMEDIT_POINT
	|	PICTURE_DISPNUMEDIT_COMMA			
	;

PICTURECLAUSE : PICTURE WS (IS WS)? PICTURESTRING
	;
