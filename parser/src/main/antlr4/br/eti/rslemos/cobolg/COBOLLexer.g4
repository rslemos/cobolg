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
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf
 */
lexer grammar COBOLLexer;
import Words;

channels { MARK, COMPILER_CHANNEL }

/**
 * Inline comment.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=67&zoom=auto,-100,160
 */
INLINECOMMENT : '*>' ~[\n\r\uEBA3]* -> channel(HIDDEN)
	;

/* separators */
/* http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=69&zoom=auto,-100,730 */

WS : ' '+
	-> channel(HIDDEN);

NEWLINE : ('\n' '\r'? | '\r' '\n'?)
	-> channel(HIDDEN);

PERIOD    : '.'; // technically speaking, it should be '.' ( ' ' | '\n' | EOF )

/**
 * Integer.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=58&zoom=auto,-100,120
 */
INTEGER : [-+]? [0-9]+
	;

/**
 * Fixed point.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=58&zoom=auto,-100,120
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=144&zoom=auto,-40,670
 */
FIXEDPOINT : [-+]? [0-9]+ ('.'|',') [0-9]+
	;

/**
 * Floating point.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=63&zoom=auto,-100,675
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=144&zoom=auto,-40,670
 */
FLOATINGPOINT : [-+]? [0-9]+ ('.'|',') [0-9]+ 'E' [-+]? [0-9]?[0-9]
	;

///**
// * Hexadecimal integer.
// * 
// * @see where is it defined?
// */
//HEXINTEGER :
//		'H' ["] [0-9A-F]+ ["]
//	|	'H' ['] [0-9A-F]+ [']
//	;

QUOTEDSTRING :
		["] ( ~["\n\r] | ["] ["] )* ["]
	|	['] ( ~['\n\r] | ['] ['] )* [']
	;

HEXSTRING :
		'X' ["] ([0-9A-F][0-9A-F])+ ["]
	|	'X' ['] ([0-9A-F][0-9A-F])+ [']
	;

QUOTEDSTRING_START	:
		["]  ( ~["\n\r\uEBA3] | ["] ["] )*
	|	[']  ( ~['\n\r\uEBA3] | ['] ['] )*
	;

COMMENT			: ('*' | '/') ~[\n\r\uEBA3]*
                { _tokenStartCharPositionInLine == 0 }?	-> channel(HIDDEN);

PICTURE : 'PICTURE'
	-> pushMode(PICTURE_MODE)
	;

PIC : 'PIC'
	-> pushMode(PICTURE_MODE)
	;

RECORDING : 'RECORDING'
	-> pushMode(RECORDING_MODE)
	;

COPY : 'COPY'
	-> channel(COMPILER_CHANNEL), pushMode(COMPILER_MODE), pushMode(COMPILER_ID_MODE)
	;

EJECT : 'EJECT'
	-> channel(COMPILER_CHANNEL), pushMode(COMPILER_MODE)
	;

fragment MARK0	: '\uEBA0';
fragment MARK1	: '\uEBA1';
fragment MARK2	: '\uEBA2';
fragment MARK3	: '\uEBA3';

TO_SEQUENCE_MODE	: MARK0 -> channel(MARK), mode(SEQUENCE_MODE);
TO_SKIPTOEOL_MODE_DEFAULT	: MARK3 -> channel(MARK), mode(SKIPTOEOL_MODE);

mode PICTURE_MODE;

PIC_WS : WS
	-> channel(HIDDEN)
	;

PIC_IS : IS
	;

// accepts any string, even malformed picture strings
// validation of picture strings is to be done elsewhere  
// using only '$' as currency symbol
/**
 * PICTURE character-string
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=67&zoom=auto,-100,420
 */
PICTURESTRING : [-+ABEGNPSVXZCRDB90/,*$()0-9.]* [-+ABEGNPSVXZCRDB90/,*$()0-9]
	-> popMode
	;

mode RECORDING_MODE;

REC_WS : WS
	-> channel(HIDDEN);

REC_MODE : MODE
	;

REC_IS : IS
	;

F : 'F'
	-> popMode
	;

V : 'V'
	-> popMode
	;

U : 'U'
	-> popMode
	;

S : 'S'
	-> popMode
	;

mode COMPILER_MODE;

COMPILER_WS : ' '+
	-> channel(HIDDEN)
	;

COMPILER_PERIOD : PERIOD
	-> channel(COMPILER_CHANNEL), popMode
	;

COMPILER_ELSE : 
	-> more, popMode
	;

mode COMPILER_ID_MODE;

COMPILER_ID_WS : ' '+
	-> channel(HIDDEN)
	;

COMPILER_ID : USERDEFINEDWORD
	-> channel(COMPILER_CHANNEL), popMode
	;

COMPILER_STRING : QUOTEDSTRING
	-> channel(COMPILER_CHANNEL), popMode
	;

COMPILER_ID_ELSE :
	-> more, popMode
	;

/* basic block ends here */

mode SEQUENCE_MODE;
SEQUENCE_MODE_NL	: NEWLINE 	-> channel(HIDDEN), mode(DEFAULT_MODE);

SEQUENCE_NUMBER		: ~[\n\r\uEBA1]+ -> channel(HIDDEN);
TO_INDICATOR_MODE	: MARK1 -> channel(MARK), mode(INDICATOR_MODE);

mode INDICATOR_MODE;
INDICATOR_MODE_NL			: NEWLINE	-> channel(HIDDEN), mode(DEFAULT_MODE);

INDICATOR_BLANK				: ' ' -> channel(HIDDEN), mode(PRE_DEFAULT_MODE);
INDICATOR_CONTINUATION		: '-' -> channel(HIDDEN), mode(PRE_CONTINUATION_MODE);
INDICATOR_COMMENT			: '*' -> channel(HIDDEN), mode(PRE_COMMENT_MODE); 

mode PRE_DEFAULT_MODE;
PRE_DEFAULT_MODE_MODE_NL	: NEWLINE	-> channel(HIDDEN), mode(DEFAULT_MODE);
TO_DEFAULT_MODE				: MARK2 -> channel(MARK), mode(DEFAULT_MODE);

mode PRE_COMMENT_MODE;
PRE_COMMENT_MODE_NL			: NEWLINE	-> channel(HIDDEN), mode(DEFAULT_MODE);
TO_COMMENT_MODE				: MARK2 -> channel(MARK), mode(COMMENT_MODE);

mode COMMENT_MODE;
COMMENT_MODE_NL				: NEWLINE	-> channel(HIDDEN), mode(DEFAULT_MODE);
FIXEDCOMMENT			    : ~[\n\r\uEBA3]+ -> channel(HIDDEN);
TO_SKIPTOEOL_MODE_COMMENT	: MARK3 -> channel(MARK), mode(SKIPTOEOL_MODE);

mode PRE_CONTINUATION_MODE;
PRE_CONTINUATION_MODE_NL	: NEWLINE	-> channel(HIDDEN), mode(DEFAULT_MODE);
TO_CONTINUATION_MODE		: MARK2 -> channel(MARK), mode(CONTINUATION_MODE);

mode CONTINUATION_MODE;
CONTINUATION_MODE_NL		: NEWLINE	-> channel(HIDDEN), mode(DEFAULT_MODE);

WS_CONT						: ' '+ -> channel(HIDDEN);

QUOTEDSTRING_MID            :  ( ["]  ( ~["\n\r\uEBA3] | ["] ["] )*     | [']  ( ~['\n\r\uEBA3] | ['] ['] )*     );
QUOTEDSTRING_END			:  ( ["]  ( ~["\n\r\uEBA3] | ["] ["] )* ["] | [']  ( ~['\n\r\uEBA3] | ['] ['] )* ['] ) -> mode(DEFAULT_MODE);

TO_SKIPTOEOL_MODE_CONTINUATION : MARK3 -> channel(MARK), mode(SKIPTOEOL_MODE);
												
mode SKIPTOEOL_MODE;
SKIPTOEOL_MODE_NL			: NEWLINE	-> channel(HIDDEN), mode(DEFAULT_MODE);
SKIP_TO_EOL					: ~[\n\r]+ -> channel(HIDDEN) ;
