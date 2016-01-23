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
lexer grammar COBOLLexer;
import COBOLKeywords, COBOLBasics;

tokens { 
	MARK // a channel instead
}

COMMENT			: ('*' | '/') .*? NEWLINE 
                { _tokenStartCharPositionInLine == 0 }?	-> channel(HIDDEN);

fragment MARK0	: '\uEBA0';
fragment MARK1	: '\uEBA1';
fragment MARK2	: '\uEBA2';
fragment MARK3	: '\uEBA3';

DOUBLEQUOTEDSTRING_START	:  ["]  ( ~["\n\r\uEBA3] | ["] ["] )* ;
SINGLEQUOTEDSTRING_START	:  [']  ( ~['\n\r\uEBA3] | ['] ['] )* ;

TO_SEQUENCE_MODE	: MARK0 -> channel(MARK), mode(SEQUENCE_MODE);
TO_SKIPTOEOL_MODE_DEFAULT	: MARK3 -> channel(MARK), mode(SKIPTOEOL_MODE);

/* 
 * This block should really be part of COBOLBasics, but as of 2015-04-25
 * ANTLR4 cannot import multi-mode Lexers: https://github.com/antlr/antlr4/issues/160
 */
PICTURE : 'PIC' 'TURE'?
	-> pushMode(PICTURE_MODE)
	;

RECORDING : 'RECORDING'
	-> pushMode(RECORDING_MODE)
	;

COPY : 'COPY'
	-> channel(COMPILER_CHANNEL), pushMode(COPY_MODE)
	;

mode PICTURE_MODE;

PIC_WS : WS
	-> channel(HIDDEN)
	;

PIC_IS : IS
	;

// accepts any string, even malformed picture strings
// validation of picture strings is to be done elsewhere  
// using only '$' as currency symbol
PICTURESTRING : [-+ABEGNPSVXZCRDB90/,.*$()0-9]* [-+ABEGNPSVXZCRDB90/,*$()0-9] 
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

mode COPY_MODE;

COPY_WS : WS
	-> channel(HIDDEN);

COPY_ID : ID
	-> channel(COMPILER_CHANNEL), mode(COPY_MODE2);

COPY_ELSE : .
	-> more, popMode;

mode COPY_MODE2;

COPY2_WS : WS
	-> channel(HIDDEN);

COPY_PERIOD : PERIOD
	-> channel(COMPILER_CHANNEL), popMode;

COPY2_ELSE : .
	-> more, popMode;

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

DOUBLEQUOTEDSTRING_MID		:  ["]  ( ~["\n\r\uEBA3] | ["] ["] )* ;
DOUBLEQUOTEDSTRING_END		:  ["]  ( ~["\n\r\uEBA3] | ["] ["] )* ["] -> mode(DEFAULT_MODE);

SINGLEQUOTEDSTRING_MID		:  [']  ( ~['\n\r\uEBA3] | ['] ['] )* ;
SINGLEQUOTEDSTRING_END		:  [']  ( ~['\n\r\uEBA3] | ['] ['] )* ['] -> mode(DEFAULT_MODE);

TO_SKIPTOEOL_MODE_CONTINUATION : MARK3 -> channel(MARK), mode(SKIPTOEOL_MODE);
												
mode SKIPTOEOL_MODE;
SKIPTOEOL_MODE_NL			: NEWLINE	-> channel(HIDDEN), mode(DEFAULT_MODE);
SKIP_TO_EOL					: ~[\n\r]+ -> channel(HIDDEN) ;
