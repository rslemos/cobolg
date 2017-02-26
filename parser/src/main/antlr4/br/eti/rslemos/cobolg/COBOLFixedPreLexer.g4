/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2017  Rodrigo Lemos
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
lexer grammar COBOLFixedPreLexer;

// ugly ANTLR4 syntax for lexers will not allow for ranges

fragment NON_NEWLINE : ~[\n\r];

// these will encode exact ranges: {n}
fragment NON_NEWLINE_2 : NON_NEWLINE NON_NEWLINE;
fragment NON_NEWLINE_4 : NON_NEWLINE_2 NON_NEWLINE_2;
fragment NON_NEWLINE_8 : NON_NEWLINE_4 NON_NEWLINE_4;
fragment NON_NEWLINE_16 : NON_NEWLINE_8 NON_NEWLINE_8;
fragment NON_NEWLINE_32 : NON_NEWLINE_16 NON_NEWLINE_16;
fragment NON_NEWLINE_64 : NON_NEWLINE_32 NON_NEWLINE_32;

fragment NON_NEWLINE_6 : NON_NEWLINE_4 NON_NEWLINE_2;
fragment NON_NEWLINE_65 : NON_NEWLINE_64 NON_NEWLINE;

// these will encode from 0 up to n ranges: {0, n}
fragment NON_NEWLINE_UPTO_1 : ( | NON_NEWLINE);
fragment NON_NEWLINE_UPTO_3 : ( | NON_NEWLINE_2) NON_NEWLINE_UPTO_1;

fragment NON_NEWLINE_UPTO_5 : NON_NEWLINE_UPTO_3 NON_NEWLINE_UPTO_1 NON_NEWLINE_UPTO_1;

// mode DEFAULT_MODE;
NEWLINE : ('\n' '\r'? | '\r' '\n'?)
	;

SEQUENCE_NUMBER : NON_NEWLINE_6
	-> mode(INDICATOR_MODE)
	;

SEQUENCE_NUMBER_SHORT : NON_NEWLINE_UPTO_5
	-> mode(DEFAULT_MODE)
	;

mode INDICATOR_MODE;
INDICATOR_CODE : ' '
	-> mode(SOURCE_MODE)
	;

INDICATOR_DEBUG : 'D'
	-> mode(SOURCE_MODE)
	;

INDICATOR_COMMENT : ('*' | '/')
	-> mode(SOURCE_MODE)
	;

INDICATOR_CONTINUATION : '-'
	-> mode(SOURCE_MODE)
	;

INDICATOR_ELSE : NON_NEWLINE
	-> mode(SOURCE_MODE)
	;

INDICATOR_SHORT : 
	-> mode(DEFAULT_MODE)
	;

mode SOURCE_MODE;
SOURCE : NON_NEWLINE_65
	-> mode(TAIL_MODE)
	;

mode TAIL_MODE;
TAIL : NON_NEWLINE*
	-> mode(DEFAULT_MODE)
	;

