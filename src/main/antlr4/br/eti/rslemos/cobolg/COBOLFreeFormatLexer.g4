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
import COBOLKeywords, COBOLBasics;

COMMENT			: ('*' | '/') .*? NEWLINE 
				{ _tokenStartCharPositionInLine == 0 }?	-> channel(HIDDEN);

/* 
 * This block should really be part of COBOLBasics, but as of 2015-04-25
 * ANTLR4 cannot import multi-mode Lexers: https://github.com/antlr/antlr4/issues/160
 * 
 * These rules are replicated in
 *  - COBOLFreeFormatLexer; and
 *  - COBOLFixedFormatLexer.
 */
PICTURE : 'PIC' 'TURE'?
	-> pushMode(PICTURE_MODE)
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
