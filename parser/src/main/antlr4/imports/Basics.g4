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
parser grammar Basics;

options { tokenVocab = COBOLLexer; }

/* 
 * classes of user defined words.
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=32&zoom=auto,-100,194
 */

dataName          : USERDEFINEDWORD;

literal :
		numericLiteral
	|	alphanumericLiteral
	|	figurativeConstant
	;

alphanumericLiteral :
		quotedString
	|	HEXSTRING
	;

figurativeConstant :
		ZERO
	|	SPACE
	|	HIGH_VALUE
	|	LOW_VALUE
	|	QUOTE
	|	ALL literal
	|	NULL
//	|	symbolicCharacter
	;

quotedString :
		QUOTEDSTRING
	|	QUOTEDSTRING_START QUOTEDSTRING_MID* QUOTEDSTRING_END
	;

/**
 * Numeric literal.
 * 
 * http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=62&zoom=auto,-100,345
 */
numericLiteral :
		INTEGER
	|	FIXEDPOINT
	|	FLOATINGPOINT
//	|	HEXINTEGER
	;
