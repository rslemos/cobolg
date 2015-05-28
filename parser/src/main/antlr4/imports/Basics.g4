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

dataName :
		ID
	;

literal :
		numericLiteral
	|	alphanumericLiteral
	|	figurativeConstant
	;

numericLiteral :
		INTEGER
	|	FIXEDPOINT
	|	HEXINTEGER
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
		DOUBLEQUOTEDSTRING
	|	SINGLEQUOTEDSTRING
	|	DOUBLEQUOTEDSTRING_START DOUBLEQUOTEDSTRING_MID* DOUBLEQUOTEDSTRING_END
	|	SINGLEQUOTEDSTRING_START SINGLEQUOTEDSTRING_MID* SINGLEQUOTEDSTRING_END
	;
