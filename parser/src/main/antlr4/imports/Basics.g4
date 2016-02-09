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

priorityNumber    : INTEGER { $INTEGER.text.matches("^[0-9]?[0-9]$") }?;

/* 
 * classes of user defined words.
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=32&zoom=auto,-100,194
 */

alphabetName      : USERDEFINEDWORD;
dataName          : USERDEFINEDWORD;
mnemonicName      : USERDEFINEDWORD;
programName       : USERDEFINEDWORD;

// this is not formally defined, but used elsewhere
computerName      : USERDEFINEDWORD;

/**
 * Environment name.
 * 
 * Further restrictions may apply:
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=138&zoom=auto,-40,330
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=139&zoom=auto,-40,670
 */
environmentName   : USERDEFINEDWORD;

/**
 * Figurative constant.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=35&zoom=auto,-100,160
 */
figurativeConstant :
		ZERO | ZEROS | ZEROES
	|	SPACE | SPACES
	|	HIGH_VALUE | HIGH_VALUES
	|	LOW_VALUE | LOW_VALUES
	|	QUOTE | QUOTES
	|	ALL literal
//	|	symbolicCharacter
	|	NULL | NULLS
	;

/**
 * Literal.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=58&zoom=auto,-100,300
 */
literal :
		numericLiteral
	|	alphanumericLiteral
	|	figurativeConstant
	;

/**
 * Alphanumeric literal.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=58&zoom=auto,-100,120
 */
alphanumericLiteral :
		QUOTEDSTRING
	|	QUOTEDSTRING_START QUOTEDSTRING_MID* QUOTEDSTRING_END
	|	HEXSTRING
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
