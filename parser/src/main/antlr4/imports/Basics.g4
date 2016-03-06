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

levelNumber returns [int value] : INTEGER { $INTEGER.text.matches("^(0?[1-9]|[1-4][0-9]|66|77|88)$") }? { $value = $INTEGER.int; };
priorityNumber    : INTEGER { $INTEGER.text.matches("^[0-9]?[0-9]$") }?;

/* 
 * classes of user defined words.
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=32&zoom=auto,-100,194
 */

alphabetName      : USERDEFINEDWORD;
conditionName     : USERDEFINEDWORD;
dataClassName     : USERDEFINEDWORD;
dataName          : USERDEFINEDWORD;
fileName          : USERDEFINEDWORD;
indexName         : USERDEFINEDWORD;
mnemonicName      : USERDEFINEDWORD;
paragraphName     : USERDEFINEDWORD;
programName       : USERDEFINEDWORD;
sectionName       : USERDEFINEDWORD;
symbolicCharacter : USERDEFINEDWORD;
xmlSchemaName     : USERDEFINEDWORD;

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
 * External file ID.
 * 
 * Further restrictions apply.
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=144&zoom=auto,-40,610
 */
externalFileId    : USERDEFINEDWORD;

/**
 * (File-control paragraph entries') Assignment name.
 * 
 * Further restrictions apply.
 *  
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=150&zoom=auto,-40,240
 */
assignmentName    : USERDEFINEDWORD;

/**
 * (character) Class name.
 * 
 * @see publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=142&zoom=auto,-40,670
 */
className         : USERDEFINEDWORD;

/**
 * System name.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=34&zoom=auto,-100,656
 * TODO: refine what a systemName is (get the list of available systemNames)
 */
systemName        : USERDEFINEDWORD;

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
 * Special register.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=38&zoom=auto,-100,610
 */
specialRegister :
//		ADDRESS OF what
		DEBUG_CONTENTS
	|	DEBUG_ITEM
	|	DEBUG_LINE
	|	DEBUG_NAME
	|	DEBUG_SUB_1
	|	DEBUG_SUB_2
	|	DEBUG_SUB_3
	|	JNIENVPTR
//	|	LENGTH OF what
	|	LINAGE_COUNTER
	|	RETURN_CODE
	|	SHIFT_IN
	|	SHIFT_OUT
	|	SORT_CONTROL
	|	SORT_CORE_SIZE
	|	SORT_FILE_SIZE
	|	SORT_MESSAGE
	|	SORT_MODE_SIZE
	|	SORT_RETURN
	|	TALLY
	|	WHEN_COMPILED
	|	XML_CODE
	|	XML_EVENT
	|	XML_INFORMATION
	|	XML_NAMESPACE
	|	XML_NAMESPACE_PREFIX
	|	XML_NNAMESPACE
	|	XML_NNAMESPACE_PREFIX
	|	XML_NTEXT
	|	XML_TEXT
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

identifier :
		USERDEFINEDWORD
	|	USERDEFINEDWORD LPAREN identifier+ RPAREN
	|	USERDEFINEDWORD LPAREN INTEGER RPAREN
	|	specialRegister
	;

/**
 * Arithmetic expression.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=279&zoom=auto,-40,185
 */
arithmeticExpression :
		LPAREN arithmeticExpression RPAREN
	|	(OP_PLUS | OP_MINUS) arithmeticExpression
	|	arithmeticExpression OP_STARSTAR arithmeticExpression
	|	arithmeticExpression (OP_STAR | OP_SLASH) arithmeticExpression
	|	arithmeticExpression (OP_PLUS | OP_MINUS) arithmeticExpression
	|	literal
	|	identifier
	;

/*
 * References
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=89&zoom=auto,-40,555
 */

/**
 * Reference to data names.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=91&zoom=auto,-40,410
 */
refDataName :
		dataName ((IN | OF) dataName)* ((IN | OF) fileName)? (LPAREN subscript+ RPAREN)? (LPAREN arithmeticExpression COLON arithmeticExpression? RPAREN)?
	;

/**
 * Subscripting.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=95&zoom=auto,-40,270
 */
subscript :
		INTEGER
	|	ALL
	|	refDataName ((OP_PLUS | OP_MINUS) INTEGER)?
	;
