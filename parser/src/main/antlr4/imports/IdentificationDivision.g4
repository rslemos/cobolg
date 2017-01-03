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
parser grammar IdentificationDivision;
import Basics;

options { tokenVocab = COBOLLexer; }

/**
 * Identification division.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=121&zoom=auto,-40,710
 */
identificationDivision :
		(IDENTIFICATION | ID) DIVISION PERIOD
		PROGRAM_ID PERIOD? programName (IS? (RECURSIVE | INITIAL) PROGRAM?)? PERIOD?
		identificationDivisionContent?
	;

identificationDivisionContent :
		(AUTHOR PERIOD? commentEntry*)?
		(INSTALLATION PERIOD? commentEntry*)?
		(DATE_WRITTEN PERIOD? commentEntry*)?
		(DATE_COMPILED PERIOD? commentEntry*)?
		(SECURITY PERIOD? commentEntry*)?
	;

/**
 * Comment entry.
 * 
 * Comment entry cyptically defined as: "any combination of characters from the 
 * character set of the computer". Moreover "on one or more lines".
 * 
 * To implement this we'll have to fiddle with the lexer, but this is out of 
 * scope right now. So sticking with the following poor definition.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=129&zoom=auto,-40,190
 */
commentEntry : USERDEFINEDWORD | literal;
