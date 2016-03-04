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
parser grammar ProcedureDivision;
import Basics, Statements;

options { tokenVocab = COBOLLexer; }

procedureDivision :
		PROCEDURE DIVISION usingPhrase? PERIOD
		( unnamedProceduralSection namedProceduralSection* | namedProceduralSection+ )
	;

/**
 * Using phrase.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=273&zoom=auto,-40,470
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=274&zoom=auto,-40,410
 */
usingPhrase : USING (byReferencePhrase | byValuePhrase)+;

byReferencePhrase : (BY? REFERENCE)? dataName+;
byValuePhrase : BY? VALUE dataName+;

unnamedProceduralSection :
		( unnamedProceduralParagraph namedProceduralParagraph* | namedProceduralParagraph+ )
	;

namedProceduralSection :
		sectionName SECTION PERIOD
		( unnamedProceduralParagraph namedProceduralParagraph* | namedProceduralParagraph+ )
	;

unnamedProceduralParagraph :
		proceduralStatement+
	;

namedProceduralParagraph :
		paragraphName PERIOD
		proceduralStatement+
	;

sectionName :
		USERDEFINEDWORD
	;

paragraphName :
		USERDEFINEDWORD
	;
