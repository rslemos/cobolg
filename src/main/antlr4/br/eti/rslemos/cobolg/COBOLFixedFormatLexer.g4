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
lexer grammar COBOLFixedFormatLexer;
import COBOLKeywords, COBOLBasics;

options {
	// to avoid token collision
	tokenVocab = COBOLFreeFormatLexer;
}

tokens { 
	MARK // a channel instead
}

fragment MARK0	: '\uEBA0';
fragment MARK1	: '\uEBA1';
fragment MARK2	: '\uEBA2';
fragment MARK3	: '\uEBA3';

TO_SEQUENCE_MODE	: MARK0 -> channel(MARK), mode(SEQUENCE_MODE);
TO_DEFAULT_MODE	: MARK2 -> channel(MARK);
TO_SKIPTOEOL_MODE	: MARK3 -> channel(MARK);
 
mode SEQUENCE_MODE;
SEQUENCE_NUMBER		: ~[\n\r\uEBA1]+ -> channel(HIDDEN);
TO_INDICATOR_MODE	: MARK1 -> channel(MARK), mode(DEFAULT_MODE);
