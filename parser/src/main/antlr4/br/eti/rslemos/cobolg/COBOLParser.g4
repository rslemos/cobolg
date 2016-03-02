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
parser grammar COBOLParser;
import IdentificationDivision, EnvironmentDivision, DataDivision, ProcedureDivision;

options { tokenVocab = COBOLLexer; }

program :
		identificationDivision
		environmentDivision?
		dataDivision?
		procedureDivision
	;

compilerStatements :
		compilerStatement*
	;

// unfortunately single token deletion is tried before single token insertion
// so EJECT is deleted before COMPILER_PERIOD is injected
// that is why they are all optional (so never injected)
compilerStatement :
		EJECT COMPILER_PERIOD?
	|	COPY (COMPILER_ID | COMPILER_STRING) COMPILER_PERIOD?
	;
