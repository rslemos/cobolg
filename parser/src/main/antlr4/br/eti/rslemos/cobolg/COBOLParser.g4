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
/**
 * This grammar is based on Enterprise COBOL for z/OS Language Reference Version 5.2
 * (SC14-7381-03).
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf
 */
parser grammar COBOLParser;
import Basics, IdentificationDivision, EnvironmentDivision, DataDivision, ProcedureDivision;

options { tokenVocab = COBOLLexer; }

/**
 * Batch.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=109&zoom=auto,-40,760
 */
batch : program+;

/**
 * Program.
 * 
 * Per reference manual identificationDivision ought to be required.
 * 
 * @see http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=107&zoom=auto,-40,720
 */
program :
		identificationDivision
		environmentDivision?
		dataDivision?
		procedureDivision?
		programTail?
	;

programTail :
		nestedProgram*
		END PROGRAM programName PERIOD
	;

nestedProgram :
		nestedIdentificationDivision
		environmentDivision?
		dataDivision?
		procedureDivision?
		programTail
	;

/* skipping class definition @ http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=113&zoom=auto,-40,710 */
/* skipping method definition @ http://publibfp.boulder.ibm.com/epubs/pdf/igy5lr20.pdf#page=117&zoom=auto,-40,710 */

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
