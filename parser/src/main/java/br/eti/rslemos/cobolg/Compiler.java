/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2017  Rodrigo Lemos
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
package br.eti.rslemos.cobolg;

import org.antlr.v4.runtime.ANTLRErrorListener;

import br.eti.rslemos.cobolg.COBOLParser.*;

public interface Compiler {

	void addErrorListener(ANTLRErrorListener listener);
	
	AlphanumericLiteralContext alphanumericLiteral();

	AtEndOfPagePhrasesContext atEndOfPagePhrases();

	AtEndPhrasesContext atEndPhrases();

	BatchContext batch();

	ConditionalExpressionContext conditionalExpression();

	ConfigurationSectionContext configurationSection();

	CorrespondingPhraseContext correspondingPhrase();

	DataDescriptionEntryContext dataDescriptionEntry();

	ExceptionPhrasesContext exceptionPhrases();

	FigurativeConstantContext figurativeConstant();

	FileControlParagraphContext fileControlParagraph();

	FileDescriptionEntryContext fileDescriptionEntry();

	FileSectionContext fileSection();

	GivingPhraseContext givingPhrase();

	IdentificationDivisionContext identificationDivision();

	ProceduralSentenceContext proceduralSentence();

	ProgramContext program();

	StmtSequentialREADContext stmtSequentialREAD(boolean b);

	StmtSequentialWRITEContext stmtSequentialWRITE(boolean b);

	StmtSETContext stmtSET();

	StmtSORTContext stmtSORT();

	StmtSTARTContext stmtSTART(boolean b);

	StmtSTOPContext stmtSTOP();

	StmtSTOPRUNContext stmtSTOPRUN();

	StmtSTRINGContext stmtSTRING(boolean b);

	StmtSUBTRACTContext stmtSUBTRACT(boolean b);

	StmtUNSTRINGContext stmtUNSTRING(boolean b);

	StmtXMLGENERATEContext stmtXMLGENERATE(boolean b);

	StmtXMLPARSEContext stmtXMLPARSE(boolean b);

	WorkingStorageSectionContext workingStorageSection();

	LiteralContext literal();

	StmtSEARCHContext stmtSEARCH(boolean b);

	StmtREWRITEContext stmtREWRITE(boolean b);

	StmtRETURNContext stmtRETURN(boolean b);

	StmtRELEASEContext stmtRELEASE();

	StmtRandomREADContext stmtRandomREAD(boolean b);

	StmtPERFORMContext stmtPERFORM();

	StmtPageWRITEContext stmtPageWRITE(boolean b);

	StmtOPENContext stmtOPEN();

	StmtMULTIPLYContext stmtMULTIPLY(boolean b);

	StmtMOVEContext stmtMOVE();

	StmtMERGEContext stmtMERGE();

	StmtINVOKEContext stmtINVOKE(boolean b);

	StmtINSPECTContext stmtINSPECT();

	StmtINITIALIZEContext stmtINITIALIZE();

	StmtGOTOContext stmtGOTO();

	StmtGOBACKContext stmtGOBACK();

	StmtEXITContext stmtEXIT();

	StmtEVALUATEContext stmtEVALUATE(boolean b);

	StmtENTRYContext stmtENTRY();

	StmtDIVIDEContext stmtDIVIDE(boolean b);

	StmtDISPLAYContext stmtDISPLAY();

	StmtDELETEContext stmtDELETE(boolean b);

	StmtCONTINUEContext stmtCONTINUE();

	StmtCOMPUTEContext stmtCOMPUTE(boolean b);

	StmtCLOSEContext stmtCLOSE();

	StmtCANCELContext stmtCANCEL();

	StmtCALLContext stmtCALL(boolean b);

	StmtALTERContext stmtALTER();

	StmtADDContext stmtADD(boolean b);

	StmtACCEPTContext stmtACCEPT();

	SpecialRegisterContext specialRegister();

	SizeErrorPhrasesContext sizeErrorPhrases();

	RoundedPhraseContext roundedPhrase();

	ProcedureDivisionContext procedureDivision();

	PictureClauseContext pictureClause();

	OverflowPhrasesContext overflowPhrases();

	NumericLiteralContext numericLiteral();

	IoControlParagraphContext ioControlParagraph();

	InvalidKeyPhrasesContext invalidKeyPhrases();
}
