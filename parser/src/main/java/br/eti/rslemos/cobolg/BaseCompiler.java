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

public abstract class BaseCompiler implements Compiler {
	private final Compiler main;
	
	protected BaseCompiler(Compiler main) {
		this.main = main;
	}

	protected Compiler delegate() {
		return main;
	}
	
	public void addErrorListener(ANTLRErrorListener listener) {
		delegate().addErrorListener(listener);
	}

	public AlphanumericLiteralContext alphanumericLiteral() {
		return delegate().alphanumericLiteral();
	}

	public AtEndOfPagePhrasesContext atEndOfPagePhrases() {
		return delegate().atEndOfPagePhrases();
	}

	public AtEndPhrasesContext atEndPhrases() {
		return delegate().atEndPhrases();
	}

	public BatchContext batch() {
		return delegate().batch();
	}

	public ConditionalExpressionContext conditionalExpression() {
		return delegate().conditionalExpression();
	}

	public ConfigurationSectionContext configurationSection() {
		return delegate().configurationSection();
	}

	public CorrespondingPhraseContext correspondingPhrase() {
		return delegate().correspondingPhrase();
	}

	public DataDescriptionEntryContext dataDescriptionEntry() {
		return delegate().dataDescriptionEntry();
	}

	public ExceptionPhrasesContext exceptionPhrases() {
		return delegate().exceptionPhrases();
	}

	public FigurativeConstantContext figurativeConstant() {
		return delegate().figurativeConstant();
	}

	public FileControlParagraphContext fileControlParagraph() {
		return delegate().fileControlParagraph();
	}

	public FileDescriptionEntryContext fileDescriptionEntry() {
		return delegate().fileDescriptionEntry();
	}

	public FileSectionContext fileSection() {
		return delegate().fileSection();
	}

	public GivingPhraseContext givingPhrase() {
		return delegate().givingPhrase();
	}

	public IdentificationDivisionContext identificationDivision() {
		return delegate().identificationDivision();
	}

	public ProceduralSentenceContext proceduralSentence() {
		return delegate().proceduralSentence();
	}

	public ProgramContext program() {
		return delegate().program();
	}

	public StmtSequentialREADContext stmtSequentialREAD(boolean b) {
		return delegate().stmtSequentialREAD(b);
	}

	public StmtSequentialWRITEContext stmtSequentialWRITE(boolean b) {
		return delegate().stmtSequentialWRITE(b);
	}

	public StmtSETContext stmtSET() {
		return delegate().stmtSET();
	}

	public StmtSORTContext stmtSORT() {
		return delegate().stmtSORT();
	}

	public StmtSTARTContext stmtSTART(boolean b) {
		return delegate().stmtSTART(b);
	}

	public StmtSTOPContext stmtSTOP() {
		return delegate().stmtSTOP();
	}

	public StmtSTOPRUNContext stmtSTOPRUN() {
		return delegate().stmtSTOPRUN();
	}

	public StmtSTRINGContext stmtSTRING(boolean b) {
		return delegate().stmtSTRING(b);
	}

	public StmtSUBTRACTContext stmtSUBTRACT(boolean b) {
		return delegate().stmtSUBTRACT(b);
	}

	public StmtUNSTRINGContext stmtUNSTRING(boolean b) {
		return delegate().stmtUNSTRING(b);
	}

	public StmtXMLGENERATEContext stmtXMLGENERATE(boolean b) {
		return delegate().stmtXMLGENERATE(b);
	}

	public StmtXMLPARSEContext stmtXMLPARSE(boolean b) {
		return delegate().stmtXMLPARSE(b);
	}

	public WorkingStorageSectionContext workingStorageSection() {
		return delegate().workingStorageSection();
	}

	public LiteralContext literal() {
		return delegate().literal();
	}

	public StmtSEARCHContext stmtSEARCH(boolean b) {
		return delegate().stmtSEARCH(b);
	}

	public StmtREWRITEContext stmtREWRITE(boolean b) {
		return delegate().stmtREWRITE(b);
	}

	public StmtRETURNContext stmtRETURN(boolean b) {
		return delegate().stmtRETURN(b);
	}

	public StmtRELEASEContext stmtRELEASE() {
		return delegate().stmtRELEASE();
	}

	public StmtRandomREADContext stmtRandomREAD(boolean b) {
		return delegate().stmtRandomREAD(b);
	}

	public StmtPERFORMContext stmtPERFORM() {
		return delegate().stmtPERFORM();
	}

	public StmtPageWRITEContext stmtPageWRITE(boolean b) {
		return delegate().stmtPageWRITE(b);
	}

	public StmtOPENContext stmtOPEN() {
		return delegate().stmtOPEN();
	}

	public StmtMULTIPLYContext stmtMULTIPLY(boolean b) {
		return delegate().stmtMULTIPLY(b);
	}

	public StmtMOVEContext stmtMOVE() {
		return delegate().stmtMOVE();
	}

	public StmtMERGEContext stmtMERGE() {
		return delegate().stmtMERGE();
	}

	public StmtINVOKEContext stmtINVOKE(boolean b) {
		return delegate().stmtINVOKE(b);
	}

	public StmtINSPECTContext stmtINSPECT() {
		return delegate().stmtINSPECT();
	}

	public StmtINITIALIZEContext stmtINITIALIZE() {
		return delegate().stmtINITIALIZE();
	}

	public StmtGOTOContext stmtGOTO() {
		return delegate().stmtGOTO();
	}

	public StmtGOBACKContext stmtGOBACK() {
		return delegate().stmtGOBACK();
	}

	public StmtEXITContext stmtEXIT() {
		return delegate().stmtEXIT();
	}

	public StmtEVALUATEContext stmtEVALUATE(boolean b) {
		return delegate().stmtEVALUATE(b);
	}

	public StmtENTRYContext stmtENTRY() {
		return delegate().stmtENTRY();
	}

	public StmtDIVIDEContext stmtDIVIDE(boolean b) {
		return delegate().stmtDIVIDE(b);
	}

	public StmtDISPLAYContext stmtDISPLAY() {
		return delegate().stmtDISPLAY();
	}

	public StmtDELETEContext stmtDELETE(boolean b) {
		return delegate().stmtDELETE(b);
	}

	public StmtCONTINUEContext stmtCONTINUE() {
		return delegate().stmtCONTINUE();
	}

	public StmtCOMPUTEContext stmtCOMPUTE(boolean b) {
		return delegate().stmtCOMPUTE(b);
	}

	public StmtCLOSEContext stmtCLOSE() {
		return delegate().stmtCLOSE();
	}

	public StmtCANCELContext stmtCANCEL() {
		return delegate().stmtCANCEL();
	}

	public StmtCALLContext stmtCALL(boolean b) {
		return delegate().stmtCALL(b);
	}

	public StmtALTERContext stmtALTER() {
		return delegate().stmtALTER();
	}

	public StmtADDContext stmtADD(boolean b) {
		return delegate().stmtADD(b);
	}

	public StmtACCEPTContext stmtACCEPT() {
		return delegate().stmtACCEPT();
	}

	public SpecialRegisterContext specialRegister() {
		return delegate().specialRegister();
	}

	public SizeErrorPhrasesContext sizeErrorPhrases() {
		return delegate().sizeErrorPhrases();
	}

	public RoundedPhraseContext roundedPhrase() {
		return delegate().roundedPhrase();
	}

	public ProcedureDivisionContext procedureDivision() {
		return delegate().procedureDivision();
	}

	public PictureClauseContext pictureClause() {
		return delegate().pictureClause();
	}

	public OverflowPhrasesContext overflowPhrases() {
		return delegate().overflowPhrases();
	}

	public NumericLiteralContext numericLiteral() {
		return delegate().numericLiteral();
	}

	public IoControlParagraphContext ioControlParagraph() {
		return delegate().ioControlParagraph();
	}

	public InvalidKeyPhrasesContext invalidKeyPhrases() {
		return delegate().invalidKeyPhrases();
	}
}
