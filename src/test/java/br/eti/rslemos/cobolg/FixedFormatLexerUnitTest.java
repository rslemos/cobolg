package br.eti.rslemos.cobolg;

import static br.eti.rslemos.cobolg.COBOLFixedFormatLexer.*;
import org.junit.Test;

import br.eti.rslemos.cobolg.Compiler.FixedFormatCompiler;

public class FixedFormatLexerUnitTest extends AbstractLexerUnitTest {

	public FixedFormatLexerUnitTest() {
		super(new FixedFormatCompiler());
	}

	@Test
	public void testEmptyLine() throws Exception {
		setSource("                                                                                ");
		
		matchToken(                  TO_SEQUENCE_MODE,          "\uEBA0",   MARK  );
		matchToken(SEQUENCE_MODE,    SEQUENCE_NUMBER,           "      ",   HIDDEN);
		matchToken(SEQUENCE_MODE,    TO_INDICATOR_MODE,         "\uEBA1",   MARK  );
		matchToken(INDICATOR_MODE,   INDICATOR_BLANK,           " ",        HIDDEN);
		matchToken(PRE_DEFAULT_MODE, TO_DEFAULT_MODE,           "\uEBA2",   MARK  );
		matchToken(                  WS,                        "                                                                 ", HIDDEN);
		matchToken(                  TO_SKIPTOEOL_MODE,         "\uEBA3",   MARK  );
		matchToken(                  WS,                        "        ", HIDDEN);
		
		matchEOF();
	}

	@Test
	public void testSequenceAreaLine() throws Exception {
		setSource("012345                                                                          ");
		
		matchToken(                  TO_SEQUENCE_MODE,          "\uEBA0",   MARK  );
		matchToken(SEQUENCE_MODE,    SEQUENCE_NUMBER,           "012345",   HIDDEN);
		matchToken(SEQUENCE_MODE,    TO_INDICATOR_MODE,         "\uEBA1",   MARK  );
		matchToken(INDICATOR_MODE,   INDICATOR_BLANK,           " ",        HIDDEN);
		matchToken(PRE_DEFAULT_MODE, TO_DEFAULT_MODE,           "\uEBA2",   MARK  );
		matchToken(                  WS,                        "                                                                 ", HIDDEN);
		matchToken(                  TO_SKIPTOEOL_MODE,         "\uEBA3",   MARK  );
		matchToken(                  WS,                        "        ", HIDDEN);
		
		matchEOF();
	}
}
