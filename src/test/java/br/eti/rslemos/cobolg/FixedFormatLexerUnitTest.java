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
		matchToken(                  TO_SKIPTOEOL_MODE_DEFAULT, "\uEBA3",   MARK  );
		matchToken(SKIPTOEOL_MODE,   SKIP_TO_EOL,               "        ", HIDDEN);
		
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
		matchToken(                  TO_SKIPTOEOL_MODE_DEFAULT, "\uEBA3",   MARK  );
		matchToken(SKIPTOEOL_MODE,   SKIP_TO_EOL,               "        ", HIDDEN);
		
		matchEOF();
	}

	@Test
	public void testSkipToEolLine() throws Exception {
		setSource("                                                                        IGNORED+");
		
		matchToken(                  TO_SEQUENCE_MODE,          "\uEBA0",   MARK  );
		matchToken(SEQUENCE_MODE,    SEQUENCE_NUMBER,           "      ",   HIDDEN);
		matchToken(SEQUENCE_MODE,    TO_INDICATOR_MODE,         "\uEBA1",   MARK  );
		matchToken(INDICATOR_MODE,   INDICATOR_BLANK,           " ",        HIDDEN);
		matchToken(PRE_DEFAULT_MODE, TO_DEFAULT_MODE,           "\uEBA2",   MARK  );
		matchToken(                  WS,                        "                                                                 ", HIDDEN);
		matchToken(                  TO_SKIPTOEOL_MODE_DEFAULT, "\uEBA3",   MARK  );
		matchToken(SKIPTOEOL_MODE,   SKIP_TO_EOL,               "IGNORED+", HIDDEN);
		
		matchEOF();
	}

	@Test
	public void testCommentLine() throws Exception {
		setSource("000001*THIS IS A COMMENT LINE                                           IGNORED+");
		
		matchToken(                  TO_SEQUENCE_MODE,          "\uEBA0",   MARK  );
		matchToken(SEQUENCE_MODE,    SEQUENCE_NUMBER,           "000001",   HIDDEN);
		matchToken(SEQUENCE_MODE,    TO_INDICATOR_MODE,         "\uEBA1",   MARK  );
		matchToken(INDICATOR_MODE,   INDICATOR_COMMENT,         "*",        HIDDEN);
		matchToken(PRE_COMMENT_MODE, TO_COMMENT_MODE,           "\uEBA2",   MARK  );
		matchToken(COMMENT_MODE,     FIXEDCOMMENT,              "THIS IS A COMMENT LINE                                           ", HIDDEN);
		matchToken(COMMENT_MODE,     TO_SKIPTOEOL_MODE_COMMENT, "\uEBA3",   MARK  );
		matchToken(SKIPTOEOL_MODE,   SKIP_TO_EOL,               "IGNORED+", HIDDEN);
		
		matchEOF();
	}
}
