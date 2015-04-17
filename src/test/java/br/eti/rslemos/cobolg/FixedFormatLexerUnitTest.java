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
		
		matchToken(TO_SEQUENCE_MODE,  "\uEBA0",   MARK  );
		matchToken(WS,                "      ",   HIDDEN);
		matchToken(TO_INDICATOR_MODE, "\uEBA1",   MARK  );
		matchToken(WS,                " ",        HIDDEN);
		matchToken(TO_DEFAULT_MODE,   "\uEBA2",   MARK  );
		matchToken(WS,                "                                                                 ", HIDDEN);
		matchToken(TO_SKIPTOEOL_MODE, "\uEBA3",   MARK  );
		matchToken(WS,                "        ", HIDDEN);
		
		matchEOF();
	}
}
