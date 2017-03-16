package br.eti.rslemos.cobolg;

import static br.eti.rslemos.cobolg.COBOLParser.*;
import static org.antlr.v4.runtime.Token.*;
import java.io.IOException;
import java.io.Reader;

import org.antlr.v4.runtime.TokenSource;
import org.junit.Test;

public class ManualCOBOLLexerUnitTest extends AbstractLexerUnitTest {
	@Test public void testEmptySource() throws IOException {
		setSource("");
		
		matchEOFExt(1, 0);
	}

	@Test public void testHead() throws IOException {
		setSource("123456");
		
		matchTokenExt(SEQUENCE_NUMBER, "123456", HIDDEN_CHANNEL, 1, 0);
		matchEOFExt(1, 6);
	}

	@Test public void testBlankIndicator() throws IOException {
		setSource("123456 ");
		
		matchTokenExt(SEQUENCE_NUMBER, "123456", HIDDEN_CHANNEL, 1, 0);
		matchTokenExt(INDICATOR_BLANK, " ", HIDDEN_CHANNEL, 1, 6);
		matchEOFExt(1, 7);
	}

	@Test public void testIDENTIFICATION_DIVISION() throws IOException {
		setSource("123456 IDENTIFICATION DIVISION.");
		
		matchTokenExt(SEQUENCE_NUMBER, "123456", HIDDEN_CHANNEL, 1, 0);
		matchTokenExt(INDICATOR_BLANK, " ", HIDDEN_CHANNEL, 1, 6);
		matchTokenExt(IDENTIFICATION, "IDENTIFICATION", DEFAULT_CHANNEL, 1, 7);
		matchTokenExt(WS, " ", HIDDEN_CHANNEL, 1, 21);
		matchTokenExt(DIVISION, "DIVISION", DEFAULT_CHANNEL, 1, 22);
		matchTokenExt(PERIOD, ".", DEFAULT_CHANNEL, 1, 30);

		matchEOFExt(1, 31);
	}

	@Override
	protected TokenSource getLexer(Reader reader) throws IOException {
		return new ManualCOBOLLexer(reader, SegmentationStrategy.FIXED_FORMAT);
	}
}
