package br.eti.rslemos.cobolg;

import java.io.IOException;
import java.io.Reader;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.TokenSource;
import org.junit.Test;

public class LineJoinerUnitTest extends AbstractLexerUnitTest {
	@Test public void singleLine() throws IOException {
		setSource("123456 THIS IS A SINGLE LINE OF NON-COBOL CODE                          IGNORED+");
	}

	@Override protected void setSource(String source) throws IOException {
		super.setSource(source);
		new LineJoiner((COBOLFixedPreLexer) stream);
	}
	
	@Override
	protected TokenSource getLexer(Reader reader) throws IOException {
		return new COBOLFixedPreLexer(new ANTLRInputStream(reader));
	}
}
