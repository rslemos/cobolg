package br.eti.rslemos.cobolg;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.antlr.v4.runtime.Token;

import br.eti.rslemos.cobolg.Compiler.FreeFormatCompiler;

public class LexerPICTURECLAUSEUnitTest extends TestCase {
	private static final String[] PICS = {
			"PIC 9999",
			"PIC 9(5)",
		};

	public LexerPICTURECLAUSEUnitTest(String name) {
		super(xlateForEclipse(name));
	}

	public static TestSuite suite() throws Exception {
		TestSuite suite = new TestSuite();

		for (String pic : PICS) {
			suite.addTest(new LexerPICTURECLAUSEUnitTest(pic));
		}
		
		return suite;
	}
	
	private Compiler compiler;
	
	@Override
	protected void setUp() throws Exception {
		compiler = new FreeFormatCompiler();
	}

	@Override
	protected void runTest() throws Throwable {
		String pic = unxlateForEclipse(getName());
		
		Token token = compiler.decompose(pic).nextToken();
		
		assertThat(token.getType(), is(equalTo(COBOLFreeFormatLexer.PICTURECLAUSE)));
		assertThat(token.getText(), is(equalTo(pic)));
	}

	// eclipse JUnit test runner doesn't like parentesis 
	private static String xlateForEclipse(String name) {
		return name.replace('(', '[').replace(')', ']');
	}

	private static String unxlateForEclipse(String name) {
		return name.replace('[', '(').replace(']', ')');
	}
}