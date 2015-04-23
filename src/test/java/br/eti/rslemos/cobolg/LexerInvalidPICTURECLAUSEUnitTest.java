package br.eti.rslemos.cobolg;

import junit.framework.TestSuite;

public class LexerInvalidPICTURECLAUSEUnitTest extends LexerPICTURECLAUSEUnitTest {
	private static final String[] PICS = {
			"PICx 9999",
		};

	private LexerInvalidPICTURECLAUSEUnitTest(String name) {
		super(name);
	}

	public static TestSuite suite() throws Exception {
		TestSuite suite = new TestSuite();

		for (String pic : PICS) {
			suite.addTest(new LexerInvalidPICTURECLAUSEUnitTest(pic));
		}
		
		return suite;
	}
	
	@Override
	protected void runTest() throws Throwable {
		try {
			super.runTest();
			fail("Got token: " + token);
		} catch (AssertionError e) {
			
		}
	}
}