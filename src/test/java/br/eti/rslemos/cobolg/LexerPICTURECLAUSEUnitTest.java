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
			// Disp-Num
			"PIC 999",
			"PIC 9(5)",
			"PIC 9(03)",
			"PIC 9(04)V9(02)",
			"PIC S9(10)V99",
			"PIC V9(12)",			// this one from http://www.itl.nist.gov/div897/ctg/suites/newcob.val.Z
			"PIC 999V",				// found googling (though don't know whether compiles ok)
			"PIC PPP999",
			"PIC S999PPP",
			
			"PIC 9999",
			"PIC 9V999",
			"PIC S9(8)",
			"PIC S9(12)V9(5)",
			"PIC 9(3)9(6)",
			"PIC 9(3)P(6)",

			// Display
			"PIC X",
			"PIC X(09)",
			"PIC X(80)",
			"PIC X9(10)",
			"PIC 9(19)X",
			"PIC X(3)9(4)",
			"PIC X(10)A(10)9(10)",
			"PIC X(10)",
			"PIC A(20)",

			// Disp-Num-Edit
			"PIC 99999,999",
			"PIC 99999,999+",
			"PIC 99999,999-",
			"PIC +9999,999",
			"PIC -9999,999",
			"PIC ++++9,999",
			"PIC ----9,999",
			"PIC ZZZZ9,999",
			"PIC +ZZZ9,999",
			"PIC ++ZZ9,999",
			"PIC BBBB9,999",
			"PIC BZZZ9,999",
			"PIC +BZZ9,999",
			"PIC +BBZ9,999",
			"PIC $9999,999",
			"PIC $$$$9,999",
			"PIC $ZZZ9,999",
			"PIC BB$$9,999",
			"PIC 99.999,999",
			"PIC 9.9.9,999",
			"PIC ZZ.ZZ9,99",
			"PIC ZZ.ZZZ.ZZZ.ZZ9,99",
			"PIC -ZZ.ZZ9,9999",
			"PIC $$.$$9,99",
			"PIC $Z.ZZ9,99",
			"PIC $(4)9.99CR",		// this and below are all from
			"PIC $*9.99DB",			// http://www.itl.nist.gov/div897/ctg/suites/newcob.val.Z
			"PIC -(4)9.99",
			"PIC $**.**CR",
			"PIC ****9",
			"PIC ****.9",
			"PIC $$,$$$,$$$,$$$,$$$,$$$.99",
			"PIC ++,+++,+++,+++,+++,+++.99",
			"PIC --,---,---,---,---,---.99",
			
			
			// possibly Disp-Num-Edit
			"PIC -999",
			"PIC -----99",
			"PIC 999-",
			"PIC +999",
			"PIC +++++99",
			"PIC 999+",
			"PIC $999",
			"PIC $$$$$99",
			"PIC -$$$$$99",
			"PIC +$$$$$99",
			"PIC *999",
			"PIC *****99",
			"PIC -*****99",
			"PIC +*****99",
			"PIC $$,$$$,$99",
			"PIC 9(3).99",
			"PIC 99/99/9999",
			"PIC 9(3)09(3)09(4)",
			"PIC 9(3)B9(3)B9(4)",
			"PIC 99CR",
			"PIC 99DB",
			"PIC Z999",
			"PIC ZZZZZ99",
			"PIC -ZZZZZ99",
			"PIC +ZZZZZ99",
		};

	protected LexerPICTURECLAUSEUnitTest(String name) {
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
	protected Token token;
	
	@Override
	protected void setUp() throws Exception {
		compiler = new FreeFormatCompiler();
	}

	@Override
	protected void runTest() throws Throwable {
		String pic = unxlateForEclipse(getName());
		
		token = compiler.decompose(pic).nextToken();
		
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