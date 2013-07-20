package br.eti.rslemos.cobolg;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.DiagnosticErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.ParseTree;

public class CompilerHelper {
	
	public static ParseTree compile(String contents) throws IOException {
		return compile(new StringReader(contents));
	}

	public static ParseTree compile(Reader reader) throws IOException {
		COBOLLexer lexer = new COBOLLexer(new ANTLRInputStream(reader));
		COBOLParser parser = new COBOLParser(new CommonTokenStream(lexer));
		lexer.addErrorListener(new DiagnosticErrorListener());
		lexer.addErrorListener(new BailOutErrorListener());
		
		parser.addErrorListener(new DiagnosticErrorListener());
		//parser.addErrorListener(new BailOutErrorListener());
		parser.setErrorHandler(new BailErrorStrategy());
		
		ParseTree tree = parser.program();
		
		return tree;
	}
}

class BailOutErrorListener extends BaseErrorListener {

	@Override
	public void syntaxError(Recognizer<?, ?> recognizer,
			Object offendingSymbol, int line, int charPositionInLine,
			String msg, RecognitionException e) {
		throw new RuntimeException(msg, e);
	}

}
