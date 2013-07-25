package br.eti.rslemos.cobolg;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.atn.PredictionMode;

import br.eti.rslemos.cobolg.COBOLParser.ProgramContext;

public class Compiler {
	
	public static ProgramContext compile(String contents) throws IOException {
		return compile(new StringReader(contents));
	}

	public static ProgramContext compile(Reader reader) throws IOException {
		return compile(null, reader);
	}
	
	public static ProgramContext compile(String fileName, Reader reader) throws IOException {
		CollectErrorListener custom = new CollectErrorListener(fileName);
		
		COBOLLexer lexer = new COBOLLexer(new ANTLRInputStream(reader));
		lexer.removeErrorListeners();
		lexer.addErrorListener(custom);
		
		COBOLParser parser = new COBOLParser(new CommonTokenStream(lexer));
		parser.removeErrorListeners();
		parser.addErrorListener(custom);
		parser.getInterpreter().setPredictionMode(PredictionMode.LL_EXACT_AMBIG_DETECTION);
		
		ProgramContext tree = parser.program();
		
		custom.verify();
		
		return tree;
	}
}

class CollectErrorListener extends BaseErrorListener {
	private final String fileName;
	List<String> errors = new ArrayList<String>();
	
	
	public CollectErrorListener(String fileName) {
		this.fileName = fileName;
	}

	@Override
	public void syntaxError(Recognizer<?, ?> recognizer,
							Object offendingSymbol,
							int line,
							int charPositionInLine,
							String msg,
							RecognitionException e) {
		
		char type;
		if (recognizer instanceof Parser)
			type = 'P';
		else if (recognizer instanceof Lexer)
			type = 'L';
		else
			type = '?';
		
		if (fileName != null)
			errors.add(String.format("[%c] %s (%s:%d,%d)", type, msg, fileName, line, charPositionInLine));
		else
			errors.add(String.format("[%c] %s (%d,%d)", type, msg, line, charPositionInLine));
	}
	
	void verify() {
		if (!errors.isEmpty()) {
			StringBuilder message = new StringBuilder();
			
			message.append(errors.size()).append(" errors\n");
			
			for (int i = 0; i < errors.size(); i++) {
				message.append(String.format("%4d.", i + 1)).append(errors.get(i)).append("\n");
			}
			
			message.setLength(message.length() - 1);

			throw new CompilationError(message.toString());
		}
	}
}

class CompilationError extends Error {

	private static final long serialVersionUID = 1355307576009905119L;

	public CompilationError() {
		super();
	}

	public CompilationError(String message, Throwable cause) {
		super(message, cause);
	}

	public CompilationError(String message) {
		super(message);
	}

	public CompilationError(Throwable cause) {
		super(cause);
	}
}
