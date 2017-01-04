/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2015  Rodrigo Lemos
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * END COPYRIGHT NOTICE
 ******************************************************************************/
package br.eti.rslemos.cobolg;

import java.io.PrintStream;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.atn.ATN;
import org.antlr.v4.runtime.atn.ATNConfig;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;

public class CollectErrorListener extends BaseErrorListener {
	
	List<CompilationError> errors = new ArrayList<CompilationError>();
	
	@Deprecated public CollectErrorListener(String fileName) { }

	@Override
	public void reportAmbiguity(Parser recognizer, DFA dfa, int startIndex, int stopIndex, boolean exact, BitSet ambigAlts, ATNConfigSet configs) {
		errors.add(new AmbiguityError(recognizer, dfa, startIndex, stopIndex, exact, ambigAlts, configs));
	}

	@Override
	public void reportAttemptingFullContext(Parser recognizer, DFA dfa, int startIndex, int stopIndex, BitSet conflictingAlts, ATNConfigSet configs) {
		errors.add(new AttemptingFullContextError(recognizer, dfa, startIndex, stopIndex, conflictingAlts, configs));
	}

	@Override
	public void reportContextSensitivity(Parser recognizer, DFA dfa, int startIndex, int stopIndex, int prediction, ATNConfigSet configs) {
		errors.add(new ContextSensitivityError(recognizer, dfa, startIndex, stopIndex, prediction, configs));
	}

	@Override
	public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine, String msg, RecognitionException e) {
		errors.add(new SyntaxError(recognizer, offendingSymbol, line, charPositionInLine, msg, e));
	}
	
	public void verify() {
		if (!errors.isEmpty()) {
			CompilationAssertionError up = new CompilationAssertionError(errors);
			up.printStackTrace();
			throw up;
		}
	}

	public static class CompilationAssertionError extends Error {

		private static final long serialVersionUID = 1355307576009905119L;
		private List<CompilationError> errors;

		public CompilationAssertionError(List<CompilationError> errors) {
			super(String.format("%d errors", errors.size()));
			this.errors = errors;
		}

		@Override
		public void printStackTrace(PrintStream s) {
			s.println(getMessage());
			for (int i = 0; i < errors.size(); i++) {
				errors.get(i).print(i, s);
			}
		}
	}
	
	private abstract static class CompilationError {
		public abstract void print(int i, PrintStream s);
	}
	
	private class SyntaxError extends CompilationError {

		public final Recognizer<?, ?> recognizer;
		public final Object offendingSymbol;
		public final int line;
		public final int charPositionInLine;
		public final String msg;
		public final RecognitionException e;

		public SyntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine, String msg, RecognitionException e) {
			this.recognizer = recognizer;
			this.offendingSymbol = offendingSymbol;
			this.line = line;
			this.charPositionInLine = charPositionInLine;
			this.msg = msg;
			this.e = e;
		}

		@Override
		public void print(int i, PrintStream s) {
			char type;
			int mode = -1;
			
			if (recognizer instanceof Parser)
				type = 'P';
			else if (recognizer instanceof Lexer) {
				type = 'L';
				mode = ((Lexer)recognizer)._mode;
			} else
				type = '?';
			
			s.format("%4d. Syntax error [%c:%d] %s (%d,%d)\n", i, type, mode, msg, line, charPositionInLine);
		}
	}
	
	private static class DFAError extends CompilationError {
		private static Field conflictingAltsFieldAccessor;
		
		static {
			try {
				conflictingAltsFieldAccessor = ATNConfigSet.class.getDeclaredField("conflictingAlts");
				conflictingAltsFieldAccessor.setAccessible(true);
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}

		public final Parser recognizer;
		public final DFA dfa;
		public final ATNConfigSet configs;

		public DFAError(Parser recognizer, DFA dfa, ATNConfigSet configs) {
			this.recognizer = recognizer;
			this.dfa = dfa;
			this.configs = configs;
		}

		protected void printDFA(PrintStream s) {
			s.println("DFA:");
			s.println(dfa.toString(recognizer.getVocabulary()));
		}

		protected void printConfigs(PrintStream s) {
			try {
				s.println("ATNConfigSet: ");
				
				for (ATNConfig config : configs.elements()) {
					s.println(config.toString(recognizer, true));
				}
				
				s.printf("Has semantic context? %s\n", configs.hasSemanticContext);
				s.printf("Unique alternative: %s\n", configs.uniqueAlt!=ATN.INVALID_ALT_NUMBER ? String.valueOf(configs.uniqueAlt) : "none");
				Object conflictingAlts = conflictingAltsFieldAccessor.get(configs);
				s.printf("Conflicting alternatives: %s\n", conflictingAlts != null ? String.valueOf(conflictingAlts) : "none");
				s.printf("Dips into outer context? %s\n", configs.dipsIntoOuterContext);
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}

		@Override
		public void print(int i, PrintStream s) {
			printDFA(s);
			printConfigs(s);
		}
	}
	
	private static class AmbiguityError extends DFAError {

		public final int startIndex;
		public final int stopIndex;
		public final boolean exact;
		public final BitSet ambigAlts;

		public AmbiguityError(Parser recognizer, DFA dfa, int startIndex, int stopIndex, boolean exact, BitSet ambigAlts, ATNConfigSet configs) {
			super(recognizer, dfa, configs);
			this.startIndex = startIndex;
			this.stopIndex = stopIndex;
			this.exact = exact;
			this.ambigAlts = ambigAlts;
		}

		@Override
		public void print(int i, PrintStream s) {
			s.printf("%4d. ambiguity: %d-%d, exact? %s, alts=%s\n", i, startIndex, stopIndex, String.valueOf(exact), ambigAlts);
			super.print(i, s);
		}
	}
	
	private static class AttemptingFullContextError extends DFAError {
		public final int startIndex;
		public final int stopIndex;
		public final BitSet conflictingAlts;

		public AttemptingFullContextError(Parser recognizer, DFA dfa, int startIndex, int stopIndex, BitSet conflictingAlts, ATNConfigSet configs) {
			super(recognizer, dfa, configs);
			this.startIndex = startIndex;
			this.stopIndex = stopIndex;
			this.conflictingAlts = conflictingAlts;
		}

		@Override
		public void print(int i, PrintStream s) {
			s.printf("%4d. attempt full context: %d-%d, alts=%s\n", i, startIndex, stopIndex, conflictingAlts);
			super.print(i, s);
		}
	}
	
	private static class ContextSensitivityError extends DFAError {

		public final int startIndex;
		public final int stopIndex;
		public final int prediction;

		public ContextSensitivityError(Parser recognizer, DFA dfa, int startIndex, int stopIndex, int prediction, ATNConfigSet configs) {
			super(recognizer, dfa, configs);
			this.startIndex = startIndex;
			this.stopIndex = stopIndex;
			this.prediction = prediction;
		}

		@Override
		public void print(int i, PrintStream s) {
			s.printf("%4d. context sensitivity: %d-%d, prediction: %d\n", i, startIndex, stopIndex, prediction);
			super.print(i, s);
		}
		
	}
}