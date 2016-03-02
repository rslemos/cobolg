/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2016  Rodrigo Lemos
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

import static org.junit.Assert.fail;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.BitSet;
import java.util.EnumSet;

import org.antlr.v4.runtime.DiagnosticErrorListener;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import org.junit.Test;

public class ErrorDetector extends DiagnosticErrorListener {
	private int syntaxError;
	private int exactAmbiguity;
	private int fullContextAttempt;
	private int contextSensitivity;
	private int nonExactAmbiguity;
	
	public ErrorDetector() {
		super(false);
	}

	@Override public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine, String msg, RecognitionException e) {
		syntaxError++;
		super.syntaxError(recognizer, offendingSymbol, line, charPositionInLine, msg, e);
	}

	@Override public void reportAmbiguity(Parser recognizer, DFA dfa, int startIndex, int stopIndex, boolean exact, BitSet ambigAlts, ATNConfigSet configs) {
		if (exact)
			exactAmbiguity++;
		else
			nonExactAmbiguity++;
		
		super.reportAmbiguity(recognizer, dfa, startIndex, stopIndex, exact, ambigAlts, configs);
		syntaxError--;
	}

	@Override public void reportAttemptingFullContext(Parser recognizer, DFA dfa, int startIndex, int stopIndex, BitSet conflictingAlts, ATNConfigSet configs) {
		fullContextAttempt++;
		super.reportAttemptingFullContext(recognizer, dfa, startIndex, stopIndex, conflictingAlts, configs);
		syntaxError--;
	}

	@Override public void reportContextSensitivity(Parser recognizer, DFA dfa, int startIndex, int stopIndex, int prediction, ATNConfigSet configs) {
		contextSensitivity++;
		super.reportContextSensitivity(recognizer, dfa, startIndex, stopIndex, prediction, configs);
		syntaxError--;
	}
	
	public void check() {
		boolean checkSyntax = true;
		boolean checkExactAmbiguity = true;
		boolean checkNonExactAmbiguity = true;
		boolean checkFullContextAttempt = true;
		boolean checkContextSensitivity = true;
		
		boolean waiveSyntax = false;
		boolean waiveExactAmbiguity = false;
		boolean waiveNonExactAmbiguity = false;
		boolean waiveFullContextAttemp = false;
		boolean waiveContextSensititivy = false;
		EnumSet<Waive.CompilationError> set = EnumSet.noneOf(Waive.CompilationError.class);

		boolean skipWaiveCheck = collectWaivers(set);
		
		checkSyntax = !set.contains(Waive.CompilationError.SYNTAX_ERROR);
		checkExactAmbiguity = !set.contains(Waive.CompilationError.EXACT_AMBIGUITY);
		checkNonExactAmbiguity = !set.contains(Waive.CompilationError.NON_EXACT_AMBIGUITY);
		checkFullContextAttempt = !set.contains(Waive.CompilationError.FULL_CONTEXT_ATTEMPT);
		checkContextSensitivity = !set.contains(Waive.CompilationError.CONTEXT_SENSITIVITY);
		
		if (!skipWaiveCheck) {
			waiveSyntax = !checkSyntax && syntaxError == 0;
			waiveExactAmbiguity = !checkExactAmbiguity && exactAmbiguity == 0;
			waiveNonExactAmbiguity = !checkNonExactAmbiguity && nonExactAmbiguity == 0;
			waiveFullContextAttemp = !checkFullContextAttempt && fullContextAttempt == 0;
			waiveContextSensititivy = !checkContextSensitivity && contextSensitivity == 0;
		}
		
		int failures = 
				(checkSyntax ? syntaxError : 0) + 
				(checkExactAmbiguity ? exactAmbiguity : 0) +
				(checkNonExactAmbiguity ? nonExactAmbiguity : 0) +
				(checkFullContextAttempt ? fullContextAttempt : 0) +
				(checkContextSensitivity ? contextSensitivity : 0);
			;
	
		String message = String.format("%d syntax errors, %d ambiguities (%d exact), %d context sensitivities, %d full context attempts", syntaxError, exactAmbiguity + nonExactAmbiguity, exactAmbiguity, contextSensitivity, fullContextAttempt);
			
		if (waiveSyntax || waiveExactAmbiguity || waiveNonExactAmbiguity || waiveFullContextAttemp || waiveContextSensititivy) {
			fail(String.format("Waiving non occurring compilation errors (%s)", message));
		} else if (failures > 0)
			fail(message);
	}

	private boolean collectWaivers(EnumSet<Waive.CompilationError> set) {
		Method testMethod = getTestMethod();
		collectInto(testMethod.getAnnotation(Waive.class), set);
		return collectInto(testMethod.getDeclaringClass().getAnnotation(Waive.class), set);
	}

	private boolean collectInto(Waive waive, EnumSet<Waive.CompilationError> set) {
		if (waive != null) {
			int before = set.size();
			set.addAll(Arrays.asList(waive.value()));
			return set.size() - before > 0;
		}
		
		return false;
	}
	
	private Method getTestMethod() {
		StackTraceElement[] callers = Thread.currentThread().getStackTrace();
		for (StackTraceElement caller : callers) {
			Method method = getMethod(caller.getClassName(), caller.getMethodName());
			if (method != null) {
				if (method.getAnnotation(Test.class) != null)
					return method;
			}
		}
		
		return null;
	}

	private static Method getMethod(String className, String methodName) {
		try {
			return Class.forName(className).getDeclaredMethod(methodName);
		} catch (Exception e) {
			return null;
		}
	}
}