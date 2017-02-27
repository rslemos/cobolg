package br.eti.rslemos.cobolg;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.misc.Interval;

public class LineJoiner implements CharStream {

	public LineJoiner(COBOLFixedPreLexer lexer) {
	}

	@Override
	public void consume() {
	}

	@Override
	public int LA(int i) {
		return 0;
	}

	@Override
	public int mark() {
		return 0;
	}

	@Override
	public void release(int marker) {
	}

	@Override
	public int index() {
		return 0;
	}

	@Override
	public void seek(int index) {
	}

	@Override
	public int size() {
		return 0;
	}

	@Override
	public String getSourceName() {
		return null;
	}

	@Override
	public String getText(Interval interval) {
		return null;
	}
}
