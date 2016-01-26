package br.eti.rslemos.cobolg;

class NumberLiteral implements LanguageBuilder {
	public final int v;
	public NumberLiteral(int v) { this.v = v; }
	@Override public String toSourceString(boolean pretty) { return String.valueOf(v); }
	@Override public String toTreeString(boolean pretty) { return "(literal (numberLiteral " + String.valueOf(v) + "))"; }
}