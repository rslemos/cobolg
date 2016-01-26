package br.eti.rslemos.cobolg;

class Identifier implements LanguageBuilder {
	public final String name;
	public Identifier(String name) { this.name = name; }
	@Override public String toSourceString(boolean pretty) { return name; }
	@Override public String toTreeString(boolean pretty) { return "(identifier " + name + ")"; }
}