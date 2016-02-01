package br.eti.rslemos.cobolg;

import org.antlr.v4.runtime.tree.RuleNode;

public interface ANTLRBuilder<T extends RuleNode> {
	T build();
}
