package br.eti.rslemos.cobolg;

import java.nio.channels.IllegalSelectorException;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class StmtADDBuilder implements LanguageBuilder {

	final Logger logger = LoggerFactory.getLogger(StmtADDBuilder.class);

	public static void main(String[] args) {
		System.out.println(ADD().identifier("X").TO().identifier("Y").toTreeString(true));
		System.out.println(ADD().identifier("X").TO().identifier("Y").ROUNDED().toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("Y").toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").TO().identifier("Y1").identifier("Y2").toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").identifier("Y2").toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").ROUNDED().identifier("Y2").toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").ROUNDED().toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").ROUNDED().toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").ROUNDED().toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").ROUNDED().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").ROUNDED().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").ROUNDED().toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").ROUNDED().toTreeString(true));
		System.out.println(ADD().identifier("X").TO().identifier("Y").onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X").TO().identifier("Y").ROUNDED().onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("Y").onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").TO().identifier("Y1").identifier("Y2").onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").identifier("Y2").onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").ROUNDED().identifier("Y2").onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").ROUNDED().onSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X").TO().identifier("Y").notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X").TO().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("Y").notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").TO().identifier("Y1").identifier("Y2").notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").identifier("Y2").notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").ROUNDED().identifier("Y2").notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X").TO().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X").TO().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").TO().identifier("Y1").identifier("Y2").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").identifier("Y2").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").ROUNDED().identifier("Y2").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).toTreeString(true));
		System.out.println(ADD().identifier("X").TO().identifier("Y").END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X").TO().identifier("Y").ROUNDED().END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("Y").END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").TO().identifier("Y1").identifier("Y2").END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").identifier("Y2").END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").ROUNDED().identifier("Y2").END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").ROUNDED().END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").ROUNDED().END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").ROUNDED().END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").ROUNDED().END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").ROUNDED().END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").ROUNDED().END_ADD().toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").END_ADD().toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").ROUNDED().END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X").TO().identifier("Y").onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X").TO().identifier("Y").ROUNDED().onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("Y").onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").TO().identifier("Y1").identifier("Y2").onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").identifier("Y2").onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").ROUNDED().identifier("Y2").onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").ROUNDED().onSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X").TO().identifier("Y").notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X").TO().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("Y").notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").TO().identifier("Y1").identifier("Y2").notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").identifier("Y2").notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").ROUNDED().identifier("Y2").notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").ROUNDED().notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X").TO().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X").TO().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").TO().identifier("Y1").identifier("Y2").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").identifier("Y2").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").literal(20).literal(30).TO().identifier("Y1").ROUNDED().identifier("Y2").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").TO().identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().literal(10).TO().identifier("X2").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").identifier("X3").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().identifier("X1").identifier("X2").TO().identifier("X3").GIVING().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));
		System.out.println(ADD().CORRESPONDING().identifier("X").TO().identifier("Y").ROUNDED().onSizeError(STOPRUN()).notOnSizeError(STOPRUN()).END_ADD().toTreeString(true));

		System.out.println();
		System.out.println(ADD("X1", "X2", "X3").TO("Y1", "Y2").toTreeString(true));

	}

	public static StmtADDBuilder ADD() {
		return new StmtADDBuilder();
	}
	
	public static StmtADDBuilder ADD(Identifier... identifier) {
		return ADD().add(identifier);
	}
	
	public static StmtADDBuilder ADD(String... names) {
		return ADD().identifier(names);
	}
	
	public static StmtADDBuilder ADD(NumberLiteral... literal) {
		return ADD().add(literal);
	}

	public static StmtADDBuilder ADD(int... ints) {
		return ADD().literal(ints);
	}

	private boolean corresponding, corr;
	
	private int to = -1;
	private int giving = -1;
	private List<LanguageBuilder> subjects = new ArrayList<LanguageBuilder>();
	private BitSet rounded = new BitSet(0);
	
	private LanguageBuilder onSizeError, sizeError;
	private LanguageBuilder notOnSizeError, notSizeError;
	private boolean endadd;
	
	public StmtADDBuilder add(Identifier... identifiers) {
		if (endadd) throw new IllegalStateException();
		if ((corresponding || corr) && to == -1 && subjects.size() != 0) throw new IllegalStateException();
		if ((corresponding || corr) && to == 1 && subjects.size() != 1) throw new IllegalStateException();
		if ((corresponding || corr) && identifiers.length > 1) throw new IllegalArgumentException();
		if (onSizeError != null || sizeError != null || notOnSizeError != null || notSizeError != null) throw new IllegalSelectorException();

		for (Identifier identifier : identifiers)
			subjects.add(identifier);
		
		return this;
	}
	
	public StmtADDBuilder add(NumberLiteral... literals) {
		if (endadd) throw new IllegalStateException();
		if (corresponding || corr) throw new IllegalStateException();
		if (rounded.nextSetBit(0) > 0) throw new IllegalStateException();
		if (onSizeError != null || sizeError != null || notOnSizeError != null || notSizeError != null) throw new IllegalSelectorException();
		
		for (NumberLiteral literal : literals)
			subjects.add(literal);
		
		return this;
	}
	
	public StmtADDBuilder add(LanguageBuilder... subjects) {
		StmtADDBuilder thiz = this;
		for (LanguageBuilder subject : subjects) {
			if (subject instanceof Identifier)
				thiz = thiz.add((Identifier)subject);
			else if (subject instanceof NumberLiteral)
				thiz = thiz.add((NumberLiteral)subject);
			else
				throw new IllegalArgumentException(String.valueOf(subject));
		}
		
		return thiz;
	}
	
	public StmtADDBuilder add(String... names) {
		return identifier(names);
	}
	
	public StmtADDBuilder add(int... ints) {
		return literal(ints);
	}

	public StmtADDBuilder add(Object... subjects) {
		LanguageBuilder[] subjects0 = new Identifier[subjects.length];
		
		for (int i = 0; i < subjects.length; i++)
			if (subjects[i] instanceof String)
				subjects0[i] = new Identifier((String)subjects[i]);
			else if (subjects[i] instanceof Integer)
				subjects0[i] = new NumberLiteral((Integer)subjects[i]);
			else
				throw new IllegalArgumentException(String.valueOf(i));
		
		return add(subjects0);
	}

	public StmtADDBuilder identifier(String... names) {
		Identifier[] identifiers = new Identifier[names.length];
		
		for (int i = 0; i < names.length; i++)
			identifiers[i] = new Identifier(names[i]);
		
		return add(identifiers);
	}
	
	public StmtADDBuilder literal(int... ints) {
		NumberLiteral[] literals = new NumberLiteral[ints.length];
		
		for (int i = 0; i < ints.length; i++)
			literals[i] = new NumberLiteral(ints[i]);
		
		return add(literals);
	}
	
	public StmtADDBuilder CORRESPONDING() {
		if (endadd) throw new IllegalStateException();
		if (corresponding || corr) throw new IllegalStateException();
		if (!subjects.isEmpty()) throw new IllegalStateException();
		
		corresponding = true;
		return this;
	}
	
	public StmtADDBuilder CORRESPONDING(Identifier identifier) {
		return CORRESPONDING().add(identifier);
	}
	
	public StmtADDBuilder CORRESPONDING(String name) {
		return CORRESPONDING().add(name);
	}
	
	public StmtADDBuilder CORR() {
		if (endadd) throw new IllegalStateException();
		if (corresponding || corr) throw new IllegalStateException();
		if (!subjects.isEmpty()) throw new IllegalStateException();
		
		corr = true;
		return this;
	}

	public StmtADDBuilder CORR(Identifier identifier) {
		return CORR().add(identifier);
	}
	
	public StmtADDBuilder CORR(String name) {
		return CORR().add(name);
	}
	
	public StmtADDBuilder TO() {
		if (endadd) throw new IllegalStateException();
		if (to > 0) throw new IllegalStateException();
		if (subjects.isEmpty()) throw new IllegalStateException();
		
		to = subjects.size();
		return this;
	}

	public StmtADDBuilder TO(Identifier... identifiers) {
		return TO().add(identifiers);
	}
	
	public StmtADDBuilder TO(NumberLiteral... literals) {
		return TO().add(literals);
	}
	
	public StmtADDBuilder TO(LanguageBuilder... subjects) {
		return TO().add(subjects);
	}
	
	public StmtADDBuilder TO(String... names) {
		return TO().add(names);
	}
	
	public StmtADDBuilder TO(int... ints) {
		return TO().add(ints);
	}

	public StmtADDBuilder TO(Object... subjects) {
		return TO().add(subjects);
	}

	public StmtADDBuilder GIVING() {
		if (endadd) throw new IllegalStateException();
		if (giving > 0) throw new IllegalStateException();
		if (to > 0 && subjects.size() != to + 1) throw new IllegalStateException();
		if (subjects.size() < 2) throw new IllegalStateException();
		
		giving = subjects.size();
		return this;
	}

	public StmtADDBuilder GIVING(Identifier... identifiers) {
		return GIVING().add(identifiers);
	}

	public StmtADDBuilder GIVING(String... names) {
		return GIVING().add(names);
	}

	public StmtADDBuilder ROUNDED() {
		if (endadd) throw new IllegalStateException();
		if (to == 0 || giving == 0) throw new IllegalStateException();
		
		rounded.set(subjects.size());
		return this;
	}

	public StmtADDBuilder ROUNDED(Identifier... identifiers) {
		return ROUNDED().add(identifiers);
	}

	public StmtADDBuilder ROUNDED(String... names) {
		return ROUNDED().add(names);
	}

	public StmtADDBuilder onSizeError(LanguageBuilder sub) {
		if (endadd) throw new IllegalStateException();
		if (onSizeError != null || sizeError != null) throw new IllegalStateException();
		if (notOnSizeError != null || notSizeError != null) throw new IllegalSelectorException();
		
		onSizeError = sub;
		return this;
	}

	public StmtADDBuilder sizeError(LanguageBuilder sub) {
		if (endadd) throw new IllegalStateException();
		if (onSizeError != null || sizeError != null) throw new IllegalStateException();
		if (notOnSizeError != null || notSizeError != null) throw new IllegalSelectorException();
		
		sizeError = sub;
		return this;
	}

	public StmtADDBuilder notOnSizeError(LanguageBuilder sub) {
		if (endadd) throw new IllegalStateException();
		if (notOnSizeError != null || notSizeError != null) throw new IllegalSelectorException();
		
		notOnSizeError = sub;
		return this;
	}

	public StmtADDBuilder notSizeError(LanguageBuilder sub) {
		if (endadd) throw new IllegalStateException();
		if (notOnSizeError != null || notSizeError != null) throw new IllegalSelectorException();
		
		notSizeError = sub;
		return this;
	}

	public StmtADDBuilder END_ADD() {
		if (endadd) throw new IllegalStateException();
		
		endadd = true;
		return this;
	}

	public String toString() {
		return String.format("corresponding: %b, corr: %b, to: %d, giving: %d, subjects: %s, rounded: %s, onSizeError: %s, sizeError: %s, notOnSizeError: %s, notSizeError: %s, endadd: %b\n",
				corresponding,
				corr,
				to,
				giving,
				subjects,
				rounded,
				onSizeError,
				sizeError,
				notOnSizeError,
				notSizeError,
				endadd
			);
	}
	@Override public String toSourceString(boolean pretty) {
		StringBuilder result = new StringBuilder();
		result.append("ADD ");
		if (corresponding) result.append("CORRESPONDING ");
		if (corr) result.append("CORR ");
		
		if (to > 0) {
			for(int i = 0; i < to; i++)
				result.append(subjects.get(i).toSourceString(pretty)).append(" ");
			
			result.append("TO ");
			
			if (giving > 0) {
				for(int i = to; i < giving; i++)
					result.append(subjects.get(i).toSourceString(pretty)).append(" ");
				
				result.append("GIVING ");
				
				to = giving;
			}
		} else if (giving > 0) {
			for(int i = 0; i < giving - 1; i++)
				result.append(subjects.get(i).toSourceString(pretty)).append(" ");
			
			result.append("GIVING ");
			
			to = giving;
		} else
			throw new IllegalStateException();
		
		for(int i = to; i < subjects.size(); i++) {
			result.append(subjects.get(i).toSourceString(pretty)).append(" ");
			if (rounded.get(i))
				result.append("ROUNDED ");
		}

		if (onSizeError != null)
			result.append("ON SIZE ERROR ").append(onSizeError.toSourceString(pretty)).append(" ");

		if (sizeError != null)
			result.append("SIZE ERROR ").append(sizeError.toSourceString(pretty)).append(" ");

		if (notOnSizeError != null)
			result.append("NOT ON SIZE ERROR ").append(notOnSizeError.toSourceString(pretty)).append(" ");
		
		if (notSizeError != null)
			result.append("NOT SIZE ERROR ").append(notSizeError.toSourceString(pretty)).append(" ");
		
		if (endadd)
			result.append("END-ADD");
		else
			result.setLength(result.length() - 1);
		
		return result.toString();
	}

	@Override public String toTreeString(boolean pretty) {
		StringBuilder result = new StringBuilder();
		result.append("(stmtADDimperative ADD ");
		if (corresponding) result.append("(correspondingPhrase CORRESPONDING) ");
		if (corr) result.append("(correspondingPhrase CORR) ");
		
		if (giving > 0) {
			
			int i = 0;
			if (to > 0) {
				for(; i < to; i++)
					result.append(subjects.get(i).toTreeString(pretty)).append(" ");
				
				result.append("TO ");
			}
			
			for(; i < giving; i++)
				result.append(subjects.get(i).toTreeString(pretty)).append(" ");
			
			result.append("(givingPhrase ");
			
			to = giving;
		} else if (to > 0) {
			for(int i = 0; i < to; i++)
				result.append(subjects.get(i).toTreeString(pretty)).append(" ");
			
			result.append("TO ");
		} else
			throw new IllegalStateException();
		
		for(int i = to; i < subjects.size(); i++) {
			result.append("(roundedPhrase ");
			result.append(subjects.get(i).toTreeString(pretty));
			if (rounded.get(i))
				result.append(" ROUNDED");
			result.append(") ");
		}

		result.setLength(result.length() - 1);
		
		if (giving > 0)
			result.append(")");
		
		result.append(") ");

		if ((onSizeError != null || sizeError != null || notOnSizeError != null || notSizeError != null) || endadd) {
			StringBuilder outer = new StringBuilder();
			
			outer.append("(stmtADDconditional ");
			outer.append(result);
			
			if (onSizeError != null || sizeError != null || notOnSizeError != null || notSizeError != null) {
				outer.append("(sizeErrorPhrases ");
				
				if (onSizeError != null)
					outer.append("(onSizeErrorPhrase ON SIZE ERROR ").append(onSizeError.toTreeString(pretty)).append(") ");
				
				if (sizeError != null)
					outer.append("(onSizeErrorPhrase SIZE ERROR ").append(sizeError.toTreeString(pretty)).append(") ");
				
				if (notOnSizeError != null)
					outer.append("(notOnSizeErrorPhrase NOT ON SIZE ERROR ").append(notOnSizeError.toTreeString(pretty)).append(") ");
				
				if (notSizeError != null)
					outer.append("(notOnSizeErrorPhrase NOT SIZE ERROR ").append(notSizeError.toTreeString(pretty)).append(") ");
				
				outer.setLength(outer.length() - 1);
			} else
				outer.append("sizeErrorPhrases");
			
			outer.append(") ");
			
			result = outer;
		}
		
		if (endadd) {
			StringBuilder outer = new StringBuilder();
			
			outer.append("(stmtADDdelimitedScope ");
			outer.append(result);
			outer.append("END-ADD)");
			
			result = outer;
		} else
			result.setLength(result.length() - 1);
		
		return result.toString();
	}
	
	private static LanguageBuilder STOPRUN() {
		return new LanguageBuilder() {
			
			@Override
			public String toTreeString(boolean pretty) {
				return "(stmtSTOPRUN STOP RUN)";
			}
			
			@Override
			public String toSourceString(boolean pretty) {
				return "STOP RUN";
			}
		};
	}
}