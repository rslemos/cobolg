package br.eti.rslemos.cobolg;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.List;

import org.antlr.v4.runtime.CommonTokenFactory;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenFactory;

import br.eti.rslemos.cobolg.COBOLParser.ArithmeticExpressionContext;
import br.eti.rslemos.cobolg.COBOLParser.IdentifierContext;
import br.eti.rslemos.cobolg.COBOLParser.ImperativeStatementContext;
import br.eti.rslemos.cobolg.COBOLParser.ProceduralStatementContext;
import br.eti.rslemos.cobolg.COBOLParser.RoundedPhraseContext;
import br.eti.rslemos.cobolg.COBOLParser.StmtCOMPUTEimperativeContext;

public class StmtCOMPUTEBuilder implements ANTLRBuilder<ProceduralStatementContext> {
	public TokenFactory<?> tokenFactory = new CommonTokenFactory();
	
	public static StmtCOMPUTEBuilder COMPUTE() {
		return new StmtCOMPUTEBuilder();
	}

	public static StmtCOMPUTEBuilder COMPUTE(IdentifierContext... identifiers) {
		return COMPUTE().add(identifiers);
	}

	public static StmtCOMPUTEBuilder COMPUTE(String... names) {
		return COMPUTE().add(names);
	}

	private List<IdentifierContext> identifiers = new ArrayList<IdentifierContext>();
	private BitSet rounded = new BitSet(0);
	private int equal = 0;
	private int op_equal = 0;
	private ArithmeticExpressionContext arithmeticExpression;
	
	private StmtCOMPUTEBuilder add(List<IdentifierContext> identifiers) {
		this.identifiers.addAll(identifiers);
		return this;
	}
	
	public StmtCOMPUTEBuilder add(IdentifierContext... identifiers) {
		return add(Arrays.asList(identifiers));
	}
	
	public StmtCOMPUTEBuilder add(String... names) {
		List<IdentifierContext> identifiers = new ArrayList<IdentifierContext>();
		for (String name : names)
			identifiers.add(buildIdentifier(name));
		
		return add(identifiers);
	}
	
	public StmtCOMPUTEBuilder identifier(IdentifierContext... identifiers) {
		return add(identifiers);
	}
	
	public StmtCOMPUTEBuilder identifier(String... names) {
		return add(names);
	}
	
	public StmtCOMPUTEBuilder ROUNDED() {
		rounded.set(identifiers.size());
		return this;
	}
	
	public StmtCOMPUTEBuilder ROUNDED(String name) {
		return identifier(name).ROUNDED();
	}
	
	public StmtCOMPUTEBuilder EQUAL() {
		equal = identifiers.size();
		return this;
	}

	public StmtCOMPUTEBuilder eq() {
		op_equal = identifiers.size();
		return this;
	}
	
	public StmtCOMPUTEBuilder EQUAL(ArithmeticExpressionContext arithmeticExpression) {
		return EQUAL().expr(arithmeticExpression);
	}

	public StmtCOMPUTEBuilder eq(ArithmeticExpressionContext arithmeticExpression) {
		return eq().expr(arithmeticExpression);
	}
	
	public StmtCOMPUTEBuilder expr(ArithmeticExpressionContext arithmeticExpression) {
		this.arithmeticExpression = arithmeticExpression;
		return this;
	}
	
	@Override public ProceduralStatementContext build() {
		StmtCOMPUTEimperativeContext stmtCOMPUTEimperative = new StmtCOMPUTEimperativeContext(null, 0);
		
		stmtCOMPUTEimperative.addChild(tokenFactory.create(COBOLLexer.COMPUTE, "COMPUTE"));
		for (int i = 0; i < identifiers.size(); i++) {
			IdentifierContext identifier = identifiers.get(i);
			
			RoundedPhraseContext roundedPhrase = new RoundedPhraseContext(stmtCOMPUTEimperative, 0);
			roundedPhrase.addChild(identifier);
			identifier.parent = roundedPhrase;
			
			if (rounded.get(i))
				roundedPhrase.addChild(tokenFactory.create(COBOLLexer.ROUNDED, "ROUNDED"));
			
			stmtCOMPUTEimperative.addChild(roundedPhrase);
		}
		
		Token EQUAL;
		
		if (equal > 0)
			EQUAL = tokenFactory.create(COBOLLexer.EQUAL, "EQUAL");
		else if (op_equal > 0)
			EQUAL = tokenFactory.create(COBOLLexer.OP_EQUAL, "=");
		else
			throw new IllegalStateException();

		stmtCOMPUTEimperative.addChild(EQUAL);

		stmtCOMPUTEimperative.addChild(arithmeticExpression);
		arithmeticExpression.parent = stmtCOMPUTEimperative;
		
		ImperativeStatementContext imperativeStatement = new ImperativeStatementContext(null, 0);
		imperativeStatement.addChild(stmtCOMPUTEimperative);
		stmtCOMPUTEimperative.parent = imperativeStatement;
		
		ProceduralStatementContext proceduralStatement = new ProceduralStatementContext(null, 0);
		proceduralStatement.addChild(imperativeStatement);
		imperativeStatement.parent = proceduralStatement;
		
		return proceduralStatement;
	}

	// helper
	private IdentifierContext buildIdentifier(String name) {
		IdentifierContext identifier = new IdentifierContext(null, 0);
		identifier.addChild(tokenFactory.create(COBOLLexer.USERDEFINEDWORD, name));
		return identifier;
	}

	public static void main(String[] args) {
		ArithmeticExpressionContext arith = new ArithmeticExpressionContext(null, 0);
		
		ProceduralStatementContext proceduralStatement = COMPUTE("X").ROUNDED().EQUAL(arith).build();
		
		System.out.println(proceduralStatement.toStringTree(Arrays.asList(COBOLParser.ruleNames)));
	}
}
