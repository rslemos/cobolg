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

import java.io.IOException;

import java.io.Reader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Formatter;
import java.util.List;
import java.util.ListIterator;

import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.DefaultErrorStrategy;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.misc.Interval;
import org.antlr.v4.runtime.tree.AbstractParseTreeVisitor;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import br.eti.rslemos.cobolg.COBOLParser.CompilerStatementContext;
import br.eti.rslemos.cobolg.COBOLParser.CompilerStatementsContext;
import br.eti.rslemos.cobolg.COBOLParser.ProgramContext;

import static java.lang.String.format;

public abstract class Compiler {
	
	final Logger logger = LoggerFactory.getLogger(Compiler.class);
	
	final Lexer lexer;
	
	public final COBOLParser preParser;
	public final COBOLParser mainParser;
	
	private Compiler (Lexer lexer) {
		this.lexer = lexer;
		this.lexer.removeErrorListeners();

		lexer.reset();
		CommonTokenStream preTokens = new CommonTokenStream(lexer, COBOLFreeFormatLexer.COMPILER_CHANNEL);
		preTokens.fill();
		
		lexer.reset();
		CommonTokenStream mainTokens = new CommonTokenStream(lexer);
		mainTokens.fill();
		
		preParser = setup(new COBOLParser(preTokens));
		mainParser = setup(new COBOLParser(mainTokens));
	}

	private static <R extends Parser> R setup(R parser) {
		parser.removeErrorListeners();
		parser.setErrorHandler(new DefaultErrorStrategy());
		//parser.getInterpreter().setPredictionMode(PredictionMode.LL);
		parser.setBuildParseTree(true);
		
		return parser;
	}

	public ProgramContext compile() throws IOException {
		CompilerStatementsContext preTree = this.preParser.compilerStatements();
		ProgramContext mainTree = this.mainParser.program();
		
		preProcess(preTree, mainTree);
		
		return mainTree;
	}
	
	public void preProcess(CompilerStatementsContext preTree, ParserRuleContext mainTree) {
		List<Neighbor<TerminalNode>> nodes = findCircumNodesTo(preTree.compilerStatement(), mainTree);
	
		for (int i = 0; i < nodes.size(); i++) {
			CompilerStatementContext statement = preTree.compilerStatement(i);
			injectCompilerStatement(mainTree, statement, nodes.get(i));
		}
	}

	private List<Neighbor<TerminalNode>> findCircumNodesTo(List<CompilerStatementContext> statements, ParserRuleContext mainTree) {
		List<Neighbor<TerminalNode>> result = new ArrayList<Neighbor<TerminalNode>>();
		
		List<TerminalNode> mainNodes = new FlattenTree().visit(mainTree);
		
		// iterate backwards
		ListIterator<TerminalNode> mainIt = mainNodes.listIterator(mainNodes.size());
		ListIterator<CompilerStatementContext> stmtIt = statements.listIterator(statements.size());
		
		while (stmtIt.hasPrevious()) {
			Interval stmtInterval = stmtIt.previous().getSourceInterval();
			
			while (mainIt.hasPrevious()) {
				TerminalNode node = mainIt.previous();
				Interval nodeInterval = node.getSourceInterval();
				
				if (!stmtInterval.startsBeforeDisjoint(nodeInterval))
					break;
			}
			
			// hasPrevious() implies hasNext()
			// (if it has previous now, it had also before, so the loop above
			// executed at least once; so at least one previous() returned an
			// element without throwing exception; so at least one next() will
			// also return [the last previous()] without throwing exception)
			result.add(new Neighbor<TerminalNode>(
					/* left */  mainIt.hasPrevious() /*&& mainIt.hasNext()*/ ? mainIt.next() : null,
					/* right */ mainIt.hasNext() ? mainIt.next() : null
				));
		}

		Collections.reverse(result);

		if (logger.isDebugEnabled())
			debug(statements, mainNodes, result);
		
		return result;
	}

	private void debug(List<CompilerStatementContext> statements, List<TerminalNode> mainNodes, List<Neighbor<TerminalNode>> result) {
		logger.debug("== findCircumNodesTo ==========================================================");

		showInput(statements);
		showRelationships(statements, mainNodes);
		showReturn(statements, result);
	}

	private void showInput(List<CompilerStatementContext> statements) {
		for (CompilerStatementContext stmt : statements)
			logger.debug(format("[%8s]: %s", stmt.getSourceInterval(), stmt.toStringTree(preParser)).toString());
	}

	@SuppressWarnings("resource")
	private void showRelationships(List<CompilerStatementContext> statements, List<TerminalNode> mainNodes) {
		logger.debug("-- statement X main node ------------------------------------------------------");
		
		Formatter header = new Formatter();
		header.format("%42s", "");
		for (CompilerStatementContext stmt : statements)
			header.format(" [%8s]", stmt.getSourceInterval());
		
		logger.debug(header.toString());
		
		logger.debug("-------------------------------------------------------------------------------");
		
		for (TerminalNode node : mainNodes) {
			Formatter line = new Formatter();
			
			Interval nodeInterval = node.getSourceInterval();
			line.format("[%8s]: %30s", nodeInterval, node);

			for (CompilerStatementContext stmt : statements) {
				Interval stmtInterval = stmt.getSourceInterval();
				
				boolean startsAfterDisjoint = stmtInterval.startsAfterDisjoint(nodeInterval);
				line.format(" %10s", startsAfterDisjoint ? "after" : "before");
			}
			
			logger.debug(line.toString());
		}
	}

	private void showReturn(List<CompilerStatementContext> statements, List<Neighbor<TerminalNode>> result) {
		logger.debug("-- return ---------------------------------------------------------------------");
		for (int i = 0; i < statements.size(); i++) {
			CompilerStatementContext stmt = statements.get(i);
			Neighbor<TerminalNode> neighbor = result.get(i);
			
			logger.debug(format("[%8s] [%8s] [%8s] - %s %s %s",
					neighbor.left != null ? neighbor.left.getSourceInterval() : "********",
					stmt.getSourceInterval(), 
					neighbor.right != null ? neighbor.right.getSourceInterval() : "********",
					neighbor.left, stmt.toStringTree(preParser), neighbor.right));
		}
	}

	private void injectCompilerStatement(ParserRuleContext mainTree, CompilerStatementContext statement, Neighbor<TerminalNode> neighbor) {
		Interval targetInterval = statement.getSourceInterval();

		ParserRuleContext rule = findRuleToInject(mainTree, neighbor.left, neighbor.right, targetInterval);
		statement.parent = rule;

		ListIterator<ParseTree> it = findPositionToInject(targetInterval, rule.children.listIterator());
		it.add(statement);
	}

	private ParserRuleContext findRuleToInject(ParserRuleContext mainTree, TerminalNode left, TerminalNode right, Interval targetInterval) {
		if (right == null) {
			// attach to the root (after the previous compilerStatements)
			return mainTree;
		} else {
			// known to be not null
			ParserRuleContext rule = (ParserRuleContext) right.getParent();
			
			while (!rule.getSourceInterval().properlyContains(targetInterval))
				rule = rule.getParent();
			
			return rule;
		}
	}
	
	private ListIterator<ParseTree> findPositionToInject(Interval targetInterval, ListIterator<ParseTree> it) {
		while (it.hasNext()) {
			Interval candidateInterval = it.next().getSourceInterval();
			
			if (candidateInterval.startsAfter(targetInterval)) {
				it.previous();
				break;
			}
		}
		
		return it;
	}
	
	public void addErrorListener(ANTLRErrorListener listener) {
		lexer.addErrorListener(listener);
		mainParser.addErrorListener(listener);
		preParser.addErrorListener(listener);
	}
	
	public static class FreeFormatCompiler extends Compiler {
		public FreeFormatCompiler(Reader source) throws IOException {
			super(new COBOLFreeFormatLexer(forANTLR(source)));
		}
	}

	public static class FixedFormatCompiler extends Compiler {
		public FixedFormatCompiler(Reader source) throws IOException {
			super(new COBOLFixedFormatLexer(forANTLR(stuffFixedWidthChars(source))));
		}

		private static StuffingReader stuffFixedWidthChars(Reader source) {
			return new StuffingReader(source, 0, '\uEBA0', 6, '\uEBA1', 7, '\uEBA2', 72, '\uEBA3'/*, 80, '\uEBA4'*/);
		}
	}
	
	private static ANTLRInputStream forANTLR(Reader source) throws IOException {
		return new ANTLRInputStream(source);
	}
}

class FlattenTree extends AbstractParseTreeVisitor<List<TerminalNode>> {
	private List<TerminalNode> flat = new ArrayList<TerminalNode>();
	
	@Override
	public List<TerminalNode> visitTerminal(TerminalNode thisNode) {
		flat.add(thisNode);
		return flat;
	}

	@Override
	protected List<TerminalNode> defaultResult() {
		return flat;
	}
}

class Neighbor<T> {
	public final T left, right;
	
	public Neighbor(T left, T right) {
		this.left = left;
		this.right = right;
	}
}
