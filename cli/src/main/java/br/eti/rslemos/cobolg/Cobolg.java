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

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.Reader;
import java.util.EnumSet;
import java.util.List;

import objectexplorer.Chain;
import objectexplorer.ObjectExplorer;
import objectexplorer.ObjectExplorer.Feature;
import objectexplorer.ObjectGraphMeasurer;
import objectexplorer.ObjectGraphMeasurer.Footprint;
import objectexplorer.ObjectVisitor;

import org.antlr.v4.runtime.Token;
import org.apache.commons.io.IOUtils;
import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

import br.eti.rslemos.alpendre.printer.ParseTreePrettyPrinter;
import br.eti.rslemos.alpendre.printer.TokenPrettyPrinter;
import br.eti.rslemos.cobolg.COBOLParser.BatchContext;

import com.google.common.base.Predicate;
import com.google.common.base.Predicates;
import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;

public class Cobolg {
	@Option(name = "-h", aliases = {"--help"})
	public boolean help;
	
	@Option(name = "-fixed", forbids = {"--free"}, usage = "input is in fixed format")
	public boolean fixed;
	
	@Option(name = "-free", forbids = {"--fixed"}, usage = "input is in free format")
	public boolean free;
	
	@Option(name = "-printTokens", usage = "print a list of tokens")
	public boolean printTokens;
	
	@Option(name = "-printTree", usage = "print the parse tree")
	public boolean printTree;
	
	@Option(name = "-lineNumbers", usage = "print line numbers along with parse tree")
	public boolean lineNumbers;
	
	@Option(name = "-printTimeStats", usage = "print the time statistics")
	public boolean printTimeStats;
	
	@Option(name = "-printMemStats", usage = "print memory usage statistics")
	public boolean printMemStats;
	
	@Option(name = "-profileExec", usage = "profile execution (call path)")
	public boolean profileExec;
	
	@Option(name = "-profileMem", usage = "profile memory usage")
	public boolean profileMem;
	
	@Option(name = "-profileGC", usage = "profile the garbage collector")
	public boolean profileGC;
	
	@Option(name = "-stream", usage = "don't read the entire file upfront (implied if file has more than 1Mb)")
	public boolean stream;
	
	@Argument(metaVar = "file", usage = "file input")
	public String file;

	private transient CmdLineParser parser;

	private long fileLength;
	private Compiler compiler;

	private long nanoReadFile = -1;
	private long nanoLexer;
	private long nanoParser;

	private static final Predicate<Object> notLexer = Predicates.not(Predicates.instanceOf(COBOLLexer.class));
	private static final Predicate<Object> notParser = Predicates.not(Predicates.instanceOf(COBOLParser.class));
	
	private Footprint lexerFootprintAfter = null;
	
	private Footprint mainParserFootprintBefore = null;
	private Footprint mainParserFootprintAfter;
	
	private Footprint preParserFootprintBefore = null;
	private Footprint preParserFootprintAfter;
	
	private Footprint tokensFootprint;
	private Footprint programFootprint;

	private CollectErrorListener collect;

	public static void main(String[] args) throws CmdLineException, IOException {
		new Cobolg().parseArguments(args).run();
	}

	private Cobolg parseArguments(String... args) throws CmdLineException {
		parser = new CmdLineParser(this);
		parser.parseArgument(args);
		
		return this;
	}

	private void run() throws IOException {
		if (help) {
			parser.printSingleLineUsage(System.out);
			parser.printUsage(System.out);
			System.exit(0);
		}

		run(file);
	}

	private static String basename(File file) {
		String fullname = file.getPath();
		return fullname.substring(fullname.lastIndexOf('/') + 1);
	}
	
	public void run(String filename) throws IOException {
		String basename;
		InputStream input;
		
		if (filename != null && !filename.equals("-")) {
			File file = new File(filename);
			basename = basename(file);
			input = open(file);
		} else {
			basename = "stdin";
			input = openstdin();
		}

		collect = new CollectErrorListener(basename);

		createCompiler(new InputStreamReader(input));

		mayPrintTokens();
		BatchContext batch = compile();
		mayPrintTree(batch);
		mayPrintTimeStats();
		mayPrintMemStats();
		
		final Predicate<Chain> predicate = new ObjectExplorer.AtMostOncePredicate();

		ObjectExplorer.exploreObject(program, new ObjectVisitor<Void>() {

			@Override
			public Void result() { return null; }

			@Override
			public objectexplorer.ObjectVisitor.Traversal visit(Chain chain) {
				if (chain.isPrimitive())
					return Traversal.SKIP;
				
				if (!predicate.apply(chain))
					return Traversal.SKIP;
				
				if (chain.getValue() != null)
					System.out.println(chain.getValueType());
				
				return Traversal.EXPLORE;
			}
			
		}, EnumSet.noneOf(Feature.class));
		collect.verify();
	}

	private void mayPrintMemStats() {
		if (printMemStats) {
			System.out.println("** Main parser (before parsing):");
			System.out.println(mainParserFootprintBefore.toString());
			
			System.out.println("** Pre parser (before parsing):");
			System.out.println(preParserFootprintBefore.toString());

			System.out.println("** Lexer (after fill):");
			System.out.println(lexerFootprintAfter.toString());
			
			System.out.println("** Main parser (after parsing):");
			System.out.println(mainParserFootprintAfter.toString());
			
			System.out.println("** Pre parser (after parsing):");
			System.out.println(preParserFootprintAfter.toString());
			
			System.out.println("** Tokens:");
			System.out.println(tokensFootprint.toString());
			
			System.out.println("** Tree:");
			System.out.println(programFootprint.toString());
		}
	}

	private void mayPrintTimeStats() {
		if (printTimeStats) {
			System.out.printf("IO: %dms\n", nanoReadFile / 1000000);
			System.out.printf("Lexer: %dms\n", nanoLexer / 1000000);
			System.out.printf("Parser: %dms\n", nanoParser / 1000000);
		}
	}

	private void mayPrintTree(BatchContext batch) throws IOException {
		if (printTree) {
			PrintStream out = new PrintStream(System.out, true, "UTF-8");

			new ParseTreePrettyPrinter(out, compiler.mainParser).printTree(batch);
		}
	}

	private void mayPrintTokens() throws IOException {
		if (printTokens) {
			PrintStream out = new PrintStream(System.out, true, "UTF-8");

			String[] channelNames = new String[] {"BLANK", "MARK", "COMPILE", "DEFAULT"};
			int[] channelMap = new int[] {3, 0, 1, 2};
			TokenPrettyPrinter tokenPrinter = new TokenPrettyPrinter(TokenPrettyPrinter.BOXMODEL, out, compiler.lexer.getVocabulary(), channelNames, channelMap);
			compiler.lexer.reset();
			tokenPrinter.printTokens(compiler.lexer.getAllTokens());
		}
	}

	private BatchContext compile() throws IOException {
		long before = System.nanoTime();
		BatchContext program = compiler.compile();
		long after = System.nanoTime();
		nanoParser = after - before;
		
		if (printMemStats) {
			mainParserFootprintAfter = ObjectGraphMeasurer.measure(compiler.mainParser, notLexer);
			preParserFootprintAfter = ObjectGraphMeasurer.measure(compiler.preParser, notLexer);
			programFootprint = ObjectGraphMeasurer.measure(program, Predicates.and(notLexer, notParser));
		}
		
		return program;
	}

	private void createCompiler(Reader source) throws IOException {
		long before = System.nanoTime();
		
		compiler = getCompiler(source);
		compiler.addErrorListener(collect);
		List<? extends Token> tokens = compiler.lexer.getAllTokens();
		
		long after = System.nanoTime();
		nanoLexer = after - before;
		
		compiler.lexer.reset();
		
		if (printMemStats) {
			lexerFootprintAfter = ObjectGraphMeasurer.measure(compiler.lexer);
			mainParserFootprintBefore = ObjectGraphMeasurer.measure(compiler.mainParser, notLexer);
			preParserFootprintBefore = ObjectGraphMeasurer.measure(compiler.preParser, notLexer);

			int objects = 0;
			int references = 0;
			Multiset<Class<?>> primitives = HashMultiset.create();
			
			for (Token token : tokens) {
				Footprint footprint = ObjectGraphMeasurer.measure(token, notLexer);
				objects += footprint.getObjects();
				references += footprint.getReferences();
				primitives.addAll(footprint.getPrimitives());
			}
			tokensFootprint = new Footprint(objects, references, primitives);
		}
	}

	private InputStream open(File file) throws FileNotFoundException, IOException {
		fileLength = file.length();
		return open0(new FileInputStream(file));
	}

	private InputStream openstdin() throws IOException {
		return open0(System.in);
	}

	private InputStream open0(InputStream input) throws IOException {
		if (stream || fileLength < (1<<20)) {
			long before = System.nanoTime();
			byte[] data = IOUtils.toByteArray(input);
			fileLength = data.length;
			input = new ByteArrayInputStream(data);
			long after = System.nanoTime();
			nanoReadFile = after - before;
		} else {
			fileLength = -1;
			input = new BufferedInputStream(input);
		}
		
		return input;
	}

	protected Compiler getCompiler(Reader source) throws IOException {
		if (fixed)
			return new Compiler.FixedFormatCompiler(source);
		
		if (free)
			return new Compiler.FreeFormatCompiler(source);
		
		throw new Error();
	}
}
