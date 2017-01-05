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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.Reader;

import org.antlr.v4.runtime.Token;
import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

import br.eti.rslemos.alpendre.printer.ParseTreePrettyPrinter;
import br.eti.rslemos.alpendre.printer.TokenPrettyPrinter;
import br.eti.rslemos.cobolg.COBOLParser.BatchContext;

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
	
	@Option(name = "-stream", usage = "don't read the entire file upfront (implied if file has more than 1Mb)")
	public boolean stream;
	
	@Argument(metaVar = "file", usage = "file input")
	public String file;

	private transient CmdLineParser parser;

	private Compiler compiler;

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
			input = new FileInputStream(file);
		} else {
			basename = "stdin";
			input = System.in;
		}

		collect = new CollectErrorListener(basename);

		createCompiler(new InputStreamReader(input));

		mayPrintTokens();
		BatchContext batch = compile();
		mayPrintTree(batch);
		
		collect.verify();
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
		return compiler.compile();
	}

	private void createCompiler(Reader source) throws IOException {
		compiler = getCompiler(source);
		compiler.addErrorListener(collect);
	}

	protected Compiler getCompiler(Reader source) throws IOException {
		if (fixed)
			return new Compiler.FixedFormatCompiler(source);
		
		if (free)
			return new Compiler.FreeFormatCompiler(source);
		
		throw new Error();
	}
}
