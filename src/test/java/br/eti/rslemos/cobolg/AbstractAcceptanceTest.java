/*******************************************************************************
 * BEGIN COPYRIGHT NOTICE
 * 
 * This file is part of program "cobolg"
 * Copyright 2013  Rodrigo Lemos
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
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Constructor;
import java.net.URL;

import junit.framework.TestCase;
import junit.framework.TestSuite;

public abstract class AbstractAcceptanceTest extends TestCase {
	private final URL file;

	protected AbstractAcceptanceTest(URL file) {
		super(basename(file));
		this.file = file;
	}

	protected static <T extends AbstractAcceptanceTest> TestSuite suite(Class<T> clazz, String... dirs) throws Exception {
		TestSuite suite = new TestSuite(clazz.getName());

		for (String dir : dirs) {
			addFiles(clazz, suite, clazz.getResource(dir));
		}

		return suite;
	}

	private static <T extends AbstractAcceptanceTest> void addFiles(Class<T> clazz, TestSuite suite, URL base) throws Exception {
		if (base == null)
			return;

		Constructor<T> ctor = clazz.getConstructor(URL.class);

		// probably this will never work inside jar files; no problem as we are just a test
		BufferedReader list = new BufferedReader(new InputStreamReader((InputStream) base.getContent()));

		String line;
		while ((line = list.readLine()) != null) {
			URL file = new URL(base, line);
			suite.addTest(ctor.newInstance(file));
		}
	}

	private static String basename(URL file) {
		String fullname = file.getPath();
		return fullname.substring(fullname.lastIndexOf('/') + 1);
	}

	@Override
	protected void runTest() throws Throwable {
		Compiler compiler = getCompiler();
		CollectErrorListener collect = new CollectErrorListener(basename(file));
		compiler.setCustomErrorListener(collect);
		compiler.compile(new InputStreamReader(new BufferedInputStream(file.openStream())));
		collect.verify();
	}

	protected abstract Compiler getCompiler();

}
