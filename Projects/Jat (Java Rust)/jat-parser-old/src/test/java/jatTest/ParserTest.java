package jatTest;

import com.github.javaparser.ParserConfiguration.LanguageLevel;

import jat.Parser;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class ParserTest extends TestCase {
	public ParserTest(final String testName) {
		super(testName);
	}

	public static Test suite() {
		return new TestSuite(ParserTest.class);
	}

	public void testParser() {
		final Parser parser = new Parser("/home/fred/Eclipse/jat-parser", LanguageLevel.JAVA_1_3);
		parser.parseProject();
		final String log = parser.getLog();
		System.err.println("== LOG =====" + System.lineSeparator() + log + "============" + System.lineSeparator());
		assertEquals(parser.getLog(), "");
	}
}
