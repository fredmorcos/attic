package main;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.json.Json;
import javax.json.stream.JsonGenerator;
import javax.json.stream.JsonGeneratorFactory;

import com.github.javaparser.JavaParser;
import com.github.javaparser.ParseResult;
import com.github.javaparser.ParserConfiguration;
import com.github.javaparser.ParserConfiguration.LanguageLevel;
import com.github.javaparser.Problem;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.serialization.JavaParserJsonSerializer;
import com.github.javaparser.symbolsolver.JavaSymbolSolver;
import com.github.javaparser.symbolsolver.resolution.typesolvers.CombinedTypeSolver;
import com.github.javaparser.symbolsolver.resolution.typesolvers.ReflectionTypeSolver;

public class Parser {
	private final CombinedTypeSolver typeSolver = new CombinedTypeSolver(new ReflectionTypeSolver(false));
	private final ParserConfiguration config = new ParserConfiguration();
	private final JavaParser parser = new JavaParser(config);

	private final String lineSep = System.lineSeparator();
	private final StringBuilder logBuilder = new StringBuilder();

	private String result = null;
	private int returnCode = 0;

	public Parser() {
		this.config.setLanguageLevel(LanguageLevel.JAVA_8);
		this.config.setSymbolResolver(new JavaSymbolSolver(typeSolver));
	}

	public String getLog() {
		final String log = this.logBuilder.toString();
		this.logBuilder.setLength(0);
		return log;
	}

	public int getReturnCode() {
		final int returnCode = this.returnCode;
		this.returnCode = 0;
		return returnCode;
	}

	public String getResult() {
		final String result = this.result;
		this.result = null;
		return result;
	}

	public void parse(final String filename) {
		final File file = new File(filename);

		try {
			final ParseResult<CompilationUnit> result = parser.parse(file);

			if (result.isSuccessful()) {
				final Map<String, ?> config = new HashMap<>();
				config.put(JsonGenerator.PRETTY_PRINTING, null);
				final JsonGeneratorFactory generatorFactory = Json.createGeneratorFactory(config);

				final JavaParserJsonSerializer json = new JavaParserJsonSerializer();
				final StringWriter writer = new StringWriter();

				try (final JsonGenerator generator = generatorFactory.createGenerator(writer)) {
					json.serialize(result.getResult().get(), generator);
				}

				this.result = writer.toString();

				return;
			}

			this.returnCode = 2;

			final List<Problem> problems = result.getProblems();

			if (problems.isEmpty()) {
				this.logBuilder.append("Unknown error");
			} else {
				this.logBuilder.append("Errors:" + this.lineSep);

				for (final Problem problem : problems) {
					this.logBuilder.append("  - " + problem.getMessage() + this.lineSep);
				}
			}
		} catch (final FileNotFoundException e) {
			logBuilder.append("Cannot find file: " + e.getMessage());
			this.returnCode = 1;
			return;
		}
	}
}
