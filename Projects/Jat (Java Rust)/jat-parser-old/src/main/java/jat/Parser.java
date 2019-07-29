package jat;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import com.github.javaparser.ParseResult;
import com.github.javaparser.ParserConfiguration.LanguageLevel;
import com.github.javaparser.Problem;
import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.symbolsolver.JavaSymbolSolver;
import com.github.javaparser.symbolsolver.resolution.typesolvers.CombinedTypeSolver;
import com.github.javaparser.symbolsolver.resolution.typesolvers.ReflectionTypeSolver;
import com.github.javaparser.symbolsolver.utils.SymbolSolverCollectionStrategy;
import com.github.javaparser.utils.ProjectRoot;
import com.github.javaparser.utils.SourceRoot;

public class Parser {
	private final Path projectPath;
	private final LanguageLevel languageVersion;
	private final StringBuilder logBuilder;

	private final String lineSeparator = System.lineSeparator();

	public Parser(final String projectPath, final LanguageLevel version) {
		this.projectPath = Paths.get(projectPath);
		this.languageVersion = version;
		this.logBuilder = new StringBuilder();
	}

	public String getLog() {
		final String log = this.logBuilder.toString();
		this.logBuilder.setLength(0);
		return log;
	}

	public ConcurrentHashMap<String, CompilationUnit> parseProject() {
		final CombinedTypeSolver combinedTypeSolver = new CombinedTypeSolver();
		combinedTypeSolver.add(new ReflectionTypeSolver());
		StaticJavaParser.getConfiguration().setSymbolResolver(new JavaSymbolSolver(combinedTypeSolver));
		StaticJavaParser.getConfiguration().setLanguageLevel(languageVersion);

//		final ProjectRoot root = new ParserCollectionStrategy(StaticJavaParser.getConfiguration()).collect(directory);
		final ProjectRoot root = new SymbolSolverCollectionStrategy(StaticJavaParser.getConfiguration())
				.collect(this.projectPath);

		this.logBuilder.append("Project Root: " + root.getRoot() + this.lineSeparator);

		final ConcurrentHashMap<String, CompilationUnit> compilationUnits = new ConcurrentHashMap<>();

		final SourceRoot.Callback handle = (final Path path, final Path absPath,
				final ParseResult<CompilationUnit> parseRes) -> {
			if (parseRes.isSuccessful()) {
				this.logBuilder.append("    " + path + ": Success" + this.lineSeparator);

				if (compilationUnits.containsKey(path.toString())) {
					this.logBuilder.append("    Error: " + path + " already found in collection" + this.lineSeparator);
					return SourceRoot.Callback.Result.TERMINATE;
				}

				compilationUnits.put(path.toString(), parseRes.getResult().get());
			} else {
				final List<Problem> problems = parseRes.getProblems();

				if (problems.isEmpty()) {
					this.logBuilder.append("    " + path + ": Errors." + this.lineSeparator);
				} else {
					this.logBuilder.append("    " + path + ": Errors:" + this.lineSeparator);

					parseRes.getProblems().forEach(problem -> {
						this.logBuilder.append("      Error: " + problem.getMessage() + this.lineSeparator);
					});
				}
			}

			return SourceRoot.Callback.Result.DONT_SAVE;
		};

		root.getSourceRoots().forEach(srcRoot -> {
			this.logBuilder.append("  Source Root: " + srcRoot.getRoot() + this.lineSeparator);

			srcRoot.setParserConfiguration(StaticJavaParser.getConfiguration());

			try {
				srcRoot.parseParallelized(handle);
			} catch (final IOException e) {
				this.logBuilder.append(": IO Error: " + e.toString() + this.lineSeparator);
			}
		});

		this.logBuilder.append("Collected " + compilationUnits.size() + " compilation units." + this.lineSeparator);
		return compilationUnits;
	}
}
