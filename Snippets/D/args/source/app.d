import std.stdio;
import std.getopt;
import std.file;

enum Verbosity {
  Quiet,
  Verbose,
  Debug
};

void printVersion () {
}

void printHelp () {
}

int main (string[] args) {
  Verbosity verbose = Verbosity.Quiet;
  string inputFilename = "-";
  string outputFilename = "-";
  string inputData;
  char[] outputData;

  void verbose_handler (string opt) {
    switch (opt) {
    case "q": verbose = Verbosity.Quiet;   break;
    case "v": verbose = Verbosity.Verbose; break;
    case "d": verbose = Verbosity.Debug;   break;
    default:
    }
  }

  try {
    getopt(args,
           "q", &verbose_handler,
           "v", &verbose_handler,
           "d", &verbose_handler,
           "i", &inputFilename,
           "o", &outputFilename);
  } catch (Exception e) {
    stderr.writeln("Error: ", e.msg);
    return 1;
  }

  try {
    inputData = cast(string) read(inputFilename);
  } catch (Exception e) {
    stderr.writeln("Error: ", e.msg);
    return 1;
  }

  writeln(inputData);

  outputData[] = inputData;

  outputData[0] = '!';

  return 0;
}
