#ifndef RFPF_LOGGER_H
#define RFPF_LOGGER_H

#include <iostream>
#include <string>
#include <vector>

using std::pair;
using std::vector;
using std::string;
using std::ostream;

enum Error {
  Success = 0,

  EngineFileNoExtension           = 10,
  EngineFileIsDirectory           = 11,
  EngineFileNotAFile              = 12,
  EngineFileDoesNotExist          = 13,
  EngineFileGeneralError          = 14,
  EngineFileExtensionNotSupported = 15,
  EngineFileParsingFailed         = 16,

  SearchRoutesError = 100,
};

class Logger {
 private:
  vector<pair<ostream&, ostream&>> streams;
  string tab;
  unsigned int nWarnings;
  unsigned int nInfos;

 public:
  explicit Logger(const bool consoleOut = true);

  void addStream(ostream& out);
  void addStreamPair(ostream& out, ostream& err);

  void debug(const string& msg);
  void info(const string& msg);
  void warn(const enum Error type, const string& msg);
  void error(const enum Error type, const string& msg);

  void operator++();
  void operator--();

  unsigned int warnings();
  unsigned int infos();

  void resetWarnings();
  void resetInfos();

  void resetCounters();
};

#endif
