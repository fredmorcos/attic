#include "Logger.hpp"
#include <cassert>
#include <vector>

using std::cout;
using std::cerr;
using std::endl;

Logger::Logger(const bool consoleOut) : tab(""), nWarnings(0), nInfos(0) {
  if (!consoleOut)
    return;

  streams.push_back(pair<ostream&, ostream&>(cout, cerr));
}

void Logger::addStream(ostream& out) {
  streams.push_back(pair<ostream&, ostream&>(out, out));
}

void Logger::addStreamPair(ostream& out, ostream& err) {
  streams.push_back(pair<ostream&, ostream&>(out, err));
}

void Logger::debug(const string& msg) {
  for (auto const& s : streams)
    s.second << "D: " << tab << msg << endl;
}

void Logger::info(const string& msg) {
  for (auto const& s : streams)
    s.first << "I: " << tab << msg << endl;
}

void Logger::warn(const enum Error type, const string& msg) {
  for (auto const& s : streams)
    s.second << "W: " << tab << msg << " (" << type << ")" << endl;
}

void Logger::error(const enum Error type, const string& msg) {
  for (auto const& s : streams)
    s.second << "E: " << tab << msg << " (" << type << ")" << endl;
}

void Logger::operator++() {
  tab = string(tab.length() + 1, ' ');
}

void Logger::operator--() {
  assert(tab.length() > 0);
  tab = string(tab.length() - 1, ' ');
}

unsigned int Logger::warnings() {
}

unsigned int Logger::infos() {
}

void Logger::resetWarnings() {
}

void Logger::resetInfos() {
}

void Logger::resetCounters() {
}
