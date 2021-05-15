#include <cassert>
#include <iostream>
#include <memory>
#include <vector>

using namespace std;

class Location {
  friend class Scanner;
  friend class Span;

  uint16_t m_index;
  uint16_t m_line;
  uint16_t m_column;

  Location() : m_index(0), m_line(1), m_column(1) {}

  void increment() {
    if (m_index == UINT16_MAX || m_column == UINT16_MAX) {
      // TODO Correct overflow exception.
      throw std::exception();
    }

    m_index++;
    m_column++;
  }

  void newline() {
    if (m_index == UINT16_MAX || m_line == UINT16_MAX) {
      // TODO Correct overflow exception.
      throw std::exception();
    }

    m_index++;
    m_column = 0;
    m_line++;
  }

public:
  friend std::ostream &operator<<(std::ostream &os, const Location &obj);
};

std::ostream &operator<<(std::ostream &os, const Location &obj) {
  os << obj.m_line << ":" << obj.m_column;
  return os;
}

class Span {
  friend class Scanner;

  shared_ptr<vector<char>> m_buffer;

  Location m_begin;
  Location m_end;

  Span(shared_ptr<vector<char>> buffer, Location begin, Location end)
      : m_buffer(buffer), m_begin(std::move(begin)), m_end(std::move(end)) {}

  auto begin() const { return m_buffer->begin() + m_begin.m_index; }
  auto end() const { return m_buffer->begin() + m_end.m_index; }

public:
  friend std::ostream &operator<<(std::ostream &os, const Span &obj);
};

std::ostream &operator<<(std::ostream &os, const Span &obj) {
  os << string{obj.begin(), obj.end()};
  return os;
}

class Scanner {
  shared_ptr<vector<char>> m_buffer;
  Location m_location;

  Location m_start;
  bool m_recording;

public:
  Scanner(vector<char> buffer)
      : m_buffer(std::make_shared<vector<char>>(std::move(buffer))),
        m_location(Location{}), m_start(Location{}), m_recording(false) {}

  shared_ptr<vector<char>> buffer() { return m_buffer; }
  Location location() const { return m_location; }

  char next() {
    if (m_location.m_index > m_buffer->size() - 1) {
      // TODO Correct out of bounds exception.
      throw std::exception();
    }

    char c = m_buffer->at(m_location.m_index);

    if (c == '\n') {
      m_location.newline();
    } else {
      m_location.increment();
    }

    return c;
  }

  void startLocationRecording() {
    if (m_recording) {
      // TODO Use correct exception for bad-state.
      throw std::exception();
    }

    m_recording = true;
    m_start = m_location;
  }

  Span stopLocationRecording() {
    if (!m_recording) {
      // TODO Use correct exception for bad-state.
      throw std::exception();
    }

    m_recording = false;
    return Span{m_buffer, m_start, m_location};
  }
};

int main() {
  string contents = "hello world";
  vector<char> buffer{contents.begin(), contents.end()};
  Scanner scanner = Scanner{std::move(buffer)};

  assert(scanner.next() == 'h');
  assert(scanner.next() == 'e');

  scanner.startLocationRecording();
  assert(scanner.next() == 'l');
  assert(scanner.next() == 'l');
  assert(scanner.next() == 'o');
  assert(scanner.next() == ' ');
  assert(scanner.next() == 'w');
  Span span = scanner.stopLocationRecording();

  assert(scanner.next() == 'o');

  std::cout << span << std::endl;

  return 0;
}
