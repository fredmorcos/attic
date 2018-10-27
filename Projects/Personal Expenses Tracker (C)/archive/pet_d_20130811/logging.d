/*
 * This file is part of PET.
 *
 * PET is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * PET is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with PET.  If not, see <http://www.gnu.org/licenses/>.
 */

import std.stdio;

class Logger {
  @property bool enableVerbose () { return m_enableVerbose; }
  @property bool enableVerbose (bool value) { return m_enableVerbose = value; }
  @property bool enableDebug () { return m_enableDebug; }
  @property bool enableDebug (bool value) { return m_enableDebug = value; }

  void writeWarning(S...)(S args) {
    stderr.writeln("Warning: ", args);
  }

  void writeError(S...)(S args) {
    stderr.writeln("Error: ", args);
  }

  void writeInfo(S...)(S args) {
    if (m_enableVerbose)
      stderr.writeln("Info: ", args);
  }

  void writeDebug(S...)(S args) {
    if (m_enableDebug)
      stderr.writeln("Debug: ", args);
  }

private:
  bool m_enableVerbose = false;
  bool m_enableDebug = false;
}
