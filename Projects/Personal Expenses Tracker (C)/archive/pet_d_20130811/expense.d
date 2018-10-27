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

import std.datetime;

class Expense {
  @property double   amount () { return m_amount; }
  @property double   amount (double value) { return m_amount = value; }
  @property Date     date () { return m_date; }
  @property Date     date (Date value) { return m_date = value; }
  @property string[] tags () { return m_tags; }
  @property string[] tags (string[] value) { return m_tags = value; }
  @property string   note () { return m_note; }
  @property string   note (string value) { return m_note = value; }
  @property string   person () { return m_person; }
  @property string   person (string value) { return m_person = value; }
  @property string   shop () { return m_shop; }
  @property string   shop (string value) { return m_shop = value; }

private:
  double   m_amount;
  Date     m_date;
  string[] m_tags;
  string   m_note;
  string   m_person;
  string   m_shop;
}
