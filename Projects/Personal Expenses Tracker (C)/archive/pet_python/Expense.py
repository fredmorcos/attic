# This file is part of PET.
#
# PET is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# PET is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with PET.  If not, see <http://www.gnu.org/licenses/>.

class Expense:
    def __init__(self, date, amount, tags, note):
        ESTR_BAD_AMT_VAL = "Bad amount value for an expense at date %s"
        ESTR_BAD_TAG_LST = "Bad tags list for an expense at date %s"
        ESTR_EMP_TAG_LST = "Tags list for an expense at date %s is empty"
        ESTR_INV_NTE     = "Note for an expense at date %s is invalid"
        ESTR_INV_TAG     = "Tag for an expense at date %s is invalid"

        if type(amount) is not float and type(amount) is not int:
            raise ValueError(ESTR_BAD_AMT_VAL % date)
        elif type(tags) is not list:
            raise ValueError(ESTR_BAD_TAG_LST % date)
        elif len(tags) == 0:
            raise ValueError(ESTR_EMP_TAG_LST % date)
        elif note is not None and type(note) is not str:
            raise ValueError(ESTR_INV_NTE % date)

        for tag in tags:
            if type(tag) is not str:
                raise ValueError(ESTR_INV_TAG)

        self.amount = float(amount)
        self.tags = tags
        self.note = note

    def __str_tags__(self):
        res = "["
        for tag in self.tags:
            res += tag + ", "
        return res[:-2] + "]"

    def __str_note__(self):
        if self.note is None or self.note == "":
            return "<none>"
        else:
            return self.note

    def __str__(self):
        return ("%.2f %s %s" % (self.amount, self.__str_tags__(),
                                self.__str_note__()))

    def to_json(self):
        return {"amount": self.amount, "tags": self.tags, "note": self.note}

    @classmethod
    def parse_amount(cls, amount_str):
        return float(amount_str)

    @classmethod
    def parse_tags(cls, tags_str, opts):
        __tags = tags_str.split(",")
        new_tags = []

        for tag in __tags:
            if tag.strip() == "":
                verbose_log(opts, "Ignoring empty tag")
                continue
            else:
                new_tags += [tag.strip()]

        if len(new_tags) == 0:
            raise ValueError("List of tags is empty")

        return new_tags

    @classmethod
    def parse_note(cls, note_str):
        return note_str.strip()

    @classmethod
    def from_json(cls, date, data):
        if data["amount"] is None:
            raise ValueError("No amount value for expense at date" % date)

        if data["tags"] is None:
            raise ValueError("No tags value for expense at date" % date)

        return Expense(date, data["amount"], data["tags"],
                       data.get("note", None))
