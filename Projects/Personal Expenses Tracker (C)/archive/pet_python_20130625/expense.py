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

from validate import validate_str_param, validate_float_param

class Expense:
    def __init__(self, amount, tags, note, person, shop):
        self.amount = validate_float_param(amount, "amount")

        if type(tags) is list:
            self.tags = tags

            for tag in self.tags:
                if type(tag) is not str and type(tag) is not int:
                    raise ValueError("Bad value for a tag: %s" % tag)
        elif type(tags) is str:
            self.tags = tags.strip().split(",")

            if len(self.tags) == 0:
                raise ValueError("Bad value for tags: %s" % tags)
        else:
            raise ValueError("Bad type for tags value: %s (%s)" %
                             (tags, type(tags)))

        self.note = validate_str_param(note, "note", indexed=False)
        self.person = validate_str_param(person, "person")
        self.shop = validate_str_param(shop, "shop")
