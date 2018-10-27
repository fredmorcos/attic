pet
===

PET is a Personal Expense Tracker written in Python and released under the GNU
General Public License version 3 (GPLv3).

PET works with Python 3.x but **has not** been tested with Python 2.x. Please
report any successes or failures with Python versions older than 3.0.

PET is meant to be simple and leaves most of the organizational work up to the
user. You can view PET better in light of a data crunching tool rather than an
explicit tool for managing expenses.

I use PET to manage my own personal expenses and this is the main goal of this
project. You will notice it is tailored to my needs and I expect to keep working
on it until it fulfills its goals for me. Bug reports, patches, suggestions and
improvements are nevertheless most welcome.

PET is currently still early in development. I decided to use JSON as the data
representation format since Python provides JSON encoding and decoding in the
standard library. This may not be the most efficient representation but it
definitely offloads some parser work. Also, unless my expense files start
growing with millions of items, I do not see the need for optimization.

**TODO:** Explain the PET format (Amount, Date, Tags, Note).

**TODO:** Explain how the PET format will aim to remain backwards-compatible.

What remains to be done for an initial release of PET:

* Do not store Note fields if they are None.

* Origin for payment (eg: shop, service).

* Person who made the payment.

* Index for things like tags, origins and persons to minimize repetition of
entire titles and replace them with integers instead. This can dramatically
reduce memory usage.

* Rework the command line arguments parsing code. Currently it is quite simple
and does not support argument fields. The Python argparse module from the
standard library may be overkill though.

* Add an interactive query system for being able to do things like **"show the
daily average amount of money spent on car fuel between this and that date"**.

* In general, stabilize the command line interface and the file format.

For later releases:

* An optimization system for the indexes to use lower integer values for more
frequently used items to save space on file.

* A synonym system.

* A correction system for tags, useful to detect typos in input or confusion
between singular and plural words. Could be based on distance between
characters.

* Add the ability to record an interactive query into a command file.

* Add the ability to replay a command file.
