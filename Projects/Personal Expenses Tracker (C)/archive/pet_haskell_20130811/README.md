pet
===

PET is a Personal Expense Tracker written in D and released under the GNU
General Public License version 3 (GPLv3).

PET is meant to be simple and leaves most of the organizational work up to the
user. You can view PET better in light of an expense number crunching tool
rather than an expense management tool.

I use PET to analyse my own personal expenses and this is the main goal of this
project. You will notice it is tailored to my needs and I expect to keep working
on it until it fulfills its goals for me. This being said, bug reports, patches,
suggestions and improvements are always most welcome.

PET is currently still early in development. I decided to use a file format that
is as compact as possible while still human readable and manageable to a certain
extent.

**TODO:** Explain the PET format (Indexes, Amount, Date, Tags, Note, Person,
Shop).

**TODO:** Explain how the PET format will aim to remain backwards-compatible.

Dependencies:

* A D v2 compiler (either dmd, ldc or gdc)
* GNU make

What remains to be done for an initial release of PET:

* Origin for payment (eg: shop, service).

* Person who made the payment.

* Index for things like tags, origins and persons to minimize repetition of
entire titles and replace them with integers instead. This can dramatically
reduce memory usage.

* Add an interactive query system for being able to do things like **"show the
daily average amount of money spent on car fuel between this and that date"**.

* In general, stabilize the command line interface and the file format.

For later releases:

* Do not store certain fields if they are null.

* An optimization system for the indexes to use lower integer values for more
frequently used items to save space on file.

* A synonym system for tags.

* A correction system for tags, useful to detect typos in input or confusion
between singular and plural words. Could be based on distance between
characters.

* Add the ability to record an interactive query into a command file.

* Add the ability to replay a command file.
