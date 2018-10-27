#!/bin/bash

mgen sbn 5 tests/sbn.s 7>&- | xclip -selection "clipboard"
echo "NOTE: RESULT (FD 7) SAVED TO X CLIPBOARD"