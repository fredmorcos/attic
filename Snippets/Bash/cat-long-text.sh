#!/bin/sh

name=sysconf
version=1
link='http://github.com/fredmorcos/powerutils'

output=/dev/null

cpufreq () {
    cat <<EOF

CPU Frequency Scaling
---------------------

CPU Frequency Scaling is a feature that allows the system to change
the processor's running frequency. This can lead to lower power
consumption if set to lower values or to higher performance if set to
higher values.

Available policies:

powersave   -> lower power consumption, slower processing
performance -> higher power consumption, faster processing
ondemand    -> system will decide dynamically, depending on its needs

Would you like to configure CPU Frequency Scaling?

[y - Yes]
[n - No]

EOF

    read res
    if [[ res == "y" || res == "Y" ]]; then
	cat <<EOF

Which policy would you like to use?

[p - powersave]
[f - performance]
[o - ondemand]

EOF

}

for arg in $@; do
    if [[ -w $arg ]]; then
	cat <<EOF

File $arg already exists. Please choose another filename.

EOF
	exit 1
    else
	output=$arg
	cpufreq
	exit 0
    fi
done

cat <<EOF

$name v$version
$link

usage: $name <script-filename>

$name will ask a series of questions and will generate a shell script
with filename <script-filename> which can be used to configure the
system during bootup or shutdown, before or after resume (suspend or
hibernate) or simply ran manually at the user's convenience. Make sure
<script-filename> is writable. Any remaining arguments will be
respectfully ignored.

Multiple shell scripts could be generated for the same machine. Ie, on
a laptop it would make sense to have \`performance\`, \`powersave\`
and \`dynamic\` scripts.

$name is aimed at laptop/notebook, netbook and desktop users. It might
also aid server administrators with some initial basic tweaking.

$name roughly provides the following:
  - Performance tuning
  - Power efficiency tweaking
  - Some housekeeping

Please take the time to report bugs or annoyances. Thank You!

EOF

exit 0
