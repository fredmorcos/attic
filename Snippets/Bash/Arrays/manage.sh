#!/bin/sh

declare -a manage_files=(
    [0]=([0]="_emacs" [1]="~/.emacs")
    [1]=([0]="terminalrc" [1]="~/.config/Terminal/terminalrc")
)

manage_install () {
    echo "installing..."
    for f in $manage_files; do
	echo "${f[0]} -> ${f[1]}"
    done
    echo "done"
}

manage_collect () {
    echo "collecting..."
    for f in $manage_files; do
	echo "${f[1]} -> ${f[0]}"
    done
    echo "done"
}

for arg in $@; do
    if [ "$arg" == "install" ]; then
	manage_install
	exit 0
    elif [ "$arg" == "collect" ]; then
	manage_collect
	exit 0
    else
	break
    fi
done

cat <<EOF
usage:
  ./manage.sh <command>

command:
  collect  copy configuration files into the current directory.
  install  install configuration files into respective places.
EOF
