#!/bin/bash

mkdir -p helios
sshfs -o idmap=user fmorcos@helios.ica.jku.at: helios -C