#!/bin/sh

user=fred
group=users

resdir=/home/$user/proj/fs-benchmarks/mult-results
testdir=/mult

schedulers="cfq deadline noop"

mkdir $resdir
chown -R $user $resdir
chgrp -R users $resdir

chown -R $user $testdir
chgrp -R users $testdir

disk=/dev/sdb
part=/dev/sdb1

