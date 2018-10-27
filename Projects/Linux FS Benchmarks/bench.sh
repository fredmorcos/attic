#!/bin/sh

user=fred

testdir=/home/$user/bonnietest
resdir=/home/$user/bonnieres

schedulers="cfq deadline noop"

mkdir $testdir
mkdir $resdir
chown -R $user $testdir
chown -R $user $resdir
chgrp -R users $testdir
chgrp -R users $resdir

disk=sda
for scheduler in $schedulers; do
    echo $scheduler > /sys/block/$disk/queue/scheduler
    bonnie++ -u fred:users -d $testdir -m $disk-$scheduler -x 2 > $resdir/$disk-$scheduler
done

rmdir $testdir
testdir='/mult/bonnietest'
mkdir $testdir
chown -R $user $testdir
chgrp -R users $testdir

disk=sdb
for scheduler in $schedulers; do
    echo $scheduler > /sys/block/$disk/queue/scheduler
    bonnie++ -u fred:users -d $testdir -m $disk-$scheduler -x 2 > $resdir/$disk-$scheduler
done

rmdir $testdir
