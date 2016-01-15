#!/bin/sh

#Script that will get the paths in order of date dicoms were created

#paths = `ls -ldrt /corral-repl/utexas/ldrc/ldrc*/raw/ldrc*/1`

# This is poor form, but I'm in a rush :)
cd /corral-repl/utexas/ldrc

for i in `ls -drt ldrc*/raw/ldrc*/1`
Do
  echo ${i}
  # Now we just need the part of the path that goes to your level 2 analysis@
  subnum=`echo ${i} | cut -d / -f 1`
  echo ${subnum}
done
