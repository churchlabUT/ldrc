#!/bin/sh

#This will create a text file that can be launched on tacc to make all the SC onset files for Austin.


rm /corral-repl/utexas/ldrc/SCRIPTS/launch_A_SC_onsets_2sec_leftover2.txt


basedir='/corral-repl/utexas/ldrc'

subnums=`ls -d ${basedir}/ldrc*c_[0-9][0-9][0-9]*`

for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/BOLD/*run*SC*`
  for run in ${run_labels}
  do
    runnum=`echo ${run} | sed 's/^.*\(.\)$/\1/' `
 
    numfiles=`ls  ${sub}/behav/SC/SC_Run${runnum}_t2/  | wc -l`
 

#A python code will actually create the onset files for the sc task. It is called make_A_SC_onsets_2sec_leftover2.py. This script simply creates a text file to be launched.

    if [ "$numfiles" -ge 1 ]  && [ "$numfiles" -le 4 ] ; then
	echo python /corral-repl/utexas/ldrc/SCRIPTS/mk_A_SC_onsets_2sec_leftover2.py ${sub}/behav/SC/*SC_Run${runnum}_t2/*subdata.pkl ${sub}/behav/SC/*SC_Run${runnum}_t2 >> launch_A_SC_onsets_2sec_leftover2.txt
    fi
  
  done
  
done