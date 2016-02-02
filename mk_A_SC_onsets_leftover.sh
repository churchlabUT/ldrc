#!/bin/sh

#This will create a text file that can be launched on tacc to make all the SC onset files for Austin.


rm /corral-repl/utexas/ldrc/SCRIPTS/launch_A_SC_onsets_2sec_leftover.txt


basedir='/corral-repl/utexas/ldrc'

subnums=`ls -d ${basedir}/ldrc*c_[0-9][0-9][0-9]*`
#subnums=`ls -d ${basedir}/ldrc_c_037`

for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/BOLD/*run*SC*`
  for run in ${run_labels}
  do
    runnum=`echo ${run} | sed 's/^.*\(.\)$/\1/' `
       
    numfiles=`ls  ${sub}/behav/SC/SC_Run${runnum}_t1/  | wc -l`
 

#A python code will actually create the onset files for the sc task. It is called make_A_SC_onsets_2sec_leftover1.py. This script simply creates a text file to be launched.

    if [ "$numfiles" -ge 1 ]  && [ "$numfiles" -le 4 ] ; then
	echo python /corral-repl/utexas/ldrc/SCRIPTS/mk_A_SC_onsets_2sec_leftover1.py ${sub}/behav/SC/*SC_Run${runnum}_t1/*subdata.pkl ${sub}/behav/SC/*SC_Run${runnum}_t1 >> launch_A_SC_onsets_2sec_leftover.txt
    fi
  
  done
  
done