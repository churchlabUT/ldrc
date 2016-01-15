#!/bin/sh

#This will create a text file that can be launched on tacc to make all the SC onset files for Austin.


rm /corral-repl/utexas/ldrc/SCRIPTS/launch_A_SC_onsets_2sec.txt


basedir='/corral-repl/utexas/ldrc'

#subnums=`ls -d ${basedir}/ldrc_?_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc_c_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc_*second`
#subnums=`ls -d ${basedir}/ldrc_*third`
#subnums=`ls -d ${basedir}/ldrc2_*`
subnums=`ls -d ${basedir}/ldrc3_?_[0-9][0-9][0-9]*`

for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/BOLD/run*SC*`
  for run in ${run_labels}
  do
    runnum=`echo ${run} | sed 's/^.*\(.\)$/\1/' `
 
    numfiles=`ls  ${sub}/behav/SC/SC_Run${runnum}/  | wc -l`
 

#A python code will actually create the onset files for the sc task. It is called make_SC_onsets.py. This script simply creates a text file to be launched.

    if [ "$numfiles" -ge 1 ]  && [ "$numfiles" -le 4 ] ; then
	echo python /corral-repl/utexas/ldrc/SCRIPTS/mk_A_SC_onsets_2sec.py ${sub}/behav/SC/SC_Run${runnum}/*subdata.pkl ${sub}/behav/SC/SC_Run${runnum} >> launch_A_SC_onsets_2sec.txt
    fi
  
  done
  
done