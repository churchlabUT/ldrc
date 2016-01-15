#!/bin/sh

rm /corral-repl/utexas/ldrc/SCRIPTS/launch_A_SST_onsets.txt
rm /corral-repl/utexas/ldrc/SCRIPTS/launch_H_SST_onsets.txt


basedir='/corral-repl/utexas/ldrc'

#subnums=`ls -d ${basedir}/ldrc_?_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc_*second`
#subnums=`ls -d ${basedir}/ldrc_c_[0-9][0-9][0-9]`

for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/BOLD/run*SST*`
  for run in ${run_labels}
  do
    runnum=`echo ${run} | sed 's/^.*\(.\)$/\1/' `
    
    numfiles=`ls  ${sub}/behav/SST/SST_Run${runnum}/  | wc -l`

    if [ "$numfiles" -ge 1 ]  && [ "$numfiles" -le 4 ] ; then
	echo python /corral-repl/utexas/ldrc/SCRIPTS/mk_SST_onsets.py ${sub}/behav/SST/SST_Run${runnum}/*subdata*pkl ${sub}/behav/SST/SST_Run${runnum} >> launch_A_SST_onsets.txt
    fi
  
  done
  
done




#subnums=`ls -d ${basedir}/H_LD*[0-9]`

for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/BOLD/run*SST*`
  for run in ${run_labels}
  do
    runnum=`echo ${run} | sed 's/^.*\(.\)$/\1/' `
 
    numfiles=`ls  ${sub}behav/SST/SST_Run${runnum}/  | wc -l`

    if [ "$numfiles" -ge 1 ]  && [ "$numfiles" -le 4 ] ; then
	echo python /corral-repl/utexas/ldrc/SCRIPTS/mk_SST_onsets.py ${sub}behav/SST/SST_Run${runnum}/*subdata.pkl ${sub}behav/SST/SST_Run${runnum} >> launch_H_SST_onsets.txt
    fi
  
  done
  
done

