#!/bin/sh


basedir='/corral-repl/utexas/ldrc'

subnums=`ls -d ${basedir}/ldrc_0[0-9][0-9]`


for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/BOLD/run*/QA/voxsfnr.png`
  for run in ${run_labels}
  do
    
    subname1=`echo ${sub}| sed -e  's/.*ldrc_//' -e  's/QA.*//'`
    
    runnum=`echo ${run} | sed -e  's/.*run//' -e 's/QA.*//'`

    mkdir /corral-repl/utexas/ldrc/SCRIPTS/movement/${subname1}_${runnum}

    cp ${subname1}_${runnum}$run /corral-repl/utexas/ldrc/SCRIPTS/movement/
    
  
  done
  
done


