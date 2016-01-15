#!/bin/sh

rm /corral-repl/utexas/ldrc/SCRIPTS/empty_sst_onset_files.txt

basedir='/corral-repl/utexas/ldrc'

subnums=`ls -d ${basedir}/ldrc_0[0-9][0-9]`


for sub in $subnums

do 
  run_labels=`ls -d ${sub}/BOLD/run*SST*`
  
  for run in ${run_labels}
  do
    runnum=`echo ${run} | sed 's/^.*\(.\)$/\1/' `

    dirpth=${sub}/model/SST/Run${runnum}.feat/ 
    
#Go through the 11 task EV files and save the first line in the txt file

    for i in $(seq 11)
      do
    checkev=`head -1 ${dirpth}/custom_timing_files/ev$i.txt`
    
#If the first line is just zeros, then save that ev's path into empty_onset_files.txt

    if [[ $checkev = "0.000000000000000000e+00 0.000000000000000000e+00 0.000000000000000000e+00" ]]
	then 
	echo ${dirpth}/custom_timing_files/ev$i.txt >> empty_sst_onset_files.txt
    fi

done

done
done
