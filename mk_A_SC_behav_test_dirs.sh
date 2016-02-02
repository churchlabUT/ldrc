#!/bin/sh

#This will create a text file that can be launched on tacc to make all the SC onset files for Austin.

#basedir='/corral-repl/utexas/ldrc'
#subnums=`ls -d ${basedir}/ldrc*c_[0-9][0-9][0-9]*`

#for sub in $subnums

#do
  
  #run_labels=`ls -d ${sub}/BOLD/*run*SC*`
  #for run in ${run_labels}
  #do
    #runnum=`echo ${run} | sed 's/^.*\(.\)$/\1/' ` 
    #origfile=`ls -d ${sub}/behav/SC/*SC_Run${runnum}`
    #echo ${origfile}

    #if [ ! -d "" ] ; then
      #echo "${origfile}_t1 exists"
      #mkdir ${origfile}_t1
      #cp ${origfile}/*_subdata.pkl ${origfile}_t1/
      #newdir=`ls -d ${sub}/behav/SC/*SC_Run${runnum}_t1`
      #echo ${newdir}
      #newfile=`ls -d ${sub}/behav/SC/*SC_Run${runnum}_t1/*_subdata.pkl`
      #echo ${newfile}
    #fi

  #done
#done

