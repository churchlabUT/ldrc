#!/bin/sh

#This will create a txt file that can be launched on tacc to make all the files needed to scrub BOLD runs.
#The mk_confound.sh script does not carry out actual scrubbing, it only creates the files to input into FEAT.
#Created by Mary Abbe Roe, June 2014

rm /corral-repl/utexas/ldrc/SCRIPTS/launch_SC_confound.txt
rm /corral-repl/utexas/ldrc/SCRIPTS/launch_SST_confound.txt 
rm /corral-repl/utexas/ldrc/SCRIPTS/launch_rest_confound.txt 

basedir='/corral-repl/utexas/ldrc'

#subnums=`ls -d ${basedir}/ldrc_?_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc_*second`
#subnums=`ls -d ${basedir}/ldrc_*third`
#subnums=`ls -d ${basedir}/ldrc2_*`
#subnums=`ls -d ${basedir}/ldrc2_*second`
#subnums=`ls -d ${basedir}/ldrc_c_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/H_LD*_second`
#subnums=`ls -d ${basedir}/H_LD*_third`
#subnums=`ls -d ${basedir}/LDFHO*`
#subnums=`ls -d ${basedir}/LDFHO*_second`
#subnums=`ls -d ${basedir}/ldrc2_*second*`
#subnums=`ls -d ${basedir}/ldrc3_?_[0-9][0-9][0-9]`
subnums=`ls -d ${basedir}/LDFHO2*_1`

# FOR SC

for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/BOLD/*run*SC*`
  for run in ${run_labels}
  do
    runnum=`echo ${run} | sed 's/^.*\(.\)$/\1/' `

    echo /corral-repl/utexas/ldrc/SCRIPTS/mk_confound.sh ${run}/bold.nii.gz 0.9 >> launch_SC_confound.txt
  
  done
  
done



# FOR SST

for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/BOLD/*run*SST*`
  for run in ${run_labels}
  do
    runnum=`echo ${run} | sed 's/^.*\(.\)$/\1/' `

    echo /corral-repl/utexas/ldrc/SCRIPTS/mk_confound.sh ${run}/bold.nii.gz 0.9 >> launch_SST_confound.txt

  done
  
done



# FOR REST

for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/BOLD/*run*REST*`
  for run in ${run_labels}
  do
    runnum=`echo ${run} | sed 's/^.*\(.\)$/\1/' `

    echo /corral-repl/utexas/ldrc/SCRIPTS/mk_confound.sh ${run}/bold.nii.gz 0.2 >> launch_rest_confound.txt
  
  done
  
done
