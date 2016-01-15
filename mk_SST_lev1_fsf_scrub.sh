#!/bin/sh

#rm /corral-repl/utexas/ldrc/SCRIPTS/launch_A_SST_lev1.txt
rm /corral-repl/utexas/ldrc/SCRIPTS/launch_H_SST_lev1.txt

basedir='/corral-repl/utexas/ldrc'

#subnums=`ls -d ${basedir}/ldrc_?_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc_*second`
#subnums=`ls -d ${basedir}/ldrc_*third`
#subnums=`ls -d ${basedir}/ldrc_c_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc2_[0-2]_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc2_*second`
subnums=`ls -d ${basedir}/ldrc3_[0-2]_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc3_c_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc3_?_[0-9][0-9][0-9]`

for sub in $subnums

do
  mkdir ${sub}/model/SST
  mkdir ${sub}/model/SST/designs
  run_labels=`ls -d ${sub}/BOLD/run*SST*`
  for run in ${run_labels}
  do
    runnum=`echo ${run} | sed 's/^.*\(.\)$/\1/' `
    nvols=`fslnvols ${run}/bold`
  
    sed   -e 's:NVOLS:'${nvols}':g'  -e 's:SUBDIRPATH:'${sub}':'  -e 's:RUNDIRPATH:'${run}':' -e 's:RUNNUM:'${runnum}':'  /corral-repl/utexas/ldrc/SCRIPTS/Feat_Templates/new_design_sst_template_scrub.fsf > ${sub}/model/SST/designs/run_${runnum}.fsf

    echo feat ${sub}/model/SST/designs/run_${runnum}.fsf >> /corral-repl/utexas/ldrc/SCRIPTS/launch_A_SST_lev1.txt

  done
  
done


#subnums=`ls -d ${basedir}/H_*_second`
#subnums=`ls -d ${basedir}/H_*_third`
subnums=`ls -d ${basedir}/LDFHO2*_1_3`

for sub in $subnums

do

  subname1=`echo ${sub}| sed -e  's/BOLD.*//'`
  
  subname=`echo ${subname1%/}`

  mkdir ${subname}/model/SST
  mkdir ${subname}/model/SST/designs  
  run_labels=`ls -d ${sub}/BOLD/run*SST*`
  for run in ${run_labels}

  do
    runnum=`echo ${run} | sed -e  's/.*SST_//' -e 's/_SENSE.*//'`

    nvols=`fslnvols ${run}/bold`

    sed   -e 's:NVOLS:'${nvols}':g'  -e 's:SUBDIRPATH:'${subname}':'  -e 's:RUNDIRPATH:'${run}':' -e 's:RUNNUM:'${runnum}':'  /corral-repl/utexas/ldrc/SCRIPTS/Feat_Templates/new_design_sst_template_scrub.fsf > ${subname}/model/SST/designs/run_${runnum}.fsf
    
    echo feat ${subname}/model/SST/designs/run_${runnum}.fsf >> /corral-repl/utexas/ldrc/SCRIPTS/launch_H_SST_lev1.txt

  done
  
done