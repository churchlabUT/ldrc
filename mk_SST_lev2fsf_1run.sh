#!/bin/sh

rm /corral-repl/utexas/ldrc/SCRIPTS/launch_A_SST_lev2fsf.txt
rm /corral-repl/utexas/ldrc/SCRIPTS/launch_H_SST_lev2fsf.txt

basedir='/corral-repl/utexas/ldrc'

#subnums=`ls -d ${basedir}/ldrc_?_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc_*second`
#subnums=`ls -d ${basedir}/ldrc_*third`
#subnums=`ls -d ${basedir}/ldrc2_*`
#subnums=`ls -d ${basedir}/ldrc_c_[0-9][0-9][0-9]`
subnums=`ls -d ${basedir}/ldrc_c_032`

for sub in $subnums

do
    
 
 echo python /corral-repl/utexas/ldrc/SCRIPTS/mk_lev2fsf_1run.py ${sub}/model/SST ${sub}/model/SST >> launch_A_SST_lev2fsf.txt
    
  
done


#subnums=`ls -d ${basedir}/H_*_second/`
#subnums=`ls -d ${basedir}/H_*_third/`
#subnums=`ls -d ${basedir}/LDFHO*/`

for sub in $subnums

do
  subname1=`echo ${sub}| sed -e  's/BOLD.*//'`
  
  subname=`echo ${subname1%/}`
 
  echo python /corral-repl/utexas/ldrc/SCRIPTS/mk_lev2fsf.py ${subname}/model/SST ${subname}/model/SST >> launch_H_SST_lev2fsf.txt
    
  
done
  


