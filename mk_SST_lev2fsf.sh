#!/bin/sh

rm /corral-repl/utexas/ldrc/SCRIPTS/launch_A_SST_lev2fsf.txt
rm /corral-repl/utexas/ldrc/SCRIPTS/launch_H_SST_lev2fsf.txt

basedir='/corral-repl/utexas/ldrc'

#subnums=`ls -d ${basedir}/ldrc_[0-9]_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc_*second`
#subnums=`ls -d ${basedir}/ldrc_*third`
#subnums=`ls -d ${basedir}/ldrc2_[0-9]_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc_c_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc2*_second`
#subnums=`ls -d ${basedir}/ldrc3_[0-9]_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc3_c_[0-9][0-9][0-9]*`

for sub in $subnums

do
    
 
 echo python /corral-repl/utexas/ldrc/SCRIPTS/mk_lev2fsf.py ${sub}/model/SST ${sub}/model/SST >> launch_A_SST_lev2fsf.txt
    
  
done


#subnums=`ls -d ${basedir}/H_*_second/`
#subnums=`ls -d ${basedir}/H_*_third/`
#subnums=`ls -d ${basedir}/LDFHO*_second/`
#subnums=`ls -d ${basedir}/LDFHO*_[0-9]/`
subnums=`ls -d ${basedir}/LDFHO2*_1_3/`

for sub in $subnums

do
  subname1=`echo ${sub}| sed -e  's/BOLD.*//'`
  
  subname=`echo ${subname1%/}`
 
  echo python /corral-repl/utexas/ldrc/SCRIPTS/mk_lev2fsf.py ${subname}/model/SST ${subname}/model/SST >> launch_H_SST_lev2fsf.txt
    
  
done
  


