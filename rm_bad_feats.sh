#!/bin/sh


basedir='/corral-repl/utexas/ldrc'


#subnums=`ls -d ${basedir}/H_*/BOLD`

subnums=`ls -d ${basedir}/ldrc_*second`
#subnums=`ls -d ${basedir}/ldrc_?_[0-9][0-9][0-9]`

for sub in $subnums

do

  #subname1=`echo ${sub}| sed -e  's/BOLD.*//'`
  
  #subname=`echo ${subname1%/}`

  
  rm -r  ${sub}/model/SC/Run*

done
  



