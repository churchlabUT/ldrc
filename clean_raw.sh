#!/bin/bash

basedir='/corral-repl/utexas/ldrc'

subnums=`ls -d ${basedir}/ldrc2_129_second`
#subnums=`ls -d ${basedir}/ldrc2_[0-9][0-9][0-9]_second/raw`


for sub in $subnums

do
 
 subid=${sub##*/} 
 raw=`ls -d ${sub}/raw/${subid}/` 

 if ( echo "$raw" | grep -q ' ' ); then
      echo 'foldername has space'
      
 fi


 for run in $raw

 nraw=${raw###}

 do
  mv ${raw} ${sub}/raw/${subid}

 


 done

done

