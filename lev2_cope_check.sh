#!/bin/sh


#subnums=`ls -d /corral-repl/utexas/ldrc/ldrc_0[0-9][0-9]`
subnums=`ls -d /corral-repl/utexas/ldrc/ldrc_*second`

rm cope_check.txt

for sub in $subnums

do 

    dirpath=${sub}/model/*/cope*gfeat/cope1.feat/stats/zstat1.nii.gz 
    
    for i in $dirpath
    
    do
      
      echo $i >> cope_check.txt
   
    done

done
