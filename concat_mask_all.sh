#!/bin/sh


subnums=`ls -d /corral-repl/utexas/ldrc/ldrc_0[0-9][0-9]`
maskdir="/corral-repl/utexas/ldrc/GROUP/maskcheck"

rm ${maskdir}/sublist.txt

count=0
for sub in $subnums

do 

    maskname=${sub}/model/SC/cope1_lev2.gfeat/mask.nii.gz
    echo ${count}'  ' ${maskname} >> ${maskdir}/sublist.txt
    if [ ${count} -eq 0 ]
    then
      cp ${maskname} ${maskdir}/mask_all.nii.gz
    else
      cp ${maskname} ${maskdir}/temp.nii.gz
      fslmerge -t  ${maskdir}/mask_all.nii.gz  ${maskdir}/mask_all.nii.gz  ${maskdir}/temp.nii.gz
     fi
    
    count=`expr ${count} + 1`
done
