#!/bin/sh

#Before running this script, all steps up to renaming the zstats need to be complete. Assuming you are in ldrc/SCRIPTS on tacc..

basedir='GROUP/zstats/thresh'
cd ../$basedir/SC
SC_zstats=`ls -d *_zstat2.nii.gz`


for zstat in ${SC_zstats}
do
  
  zstat_temp=$(echo $zstat | cut -d "." -f 1)

  fslmaths ${zstat} -mul -1 ${zstat_temp}_neg.nii.gz

done



cd ../SST
SST_zstats=`ls -d *_zstat2.nii.gz`

for zstat in ${SST_zstats}
do
  
  zstat_temp=$(echo $zstat | cut -d "." -f 1)

  fslmaths ${zstat} -mul -1 ${zstat_temp}_neg.nii.gz

done

cd ../../../../SCRIPTS
