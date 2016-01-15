#This will create a text file that can be launched on tacc to make all the SC onset files. 

# change the cope and task depending on which one you're working on

basedir='/corral-repl/utexas/ldrc'


rm /corral-repl/utexas/ldrc/SCRIPTS/lev2_QC/SC/lev2_copes_1.txt
rm /corral-repl/utexas/ldrc/SCRIPTS/lev2_QC/SC/lev2_copes_2.txt
rm /corral-repl/utexas/ldrc/SCRIPTS/lev2_QC/SC/lev2_copes_3.txt
rm /corral-repl/utexas/ldrc/SCRIPTS/lev2_QC/SC/lev2_copes_4.txt
rm /corral-repl/utexas/ldrc/SCRIPTS/lev2_QC/SC/lev2_copes_5.txt
rm /corral-repl/utexas/ldrc/SCRIPTS/lev2_QC/SC/lev2_copes_6.txt
rm /corral-repl/utexas/ldrc/SCRIPTS/lev2_QC/SC/lev2_copes_7.txt
rm /corral-repl/utexas/ldrc/SCRIPTS/lev2_QC/SC/lev2_copes_8.txt
rm /corral-repl/utexas/ldrc/SCRIPTS/lev2_QC/SC/lev2_copes_9.txt
rm /corral-repl/utexas/ldrc/SCRIPTS/lev2_QC/SC/lev2_copes_10.txt



#subnums=`ls -d ${basedir}/ldrc_c_[0-9][0-9][0-9]*`
subnums=`ls -d ${basedir}/ldrc3_c_[0-9][0-9][0-9]*`
#subnums=`ls -d ${basedir}/ldrc_*second`
#subnums=`ls -d ${basedir}/ldrc_*third`
#subnums=`ls -d ${basedir}/ldrc_[0-9]_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc2_[0-9]_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc2_[0-2]_[0-9][0-9][0-9]_second`
#subnums=`ls -d ${basedir}/ldrc3_[0-2]_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/H_LD*_second`
#subnums=`ls -d ${basedir}/H_LD*_third`
#subnums=`ls -d ${basedir}/LDFHO*_[0-9]`
#subnums=`ls -d ${basedir}/LDFHO*_second`

task="SC"

cope=1
for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/model/${task}/cope${cope}_lev2.gfeat/cope1.feat/stats/cope1.nii.gz`
  for run in ${run_labels}
  do


 echo ${run} >> lev2_copes_1.txt
    
  
  done
  
done


cope=2
for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/model/${task}/cope${cope}_lev2.gfeat/cope1.feat/stats/cope1.nii.gz`
  for run in ${run_labels}
  do


 echo ${run} >> lev2_copes_2.txt
    
  
  done
  
done

cope=3
for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/model/${task}/cope${cope}_lev2.gfeat/cope1.feat/stats/cope1.nii.gz`
  for run in ${run_labels}
  do


 echo ${run} >> lev2_copes_3.txt
    
  
  done
  
done

cope=4
for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/model/${task}/cope${cope}_lev2.gfeat/cope1.feat/stats/cope1.nii.gz`
  for run in ${run_labels}
  do


 echo ${run} >> lev2_copes_4.txt
    
  
  done
  
done


cope=5
for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/model/${task}/cope${cope}_lev2.gfeat/cope1.feat/stats/cope1.nii.gz`
  for run in ${run_labels}
  do


 echo ${run} >> lev2_copes_5.txt
    
  
  done
  
done

cope=6
for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/model/${task}/cope${cope}_lev2.gfeat/cope1.feat/stats/cope1.nii.gz`
  for run in ${run_labels}
  do


 echo ${run} >> lev2_copes_6.txt
    
  
  done
  
done

cope=7
for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/model/${task}/cope${cope}_lev2.gfeat/cope1.feat/stats/cope1.nii.gz`
  for run in ${run_labels}
  do


 echo ${run} >> lev2_copes_7.txt
    
  
  done
  
done

cope=8
for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/model/${task}/cope${cope}_lev2.gfeat/cope1.feat/stats/cope1.nii.gz`
  for run in ${run_labels}
  do


 echo ${run} >> lev2_copes_8.txt
    
  
  done
  
done

cope=9
for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/model/${task}/cope${cope}_lev2.gfeat/cope1.feat/stats/cope1.nii.gz`
  for run in ${run_labels}
  do


 echo ${run} >> lev2_copes_9.txt
    
  
  done
  
done

cope=10
for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/model/${task}/cope${cope}_lev2.gfeat/cope1.feat/stats/cope1.nii.gz`
  for run in ${run_labels}
  do


 echo ${run} >> lev2_copes_10.txt
    
  
  done
  
done
