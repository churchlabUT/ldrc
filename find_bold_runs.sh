#This will create a text file that can be launched on tacc to make all the SC onset files. 

rm /corral-repl/utexas/ldrc/SCRIPTS/flipped_runs.txt

basedir='/corral-repl/utexas/ldrc'

#subnums=`ls -d ${basedir}/ldrc_c_[0-9][0-9][0-9]*`
subnums=`ls -d ${basedir}/ldrc_*_[0-9][0-9][0-9]*`
#subnums=`ls -d ${basedir}/ldrc_*second`

for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/BOLD/run*`
  for run in ${run_labels}
  do


 echo /corral-repl/utexas/ldrc/SCRIPTS/flip_brain.sh ${run} >> flipped_runs.txt
    
  
  done
  
done

