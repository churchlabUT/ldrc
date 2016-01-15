
#!/bin/sh


#This first part of code creates the variable subnums and looks in our ldrc directory on tacc for all directories starting with ldrc_xxx where x is a number between 1-9. 

### Runs the code for Austin data ###

basedir='/corral-repl/utexas/ldrc'


#subnums=`ls -d ${basedir}/ldrc_?_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc_*second`
#subnums=`ls -d ${basedir}/ldrc_c_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc_*third`
#subnums=`ls -d ${basedir}/ldrc2_*`
#subnums=`ls -d ${basedir}/ldrc2_*second*`
#subnums=`ls -d ${basedir}/ldrc3_[0-2]_[0-9][0-9][0-9]`


#Use echo $subnums to see which subjects the script found.


for sub in $subnums

do
  
#This part of the codes looks in the BOLD directory in each subject's ldrc diretory for the SC runs. If the naming convention changes for the BOLD runs you would have to make the change here. The script then chops off the last number of the BOLD runs name and saves it to runnum, which is the run number. For instance,/corral-repl/utexas/ldrc/ldrc_001/BOLD/run02_10_SC_1 would be what the code finds and the one at the end of "SC_1" indicates which run it is. 

  run_labels=`ls -d ${sub}/BOLD/*run*`
  for run in ${run_labels}
  do
    runnum=`echo ${run} | sed 's/^.*\(.\)$/\1/' `
   
 ctr=1

#This part of code splits the motion parameters into 6 seperate files using the bold_mcf.par file the preprocessing code creates. It then saves 6 files called mot_pars_x, where x is a number 1-6 in the subject's BOLD directory. These are the motion parameter files that will be used as EVs in the level 1 FEAT analysis. 

   for x in 1 3 5 7 9 11
     do
     cut -f $x -d' ' ${run}/bold_mcf.par > ${run}/motpars_${ctr}.txt
     ctr=`expr $ctr + 1`
   done
  
  done
done


### Runs the same code for Houston data ###


#subnums=`ls -d ${basedir}/H_*`
#subnums=`ls -d ${basedir}/LDFHO*_second`
subnums=`ls -d ${basedir}/LDFHO2*_1`

for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/BOLD/run*`
  for run in ${run_labels}

  do
    runnum=`echo ${run} | sed 's/^.*\(.\)$/\1/' `

ctr=1

   for x in 1 3 5 7 9 11
     do
     cut -f $x -d' ' ${run}/bold_mcf.par > ${run}/motpars_${ctr}.txt
     ctr=`expr $ctr + 1`
   done

  
  done
done