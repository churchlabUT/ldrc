#!/bin/sh 

#This will create a text file that can be launched on tacc to make all the SC event and SST files. 

rm /corral-repl/utexas/ldrc/SCRIPTS/launch_A_SC_events.txt

#rm /corral-repl/utexas/ldrc/SCRIPTS/launch_H_SC_events.txt 

rm /corral-repl/utexas/ldrc/SCRIPTS/launch_A_SST_events.txt

#rm /corral-repl/utexas/ldrc/SCRIPTS/launch_H_SST_events.txt 

basedir='/corral-repl/utexas/ldrc'

#subnums=`ls -d ${basedir}/ldrc_?_[0-9][0-9][0-9]`

subnums=`ls -d ${basedir}/ldrc_c_[0-9][0-9][0-9]`

#subnums=`ls -d ${basedir}/ldrc_*second`

#subnums=`ls -d ${basedir}/ldrc_*_[0-9][0-9][0-9]*` #all

#subnums=`ls -d ${basedir}/ldrc2_*_[0-9][0-9][0-9]*` #all


#AUSTIN SC
for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/behav/SC/*SC_Run* | grep -v "x_"`
  for run in ${run_labels}
  do
 #A python code will actually create the event files for the sc task. It is called SC_event_maker.py.

 echo python /corral-repl/utexas/ldrc/SCRIPTS/SC_event_maker.py ${run}/*subdata.pkl ${run} >> launch_A_SC_events.txt
    
  
  done
  
done


#HOUSTON SC
#subnums=`ls -d ${basedir}/H_LD*_*_*`

#for sub in $subnums

#do
  
#  run_labels=`ls -d ${sub}/behav/SC/*SC_Run* | grep -v "x_"`

#  for run in ${run_labels}

#  do
    
 
# echo python /corral-repl/utexas/ldrc/SCRIPTS/SC_event_maker.py ${run}/*subdata.pkl ${run} >> launch_H_SC_events.txt
    
  
#  done
  
#done

# AUSTIN SST

#subnums=`ls -d ${basedir}/ldrc2_*_[0-9][0-9][0-9]*` #all

for sub in $subnums

do
  
  run_labels=`ls -d ${sub}/behav/SST/*SST_Run* | grep -v "x_"`
  for run in ${run_labels}
  do
 #A python code will actually create the event files for the sc task. It is called SST_event_maker.py.

 echo python /corral-repl/utexas/ldrc/SCRIPTS/SST_event_maker.py ${run}/*subdata.pkl ${run} >> launch_A_SST_events.txt
    
  
  done
  
done


#HOUSTON SST
#subnums=`ls -d ${basedir}/H_LD*_*_*`

#for sub in $subnums

#do
  
#  run_labels=`ls -d ${sub}/behav/SST/*SST_Run* | grep -v "x_"`

#  for run in ${run_labels}

#  do
    
 
# echo python /corral-repl/utexas/ldrc/SCRIPTS/SST_event_maker.py ${run}/*subdata.pkl ${run} >> launch_H_SST_events.txt
    
  
#  done
  
#done



#TO run Scripts afterwards.

#launch -s launch_A_SC_events.txt -j Analysis_Lonestar -p 120 -e 2way -r 04:00:00 -m churchlab@austin.utexas.edu 
#launch -s launch_A_SST_events.txt -j Analysis_Lonestar -p 120 -e 2way -r 04:00:00 -m churchlab@austin.utexas.edu 
