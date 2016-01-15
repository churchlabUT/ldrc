#!/bin/sh 

#IMPORTANT, Use mk_events.sh first to create the event files for the groups you want to make fidl files for.


#bash script to create all of the fidle files at once.
#or you can copy and paste each section into terminal
#The first line that has setsubs in it creates the fidl_subs.txt file so you can edit the subjects you want to run before you run the SC or SST lines, which make the actual
#fidl files
#The line with SC makes the SC fidl files, and so forth with SST.
#THe r or k at the end of each line states whether to remove or keep runs with bad
#motion

#Fidl file maker uses the output Events files from the mk_SC_onsets.py and mk_SST_onsets.py

#To make the fidl_subs.txt file so you can edit who is run type: 'Rscript fidl_maker.r setsubs {the name of the group you want to run}'
#Example, To make the fidl_subs.txt file for first austin you would type: 'Rscript fidl_maker.r 1 A_first'
#Groups: Austin second = 'A_second', all = 'All', houston = 'Houston, third = 'A_third, Controls = 'A_controls', Intervention2 = 'A2_first'

#To make the actual fidl files type: 'Rscript fidl_maker.r {SC or SST} {Output folder}'
#Example, To make the SC fidl_files for the subs in the fidl_subs.txt AND you want the output in the intervention_second folder you would type: 'Rscript fidl_maker.r SC Intervention_second'
#Example, To make the SST fidl files for subs in the fidl_subs.txt AND output to intervention_second folder you would type: 'Rscript fidl_maker.r SST Intervention_second'
#Ouput folders: controls = 'Controls', Intervention first = 'Intervention', third = 'Intervention_third', houston = 'Houston', Intervention 2 = 'Intervention2'

#I generally do one section at a time to see if there is an error. If there is, I look to see which
#was the last person it ran because that's who made the script stop.
#then I manually go through the fidl_maker.r script using that person's data only to see where
#the error occured. (In the r fidl_maker.r script, this involves just setting the variable 'i' 
#equal to the index of the subject inside of the variable 'filenames'


#If there is an error, it will be shown inside of fidl_error.txt


#Clear contents of the EVENT_Files folders, you can change the * to whatever the folder name should be if you want it to be specific to a group.
#Example: rm /corral-repl/utexas/ldrc/WU_analysis/EVENT_FILES/Intervention2/*txt*
#rm /corral-repl/utexas/ldrc/WU_analysis/EVENT_FILES/*/*fidl*
#rm /corral-repl/utexas/ldrc/WU_analysis/EVENT_FILES/*/*/*fidl*






#To remove runs with bad motion, ends up in NoMovement Folder.

echo 'Controls - r'
#Controls
Rscript fidl_maker.r setsubs A_controls r
Rscript fidl_maker.r SC Controls r
Rscript fidl_maker.r SST Controls r

echo 'First - r'
#Austin First
Rscript fidl_maker.r setsubs A_first r
Rscript fidl_maker.r SC Intervention r
Rscript fidl_maker.r SST Intervention r

echo 'Second - r'
#Austin Second
Rscript fidl_maker.r setsubs A_second r
Rscript fidl_maker.r SC Intervention_second r
Rscript fidl_maker.r SST Intervention_second r

echo 'Third - r'
#Austin third
Rscript fidl_maker.r setsubs A_third r
Rscript fidl_maker.r SC Intervention_third r
Rscript fidl_maker.r SST Intervention_third r

echo 'First2 - r'
#Austin First2
Rscript fidl_maker.r setsubs A2_first r
Rscript fidl_maker.r SC Intervention2 r
Rscript fidl_maker.r SST Intervention2 r


echo 'Houston - r'
#Houston
Rscript fidl_maker.r setsubs Houston r
Rscript fidl_maker.r SC Houston r
Rscript fidl_maker.r SST Houston r


#To keep runs with bad motion, ends up in main folder

echo 'Controls - k'
#Controls
Rscript fidl_maker.r setsubs A_controls k
Rscript fidl_maker.r SC Controls k
Rscript fidl_maker.r SST Controls k

echo 'First - k'
#Austin First
Rscript fidl_maker.r setsubs A_first k
Rscript fidl_maker.r SC Intervention k
Rscript fidl_maker.r SST Intervention k

echo 'Second - k'
#Austin Second
Rscript fidl_maker.r setsubs A_second k
Rscript fidl_maker.r SC Intervention_second k
Rscript fidl_maker.r SST Intervention_second k

echo 'Third - k'
#Austin third
Rscript fidl_maker.r setsubs A_third k
Rscript fidl_maker.r SC Intervention_third k
Rscript fidl_maker.r SST Intervention_third k

echo 'First2 - r'
#Austin First2
Rscript fidl_maker.r setsubs A2_first k
Rscript fidl_maker.r SC Intervention2 k
Rscript fidl_maker.r SST Intervention2 k

echo 'Houston - k'
#Houston
Rscript fidl_maker.r setsubs Houston k
Rscript fidl_maker.r SC Houston k
Rscript fidl_maker.r SST Houston k
