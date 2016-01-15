#!/usr/bin/env python

#First authors: Jeanette Mumford, Leo Olemedo
#Editors: Mary Abbe Roe, Joel Martinez, Oct-Nov 2013


#this script creates these files in each sub /behav/SST_run* folder
  #*_output_R.txt (a text file that is used to run behavioral analysis in R)           
  #*_output_Events.txt (used for fidl analysis)
  #junk.txt (omissions)
  #*_cor.txt and *_incor.txt, where the * is go or stop;
  #rt_all.txt (rt on all correct and incorrect go and stop trials)

#Codes:
  #Cond: 0= arrow points left, 1= arrow points right, 2= arrow points left and stop , 3= arrow points right and stop
  #Stim: 0= left arrow, 1= right arrow
  #Resp: 0=no button press , 1=left button , 4=right button
  #Response/correct: 0=go_incorrect, 1=go_correct, 2=stop_fail_incorrect, 3=stop_fail_correct, 4=stop_correct, 5=go_omission (4 and 5 are trials that have no response)

import sys
import numpy as N
import pickle
import os
import subprocess


#function to replace empty 3 column format with 000
def fixEmpty(input):
       if input.shape[0]==0:
            onset=N.vstack([0,0,0]).T
       else:
            onset=input
       return onset

filename=sys.argv[1]
outdir=sys.argv[2]

if filename.endswith('pkl')==False:
    print "WARNING:First input must be a pkl file"
    sys.exit()
    
if os.path.isfile(filename)==False:
    print "WARNING:First input does not exist"
    sys.exit()


if os.path.isdir(outdir)==False:
    print "WARNING:second input must be output directory path"
    sys.exit()


f=open(filename,'rb')
data=pickle.load(f)
f.close()

#onset vector
onset_vec=N.array(data['stimons'].values())

#Construct subject response
correct=[]
rt_vec=[]

#Create the R output for FEAT
R_out=filename.replace('.pkl','_output_R.txt')
f=open(R_out,'w')
f.write('TrialNum\tOnset\tTrueOns\tcond\tStim\tResp\tRT\tStopTrial\tSSD\tcorrect\n')

for t in range(len(data['onsets'])):
    resp_vec=data['resp'][t]
    if resp_vec=='':
        resp_vec='0'
    if data['rt'].has_key(t):
        rt_loop=data['rt'][t]
    else:
        rt_loop=0.0
    ons=data['onsets'][t]
    if data['ssd'].has_key(t):
        ssd=data['ssd'][t]
        stoptrial=1
    else:
        ssd=0.0
        stoptrial=0
    trueons=data['stimons'][t]
    if data['stim'][t]=='<':
        stim=0
    else:
        stim=1
    cond=data['cond'][t]

#For trials with RT values, 0-go_incorrect 1-go_correct 2-stop_fail_incorrect 3-stop_fail_correct
    if rt_loop>0:      
        rt_vec.append(rt_loop)
        if (cond==0 and resp_vec=='1') or (cond==1 and resp_vec=='4'): 
            correct.append(1)
        elif (cond==2 and resp_vec=='4') or (cond==3 and resp_vec=='1'):
            correct.append(2)
        elif (cond==2 and resp_vec=='1') or (cond==3 and resp_vec=='4'):
            correct.append(3)
        else:
            correct.append(0)
            
#For trials with no response, 4-stop_correct 5-go_omission 
    elif rt_loop==0:      
        rt_vec.append(rt_loop)
        if (cond==2 and resp_vec=='0') or (cond==3 and resp_vec=='0'): 
            correct.append(4)
        else:
            correct.append(5)
    
    f.write('%.2f\t%.2f\t%.2f\t%d\t%d\t%s\t%f\t%d\t%0.3f\t%s\n'%(t,ons,trueons,cond,stim,resp_vec,rt_loop,stoptrial,ssd,correct[t]))
    
f.close()



#Create the R output for FIDL

#Construct subject response
correct1=[]
rt_vec1=[]

fidl_out=filename.replace('.pkl','_output_Events.txt')
f=open(fidl_out,'w')
f.write('2\tIncorrect_go\tCorrect_go\tFailed_Stop\tCorrect_Stop\n')

for t in range(len(data['onsets'])):
    resp_vec1=data['resp'][t]
    if resp_vec1=='':
        resp_vec1='0'
    if data['rt'].has_key(t):
        rt_loop1=data['rt'][t]
    else:
        rt_loop1=0.0
    trueons=data['stimons'][t]
    cond=data['cond'][t]
#For trials with RT values, 0-go_incorrect 1-go_correct 2-stop_fail_incorrect and stop_fail_correct
    if rt_loop1>0:      
        rt_vec1.append(rt_loop1)
        if (cond==0 and resp_vec1=='1') or (cond==1 and resp_vec1=='4'): #left arrow, left button or right arrow right button
            correct1.append(1) #go correct
            f.write('%.2f\t%s\t%s\t%f\n'%(trueons,correct1[t],'1',rt_loop1))
        elif (cond==2 and resp_vec1=='4') or (cond==3 and resp_vec1=='1'): #Left X R Button or Right X and L button
            correct1.append(2) #stopfail incorrect
            f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'1',''))
        elif (cond==2 and resp_vec1=='1') or (cond==3 and resp_vec1=='4'):
            correct1.append(2) #stop fail correct
            f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'1',''))
        else:
            correct1.append(0)
            f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'1',''))
#For trials with no response, 3-stop_correct 0-go_omission (go incorrect)
    elif rt_loop1==0:      
        rt_vec1.append(rt_loop1)
        if (cond==2 and resp_vec1=='0') or (cond==3 and resp_vec1=='0'): 
            correct1.append(3)
            f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'1',''))
        else:
            correct1.append(0)
            f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'1',''))
    
f.close()


correct=N.array(correct)
rt_vec=N.array(rt_vec)

duration=1

#create junk regressor for 5-go_omission
junk_ons=onset_vec[[correct==5]]
junk_dur=N.zeros((junk_ons.shape))+duration
junk_pm=N.zeros((junk_ons.shape))+1
junk_3col=N.vstack([junk_ons, junk_dur, junk_pm]).T
junk_3col=fixEmpty(junk_3col)

N.savetxt('%s/junk.txt'%(outdir), junk_3col)

#create single RT regressors for 0-go_incorrect 1-go_correct 2-stop_fail_incorrect 3-stop_fail_correct
rt_all_ons=onset_vec[[((correct==0) | (correct==1) | (correct==2) | (correct==3))]]
rt_all_pm=rt_vec[[((correct==0) | (correct==1) | (correct==2) | (correct==3))]]
rt_all_dur=N.zeros((rt_all_ons.shape))+N.mean(rt_all_pm)
if len(rt_all_pm>0):
       rt_all_pm=rt_all_pm-N.mean(rt_all_pm)
rt_all_3col=N.vstack([rt_all_ons, rt_all_dur, rt_all_pm]).T
rt_all_3col=fixEmpty(rt_all_3col)

N.savetxt('%s/rt_all.txt'%(outdir), rt_all_3col)

#create Go - Incorrect [0]

go_incor_ons=onset_vec[[correct==0]]
go_incor_dur=N.zeros((go_incor_ons.shape))+duration
go_incor_pm=N.zeros((go_incor_ons.shape))+1
go_incor_3col=N.vstack([go_incor_ons, go_incor_dur, go_incor_pm]).T
go_incor_3col=fixEmpty(go_incor_3col)

N.savetxt('%s/go_incor.txt'%(outdir), go_incor_3col)

#create Go - Correct [1]

go_cor_ons=onset_vec[[correct==1]]
go_cor_dur=N.zeros((go_cor_ons.shape))+duration
go_cor_pm=N.zeros((go_cor_ons.shape))+1
go_cor_3col=N.vstack([go_cor_ons, go_cor_dur, go_cor_pm]).T
go_cor_3col=fixEmpty(go_cor_3col)

N.savetxt('%s/go_cor.txt'%(outdir), go_cor_3col)

#create Stop Fail - correct & incorrect [3,2]

stop_fail_ons=onset_vec[[((correct==2) | (correct==3))]]
stop_fail_dur=N.zeros((stop_fail_ons.shape))+duration
stop_fail_pm=N.zeros((stop_fail_ons.shape))+1
stop_fail_3col=N.vstack([stop_fail_ons, stop_fail_dur, stop_fail_pm]).T
stop_fail_3col=fixEmpty(stop_fail_3col)

N.savetxt('%s/stop_fail.txt'%(outdir), stop_fail_3col)

#create Stop - correct [4]
stop_cor_ons=onset_vec[[correct==4]]
stop_cor_dur=N.zeros((stop_cor_ons.shape))+duration
stop_cor_pm=N.zeros((stop_cor_ons.shape))+1
stop_cor_3col=N.vstack([stop_cor_ons, stop_cor_dur, stop_cor_pm]).T
stop_cor_3col=fixEmpty(stop_cor_3col)

N.savetxt('%s/stop_cor.txt'%(outdir), stop_cor_3col)
