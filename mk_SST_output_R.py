#!/usr/bin/env python

# First authors: Jeanette Mumford, Leo Olemedo
# Editors: Mary Abbe Roe (Dec. 2013)

# this script creates the *_output_R.txt file (a text file that is used to run behavioral analysis in R) in each sub /behav/SST_run* folder
# it was adapted just for behavioral analysis; if need to create the onsets files, must use mk_SST_onsets.py script! 

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
