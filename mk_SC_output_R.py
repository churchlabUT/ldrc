#!/usr/bin/env python

#First authors: Jeanette Mumford, Leo Olemedo
#Editors: Mary Abbe Roe, Joel Martinez, Oct-Nov 2013

#this script creates these files in each sub /behav/SC_run* folder
  #*_output_R.txt (a text file that is used to run behavioral analysis in R)           
  #R.Rout (used to create the summary_plots.png in R)
  #summary_plots.png (looks at RT across all trials, by condition, and by response type for that specific run)
  #*_output_Events.txt (used for fidl analysis)
  #junk.txt
  #*_correct.txt and *_incorrect.txt, where the * is active_s, active_ns, passive_s, or passive_ns (EVs used in FSL level analyses; two for each condition)

#codes:
  #buttons: if left is yes, sub_yes = 1; if right is yes, sub_yes = 4
  #conditions: 1 = sens, 0 = nonsens
  #combined conditions: 1 = active_sens, 2 = active_nonsens, 3 = passive_sens, 4 = passive_nonsens
  #responses: 0 = incorrect, 1 = correct, 3 = mismatched repeated response or too short/long RT, 4 = no resp
  

import sys
import numpy as N
import pickle
import os
import subprocess



#a function to determine if all responses match
def checkEqual(lst):
       return lst[1:] == lst[:-1]

#function to replace empty EV 3 column format with 000
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

# create R_output.txt file
f=open(filename,'rb')
data=pickle.load(f)
f.close()

#Determine whether 1 yes or no
version=data['info']['Yes']
if version==u'left':
    sub_yes='1'
elif version==u'right':
    sub_yes='4'
else:
    print "WARNING: Cannot read info on r/l version"
    sys.exit()

#sensible conditions=1 for sensible and =0 for not sensible
cond_array=N.array(map(int,data['cond'].values()))
sense_cond=N.zeros((len(cond_array)))
sense_cond[N.logical_or(cond_array==1,cond_array==3)]=1

#onset vector
onset_vec=N.array(data['stimons'].values())

#Construct subject response
correct=[]
# correct:0=incor, 1=corr, 3=mismatch, 4=noresp
rt_vec=[]

#mismatches should be considered incorrect
#first RT cannot be faster than 2s; changing this to 500ms
R_out=filename.replace('.pkl','_output_R.txt')
f=open(R_out,'w')
f.write('TrialNum\tOnset\tTrueOns\tCond\tSentence\tResp\tRT\tcorrect\n')
for t in range(len(data['onsets'])):
    rt_loop=data['rt'][t]
    sense_cond_loop=sense_cond[t]
    resp_vec=data['resp'][t]
    if resp_vec==[]:
        correct.append(4)
        rt_vec.append(1000)
    elif checkEqual(resp_vec) and rt_loop[0]>.5 and rt_loop[0]<12:
        #check if responses are equal and rt meets criteria       
        rt_vec.append(rt_loop[0])
        if (resp_vec[0]==sub_yes and sense_cond_loop==1) or (resp_vec[0]!=sub_yes and sense_cond_loop==0): 
            correct.append(1)
        else:
            correct.append(0)
    else:
        rt_vec.append(rt_loop[0])
        correct.append(3)
    #This loop creates the text file for R
    trueons=data['stimons'][t]
    ons=data['onsets'][t]
    cond=data['cond'][t]
    sentences=[i.strip() for i in data['sentence_info']['sentence']]
    sentence=sentences[t]
    if len(data['rt'][t])==0:
           f.write('%.2f\t%.2f\t%.2f\t%s\t%s\t%s\t%s\t%s\n'%(t,ons,trueons,cond,sentence,'NaN','NaN', correct[t]))
    else:
           for trialrep in range(len(data['rt'][t])):
                  if resp_vec[trialrep]==sub_yes:
                         sub_resp='sense'
                  else:
                         sub_resp='nonsense'
                  f.write('%.2f\t%.2f\t%.2f\t%s\t%s\t%s\t%s\t%s\n'%(t,ons,trueons,cond,sentence,sub_resp,rt_loop[trialrep], correct[t]))
 
f.close
