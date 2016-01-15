#!/usr/bin/env python

#First authors: Jeanette Mumford, Leo Olemedo
#Editors: Mary Abbe Roe, Joel Martinez, Oct-Nov 2013, March-April 2014

#this script creates these files in each sub /behav/SC_run* folder
  #*_output_R.txt (a text file that is used to run behavioral analysis in R)           
  #R.Rout (used to create the summary_plots.png in R)
  #summary_plots.png (looks at RT across all trials, by condition, and by response type for that specific run)
  #*_output_Events.txt (used for fidl analysis)
  #junk.txt (omissions)
  #*_all.txt, where the * is active_s, active_ns, passive_s, or passive_ns (EVs used in FSL level analyses; two for each condition)

#codes:
  #buttons: if left is yes, sub_yes = 1; if right is yes, sub_yes = 4
  #conditions: 1 = sens, 0 = nonsens
  #combined conditions: 1 = active_sens, 2 = active_nonsens, 3 = passive_sens, 4 = passive_nonsens
  #responses: 0 = incorrect, 1 = correct, 2 = mismatch/repeat, 3 = too fast, 4 = no resp
  

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




#create R output file

#sensible conditions=1 for sensible and =0 for not sensible
cond_array=N.array(map(int,data['cond'].values()))
sense_cond=N.zeros((len(cond_array)))
sense_cond[N.logical_or(cond_array==1,cond_array==3)]=1

#onset vector
onset_vec=N.array(data['stimons'].values())

#Construct subject response
correct=[]
# correct:0=incor, 1=corr, 2=mismath/repeat, 3=too fast, 4=noresp
rt_vec=[]

#mismatches should be considered incorrect
#first RT cannot be faster than 500ms
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
    elif checkEqual(resp_vec) and rt_loop[0]>.5 and rt_loop[0]<10:
        #check if responses are equal and rt meets criteria       
        rt_vec.append(rt_loop[0])
        if (resp_vec[0]==sub_yes and sense_cond_loop==1) or (resp_vec[0]!=sub_yes and sense_cond_loop==0): 
            correct.append(1)
        else:
            correct.append(0)
    elif checkEqual(resp_vec)!=True and rt_loop[0]>.5 and rt_loop[0]<10:
        #check if repeat response
        rt_vec.append(rt_loop[0])
        correct.append(2)
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





#create FIDL output file

#Construct new empty vectors for subject response
correct1=[]
rt_vec1=[]

#construct new vectors for conditions; sensible conditions=1 for sensible and =0 for not sensible
cond_array=N.array(map(int,data['cond'].values()))
sense_cond=N.zeros((len(cond_array)))
sense_cond[N.logical_or(cond_array==1,cond_array==3)]=1

fidl_out=filename.replace('.pkl','_output_Events.txt')
f=open(fidl_out,'w')
f.write('2\tJunk\tActSens_Correct\tActSens_Incorrect\tActNon_Correct\tActNon_Incorrect\tPassSens_Correct\tPassSens_Incorrect\tPassNon_Correct\tPassNon_Incorrect\n')

for t in range(len(data['onsets'])):
    rt_loop1=data['rt'][t]
    cond=data['cond'][t]
    resp_vec1=data['resp'][t]
    sense_cond_loop=sense_cond[t]
    if resp_vec1==[]:
        correct1.append(0)
        rt_vec1.append('')
    elif checkEqual(resp_vec1) and rt_loop1[0]>.5 and rt_loop1[0]<10:
        #check if responses are equal and rt meets criteria       
        rt_vec1.append(rt_loop1[0])
        if (resp_vec1[0]==sub_yes and sense_cond_loop==1) or (resp_vec1[0]!=sub_yes and sense_cond_loop==0): 
            correct1.append('y')
        else:
            correct1.append('n')
    elif checkEqual(resp_vec1)!=True and rt_loop1[0]>.5 and rt_loop1[0]<10:
        #check if repeat response
        rt_vec1.append(rt_loop1[0])
        correct1.append('n')
    else:
        rt_vec1.append(rt_loop1[0])
        correct1.append(3)
    #This loop creates the text file for R
    trueons=data['stimons'][t]
    if correct1[t]=='y' and cond_array[t]==1:
           correct1[t]=1
           f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'8',rt_vec1[t]))
    elif correct1[t]=='y' and cond_array[t]==2:
           correct1[t]=3
           f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'8',rt_vec1[t]))
    elif correct1[t]=='y' and cond_array[t]==3:
           correct1[t]=5
           f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'8',rt_vec1[t]))
    elif correct1[t]=='y' and cond_array[t]==4:
           correct1[t]=7
           f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'8',rt_vec1[t]))
    elif correct1[t]=='n' and cond_array[t]==1:
           correct1[t]=2
           f.write('%.2f\t%s\t%s\t%s\t%s\n'%(trueons,correct1[t],'8','',rt_vec1[t]))
    elif correct1[t]=='n' and cond_array[t]==2:
           correct1[t]=4
           f.write('%.2f\t%s\t%s\t%s\t%s\n'%(trueons,correct1[t],'8','',rt_vec1[t]))
    elif correct1[t]=='n' and cond_array[t]==3:
           correct1[t]=6
           f.write('%.2f\t%s\t%s\t%s\t%s\n'%(trueons,correct1[t],'8','',rt_vec1[t]))
    elif correct1[t]=='n' and cond_array[t]==4:
           correct1[t]=8
           f.write('%.2f\t%s\t%s\t%s\t%s\n'%(trueons,correct1[t],'8','',rt_vec1[t]))
    else:
           correct1[t]=0
           f.write('%.2f\t%s\t%s\t%s\t%s\t%s\n'%(trueons,correct1[t],'8','', '',rt_vec1[t]))

f.close





#create regressors

#correct: 1=yes, 0=no, 2=mismatch, 3=RT out of range, 4=no response
correct=N.array(correct)
rt_vec=N.array(rt_vec)

duration=8

#create junk regressor (noresp: correct==4, mismatch: correct==3)
junk_ons=onset_vec[[N.logical_or(correct==3, correct==4)]]
junk_dur=N.zeros((junk_ons.shape))+duration
junk_pm=N.zeros((junk_ons.shape))+1
junk_3col=N.vstack([junk_ons, junk_dur, junk_pm]).T
junk_3col=fixEmpty(junk_3col)

N.savetxt('%s/junk.txt'%(outdir), junk_3col)


#create RT correct/incorrect (includes correct, and incorrect/mismatch, and uses mean RT instead of 1 in rtall_dur)

rt_all_rt=rt_vec[[((correct==1) | (correct==0) | (correct==2))]]

rt_all_ons=onset_vec[[((correct==1) | (correct==0) | (correct==2))]]
rt_all_pm=rt_vec[[((correct==1) | (correct==0) | (correct==2))]]
rt_all_dur=N.zeros((rt_all_ons.shape))+N.mean(rt_all_rt)
if len(rt_all_pm)>1:
       rt_all_pm=rt_all_pm-N.mean(rt_all_rt)
rt_all_3col=N.vstack([rt_all_ons, rt_all_dur, rt_all_pm]).T
rt_all_3col=fixEmpty(rt_all_3col)

N.savetxt('%s/rt_all.txt'%outdir, rt_all_3col)



#leftover RT regressor for time between response and end of 8sec trial
rt_left_ons1=onset_vec[[((correct==1) | (correct==0) | (correct==2))]]
rt_left_rt=rt_vec[[((correct==1) | (correct==0) | (correct==2))]]

rt_left_ons=rt_left_ons1+rt_left_rt
rt_left_dur=duration-rt_left_rt
rt_left_pm=N.zeros((rt_left_ons.shape))+1
rt_left_3col=N.vstack([rt_left_ons, rt_left_dur, rt_left_pm]).T
rt_left_3col=rt_left_3col[N.all(rt_left_3col > 0, axis=1)]
rt_left_3col=fixEmpty(rt_left_3col)

N.savetxt('%s/rt_left.txt'%outdir, rt_left_3col)



#correct/incorrect for each sentence type
sent_name=('active_s', 'active_ns', 'passive_s', 'passive_ns')
for sent_type in [1,2,3,4]:
    for cor_ind in [1]:
        loop_ons=onset_vec[[N.logical_and(correct==cor_ind, cond_array==sent_type)]]
        loop_dur=N.zeros((loop_ons.shape))+N.mean(rt_all_rt)
        loop_pm=N.zeros((loop_ons.shape))+1
        loop_3col=N.vstack([loop_ons, loop_dur, loop_pm]).T
        loop_3col=fixEmpty(loop_3col)
        if cor_ind==1:
            cortype='correct'
        if cor_ind==0:
            cortype='incorrect'
            
        N.savetxt('%s/%s_%s.txt'%(outdir,sent_name[sent_type-1], cortype), loop_3col)  


#overall incorrect across sentence types (correct==0,correct==2)
incor_ons=onset_vec[[((correct==0)|(correct==2))]]
incor_dur=N.zeros((incor_ons.shape))+N.mean(rt_all_rt)
incor_pm=N.zeros((incor_ons.shape))+1
incor_3col=N.vstack([incor_ons, incor_dur, incor_pm]).T
incor_3col=fixEmpty(incor_3col)

N.savetxt('%s/incor_all.txt'%outdir, incor_3col)



#I need to get the path of this python script
py_path=os.path.dirname(sys.argv[0])


subprocess.Popen("R --no-save --args %s < %s/check_rt.R > %s/R.Rout"%(R_out, py_path,outdir), shell=True)
