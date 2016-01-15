#Creates output_Events.txt only for SC


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
f.write('2\tActSens_Correct\tActSens_Incorrect\tActNon_Correct\tActNon_Incorrect\tPassSens_Correct\tPassSens_Incorrect\tPassNon_Correct\tPassNon_Incorrect\n')

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
           correct1[t]=0
           f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'8',rt_vec1[t]))
    elif correct1[t]=='y' and cond_array[t]==2:
           correct1[t]=2
           f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'8',rt_vec1[t]))
    elif correct1[t]=='y' and cond_array[t]==3:
           correct1[t]=4
           f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'8',rt_vec1[t]))
    elif correct1[t]=='y' and cond_array[t]==4:
           correct1[t]=6
           f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'8',rt_vec1[t]))
    elif (correct1[t]=='n' or correct1[t]==0) and cond_array[t]==1:
           correct1[t]=1
           f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'8',''))
    elif (correct1[t]=='n' or correct1[t]==0) and cond_array[t]==2:
           correct1[t]=3
           f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'8',''))
    elif (correct1[t]=='n' or correct1[t]==0) and cond_array[t]==3:
           correct1[t]=5
           f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'8',''))
    elif (correct1[t]=='n' or correct1[t]==0) and cond_array[t]==4:
           correct1[t]=7
           f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'8',''))
    else:
           correct1[t]=999
           f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'8',''))

f.close
