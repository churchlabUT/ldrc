#Create output_Event.txt file for SST

#Codes:
  #Cond: 0= arrow points left, 1= arrow points right, 2= arrow points left and stop , 3= arrow points right and stop
  #Stim: 0= left arrow, 1= right arrow
  #Resp: 0=no button press , 1=left button , 4=right button
  #Response/correct: 0=go_incorrect, 1=go_correct, 2=stop_fail, 3=stop_correct



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
        if (cond==0 and resp_vec1=='4') or (cond==1 and resp_vec1=='1'): #L <-/L button or R ->/R button
            correct1.append(1) #go correct
	    f.write('%.2f\t%s\t%s\t%f\n'%(trueons,correct1[t],'1',rt_loop1))
        elif (cond==2 and resp_vec1=='1') or (cond==3 and resp_vec1=='4'): #L X/R Button or R X/L button
            correct1.append(2) #stopfail incorrect
	    f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'1',''))
        elif (cond==2 and resp_vec1=='4') or (cond==3 and resp_vec1=='1'): #L X/L Button or R X/R Button
            correct1.append(2) #stop fail correct
	    f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'1',''))
        else:
            correct1.append(0)
	    f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'1',''))
#For trials with no response, 4-stop_correct 5-go_omission 
    elif rt_loop1==0:      
        rt_vec1.append(rt_loop1)
        if (cond==2 and resp_vec1=='0') or (cond==3 and resp_vec1=='0'): 
            correct1.append(3)
	    f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'1',''))
        else:
            correct1.append(0)
	    f.write('%.2f\t%s\t%s\t%s\n'%(trueons,correct1[t],'1',''))
    
    
f.close()
