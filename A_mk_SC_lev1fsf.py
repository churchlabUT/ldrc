#!/usr/bin/env python


import os, sys, glob
import numpy as N

#function for makign basic fsf swaps
def makeBasicFsf(sub,cur_run, run_num):
    stubfile='/corral-repl/utexas/ldrc/SCRIPTS/Feat_Templates/new_lev1_stub.fsf'
    f=open(stubfile)
    design_file=open("%s/model/SC/designs/run%s.fsf"%(sub,run_num),'w')
    nvols=os.popen("fslnvols %s/bold.nii.gz"%(cur_run)).read().strip()
    for l in f.readlines():
        if l.find('NVOLS')>0:
            design_file.write(l.replace('NVOLS',nvols))
        elif (l.find('SUBDIRPATH')>0) | (l.find('RUNNUM')>0) | (l.find('RUNDIRPATH')>0) :
            design_file.write(l.replace('SUBDIRPATH', sub).replace('RUNNUM', run_num).replace('RUNDIRPATH',cur_run))
        else:
            design_file.write(l) 
    f.close()
    design_file.close()

#function to add single constrast info to fsf file
def writeSingleCon(sub,run_num, connum, true_con):
    design_file=open("%s/model/SC/designs/run%s.fsf"%(sub,run_num),'a')
    for cind in range(0, len(true_con)):
        design_file.write('# Real contrast_real vector %s element %s\n'%((connum+1), (cind+1)))
        design_file.write('set fmri(con_real%s.%s) %s\n\n'%((connum+1), (cind+1),true_con[cind]))
    for cind2 in range(0, len(true_con)/2):
         design_file.write('# Real contrast_orig vector %s element %s\n'%((connum+1), (cind2+1)))
         design_file.write('set fmri(con_orig%s.%s) %s\n\n'%((connum+1), (cind2+1),true_con[2*cind2]))
    design_file.close()
                        

basedir='/corral-repl/utexas/ldrc'

#sub_dirs=glob.glob("/corral-repl/utexas/ldrc/ldrc_?_[0-9][0-9][0-9]")

#sub_dirs=glob.glob("/corral-repl/utexas/ldrc/ldrc_*second")

sub_dirs=glob.glob("/corral-repl/utexas/ldrc/ldrc_c_[0-9][0-9][0-9]")

con_orig=N.array([[1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[1,0,1,0,-1,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[-1,0,1,0,-1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[-1,0,-1,0,-1,0,-1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[1,0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]])

#remember there's a deriv between each and 12 more regressors (total=34)
#onset_files=N.array(['active_ns_correct.txt','active_ns_incorrect.txt','active_s_correct.txt','active_s_incorrect.txt','passive_ns_correct.txt','passive_ns_incorrect.txt','passive_s_correct.txt','passive_s_incorrect.txt','rt_cor.txt','rt_incor.txt','junk.txt'])
onset_files=N.array(['active_ns_correct.txt','active_s_correct.txt','passive_ns_correct.txt','passive_s_correct.txt','incor_all.txt','junk.txt','rt_all.txt','rt_left.txt'])

runfeats=open("/corral-repl/utexas/ldrc/SCRIPTS/launch_A_SC_lev1.txt",'w')

for sub in sub_dirs:
    os.system("mkdir  %s/model/SC"%(sub))
    os.system("mkdir  %s/model/SC/designs"%(sub))
    run_loop=glob.glob("%s/BOLD/run*SC*"%(sub))
    for cur_run in run_loop:
        #create basic fsf template (contrasts added later)
        not_empty=N.ones((con_orig.shape[1],))
        run_num=cur_run[-1:]
        makeBasicFsf(sub,cur_run, run_num)
        runfeats.write("feat %s/model/SC/designs/run%s.fsf\n"%(sub,run_num))
        #figure out which onset files are empty
        for ons_num, line in enumerate(onset_files):
            ons_cur=N.loadtxt('%s/behav/SC/SC_Run%s/%s'%(sub,run_num, onset_files[ons_num]))
            if (len(ons_cur.shape)==1) & (N.sum(ons_cur[0])==0):
                not_empty[(2*ons_num):(2*ons_num+2)]=0
        #figure out proper contrasts
        for connum in range(0, con_orig.shape[0]):
            true_con=con_orig[connum]*not_empty
            if sum(true_con==1)>0:
                n_ones=sum(true_con==1)
                true_con[true_con==1]=1./n_ones
            if sum(true_con==-1)>0:
                n_negones=sum(true_con==-1)
                true_con[true_con==-1]=-1./n_negones
            #one last case, if we lost all of the 1's or -1's contrast must go
            if ((sum(con_orig[connum]==1)>0) & (sum(true_con>0)==0)) | ((sum(con_orig[connum]==-1)>0) & (sum(true_con<0)==0)):
                true_con=con_orig[connum]
            writeSingleCon(sub,run_num, connum, true_con)

runfeats.close()



        
    
