#!/usr/bin/env python


import os, glob, sys

copenum=sys.argv[1]

sub_dirs=glob.glob("/corral-repl/utexas/ldrc/ldrc_?_[0-9][0-9][0-9]")
h_sub_dirs=glob.glob("/corral-repl/utexas/ldrc/H_*_MR1")


os.system("rm /corral-repl/utexas/ldrc/SCRIPTS/A_SC_all_featdir_cope%s.txt"%(copenum))
os.system("rm /corral-repl/utexas/ldrc/SCRIPTS/H_SC_all_featdir_cope%s.txt"%(copenum))


f=open("/corral-repl/utexas/ldrc/SCRIPTS/A_SC_all_featdir_cope%s.txt"%(copenum), 'w')

for sub in sub_dirs:
    if os.path.exists("%s/model/SC/cope%s_lev2.gfeat"%(sub, copenum)):
        f.write("%s/model/SC/cope%s_lev2.gfeat/cope1.feat/stats/cope1.nii.gz \n"%(sub, copenum))

f=open("/corral-repl/utexas/ldrc/SCRIPTS/H_SC_all_featdir_cope%s.txt"%(copenum), 'w')

for sub in h_sub_dirs:
    if os.path.exists("%s/model/SC/cope%s_lev2.gfeat"%(sub, copenum)):
        f.write("%s/model/SC/cope%s_lev2.gfeat/cope1.feat/stats/cope1.nii.gz \n"%(sub, copenum))


f.close()   


sub_dirs1=glob.glob("/corral-repl/utexas/ldrc/ldrc_?_[0-9][0-9][0-9]")
h_sub_dirs1=glob.glob("/corral-repl/utexas/ldrc/H_*_MR1")


os.system("rm /corral-repl/utexas/ldrc/SCRIPTS/A_SST_all_featdir_cope%s.txt"%(copenum))
os.system("rm /corral-repl/utexas/ldrc/SCRIPTS/H_SST_all_featdir_cope%s.txt"%(copenum))


f=open("/corral-repl/utexas/ldrc/SCRIPTS/A_SST_all_featdir_cope%s.txt"%(copenum), 'w')

for sub in sub_dirs1:
    if os.path.exists("%s/model/SC/cope%s_lev2.gfeat"%(sub, copenum)):
        f.write("%s/model/SC/cope%s_lev2.gfeat/cope1.feat/stats/cope1.nii.gz \n"%(sub, copenum))

f=open("/corral-repl/utexas/ldrc/SCRIPTS/H_SST_all_featdir_cope%s.txt"%(copenum), 'w')

for sub in h_sub_dirs1:
    if os.path.exists("%s/model/SC/cope%s_lev2.gfeat"%(sub, copenum)):
        f.write("%s/model/SC/cope%s_lev2.gfeat/cope1.feat/stats/cope1.nii.gz \n"%(sub, copenum))


f.close()   
