#!/usr/bin/env python


import os, glob, sys

copenum=sys.argv[1]

sub_dirs=glob.glob("/corral-repl/utexas/ldrc/H*_c_*second")
#sub_dirs=glob.glob("/corral-repl/utexas/ldrc/H*_c_*third")


os.system("rm /corral-repl/utexas/ldrc/SCRIPTS/H_SST_contr_featdir_cope%s.txt"%(copenum))

f=open("/corral-repl/utexas/ldrc/SCRIPTS/H_SST_contr_featdir_cope%s.txt"%(copenum), 'w')

for sub in sub_dirs:
    if os.path.exists("%s/model/SST/cope%s_lev2.gfeat"%(sub, copenum)):
        f.write("%s/model/SST/cope%s_lev2.gfeat/cope1.feat/stats/cope1.nii.gz \n"%(sub, copenum))


f.close()   
    
