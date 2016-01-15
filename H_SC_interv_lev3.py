#!/usr/bin/env python


import os, glob, sys

copenum=sys.argv[1]

#sub_dirs=glob.glob("/corral-repl/utexas/ldrc/H*_[0-9]_*second")
sub_dirs=glob.glob("/corral-repl/utexas/ldrc/H*_[0-9]_*third")
#sub_dirs=glob.glob("/corral-repl/utexas/ldrc/H*_?_*")
#sub_dirs=glob.glob("/corral-repl/utexas/ldrc/LDFHO*_[0-9]*")

os.system("rm /corral-repl/utexas/ldrc/SCRIPTS/H_SC_interv_featdir_cope%s.txt"%(copenum))

f=open("/corral-repl/utexas/ldrc/SCRIPTS/H_SC_interv_featdir_cope%s.txt"%(copenum), 'w')

for sub in sub_dirs:
    if os.path.exists("%s/model/SC/cope%s_lev2.gfeat"%(sub, copenum)):
        f.write("%s/model/SC/cope%s_lev2.gfeat/cope1.feat/stats/cope1.nii.gz \n"%(sub, copenum))


f.close()   
    
