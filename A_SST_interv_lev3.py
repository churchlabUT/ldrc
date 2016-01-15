#!/usr/bin/env python


import os, glob, sys

copenum=sys.argv[1]

sub_dirs=glob.glob("/corral-repl/utexas/ldrc/ldrc_[0-9]_[0-9][0-9][0-9]*")
#sub_dirs=glob.glob("/corral-repl/utexas/ldrc/ldrc_*second")
#sub_dirs=glob.glob("/corral-repl/utexas/ldrc/ldrc_*third")

os.system("rm /corral-repl/utexas/ldrc/SCRIPTS/A_SST_interv_featdir_cope%s.txt"%(copenum))

f=open("/corral-repl/utexas/ldrc/SCRIPTS/A_SST_interv_featdir_cope%s.txt"%(copenum), 'w')

for sub in sub_dirs:
    if os.path.exists("%s/model/SST/cope%s_lev2.gfeat"%(sub, copenum)):
        f.write("%s/model/SST/cope%s_lev2.gfeat/cope1.feat/stats/cope1.nii.gz \n"%(sub, copenum))


f.close()   
    
