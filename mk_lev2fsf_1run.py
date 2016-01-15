#!/usr/bin/env python
#title             : mk_lev2fsf.py
#description       : This will create fsf files and text file
#                    for submitting jobs to tacc grid
#author            : Jeanette Mumford
#date              : 20130207
#usage             : mk_lev2fsf.py first_lev_parent_dir output_dir
#                    first_lev_parent_dir is the parent directory
#                    that contains all first level feats named run*.feat
#                    output_dir is where fsf files and lev2 gfeats will go
#notes             : Make sure the lev2_stub.fsf is in same directory as
#                    this script file
#==========================================================================

import os, sys, glob
import numpy as N


#a function that load matrix after /Matrix in mat and con files
def readMat(file):
    f=open(file, 'r')
    for i, line in enumerate(f):
        if line.startswith('/Matrix'):
            matrix_offset= i+1
    f.close
    mat=N.loadtxt(file, skiprows=matrix_offset)
    return mat

#a function to construct a dictionary for the good copes
def makeCopeList(ncopes):
    clist=dict()
    for c in range(0, ncopes):
        clist['cope%s'%(c+1)]=[]
    return clist

#function for creating fsf file
def makeFsf(cope_vec, fsf_store):
    py_path=os.path.dirname(sys.argv[0])
    stub="%s/lev2_stub.fsf"%(py_path)
    cope_vec_split=cope_vec[0].split("/")
    copename=cope_vec_split[-1]
    tmp_fsf="%s/%s.fsf"%( fsf_store, copename)
    os.system("cp %s %s"%(stub, tmp_fsf))
    nruns=len(cope_vec)
    f=open(tmp_fsf, 'a')
    f.write('#Total volumes \nset fmri(npts) %s \n\n'%(nruns))
    f.write('#Output directory \nset fmri(outputdir) "%s/%s_lev2"\n\n'%(fsf_store,copename))
    f.write('# Number of first-level analyses\n\nset fmri(multiple) %s\n\n'%(nruns))
    for run in range(0,nruns):
        f.write('# Higher-level EV value for EV 1 and input %s \nset fmri(evg%s.1) 1.0 \n \n'%((run+1), (run+1)))
        f.write('#Group membership for input %s \n\nset fmri(groupmem.%s) 1\n\n'%((run+1), (run+1)))
        f.write('# 4D AVW data or FEAT directory (%s) \n\nset feat_files(%s) "%s"\n\n'%((run+1), (run+1),cope_vec[run]))
    f.close()


#directory containing all first level feats
mod_dir=sys.argv[1]

#place to stick fsf files that are generated & feat analyses
outdir=sys.argv[2]

#this will need to be changed to run*.feat for Russ' stuff
feat_dirs=glob.glob('%s/Run1.feat'%(mod_dir))

for currun in feat_dirs:
    #Read in conmat and desmat
    confile="%s/design.con"%(currun.strip())
    conmat=readMat(confile)
    desfile="%s/design.mat"%(currun.strip())
    desmat=readMat(desfile)
    #create a vector that is zero if all values in column are 0
    num_nonzero=(desmat!=0).sum(axis=0)
    ncon=conmat.shape[0]
    for c in range(0,ncon):
        con_loop=conmat[c]
        #bad takes on a nonzero value if this is a bad con
        #if/then is to handle contrasts that are all 0s
        if sum(con_loop!=0)>0:
            bad=sum(num_nonzero[con_loop!=0]==0)
        else:
            bad=1
        #I need to generate the copelist dictionary if it does not exist
        if ('copelist' in locals()) ==False:
            copelist=makeCopeList(ncon)
        if bad==0:
            copelist['cope%s'%(c+1)].append("%s/stats/cope%s"%(currun,(c+1)))

#construct fsf files

g=open("%s/run_feat.txt"%(outdir), 'w')
for c in sorted(copelist.keys()):
    if len(copelist[c])>0:
        cope_vec=copelist[c]
        cope_vec_split=cope_vec[0].split("/")
        copename=cope_vec_split[-1]
        tmp_fsf="%s/%s.fsf"%(outdir, copename)
        makeFsf(cope_vec, outdir)
        g.write("feat %s\n"%(tmp_fsf))
g.close()        




