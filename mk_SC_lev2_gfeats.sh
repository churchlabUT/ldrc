#!/bin/sh


rm /corral-repl/utexas/ldrc/SCRIPTS/launch_A_SC_lev2_gfeats.txt

rm /corral-repl/utexas/ldrc/SCRIPTS/launch_H_SC_lev2_gfeats.txt

#subnums=`ls -d /corral-repl/utexas/ldrc/ldrc_[0-9]_[0-9][0-9][0-9]`
#subnums=`ls -d /corral-repl/utexas/ldrc/ldrc_*second`
#subnums=`ls -d /corral-repl/utexas/ldrc/ldrc_*third`
#subnums=`ls -d /corral-repl/utexas/ldrc/ldrc_c_[0-9][0-9][0-9]`
#subnums=`ls -d /corral-repl/utexas/ldrc/ldrc2_[0-9]_[0-9][0-9][0-9]`
#subnums=`ls -d /corral-repl/utexas/ldrc/ldrc2_*second`
#subnums=`ls -d /corral-repl/utexas/ldrc/ldrc3_[0-2]_[0-9][0-9][0-9]`
subnums=`ls -d /corral-repl/utexas/ldrc/ldrc3_c*`

for sub in $subnums

do 

    dirpth=${sub}/model/SC

    cope=${dirpth}/run_feat.txt

    cat ${cope} >> launch_A_SC_lev2_gfeats.txt


done

#subnums=`ls -d /corral-repl/utexas/ldrc/H*_?_second`
#subnums=`ls -d /corral-repl/utexas/ldrc/H*_?_third`
#subnums=`ls -d /corral-repl/utexas/ldrc/LDFHO*_second`
#subnums=`ls -d /corral-repl/utexas/ldrc/LDFHO*_[0-9]`
#subnums=`ls -d /corral-repl/utexas/ldrc/LDFHO2*_1_3`

for sub in $subnums

do

   dirpth=${sub}/model/SC

   cope=${dirpth}/run_feat.txt

   cat ${cope} >> launch_H_SC_lev2_gfeats.txt


done