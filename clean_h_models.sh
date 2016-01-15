#!/bin/bash

# this script will move old model files into a new folder

basedir='/corral-repl/utexas/ldrc'


#subnums=`ls -d ${basedir}/*LD*`
#subnums=`ls -d ${basedir}/H_LD*_second`
subnums=`ls -d ${basedir}/H_LD*_third`
#subnums=`ls -d ${basedir}/LDFHO*_*`

for sub in $subnums

do
    if [[ ! -d `${sub}/model/SC/with_5` ]]
    then 
    mkdir ${sub}/model/SC/with_5
    mv ${sub}/model/SC/* ${sub}/model/SC/with_5
    fi
  
    if [[ ! -d `${sub}/model/SST` ]]
    then 
    mkdir ${sub}/model/SST/with_5
    mv ${sub}/model/SST/* ${sub}/model/SST/with_5
    fi
 


done
