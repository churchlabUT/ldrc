#!/bin/bash

# this script will move old log files into a new folder, then remove all data except original raw imaging files, behavioral files, and WU_preprocess

basedir='/work/IRC/church/ldrc'

#subnums=`ls -d ${basedir}/ldrc*_*_[0-9][0-9][0-9]*`
#subnums=`ls -d ${basedir}/ldrc_c_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc_0_[0-9][0-9][0-9]`
#subnums=`ls -d ${basedir}/ldrc_*second`
#subnums=`ls -d ${basedir}/ldrc_*third`
#subnums=`ls -d ${basedir}/ldrc2_*`


for sub in $subnums

do 

  # remove data from /${sub}/behav/SC/*Run*   

    if [ -d `${sub}/behav/SC/*Run*` ]
    then 
    mkdir ${sub}/behav/SC/*Run*/temp
    mv ${sub}/behav/SC/*Run*/*_subdata.pkl ${sub}/behav/SC/*Run*/temp
    rm -f ${sub}/behav/SC/*Run*/*.*
    mv  ${sub}/behav/SC/*Run*/temp/*_subdata.pkl ${sub}/behav/SC/*Run*/
    rm -r "${sub}"/BOLD/*run*/temp
    fi


  # remove data from /${sub}/model/SC   

    if [ -d `${sub}/model` ]
    then 
    rm -r "${sub}"/model/SC/*
    fi
  
done
