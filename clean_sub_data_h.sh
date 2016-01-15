#!/bin/bash

# this script will remove all BOLD data (except bold.nii.gz)and model data from Houston participants in order to rerun BOLD analyses

basedir='/corral-repl/utexas/ldrc'

#subnums=`ls -d ${basedir}/H_LD*_second`
#subnums=`ls -d ${basedir}/H_LD*_third`
#subnums=`ls -d ${basedir}/LDFHO*[0-9][0-9][0-9][0-9]_[0-9]`
subnums=`ls -d ${basedir}/LDFHO*_second*`

for sub in $subnums

do  

  # remove data from /${sub}/model

    #if [ -d `${sub}/model/` ]
    #then 
    #rm -r "${sub}"/model/SC
    #rm -r "${sub}"/model/SST
    #fi
  
  # remove data from /${sub}/anatomy   

    #if [ -d `${sub}/anatomy` ]
    #then 
    #rm -r "${sub}"/anatomy/*
    #fi

  # remove data from /${sub}/BOLD   

    #if [ -d `${sub}/BOLD` ]
    #then 
    #rm -r "${sub}"/BOLD/*
    #fi

  # remove .nii.gz data from /${sub}/raw and rename the raw folder with the main sub directory folder 

    #if [ -d `${sub}/raw` ]
    #then
    #rm -f "${sub}"/raw/*.nii.gz

    #subid=${sub##*/}
    #subraw=`ls -d ${sub}/raw/*LD*`
    #subrawid=${subraw##*/}
    #mv ${sub}/raw/${subrawid} ${sub}/raw/${subid}
    #fi



  # remove data from  /${sub}/BOLD (all except bold.nii.gz). This section moves bold.nii.gz into a temporary directory, deletes other files, renames old QA for reference, and then deletes temp

    runnums=`ls -d ${sub}/BOLD/*RUN*`

    for run in $runnums

    do
      if [ -d `${run}` ]
      then
      mkdir ${run}/temp
      mv ${run}/bold.nii.gz ${run}/temp
      mv ${run}/bold_orig.nii.gz ${run}/temp
      rm -f ${run}/*.*
      #mv ${run}/scrub_files ${run}/old_scrub_files
      #mv ${run}/QA ${run}/old_QA
      #mv ${run}/old_QA ${run}/old_scrub_files
      mv ${run}/temp/bold.nii.gz ${run}
      mv ${run}/temp/bold_orig.nii.gz ${run}
      rm -r "${run}"/temp
      fi

   done

done
