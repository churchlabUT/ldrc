#!/bin/sh

# Script for making the confound file based on parameters discussed
# with Jess Church-Lang in 05/2014
# Goal of this script:  Supply "mask" regressors for each
# TR with an FD larger than 0.9.
# Also, separate motion parameter regressors will be created.
# After running this script you will want to add the output files.
# This script assumes the bold file is in a directory with a bold_mcf.par file
# to FEAT in the following manner (all can be found in scrub_files directory
#  1)  Add each of the 6 motions parameters by hand as separate EVs
#      withOUT HRF convolution, but DO turn on the derivative
#  2)  Add "fd_confounds.txt" in the "Add additional confound EVs
#  created 5/29/2014 by Jeanette Mumford

if [ "$1" == "-h" ]
then
  echo "Usage: make_confound.sh <bold file name> <fd thresh>"
fi

bold_file=$1
fd_thresh=$2


###################
# Check that bold file exists

if [ !  -e "$bold_file" ]
then
  echo "$bold_file does not exist, check input path and make sure bold file is entered before fd threshold"
fi


#########################
# set directory and check for parfile
dir_loc=`dirname ${bold_file}`
par_file=${dir_loc}/bold_mcf.par
mkdir $dir_loc/scrub_files

if [ !  -e "$par_file" ]
then
  echo "$par_file does not exist, run mcflirt with proper flags"
fi


####################
# Split up .par file

echo "Creating separate motion parameter files ..."

ctr=1
for x in 1 3 5 7 9 11
do
cut -f $x -d' ' ${par_file} > $dir_loc/scrub_files/motpars_${ctr}.txt
ctr=`expr $ctr + 1`
done


####################
# Obtain TR mask

echo -e "Computing FD and scrubbing mask regressors \n"
echo -e "(this will take a few minutes) ... \n\n"

fsl_motion_outliers -i $1 -o $dir_loc/scrub_files/fd_confounds.txt --fd --thresh=$fd_thresh -v -s $dir_loc/scrub_files/fd_series.txt -p $dir_loc/scrub_files/fd_plot.png
