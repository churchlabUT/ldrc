#title             : Readme file for conducting SC Feat analysis
#description       : This readme file will guide you in conducting SC Feat analysis
#author            : Leonel Olmedo
#date              : 2013/03/06
#notes             : To run most scripts you must be in /corral-repl/utexas/ldrc/SCRIPT directory
		     To make exectutble type chmod +x *sh and chmod +x *py (incase scripts aren't running) 
		     Once executable, simply type ./scriptname to run the script
#==========================================================================


#All scripts called are located in /corral-repl/utexas/ldrc/SCRIPTS. Emacs must be opened while in /SCRIPTS and tacc must be in /SCRIPTS to run each script.
#All scripts have comments to explain what the script does. Simply load it into emacs to read comments or make changes.
#Make sure runs that will not be used have an x_ placed before the behav and BOLD files or that they are moved to an omitted folder.If not, the script will include them in the launch files.


###################################################################

STEP 1 -  Level 1 Analysis: Creating feat directories for each run

################################################################### 

#MAKE SURE ONLY RUNS THAT PASSED QUALITY CONTROL (BEHAV & BOLD) ARE PRESENT, OTHERWISE THE LEVEL 1 ANALYSIS WILL HAVE TO BE REPEATED. 

## Scripts used in Level 1:

1. A_mk_SC_lev1fsf_2_scrub.py (Creating Austin feat directories for each run; older versions:A_mk_SC_lev1fsf.py, A_mk_SC_lev1fsf_2.py)
2. H_mk_SC_lev1fsf_2_scrub.py (Creating Houston feat directories for each run; older versions:H_mk_SC_lev1fsf.py, H_mk_SC_lev1fsf_2.py)


     ** A_mk_SC_lev1fsf_2_scrub.py - Open this python script in emacs to change the group, then run the script in tacc to create the launch text file below for Austin.  
		
		## Open the txt file (launch_A_SC_lev1.txt) in emacs to delete the subjects you don't need, and launch the txt file below in terminal. Make sure it is the text file that the python script saves to; you can find this in the python script.

                   This text file will be used to make all the fsf designs for Feat with the proper contrast parameters; it uses each fsf design to create feat directories saved in each subjects directory under /model/SC/Run*.feat where * is 1-3.

		   ## LARGEMEM NODE:

                      Use the largemem node to run about 5-10 subjects at a time that have a lot of scrubbing regressors. The scrubbing regressors require more memory to process the designs during level 1. This is the only level you will need the largemem node for.

		      24:00:00 = how many hours you want it to run for. The largemem only goes by 24 or 48 for -p, and -e 1way only

		     **in terminal type (for running it on the largemem node): launch -s launch_A_SC_lev1.txt -j Analysis_Lonestar -p 48 -e 1way -r 24:00:00 -q largemem -m churchlab@austin.utexas.edu

	           ## NORMAL NODE:

                      04:00:00 = how many hours you want it to run for. -p 120 is for 10 lines (12 for each line)

                      **in terminal type (for running it on the normal node): launch -s launch_A_SC_lev1.txt -j Analysis_Lonestar -p 120 -e 2way -r 04:00:00 -m churchlab@austin.utexas.edu
			    

     ** H_mk_SC_lev1fsf_2_scrub.py - Open this python script in emacs to change the group, then run the script in tacc to create the launch text file below for Houston. It works the same as the python script for Austin.

     		## Open the txt file (launch_H_SC_lev1.txt or launch_H_SC_lev1_1.txt) in emacs to delete the subjects you don't need, and launch the txt file below in terminal.

		      **in terminal type (on largemem node): launch -s launch_H_SC_lev1.txt -j Analysis_Lonestar -p 48 -e 1way -r 24:00:00 -q largemem -m churchlab@austin.utexas.edu
                    
		      **in terminal type (on normal node): launch -s launch_H_SC_lev1.txt -j Analysis_Lonestar -p 60 -e 2way -r 04:00:00 -m churchlab@austin.utexas.edu
			    

     ** LEVEL 1 QC:

	** After the job is complete, check that the feat directories were made in each subjects directory under /model/SC/Run*.feat where * is 1-3.

	** design.png

	   ## des_check_SC_lev2.sh - run this script in terminal inside SCRIPTS to grab all the design.png files from each subs run.  Puts them into an html file to look at each group - saved in SCRIPTS/SC/Lev2_QC

	      ** On the Preprocessing_QC sheet, note copes that should be left out of group analysis because of empty EVs.
		      
	** registration

	   ## reg_check_SC_lev2.sh - run this script in terminal inside SCRIPTS to grab all the design.png files from each subs run.  Puts them into an html file to look at each group - saved in SCRIPTS/SC/Lev2_QC
		      
	      ** make a note of how well the registration worked for each subs run

	** report.html - Inside the model directory, open the report.html file for each cope to look for any errors and to check that registration looked ok..

	** filtered_func_data.nii.gz - Look at filtered_func data for any "blank volumes"

	** mask.nii.gz - Check mask.nii.gz, bg_image.nii.gz (average of brains across subjects - used for overlaying stats maps onto), zstat, and varcope are there for each cope.

	## check_collin_SC.R - Check for any collinearity issues in the data

		## Open the script in emacs.  Open R in tacc inside /SCRIPTS. Run the code to look at vif numbers that are too high (above 10), indicating collinearity with between EVs. There are notes in the code that describe how it works. 

                   ** (OLD: Right now (4/23/14) a few subjects have high V15 (leftover_RT) vif values and have been noted to investigate during level 3)
		   ** V1 - active_ns
		      V2 - active_ns derivative
		      V3 - active_s
		      V4
		      V5 - passive_ns
		      V6
		      V7 - passive_s
		      V8
		      V9 - incorr_all
		      V10
		      V11 - junk
		      V12
		      V13 - RT_all
		      V14
		      V15 on - motion regressors
		      


###############################################################

STEP 2  - Level 2 Analysis: Creating gfeats (group feats) for each subject


################################################################


#MAKE SURE ONLY RUNS THAT PASSED QUALITY CONTROL ARE PRESENT, OTHERWISE THE LEVEL 2 ANALYSIS WILL HAVE TO BE REPEATED. 


** Scripts used in Level 2:

1. mk_SC_lev2fsf.sh
2. mk_lev2fsf.py OR mk_lev2fsf_run2.py
3. mk_SC_lev2_gfeats.sh


  ** mk_SC_lev2fsf.sh - Edit this script in emacs, then run it to create the two launch files listed below.

     ## Then run the launch files below in terminal if only running 2-5 subjects. Launch them if >5 subjects. These text files use the python script **mk_lev2fsf.py** to make the second level cope fsf files for both Austin and Houston. (12*# of jobs) (use mk_lev2fsf_run2.py if there is no usable Run1, because it's hardcoded)

	 ** To make Austin SC fsf files in terminal type: launch -s launch_A_SC_lev2fsf.txt -j Analysis_Lonestar -r 01:00:00 -m churchlab@austin.utexas.edu

	 ** To make Houston SC fsf files in terminal type: launch -s launch_H_SC_lev2fsf.txt -j Analysis_Lonestar -r 04:00:00 -m churchlab@austin.utexas.edu

     ## Make sure there is a cope (Contrast Of Parameter Estimate) for each contrast in the task folders within the subjects model folder. 


  ** mk_SC_lev2_gfeats.sh - Edit this script in emacs, then run it to create the level 2 gfeats by running feat on the fsf files we created earlier. This script makes .txt files. 

     ## Then launch the files below. If there are only a few runs, combine them with the SST .txt files and launch them together. A gfeat is a group feat directory. Each gfeat corresponds to a different cope and takes up one line in the launch files listed below.

	 ** To make Austin SC gfeats in terminal type: launch -s launch_A_SC_lev2_gfeats.txt -j Analysis_Lonestar -p 120 -e 2way -r 02:00:00 -m churchlab@austin.utexas.edu

	 ** To make Houston SC gfeats in terminal type: launch -s launch_H_SC_lev2_gfeats.txt -j Analysis_Lonestar -p 120 -e 2way -r 04:00:00 -m churchlab@austin.utexas.edu
			   
     ## To launch individual copes for individual subjects, just open up the text file and use the lines for the subjects you want to run analysis on. Each cope corresponds to a different contrast, set in the first level, 

	and will make a second level copeX_lev2.gfeat, where X correponds to a different contrast number.


  ## LEVEL 2 QC: Check that the copes for all of the subjects are created before running level 3 analysis and those that need to be left out of group analysis are noted.

     ** run find_lev2_copes.sh found in SCRIPTS/lev2_QC/SC to create quick txt files of which subjects have which lev2 copes created



###############################################################

STEP 3  - Level 3 Analysis: Running group analysis

################################################################

## All scripts are located in /corral-repl/utexas/ldrc/SCRIPTS. 
## Make sure all subjects have completed level 2 before running level 3 analysis.

----
## Section 1 - Generate feat directories 

   ##  The contrasts as of 4/28/14 for Project 4 SC are as follows. Make sure you know which EVs were labeled 1, 0 or -1, as this will be important for level 3. Refer to these when making the feat directories/contrasts. 

       The current list of contrasts are in the mk_ts.sh script under /GROUPS. You can also find them in the design.png for Level 1.

	     #COPE1 = ACTIVE_NS (1) VS. BASELINE (0)
	     
	     #COPE2 = ACTIVE_S (1) VS. BASELINE (0)

	     #COPE3 = PASSIVE_NS (1) VS. BASELINE (0)

	     #COPE4 = PASSIVE_S (1) VS. BASELINE (0)

	     #COPE5 = ACTIVE (1) VS. PASSIVE (-1)

	     #COPE6 = SENSIBLE (1) VS. NON-SENSIBLE (-1)

	     #COPE7 = CORRECT (1) VS. INCORRECT (-1)

	     #COPE8 = INCORRECT (1) VS. BASELINE (0)

	     #COPE9 = RT_ALL (1) VS. BASELINE (0)

	     #COPE10 = CORRECT (1) VS. BASELINE (0)

	     #COPE11 = RT_LEFT (1) VS. BASELINE (0)


   ## The 3 scripts below are used to generate a list of feat directories to be used in level 3 analysis. Type the script name and a number, indicating the contrast (cope) you want to look at. 

      For example, to create the txt file for cope1 SC interventions in terminal you should type: python A_SC_interv_lev3.py 1 

      1. # all_lev3.py  ** THIS DOES NOT EXIST RIGHT NOW **
      2. A_SC_interv_lev3.py
      3. A_SC_contr_lev3.py
      4. H_SC_interv_lev3.py


     ## INTERVENTION
     
	**  A_SC_interv_lev3.py - use this script to generate the list of all intervention subjects that is saved to A_SC_interv_featdir_copeX.txt where x corresponds to the cope you specified when you ran the script


     ## CONTROLS

	**  A_SC_contr_lev3.py - use this script to generate the list of all control subjects that is saved to A_SC_contr_featdir_copeX.txt where x corresponds to the cope you specified when you ran the script


     ## ALL (INT, CONTROLS, SC, & SST) - both intervention and controls for  Austin and Houston SC and SST - DOES NOT EXIST RIGHT NOW		

	**  all_lev3.py - run this script in terminal to generate the list of all LDRC subjects for both Austin and Houston and both SST and SC. 

			  In the text files generated from this script, X corresponds to the cope you specified when you ran the script. This text file will have a list of the cope images you will use in Feat, detailed below in section 2. 

			  For example, to look at COPE 1, correct vs baseline where correct was given a value of 1 and everything else was 0, you would type: ./all_lev3.py 1

	    ## This creates the text file A_SC_all_featdir_copeX.txt to list the Austin subjects for SC

	    ## This creates the text file A_SST_all_featdir_copeX.txt to list the Austin subjects for SST
	    
	    ## This creates the text file H_SC_all_featdir_copeX.txt to list the Houston subjects for SC

	    ## This creates the text file H_SST_all_featdir_copeX.txt to list the Houston subjects for SST
	


------

## Section 2 - Create contrasts in Feat GUI (Review how to make models depending on the type of group comparisons looking at)

   ## Type "Feat &" in terminal (in GROUP/) and then load  /corral-repl/utexas/ldrc/GROUP/designs/design_template.fsf. This will preload the specifications for the level 3 analysis. 

      ## Under the Data tab:
	    
	   1. Change the number of inputs to the number of subjects you are loading. Then click on "Select cope images", then click "Paste." 

	   2. Open the txt file you created in Section 1 and copy and paste it into the Feat window. Click ok, and make sure all the inputs appear on the next page. 

	   3. If you are running a paired t-test (comparing timepoints) the subjects need to be entered in order by S1T1, S1T2, S2T1, S2T2, etc.

	      If you are comparing two groups, all of the subs in the first group are entered first, and then all the subs in the second group are entered.

	      If any copes are missing you will get an error which specifies which subject is the issue. Check that subject, rerun level2 if need be; otherwise, click ok and you should be good to go. 

	   4. In the Output directory, replace design_template.fsf with the name of the contrast you are looking at, such as "corr_v_baseline" with the path name /corral-repl/utexas/ldrc/GROUP/Controls/SC/corr_v_baseline

	      **  MAKE SURE NOT TO OVERWRITE THE DESIGN_TEMPLATE.FSF FILE. **  If this does happen, there is a copy of the design_template in /corral-repl/utexas/ldrc/SCRIPTS/ called design_template_lev3.py

		  For now we are keeping the Austin and Houston level 3 analysis separate. Ask Jessica which contrasts to look at and check the mk_ts.py under /corral-repl/utexas/ldrc/GROUP/ for the correct spelling of the contrasts.

      ## Under the Stats tab:

	   1. Click Full Model Setup. 

	   2. Under the EVs tab:

	        GROUP always has 1's - do NOT change this.

		     Model 1: One group - Only 1 EV. EV1 contains all 1s if you are only running one group (Controls, First Interventions, Second Interventions). 

		     Model 2: Two groups - 1. Only 1 EV: EV1 contains all 1s for group1 (ex: first interventions) and all -1s for group2 (controls). 

					 **2. 2 EVs: EV1 contains all 1s for group1, 0s for group2, and EV2 contains all 0s for group1 and 1s for group2 (usually use)

		     Model 3: Timepoints - Several EVs, depending on number of subjects and timepoints.  Set it up like a paired t-test (see Jeanette's lectures/textbook). EV1 contains alternating 1's and -1's, set up the rest accordingly.	       

      ## Under the Contrasts & F-tests tab,

	    Model 1: One group -  check that there are two contrasts and that C1 is greater than baseline (1) and C2 is less than baseline(-1). [ 1]
																		[-1]

		     For example, C1 would indicate when correct > baseline and C2 would be when correct < baseline;  C1 would be when correct > incorrect and C2 would be when correct < incorrect.

	    Model 2: Two groups - [1 -1]
			          [-1 1]

            ## Click View design to check the model before clicking Done, or click Done to finish the full model setup (you will still automatically view design when you click Done)	   

      ## Double check EVERYTHING, just in case you missed something.

	 ## Once you're sure about everything, save your new design in the design folder within the group you are running as SC_copeX.fsf where X is the cope number. 

            For example, if you are working on Controls then the path you are saving the design to is /corral-repl/utexas/ldrc/GROUP/Controls/Designs/SC_cope1.fsf
		    
            Click ok once you are sure you're saving it to the right folder. Then check that it saved to that folder in Fetch; you can move everything that is not a .fsf file into the junk folder.

      ## Repeat all of the steps in section 1 and 2 for each contrast before launching the SC designs in the last section below.

	    
----
## Section 3 - Launching the designs

   ## Launch the designs you made for each contrast:

      ** Intervention: Open the launch_lev3.txt file in emacs (under SCRIPTS/) and change the feats for the subjects you plan on running

	 ** Launch the file in terminal: launch -s launch_lev3.txt -j Analysis_Lonestar -p 120 -e 2way -r 01:00:00 -m churchlab@austin.utexas.edu


      ** Controls: Open the launch_lev3_controls.txt file in emacs (under SCRIPTS/) and change the feats for the subjects you plan on running

	 ** Launch the file in terminal: launch -s launch_lev3_controls.txt -j Analysis_Lonestar -p 120 -e 2way -r 04:00:00 -m churchlab@austin.utexas.edu


   ## Check that the .gfeats for all of your contrasts are in the correct folder after launching, then check that the zstat1.nii.gz and zstat2.nii.gz are there - for example in 'SC/active_v_passive/cope1.feat/stats' folder

----
## Section 4 - Copying and renaming the thresholded and unthresholded zstat.nii.gz files

   ## This script renames each groups level 3 comparison stats in order to easily identify the data when loading it onto inflated brains in Caret

   ** Open the script rename_zstat.R

     ## run the functions that copy and rename the thresholded and unthresholded files

     ## run each group (SC and SST) that you need to copy; make sure to change the subs variable if the number of subjects in the group has changed

     ## double check that all of the files copied into the GROUP/zstats folder in addition to the original comparison folders for each group; these are all copied into one location in order to easily
                  
        fetch all of the zstat.nii.gz files in order to read into Caret.

     ## To turn the zstat thresh images into -1 rather than 1 (so that you can display both zstats on the same brain), using the following fsl command line; ONLY DO THIS TO ZSTAT2 OF THRESHOLDED IMAGES; there is now a script that does this for you called mk_neg_zstat2.sh

	** fslmaths input -mul -1 output

----
## Section 5 - Applying the zstat files to inflated brains in Caret

   ## See the PDF on the server under Church_Lab/


----
NEXT STEP: ROI ANALYSIS

## Open the /SCRIPTS/mk_all_ts.sh script in emacs.

   ## Make ROIs:
   
      ## Open the PREDICTIONS.xlsx file in Finder under Church_Lab/Church_Lab/PREDICTIONS.xlsx.  This file lists all of the MNI coordinates, their region descriptions, and their community definitions that we could look at.  

	 It also includes every hypothesis we could possibly explore for SC, SST, and DTI/anatomy. Under SC, the areas highlighted in yellow are those that have been looked at by previous research groups (listed at the bottom of the SC tab).

	 This list is old, however, so check with Jess/Mary Abbe first.

      ## Check which ROIs have already been made by looking in one of the ROI directories: /corral-repl/utexas/ldrc/GROUP/S1_S2_5_23_13/rois

      ** In the first section in mk_all_ts.sh, change the MNI coordinates to the one you plan on making. Then open fslview to find the voxel coordinates for those MNI coordinates and enter them into the mk_all_ts.sh script as well.

      ** In tacc, in /corral-repl/utexas/ldrc/GROUP/S1_S2_5_23_13/rois, run the first section of mk_all_ts.sh to create the roi.  It will save it in /corral-repl/utexas/ldrc/GROUP/S1_S2_5_23_13/rois.

	 ## Check if the roi's were made by seeing if they exist in the rois folder and opening them in fslview, to make sure the coordinate were correct.

   ## Apply ROIs onto different SC or SST contrasts

      ** In the mk_ts.sh script, add the ROI you just made to the SST_rois = (...) or SC_rois = (...) lists.
   
      ** CHANGE THE SUBDIRS FOR THE GROUP YOU'RE RUNNING BEFORE RUNNING THE SCRIPT WITHIN ROI DIRECTORY IN TERMINAL!  

      ** Run the SC or SST section of the script for applying ROIs onto different contrasts in tacc in /corral-repl/utexas/ldrc/GROUP/S1_S2_5_23_13/rois

   ## Run ROI analyses R script (located in /SCRIPTS) for stats and stat figures

      ** roi_analyses_5_2_14.R - open this script in emacs and run each part according to script.


   ## View ROI spheres in CARET



















############### OLD
   ** Create figures for each ROI, contrast, task, etc.

      ** In /corral-repl/utexas/ldrc/GROUP/S1_S2_5_23_13, open S1_S2_perm_test.R script in emacs and open R in terminal

	 ** The first part of the script contains the function that grabs the rois and gfeats, obtains the original stats and reads in data, runs the permutation test, and computes the corrected p-values.

	    The output contains p-values as well as reads in data sets if you need to make plots for significant things.

	    ## Make sure to set nperms to the number of permutations you'd like to run.  Then run the first part of the script (the function and the nperms) in R in terminal.

	 ## Change the tasks="SST" to SST or SC, depending on the task you are analyzing, and that the right contrasts for each task are included. Run the rest of the script.

	    ** This creates dat_p_perm_X, where X is the contrast, to show you corrected and original p values for each contrast. 

	    ** It also creates dat_c_contrast dataframe for the controls

	    ** And it creates % signal change figures for AUSTIN INTERVENTION SUBJECTS and % signal change figures for AUSTIN INTERVENTION + CONTROL SUBJS


	    
    ** Old contrasts from 3/6/13 for Project 4 SC

	     #COPE 1 = CORRECT (1) VS BASELINE (0)		corr_v_baseline			       c1 = correct greater than baseline (1)	     and   c2 = correct less than baseline (-1)

	     #COPE 2 = RT_CORR (1) VS BASELINE (0)

	     #COPE 3 = ACTIVE_NS (1) VS BASELINE (0)

	     #COPE 4 = ACTIVE_S (1) VS BASELINE (0)

	     #COPE 5 = PASSIVE_NS (1) VS BASELINE (0)

	     #COPE 6 = PASSIVE_S (1) VS BASELINE (0)

	     #COPE 7 = SENSIBLE (1) VS NON-SENSIBLE(-1)		sensible_v_non_sensible			c1 = sensible greater than non-sensible (1)   and   c2 = sensible less than non-sensible (-1)

	     #COPE 8 = ACTIVE (1) VS PASSIVE (-1)		active_v_passive			c1 = active greater than passive (1)	      and   c2 = active less than passive (-1)

	     #COPE 9 = INCORRECT (1) VS BASELINE (0)

	     #COPE 10 = CORRECT (1) VS INCORRECT (-1)	



	    

