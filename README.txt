# Data Analysis for Project 4 - Neuroimaging (on Lonestar)
# Created by Leo, edited/updated by Mary Abbe 2014
# Steps: transfer data, relabel BOLD runs, relabel subject directory, quality control anatomy and BOLD, fix flipped runs, upload behav data, create onset files/behavioral data analysis, make motion files

################################################################

STEP 0 - TRANSFER DATA TO TACC

###############################################################

1. Check data on XNAT

   ** Sign onto the XNAT website, using your eid and password. Find the subject you want to transfer, click on the subject name.

   ** Check that all of the correct runs are there and that they are complete by clicking on Total Counts (Austin - T1/T2: 176, SC: 212, REST/SST: 180; Houston - T1/T2: 176, SC: 217, SST: 185)

2. Run setup_subject script

   ** AUSTIN

      ** run_setup_a.sh

        ** Open emacs in ldrc - open run_setup_a_1.sh in emacs. Change the SUBCODE to the subject name on XNAT (located in the gray box on the top of the subjects page that displays the list of scans).  This must be exactly the same, so copy it if you need to!

        ** SAVE the script. 

        ** Launch the script - In terminal (in ldrc) type " ./run_setup_a.sh ", the window will prompt you for your tacc username and password.  

	   ** If you enter it wrong at all (ex: accidentally tab after your name or password) the script won't work. Wait for it to give you a new login4$ line, type "qdel #######", where ####### is the job number for the job you just tried to launch, and press enter.

	      This is ok - it just means you made the file folder structure for the subject, but nothing was grabbed from XNAT. Go to Fetch (or through terminal) and delete the subject's directory. Rerun the launch script correctly.

	   ** If you entered your tacc username/password correctly, after a minute or so terminal will tell you when each scan folder has been located on xnat.  Do not use the window during this period - monitor that everything was grabbed.  After it locates them, the script will launch. 



   ** HOUSTON 

      ** run_setup_h_1.sh - to run dcm2nii

        ** Open emacs in ldrc. Open run_setup_h.sd in emacs.  Change the SUBCODE to the subject name on XNAT (located in the gray box on the top of the subjects page that displays the list of scans).  This must be exactly the same, so copy it if you need to!

        ** SAVE the script. 
 
        ** Launch the script - In terminal (in ldrc) type " ./run_setup_h_1.sh ", the window will prompt you for your tacc username and password.  

	   ** If you enter it wrong at all (ex: accidentally tab after your name or password) the script won't work. Wait for it to give you a new login4$ line, type "qdel #######", where ####### is the job number for the job you just tried to launch, and press enter.

	      This is ok - it just means you made the file folder structure for the subject, but nothing was grabbed from XNAT. Go to Fetch (or through terminal) and delete the subject's directory. Rerun the launch script correctly.

	   ** If you entered your tacc username/password correctly, after a minute or so terminal will tell you when each scan folder has been located on xnat.  Do not use the window during this period - monitor that everything was grabbed.  After it locates them, the script will launch. 

      ** run trim_bold_h.sh; before running this, if you are restarting from existing Houston data use the clean_sub_data_h.sh script to remove all unecessary existing data; then run the trim script and proceed with running the rest of setup subject

           ## SC:        launch -s  launch_SC_trim_h.txt -j Analysis_Lonestar -r 02:00:00 -m churchlab@austin.utexas.edu

           ## SST/Rest:  launch -s  launch_SST_rest_trim_h.txt -j Analysis_Lonestar -r 02:00:00 -m churchlab@austin.utexas.edu

      ** run_setup_h_2.sh  - to run the rest of setup subject






3. Check that the transfer went well (the script takes ~1.5-2 hours)

   ** Check that the anatomy and all of the BOLD run directories are there and contain the right amount of data


################################################################

STEP 1 -  Pre-Processing and Quality Control

################################################################

# NOTE: Every step should be noted in the Processing_QC google doc as you move through the READMEs.
	These steps occur after the setup_subject script has been run and the data is on tacc.


1. Use an excel sheet called "Quality_Control.xlsx"; below are some details about what the sheet contains

   ** The following information will be stored in this sheet:

	  Subject #: Sequential number of subject for current study based on scanning date

	  Subject ID: Participation number from RedCap, given when entered into Project 4 database

	  Counter Balancing: Number given when subject shows up for scan, taken from "Counterbalancing Task for Scans.xlsx" file located in shared dropbox directory "Project 4"

	  WU Quality Check: Quality check for any issues using fidl software, indicating when registration diverges from norm

	  L/R Flip check: Checking for L/R flipping (most important for runs that got flipped upside down)

	  Anatomy quality: Load structural highres001_brain.nii.gz image found in "anatomy" directory in each subject's folder created by pre-processing - check for any issues such as banding, pieces of skull, fascia, eye balls, etc

    ** Do these steps before checking BOLD quality:

	 - Go to "BOLD" directory for each subject in their folders created by pre-processing. The first part of each directory label should indicate if it is a REST, SST, or SentComp run. The second string indicates which run it was, run1, run2, run3. 

	 ** The third part of label indicates the scanner number given to this particular run. This will be crucial in determining the correct order of the runs since errors or mislabeling could have occured. You will store this number in excel sheet "Quality_Control.xlsx" for each run in case data needs to be reloaded from XNAT.



2.  Relabel BOLD runs

    ## Run script that relabels bold runs: 
    
    ** You will manually have to relabel each run directory in fetch to insure it starts with "run" then the run # (0-7), then the scanner number, then the type of run/task (REST, SC, SST) then the run/task number (0-3)

	 For example REST_run1_8 becomes run01_8_REST_1 so that all the scripts will run

	 For example SentComp_run1_9 becomes run02_9_SC_1

    ** The run # corresponds to the scan number, but when assesing the Quality Control in step 4 it is important to assess the runs by task, not run #



3. If not already labelled, rename the Subject Directories in fetch as follows:   **APPLIES TO SECOND AND HOUSTON**

   P3 Qualifying sheet found in Church_Lab/Subjects/P3_qualifying 

      ** For business as usual subjects denoted by a 0 in P3 qualifiying sheet - ldrc_0_XXX where XXX is there subject number, such as 009

      ** For subjects in 1 year of intevention and a 1 in P3 qualifiying sheet - ldrc_1_XXX where XXX is there subject number, such as 009

      ** For subjects in 2 years of intevention and a 2 in P3 qualifiying sheet - ldrc_2_XXX where XXX is there subject number, such as 009
      
      ** For subjects scanned as controls and not part of intervention study - ldrc_c_XXX where XXX is there subject number, such as 009
      
   run fixperms on the directory through terminal



4. Use fslview& to do quality check of Anatomy and BOLD, and record observations in Quality_Control.xlsx sheet in Project 4 directory in dropbox. Make notes of moments when subject has movement or weird artifacts appear, like bone. 

   ** Type fslview& into terminal, go to subject directory, and upload their highres001_brain.nii.gz for anatomy, and the bold_mcf_brain.nii.gz for BOLD runs. Click on the movie to go through the frames for the BOLD runs and make a note of which volumes have movement.
   
	** Open the highres001_brain.nii.gz in fslview (through terminal) to check how well the brain extraction worked

	   ** If concerned about missing brain or too much skull leftover, can open the highres001.nii.gz (whole head, including skull), then add the highres001_brain_mask.nii.gz to see how well extraction worked; you can change how translucent the mask is using the dial at the bottom of the window

	   ** Flag a subject with too much leftover skull or missing brain in the Quality Control.  Then rerun BET (brain extraction tool). Note in Processing_QC that you have edited the anatomy.

	      ** cd into the subject's anatomy folder. Create a new folder called "BET", either in Fetch or using mkdir in the command line. Copy the highres001.nii.gz image into the BET folder (using Fetch or cp in command line). In terminal, cd into the BET folder.

	      ## in the command line, first try: bet highres001.nii.gz highres001_brain.nii.gz -m -f 0.3 -R

		 ** -m creates the mask

		 ** -f is the fractional intensity threshold (on a scale from 0-1); 0.5 is the default; smaller values give larger brain estimates

		 ** -R runs a more robust brain centre estimation; this improves brain extraction when the input data contains a lot of nonbrain matter
		 
		 ** -g is the vertical gradient in frantional intensity threshold (values -1 to 1); not normally run, but can be added if need less brain stem and more brain at the top

	      ** Compare the new highres001_brain.nii.gz in the BET folder to the original one in the main anatomy folder; evaluate the image and proceed to play with the images in the BET folder (delete the brain and brain_mask and rerun the bet line as adjusted). It is ok to have dura and skull, but it is NOT okay to be missing brain matter!
	      
	      ** Once you've settled with a better image (or decide to keep the original), cd back to the main anatomy folder, create a folder called "bad", move the original images into it.  Then copy the new images into the main anatomy folder (IMPORTANT - THESE ARE THE IMAGES THAT WILL BE USED IN THE REST OF THE PROCESSING STREAM!)

	** In terminal, cd into the BOLD/ folder, then into each run folder's QA folder. Type "display QA_report.pdf" to open the BOLD QA report for each run. You can also use "firefox QA_report.pdf"
	   
	   ** Check to see that the Mean MR Signal in the first graph does not drastically drift.

	   ** Note the number of frames removed from threshold per run in the QC sheet.  This will give us a context of the range of quality across runs and people, though we will later run our own scrubbing protocol to detect this.

	   ** If there is no QA_report.pdf, create one by launching the txt file **run_fmriqa.txt** ; edit the file to run the **fmriqa.py** script on the appropriate subjects before you launch it.

	** In terminal, cd into the DTI/DTI_1_QA folder. Type "firefox QA_report.pdf" to open the DTI QA report for each run.  If the QA_report.pdf file does not exist, rerun DTI QA by opening the script rm_DTIqa.r in SCRIPTS/ in emacs. Open R in SCRIPTS in terminal.  Follow the instructions inside the script - make sure to set the right groups.

	   ** Record the mean SNR and potentially bad gradients in the processing_QC



5. Check if runs are flipped 

   ## If runs are flipped run find_bold_runs.sh in /New_Scripts on tacc. Open flipped_runs.txt in emacs and delete the lines that do not need to be flipped, then save. Launch the code below in tacc:
	 
	 ## launch -s flipped_runs.txt -j Analysis_Lonestar -p 120 -e 2way -r 04:00:00 -m churchlab@austin.utexas.edu 
	 
   ** After launching, check to make sure the number of commands matches the number of lines in flipped_runs.txt, and type in qstat to check the status of the job. Launching sends the job to a grid of computers with a lot more memory (more info on poldrack wiki). Check for the flipped run files in fetch after it's complete.
	   
	 -p sets the memory to gigs in multiples of 12

	 -e 2way determines how many cores on each processor one wants to use

	 ** To delete a job before it starts running, type 'qdel *******' in tacc, where ******* is the job #



6. Uploading pickle files from Dropbox to tacc

   ** In Dropbox create a SC and SST directory in each behav file for each subject with the correct number of Run directories within each SC and SST file. The Run directories should be labeled as SC_Run1, SC_Run2, and SC_Run3 and SST_Run1 and SST_Run2 to get file paths like 2013 Output SC8<044<SC<SC_Run1 and 2013 Output SST<SST<SST_Run1

   ** In Dropbox move the .pkl files, associated with their behavioral data that gets outputted during a scanning session, into the appropriate Run file for both SC and SST that you just created. You don't need to move the .log files.  

	 Each .pkl file should end with subdata.pkl and only one file can go into each Run directory. Insure the appropriate .pkl file is uploaded to the appropriate run directory by matching the name in the .pkl file. 
   
	 It should be labeled during the scanner session as XXX_runY_*_subdata.pkl where XXX is subject number (i.e. 009), Y is run number, and * is usually some form of time stamp. SC and SST may or may not be in the .pkl file name. 

	 Once you run level 1, it will be easier to check if you uploaded the correct .pkl file, but be careful to avoid restarting.  

   ** After moving the .pkl files in Dropbox, use Fetch to upload the directories into tacc. In Fetch, use Put to upload the files from Dropbox. Make sure you are in the subjects behav file before uploading.

	 The file paths in tacc should now look like ldrc_c_018/behav/SST/SST_Run1 and ldrc_c_018/behav/SC/SC_Run1 with the appropriate .pkl files in each Run directory.



7. Make Behav onset files - run each onset script for SC and SST and their respective launch .txt files to create the onset files (mk_SC_onsets.sh and mk_SST_onsets.sh are the shell scripts that contain both Austin and Houston)

   ## SC ONSET FILES (OLD: mk_A_SC_onsets.sh, launch_A_SC_onsets.txt - these scripts do not account for the 2sec TR of instructions at the beginning of SC.

			   

	## AUSTIN SUBS

           ## If cleaning old SC data, use clean_sub_data_a.sh to remove old onset files before running the mk_A_SC_sets_2sec.sh

           ## mk_A_SC_onsets.sh - Run this script in terminal inside of the SCRIPTS/ directory to create the onset files needed to run the behavioral analysis scripts. 

				       Before running the scripts, open them in emacs to make sure you are running the correct group. Change the python script if need to. uses **mk_A_SC_onsets_2sec.py** 

				       which adds 2 seconds to all onsets and accounts for the first TR of instructions in junk regressor

           ## launch_A_SC_onsets_2sec.txt - Run these python scripts in terminal if running less than 5 subjects - open the launch file in emacs and copy the specific python lines into terminal. 

					    If running more than 5 subjects at a time, launch the file by copying the the line below into a new terminal line (in SCRIPTS/) and hitting enter.
	
					    ## launch -s  launch_A_SC_onsets_2sec.txt -j Analysis_Lonestar -r 02:00:00 -m churchlab@austin.utexas.edu

           ** Check to see if the onset files were created in the behav folders for the group you ran.


	## HOUSTON SUBS

	   ## mk_H_SC_onsets.sh  - Run this script in terminal inside of the SCRIPTS/ directory to create the onset files needed to run the behavioral analysis scripts. 

				   Before running the scripts, open them in emacs to make sure you are running the correct group.

	      ** This script will create a launch file to make the SC onset files for Houston. It calls the python script **mk_H_SC_onsets.py** and then finds the pkl files in the run directories 

	         for each subject ldrc_?_[0-9[0-9][0-9]/behav/SC/SC_Run*, where * is 1-3. 
			  
	      ** It saves the onset files inside of each subject's run directories along with the R text files for behavioral analysis labeled *output_ R.txt and a text file labeled *output_Events.txt that are used for fidl analysis. 		 

           ## launch_H_SC_onsets.txt - Run these python scripts in terminal if running less than 5 subjects - open the launch file in emacs and copy the specific python lines into terminal. 

				       If running more than 5 subjects at a time, launch the file by copying the the line below into a new terminal line (in SCRIPTS/) and hitting enter.

				       ## launch -s  launch_H_SC_onsets.txt -j Analysis_Lonestar -r 02:00:00 -m churchlab@austin.utexas.edu

	   ** Check to see if the onset files were created in the behav folders for the group you ran.

				   
 
   ## SST ONSET FILES

	 ## AUSTIN SUBS

	    ## mk_A_SST_onsets.sh - Run this script in terminal inside of the SCRIPTS/ directory to create the onsets files for SST.  

	       ** It will create the launch files to make the SST onset files for Austin. It calls the python script ** mk_SST_onsets.py ** and then finds the text files in the run directories 

	          for each subject ldrc_?_[0-9[0-9][0-9]/behav/SST/SST_Run*, where * is 1-2. 

	       ** It saves the onset files inside of each subject's run directories along with the R text files for behavioral analysis labeled *output_ R.txt and a text file labeled *output_Events.txt that are used for fidl analysis. 
	
	    ## launch_A_SST_onsets.txt - Run the python scripts in terminal if running less than 5 subjects - open the launch file in emacs and run the python scripts in terminal. 

					 If running more than 5 subjects at a time, launch the file by copying the the line below into a new terminal line (in SCRIPTS/) and hitting enter.

					 ## launch -s  launch_A_SST_onsets.txt -j Analysis_Lonestar -r 02:00:00 -p 120 -e 2way -m churchlab@austin.utexas.edu

	   ** Check to see if the onset files were created in the behav folders for the group you ran.



	## HOUSTON SUBS

	   ## mk_H_SST_onsets.sh -  Run this script in terminal inside of the SCRIPTS/ directory to create the onsets files for SST.  

	      ** It will create the launch file to make the SST onset files for Houston. It calls the python script ** mk_SST_onsets.py ** and then finds the text files in the run directories 

		 for each subject. See the python script for more details.

	      ** It saves the onset files inside of each subject's run directories along with the R text files for behavioral analysis labeled *output_ R.txt and a text file labeled *output_Events.txt that are used for fidl analysis. 

	   ## launch_H_SST_onsets.txt - Run the python scripts in terminal if running less than 5 subjects - open the launch file in emacs and run the python scripts in terminal. 

					If running more than 5 subjects at a time, launch the file by copying the the line below into a new terminal line (in SCRIPTS/) and hitting enter.

					## launch -s  launch_H_SST_onsets.txt -j Analysis_Lonestar -r 02:00:00 -m churchlab@austin.utexas.edu

	   ** Check to see if the onset files were created in the behav folders.


    ## EDITING/CREATING FIDL FILES (BOTH AUSTIN AND HOUSTON)

       ## 'fidl_master.sh' - is a launch file that uses fidl_maker.r to combine the event files into fidl files for both SC and SST. It includes files with and without bad movement runs. 

			     The first line of each block of code in fidl_master.sh writes to 'fidl_subs.txt' to list all of the subjects that will have fidl files made. 

			     You can edit this file if you want to have control over who gets their fidl files made before you run the rest of the block of code. 
	
       ## 'SC_event_maker.py' - this script will create the output_EVENT.txt files that mk_SC_onset.py will make, except it only* makes this file for the subjects if you don't want to rerun everything from
	
				mk_SC_onsets.py. Same for SST.
	
       ## 'mk_events.sh' - this script creates launch files to create event files for everyone (uses the SC_event_maker.py script). Creates 'launch_A_SC_events.txt', 'launch_H_SC_events.txt',

			   'launch_A_SST_events.txt', 'launch_H_SST_events.txt'





8. Behavioral analysis processing

   ## Run the scripts below in R in terminal in order to calculate the overall accuracy for each SC and SST run. 

	  ## SC_r_analysis.R - open this script in emacs and run each loop separately in order in R. Run the script the first time without doing the cutoffs (.6, .6), then make a note in the excel sheet of the %'s and the runs that should be excluded. Run the script a second time with the cutoffs to check that they were excluded.

	     ** To add this information to the Quality Control sheet:
		    
		    ** In R type prop_correct_* where * is control, first, second, H_first, or H_second depending on what group your subject(s) is in and copy the values for each SC run into the Quality Control sheet. Then change the color for the runs that should be excluded. 

		       In Fetch, add an x_ to the SC run under the behav file and the BOLD file if it needs to be excluded.

	  ## SST_r_analysis.R - open this script in emacs and run each loop separately in order in R. Run the script the first time without doing the cutoffs (.75, .3), then make a note in the excel sheet of the %'s and the runs that should be excluded. Run the script a second time with the cutoffs to check that they were excluded.

	     ** To add this information to the Quality Control sheet:
		
		    ** In R type go_correct_* where * is control, first, second, H_first, or H_second depending on what group your subject(s) is in and copy the values for each SST run into the Quality Control sheet. Change the color for the runs that should be excluded. 

		       In Fetch, add an x_ to the SC run under the behav file and BOLD file if it is excluded.

		    ** In R type stop_correct_* where * is control, first, second, H_first, or H_second depending on what group your subject(s) is in and copy the values for each SST run into the Quality Control sheet. Change the color for the runs that should be excluded. 

		       In Fetch, add an x_ to the SC run under the behav file and BOLD file if it is excluded.


    #### MAKE SURE TO UPDATE THE QUALITY CONTROL SHEET AND TO ADD  x_ BEFORE ANY BEHAV RUN AND BOLD RUN THAT WILL NOT BE INCLUDED IN LEVEL 1-3 PROCESSING; ADD m_ BEFORE ANY BEHAV/BOLD RUN THAT SHOULD BE EXCLUDED FOR MOVEMENT####


9. Make motion files

    ## mk_mot_files.sh - Run this code in terminal to make 6 motion parameter files for FEAT. Open the script in emacs to make sure you are running the right group. The script has comments to explain what it does. 

       ** Check to make sure motion files were created by looking in each BOLD run for each subject and task. You only need to run this script once.



10. Make motion scrubbing files

    ** For task data

       ## mk_bold_confound.sh - open this script in emacs and change the group if you need to.  This script creates three launch txt files, one for SC, SST, and REST, for the specific group it is run on. 

	  The launch files will run the script *mk_confound.sh* on the appropriate BOLD runs with the FD 0.9 (task data threshold). It will create a scrub_files directory within each BOLD directory that contains the motpars files,

	  fd_plot.png (plot of FD), fd_series.txt (not to be used for scrubbing), and fd_confounds.txt (a set of regressors that models out single TRs, but *only* the TR's where FD is over the threshold that you set).

       ** After running mk_bold_confound.sh in terminal, open the launch files to check them.  Launch the files using the command lines below.

	    ** launch -s launch_SC_confound.txt -j Analysis_Lonestar -r 04:00:00 -p 120 -e 2way -m churchlab@austin.utexas.edu

	    ** launch -s launch_SST_confound.txt -j Analysis_Lonestar -r 04:00:00 -p 120 -e 2way -m churchlab@austin.utexas.edu
  
	    ** launch -s launch_rest_confound.txt -j Analysis_Lonestar -r 04:00:00 -p 120 -e 2way -m churchlab@austin.utexas.edu

    ** Instructions to execute scrubbing in the FEAT GUI are included in the mk_confound.sh script.



END


#######################################################################################

** Open README_SC.txt and README_SST.txt to go through levels 1, 2, 3, and ROI analyses.

######
