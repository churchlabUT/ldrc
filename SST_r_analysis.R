# Authors: Leo Olmedo (First); Mary Abbe Roe (Dec.2013 - Feb. 2014)
# This script reads in SST behavioral data and calculates RT and ACC for all groups
# Graphs are located in SST_r_graphs.R

rm(list=ls())

#### LIBRARIES, FUNCTIONS, READING IN DATA ####


# LIBRARIES

  library(gtools)
  library(ggplot2)
  library(plyr)
  library(miscTools)
  library(nlme)
  library(reshape)
  library(multcomp)
  library(grid)

  wd=getwd()


# FUNCTIONS

  # function for extracting end of string
    substrRight=function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }

  # function for extracting start of string
    substrLeft=function(x, n){
      substr(x, 1, n)
    }


# READING IN DATA AND CREATING DATA FRAMES FOR EACH GROUP

  group=c("ldrc_[0-9]_[0-9][0-9][0-9]", "ldrc_*second", "ldrc_*third", "ldrc2_[0-9]_[0-9][0-9][0-9]","ldrc2_*second*", "ldrc3_[0-9]_[0-9][0-9][0-9]", "PHILIPS/H_LD*_[0-9]", "H_LD*_[0-9]_second", "H_LD*_[0-9]_third", "LDFHO*_[0-2]", "LDFHO*_second", "LDFHO2[0-9][0-9][0-9][0-9]_1_3", "ldrc*_c_[0-9][0-9][0-9]", "PHILIPS/H_LD*[0-9][0-9][0-9][0-9]", "H_LD*_c_second", "H_LD*_c_third")
  chars=c(10,17,16,11,18,11,13,20,19,12,19,14,11,11,20,19)

  # GROUP
  for (i in 1:length(group)){ 
    subdirs=c(Sys.glob(sprintf("/corral-repl/utexas/ldrc/%s" ,group[i])))#, Sys.glob(sprintf("/corral-repl/utexas/ldrc/x_%s" ,group[i])))
    rm("dat_all")
    print(group[i])

    # SUB
    for (sub in subdirs){
      behav_dirs=c(Sys.glob(sprintf("%s/behav/SST/SST_R*", sub)))#, Sys.glob(sprintf("%s/behav/SST/m_SST_R*", sub))) 
      subnum=substrRight(sub, chars[i])
       

       # RUN
       for (run in behav_dirs){
         run_num=substrRight(run,1)
         Rfile=Sys.glob(sprintf("%s/*R.txt", run))

         if (length(Rfile)==0){
           warning(sprintf("cannot find R.txt file for %s", run))
           next
         }
         
         # READ IN RUN RFILE, ADD SUBIND AND RUNNUM COLUMNS
         dat_loop=read.table(Rfile, header=TRUE, sep="\t", na.strings="NaN")
         dat_loop$subind=rep(subnum, dim(dat_loop)[1])
         dat_loop$runnum=rep(run_num, dim(dat_loop)[1])
       
         # CREATE DAT_ALL IF DOESN'T ALREADY EXIST
         if (exists("dat_all")==FALSE){
           dat_all=dat_loop 
         }  else { 
           dat_all=rbind(dat_all, dat_loop)
         } 
    
       } # END RUN LOOP
    }  # END SUB LOOP


    # creates vector of 0's and 1's from TrialNum, that does not include repeated correct responses 
    # (whether or not there was a response to each trial)
    indicator=c(dat_all$TrialNum,0)-c(0, dat_all$TrialNum)						  # ADDS 0 TO BEGINNING AND END OF TrialNum AND SUBTRACTS, SO IF REPEAT IT SUBTRACTS ITSELF AND BECOMES 0
    indicator[indicator!=0]=1  										  # SETS EVERYTHING NOT = 0 TO 1, to get rid of -31's
    dat_all$first=indicator[1:(length(indicator)-1)]							  # SUBTRACT EXTRA NUMBER ON END

    # ADD GROUP IDENTIFIER
    dat_all$group_num=i

    # CREATE SEPARATE DATA FRAME FOR EACH GROUP
    assign(paste("dat_all",i,sep="_"),dat_all)

  } # END GROUP LOOP




# DF OF ALL GROUPS
    dat_all_1$group = "A_first"
    dat_all_2$group = "A_second"
    dat_all_3$group = "A_third"
    dat_all_4$group = "A2_first"
    dat_all_5$group = "A2_second"
    dat_all_6$group = "A3_first"
    dat_all_7$group = "H_first"
    dat_all_8$group ="H_second"
    dat_all_9$group = "H_third"
    dat_all_10$group = "H2_first"
    dat_all_11$group = "H2_second"
    dat_all_12$group = "H3_first"
    dat_all_13$group = "A_control"
    dat_all_14$group = "H_control_first"
    dat_all_15$group = "H_control_second"
    dat_all_16$group = "H_control_third"

    dat = rbind(dat_all_1,dat_all_2, dat_all_3, dat_all_4, dat_all_5, dat_all_6, dat_all_7, dat_all_8, dat_all_9, dat_all_10, dat_all_11, dat_all_12, dat_all_13, dat_all_14, dat_all_15, dat_all_16)
    write.csv(dat, file="/corral-repl/utexas/ldrc/SCRIPTS/sst_behav_data.csv")


# CODES:
  # dat_all$cond: 	0= arrow points left, 1= arrow points right, 2= arrow points left and stop , 3= arrow points right and stop
  # dat_all$Stim: 	0= left arrow, 1= right arrow
  # dat_all$Resp: 	0=no button press , 1=left button , 4=right button
  # dat_all$correct: 	0=go_incorrect, 1=go_correct, 2=stop_fail_incorrect, 3=stop_fail_correct, 4=stop_correct, 5=go_omission (4 and 5 are trials that have no response)




#### RT and ACC ANALYSES ####

sessions = length(group)


# GO LOOP

for (k in 1:sessions){

	if (k==1){
	dat_all=dat_all_1
	group="A_first"

	} else {

	if (k==2){
	dat_all=dat_all_2
	group="A_second"

	} else {

	if (k==3){
	dat_all=dat_all_3
	group="A_third"

	} else {

	if (k==4){
	dat_all=dat_all_4
	group="A2_first"

	} else {

        if (k==5){
	dat_all=dat_all_5
	group="A2_second"

        } else {

	if (k==6){
	dat_all=dat_all_6
	group="A3_first"

        } else {

	if (k==7){
	dat_all=dat_all_7
	group="H_first"

	} else {

	if (k==8){
	dat_all=dat_all_8
	group="H_second"

	} else {

	if (k==9){
	dat_all=dat_all_9
	group="H_third"

	} else {

	if (k==10){
	dat_all=dat_all_10
	group="H2_first"

	} else {

	if (k==11){
	dat_all=dat_all_11
	group="H2_second"

	} else {

	if (k==12){
	dat_all=dat_all_12
	group="H3_first"
	
	} else {

	if (k==13){
	dat_all=dat_all_13
	group="A_control"
	
	} else {

	if (k==14){
	dat_all=dat_all_14
	group="H_control_first"

	} else {

	if (k==15){
	dat_all=dat_all_15
	group="H_control_second"

        } else {

	if (k==16){
	dat_all=dat_all_16
	group="H_control_third"

        }
	}
	}
	}
	}
        }
	}
	}
        }
        }
        }
        }
        }
        }
        }
        }

  # GO CORRECT RT and ACC

    # EACH RUN FOR EACH SUB (# lines are cutoffs)
 
      go_run=ddply(dat_all, .(subind, runnum, group), summarise, N=length(subind), go_rt_med=median(RT[correct==1], na.rm=TRUE), go_rt_sd=sd(RT[correct==1], na.rm=TRUE), go_acc=length(correct[correct==1])/length(cond[cond==1|cond==0]), go_error=length(correct[correct==0])/length(cond[cond==1|cond==0]), go_omit=length(correct[correct==5])/length(cond[cond==1|cond==0]))
      #go_run$go_rt_med[go_run$go_acc < .60 | go_run$go_error > .1] = NA
      #go_run$go_acc[go_run$go_acc < .60 | go_run$go_error > .1] = NA 
      assign(paste("go_run",k,sep="_"),go_run)

    # ACROSS ALL RUNS FOR EACH SUB (# lines are ACC cutoffs) - useful for SSRT calculation
   
      go_sub=ddply(go_run, .(subind, group), summarise, N=length(subind), go_rt_mean=mean(go_rt_med, na.rm=TRUE), go_rt_sd=sd(go_rt_med)/sqrt(N), go_acc_mean=mean(go_acc, na.rm=TRUE), go_error_mean=mean(go_error, na.rm=TRUE))
      assign(paste("go_sub",k,sep="_"),go_sub)

    # ACROSS SUBS FOR EACH RUN

      go_run_all=ddply(go_run, .(runnum, group), summarise, N=length(runnum), go_rt_me=mean(go_rt_med, na.rm=TRUE), go_rt_sd=sd(go_rt_med, na.rm=TRUE)/sqrt(N), go_acc_me=mean(go_acc, na.rm=TRUE), go_error_me=mean(go_error, na.rm=TRUE))
      assign(paste("go_run_all",k,sep="_"),go_run_all)   


} # END GO LOOP





# STOP LOOP
 group=c("ldrc_[0-9]_[0-9][0-9][0-9]", "ldrc_*second", "ldrc_*third", "ldrc2_[0-9]_[0-9][0-9][0-9]","ldrc2_*second*", "ldrc3_[0-9]_[0-9][0-9][0-9]", "PHILIPS/H_LD*_[0-9]", "H_LD*_[0-9]_second", "H_LD*_[0-9]_third", "LDFHO*_[0-9]", "LDFHO*_second", "LDFHO2*_1_3", "ldrc*_c_[0-9][0-9][0-9]", "PHILIPS/H_LD*[0-9][0-9][0-9][0-9]", "H_LD*_c_second", "H_LD*_c_third")
sessions = length(group)

for (k in 1:sessions){

	if (k==1){
	dat_all=dat_all_1
	group="A_first"

	} else {

	if (k==2){
	dat_all=dat_all_2
	group="A_second"

	} else {

	if (k==3){
	dat_all=dat_all_3
	group="A_third"

	} else {

	if (k==4){
	dat_all=dat_all_4
	group="A2_first"

	} else {

        if (k==5){
	dat_all=dat_all_5
	group="A2_second"

        } else {

	if (k==6){
	dat_all=dat_all_6
	group="A3_first"

        } else {

	if (k==7){
	dat_all=dat_all_7
	group="H_first"

	} else {

	if (k==8){
	dat_all=dat_all_8
	group="H_second"

	} else {

	if (k==9){
	dat_all=dat_all_9
	group="H_third"

	} else {

	if (k==10){
	dat_all=dat_all_10
	group="H2_first"

	} else {

	if (k==11){
	dat_all=dat_all_11
	group="H2_second"

	} else {

	if (k==12){
	dat_all=dat_all_12
	group="H3_first"
	
	} else {

	if (k==13){
	dat_all=dat_all_13
	group="A_control"
	
	} else {

	if (k==14){
	dat_all=dat_all_14
	group="H_control_first"

	} else {

	if (k==15){
	dat_all=dat_all_15
	group="H_control_second"

        } else {

	if (k==16){
	dat_all=dat_all_16
	group="H_control_third"

        }
	}
	}
	}
	}
        }
	}
	}
        }
        }
        }
        }
        }
        }
        }
        }


  # EACH RUN FOR EACH SUB

    # DF calculates stop_acc and SSD
      stop_run=ddply(dat_all, .(subind, runnum, group), summarise, N=length(subind), stop_acc=length(correct[correct==4])/length(cond[cond==2|cond==3]), stop_error=length(correct[correct==2|correct==3])/length(cond[cond==2|cond==3]), 
                     stop_fail_cor=length(correct[correct==3])/length(cond[cond==2|cond==3]), stop_fail_incor=length(correct[correct==2])/length(cond[cond==2|cond==3]), ssd_med=median(SSD[cond==2|cond==3], na.rm=TRUE), 
                     ssd_mean=mean(SSD[cond==2|cond==3], na.rm=TRUE))
      assign(paste("stop_run",k,sep="_"),stop_run)

    # calculate quantile RT and SSRT per run 
      # create separate DF for stop_error's
        stop_error_run=data.frame(subind=stop_run$subind, runnum=stop_run$runnum, stop_error=stop_run$stop_error)

      # create DF with go_correct's in ascending order and stop_error
        RT_asc_run=data.frame(subind=dat_all$subind[dat_all$correct==1], runnum=dat_all$runnum[dat_all$correct==1], TrialNum=dat_all$TrialNum[dat_all$correct==1], correct=dat_all$correct[dat_all$correct==1], RT=dat_all$RT[dat_all$correct==1])
        RT_asc_run=RT_asc_run[order(RT_asc_run[,1], RT_asc_run[,2], RT_asc_run[,5], decreasing=FALSE), ]
        RT_asc_stop_error_run=merge(RT_asc_run, stop_error_run)

      # caculate quantileRT, add to stop_run DF, calculate SSRT
        quantile_RT=ddply(RT_asc_stop_error_run, .(subind, runnum, group), summarise, N=length(subind), quantileRT=quantile(RT, mean(stop_error)))

        stop_run$quantileRT=quantile_RT$quantileRT
        stop_run$SSRT=stop_run$quantileRT-stop_run$ssd_mean
        stop_run=stop_run[c("subind", "runnum", "N", "stop_acc", "stop_error", "stop_fail_cor", "stop_fail_incor", "ssd_med", "ssd_mean", "quantileRT", "SSRT", "group")]
        assign(paste("stop_run",k,sep="_"),stop_run)


  # ACROSS ALL RUNS FOR EACH SUB

    stop_sub=ddply(stop_run, .(subind, group), summarise, N=length(subind), stop_acc_me=mean(stop_acc, na.rm=TRUE), stop_error_me=mean(stop_error, na.rm=TRUE), stop_fail_cor_me = mean(stop_fail_cor, na.rm=TRUE), stop_fail_incor_me=mean(stop_fail_cor, na.rm=TRUE), ssd=mean(ssd_mean, na.rm=TRUE), quantileRT_me=mean(quantileRT, na.rm=TRUE), SSRT_me=mean(SSRT, na.rm=TRUE))
    assign(paste("stop_sub",k,sep="_"),stop_sub)


  # ACROSS SUBS FOR EACH RUN

    stop_run_all=ddply(stop_run, .(runnum, group), summarise, N=length(runnum), stop_acc_me=mean(stop_acc, na.rm=TRUE), stop_acc_sd=sd(stop_acc, na.rm=TRUE), stop_error_me=mean(stop_error, na.rm=TRUE), stop_error_sd=sd(stop_error, na.rm=TRUE), ssd_me=mean(ssd_mean, na.rm=TRUE), ssd_sd=sd(ssd_mean), quantileRT_me=mean(quantileRT, na.rm=TRUE), SSRT_me=mean(SSRT, na.rm=TRUE), SSRT_sd=sd(SSRT))
    assign(paste("stop_run_all",k,sep="_"),stop_run_all)   


} # END STOP LOOP

    


# ALL GROUP DFs *** ONLY RUN ON FULL DATA SET, DO NOT RUN ON DATA THAT EXCLUDES SUBS FOR BEHAV PERFORMANCE ***

  # GO

    all_go_run=rbind(go_run_1, go_run_2, go_run_3, go_run_4, go_run_5, go_run_6, go_run_7, go_run_8, go_run_9, go_run_10, go_run_11, go_run_12, go_run_13, go_run_14)
    write.csv(all_go_run, file=sprintf("%s/data_frames/SST/GO/go_all_subs_groups_by_run.csv", wd), na="NA", row.names=FALSE)

    all_go_sub=rbind(go_sub_1, go_sub_2, go_sub_3, go_sub_4, go_sub_5, go_sub_6, go_sub_7, go_sub_8, go_sub_9, go_sub_10, go_sub_11,go_sub_12, go_sub_13, go_sub_14)
    write.csv(all_go_sub, file=sprintf("%s/data_frames/SST/GO/go_all_subs_groups_by_sub_m.csv", wd), na="NA", row.names=FALSE)

    go_run_allsub=rbind(go_run_all_1, go_run_all_2, go_run_all_3, go_run_all_4, go_run_all_5, go_run_all_6, go_run_all_7, go_run_all_8, go_run_all_9, go_run_all_10, go_run_all_11, go_run_all_12, go_run_all_13, go_run_all_14)
    write.csv(go_run_allsub, file=sprintf("%s/data_frames/SST/GO/go_runnum_across_groups.csv", wd), na="NA", row.names=FALSE)

  # STOP

    all_stop_run=rbind(stop_run_1, stop_run_2, stop_run_3, stop_run_4, stop_run_5, stop_run_6, stop_run_7, stop_run_8, stop_run_9, stop_run_10, stop_run_11, stop_run_12, stop_run_13, stop_run_14)
    write.csv(all_stop_run, file=sprintf("%s/data_frames/SST/STOP/stop_all_subs_groups_by_run.csv", wd), na="NA", row.names=FALSE)

    all_stop_sub=rbind(stop_sub_1, stop_sub_2, stop_sub_3, stop_sub_4, stop_sub_5, stop_sub_6, stop_sub_7, stop_sub_8, stop_sub_9, stop_sub_10, stop_sub_11, stop_sub_12, stop_sub_13, stop_sub_14)
    write.csv(all_stop_sub, file=sprintf("%s/data_frames/SST/STOP/stop_all_subs_groups_by_sub_m.csv", wd), na="NA", row.names=FALSE)

    stop_run_allsub=rbind(stop_run_all_1, stop_run_all_2, stop_run_all_3, stop_run_all_4, stop_run_all_5, stop_run_all_6, stop_run_all_7, stop_run_all_8, stop_run_all_9, stop_run_all_10, stop_run_all_11, stop_run_all_12, stop_run_all_13, stop_run_all_14)
    write.csv(stop_run_allsub, file=sprintf("%s/data_frames/SST/STOP/stop_runnum_across_groups.csv", wd), na="NA", row.names=FALSE)




----------------

# DATAFRAMES FOR ALL SUBS GO AND STOP

  # COMBINE GO, STOP, AND GROUP DATA FRAMES
  all_go_subs = rbind(go_sub_1, go_sub_2, go_sub_3, go_sub_4, go_sub_5, go_sub_6, go_sub_7, go_sub_8, go_sub_9, go_sub_10, go_sub_11,go_sub_12, go_sub_13, go_sub_14, go_sub_15, go_sub_16)
  all_stop_subs = rbind(stop_sub_1, stop_sub_2, stop_sub_3, stop_sub_4, stop_sub_5, stop_sub_6, stop_sub_7, stop_sub_8, stop_sub_9, stop_sub_10, stop_sub_11, stop_sub_12, stop_sub_13, stop_sub_14, stop_sub_15, stop_sub_16)
  all_subs = join(all_go_subs, all_stop_subs)
  
  # REMOVE REPEAT HOUSTON CONTROLS
  all_subs=all_subs[all_subs$subind!="H_LDIMG8960_c_third",]

  # TRIM DOWN SUBIDS AND ADD IDENTIFIERS
  all_subs$group2 = all_subs$group
  all_subs$group2[grep('first', all_subs$group)] = "first"
  all_subs$group2[grep('second', all_subs$group)] = "second"
  all_subs$group2[grep('third', all_subs$group)] = "third"
  all_subs$group2[grep('control', all_subs$group)] = "control"
  all_subs$subind[all_subs$group=="A_second" | all_subs$group=="A_third"] = substrLeft(all_subs$subind[all_subs$group=="A_second" | all_subs$group=="A_third"],10)
  all_subs$subind[all_subs$group=="A2_second"] = substrLeft(all_subs$subind[all_subs$group=="A2_second"],11)
  all_subs$subind[all_subs$group=="A_first" | all_subs$group=="A_second" | all_subs$group=="A_third" | all_subs$group=="A2_first" | all_subs$group=="A2_second" | all_subs$group=="A_control"] = substrRight(all_subs$subind[all_subs$group=="A_first" | all_subs$group=="A_second" | all_subs$group=="A_third" | all_subs$group=="A2_first" | all_subs$group=="A2_second" | all_subs$group=="A_control"],5)
  all_subs$subind[all_subs$group=="H_second" | all_subs$group=="H_control_second" | all_subs$group=="H_third" | all_subs$group=="H_control_second" | all_subs$group=="H_control_third"]=substrLeft(all_subs$subind[all_subs$group=="H_second" | all_subs$group=="H_control_second"| all_subs$group=="H_third" | all_subs$group=="H_control_second" | all_subs$group=="H_control_third"],13)
  all_subs$subind[all_subs$group=="H_first" | all_subs$group=="H_second" | all_subs$group=="H_control_second"| all_subs$group=="H_third" | all_subs$group=="H_control_second" | all_subs$group=="H_control_third"]=substrRight(all_subs$subind[all_subs$group=="H_first" | all_subs$group=="H_second" | all_subs$group=="H_control_second"| all_subs$group=="H_third" | all_subs$group=="H_control_second"| all_subs$group=="H_control_third" ],6) 
  all_subs$subind[all_subs$group=="H2_first"]=substrRight(all_subs$subind[all_subs$group=="H2_first"],7) 
  all_subs$subind[all_subs$group=="H2_second"]=substrLeft(all_subs$subind[all_subs$group=="H2_second"],12)
  all_subs$subind[all_subs$group=="H2_second"]=substrRight(all_subs$subind[all_subs$group=="H2_second"],7) 
  all_subs$subind[all_subs$group=="H_control_first"]=substrRight(all_subs$subind[all_subs$group=="H_control_first"],4)
  all_subs$ID = substrLeft(all_subs$subind, 1)
  all_subs$ID[all_subs$group2=="control"] = "c"
  all_subs$ID2 = all_subs$ID
  all_subs$ID2[all_subs$ID == "0"] = "0"
  all_subs$ID2[all_subs$ID == "1" | all_subs$ID == "2"] = "1"
  all_subs$city = "austin"
  all_subs$city[grep('H', all_subs$group)] = "houston"
  all_subs = all_subs[all_subs$group2!="third" & all_subs$group!="H_first" & all_subs$group!="H_control_first",]
  all_subs$rep = 'u'
  all_subs$rep[(duplicated(all_subs['subind'])) | (duplicated(all_subs['subind'], fromLast = TRUE))] = 'r'


  write.csv(all_subs, file=sprintf("%s/data_frames/SST/all_subs_go_stop_feb16.csv", wd), na="NA", row.names=FALSE)

  write.csv(all_subs[all_subs$city=="austin",], file=sprintf("%s/figures/Project_4/SST/all_subs_austin_go_stop_feb16.csv", wd), na="NA", row.names=FALSE)

  # number of subs in each group and averages
  all_group = ddply(all_subs, .(group), summarise, N=length(group), go_rt_me=(mean(go_rt_mean, na.rm=TRUE)), go_rt_se=sd(go_rt_mean, na.rm=TRUE)/sqrt(N), go_acc_me=(mean(go_acc_mean, na.rm=TRUE)), go_acc_se=sd(go_acc_mean, na.rm=TRUE)/sqrt(N),SSRT_mean=(mean(SSRT_me, na.rm=TRUE)), SSRT_se=sd(SSRT_me, na.rm=TRUE)/sqrt(N), stop_acc_mean=(mean(stop_acc_me, na.rm=TRUE)), stop_acc_se=sd(stop_acc_me, na.rm=TRUE)/sqrt(N), ssd_mean = mean(ssd, na.rm=TRUE), ssd_se = sd(ssd, na.rm = TRUE)/sqrt(N))

  # group averages by ID
  all_ID = ddply(all_subs, .(ID, group), summarise, N=length(ID), go_rt_me=(mean(go_rt_mean, na.rm=TRUE)), go_rt_se=sd(go_rt_mean, na.rm=TRUE)/sqrt(N), go_acc_me=(mean(go_acc_mean, na.rm=TRUE)), go_acc_se=sd(go_acc_mean, na.rm=TRUE)/sqrt(N), SSRT_mean=(mean(SSRT_me, na.rm=TRUE)), SSRT_se=sd(SSRT_me, na.rm=TRUE)/sqrt(N), stop_acc_mean=(mean(stop_acc_me, na.rm=TRUE)), stop_acc_se=sd(stop_acc_me, na.rm=TRUE)/sqrt(N))


  all_group_a = ddply(all_subs[all_subs$city=="austin",], .(group), summarise, N=length(group), go_rt_me=(mean(go_rt_mean, na.rm=TRUE)), go_rt_se=sd(go_rt_mean, na.rm=TRUE)/sqrt(N), go_acc_me=(mean(go_acc_mean, na.rm=TRUE)), go_acc_se=sd(go_acc_mean, na.rm=TRUE)/sqrt(N),SSRT_mean=(mean(SSRT_me, na.rm=TRUE)), SSRT_se=sd(SSRT_me, na.rm=TRUE)/sqrt(N), stop_acc_mean=(mean(stop_acc_me, na.rm=TRUE)), stop_acc_se=sd(stop_acc_me, na.rm=TRUE)/sqrt(N), ssd_mean = mean(ssd, na.rm=TRUE), ssd_se = sd(ssd, na.rm = TRUE)/sqrt(N))
 write.csv(all_group_a, file=sprintf("%s/figures/Project_4/SST/all_austin_group_avgs_feb16.csv", wd), na="NA", row.names=FALSE)

  all_a_repeats = all_subs[all_subs$rep == "r" & all_subs$city == "austin",]
  write.csv(all_subs[all_subs$rep == "r" & all_subs$city == "austin",], file=sprintf("%s/figures/Project_4/SST/all_subs_austin_repeats_feb16.csv", wd), na="NA", row.names=FALSE)

  all_repeats_a = ddply(all_subs[all_subs$rep == "r" & all_subs$city == "austin",], .(group), summarise, N=length(group), go_rt_me=(mean(go_rt_mean, na.rm=TRUE)), go_rt_se=sd(go_rt_mean, na.rm=TRUE)/sqrt(N), go_acc_me=(mean(go_acc_mean, na.rm=TRUE)), go_acc_se=sd(go_acc_mean, na.rm=TRUE)/sqrt(N),SSRT_mean=(mean(SSRT_me, na.rm=TRUE)), SSRT_se=sd(SSRT_me, na.rm=TRUE)/sqrt(N), stop_acc_mean=(mean(stop_acc_me, na.rm=TRUE)), stop_acc_se=sd(stop_acc_me, na.rm=TRUE)/sqrt(N), ssd_mean = mean(ssd, na.rm=TRUE), ssd_se = sd(ssd, na.rm = TRUE)/sqrt(N))
 write.csv(all_repeats_a, file=sprintf("%s/figures/Project_4/SST/all_austin_repeats_avgs_feb16.csv", wd), na="NA", row.names=FALSE)



# GO STATS

  # GO STATS

     # AUSTIN SUBS

     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/go_stats_austin_feb16.txt")

     rt_s1vc_a = t.test(go_rt_mean ~ group2, data = all_subs[all_subs$group2 == "first" & all_subs$city == "austin" | all_subs$group2 == "control" & all_subs$city == "austin" ,])
     print("Mean Go RT - S1 vs. Controls")
     print(rt_s1vc_a)

     acc_s1vc_a = t.test(go_acc_mean ~ group2, data = all_subs[all_subs$group2 == "first" & all_subs$city == "austin" | all_subs$group2 == "control" & all_subs$city == "austin" ,])
     print("Mean Go Accuracy - S1 vs. Controls")
     print(acc_s1vc_a)

     rt_s2vc_a = t.test(go_rt_mean ~ group2, data = all_subs[all_subs$group2 == "second" & all_subs$city == "austin" | all_subs$group2 == "control" & all_subs$city == "austin" ,])
     print("Mean Go RT - S2 vs. Controls")
     print(rt_s2vc_a)

     acc_s2vc_a = t.test(go_acc_mean ~ group2, data = all_subs[all_subs$group2 == "second" & all_subs$city == "austin" | all_subs$group2 == "control" & all_subs$city == "austin" ,])
     print("Mean Go Accuracy - S2 vs. Controls")
     print(acc_s2vc_a)

     rt_s1vs2_a = t.test(go_rt_mean ~ group2, data = all_subs[all_subs$group2 == "first" & all_subs$city == "austin" | all_subs$group2 == "second" & all_subs$city == "austin" ,])
     print("Mean Go RT - S1 vs. s2")
     print(rt_s1vs2_a)

     acc_s1vs2_a = t.test(go_acc_mean ~ group2, data = all_subs[all_subs$group2 == "first" & all_subs$city == "austin" | all_subs$group2 == "second" & all_subs$city == "austin" ,])
     print("Mean Go Accuracy - S1 vs. S2")
     print(acc_s1vs2_a)

     sink()

     # AUSTIN REPEAT SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/go_stats_austin_repeat_feb16.txt")

     print(all_subs[all_subs$rep == "r" & all_subs$city == "austin",])

     rt_repeat_a = t.test(go_rt_mean ~ group2, data = all_subs[all_subs$rep == "r" & all_subs$city == "austin",], paired=T)
     print("Mean Go RT - S1 vs. s2 Repeats")
     print(rt_repeat_a)

     acc_repeat_a = t.test(go_acc_mean ~ group2, data = all_subs[all_subs$rep == "r" & all_subs$city == "austin",], paired=T)
     print("Mean Go Accuracy - S1 vs. S2 Repeats")
     print(acc_repeat_a)

     sink()


     # AUSTIN UNIQUE SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/go_stats_austin_uniq_feb16.txt")

     print(all_subs[all_subs$group2!="control" &  all_subs$rep == "u" & all_subs$city == "austin",])

     rt_uniq_a = t.test(go_rt_mean ~ group2, data = all_subs[all_subs$group2!="control" & all_subs$rep == "u" & all_subs$city == "austin",])
     print("Mean Go RT - S1 vs. s2 Unique")
     print(rt_uniq_a)

     acc_uniq_a = t.test(go_acc_mean ~ group2, data = all_subs[all_subs$group2!="control" & all_subs$rep == "u" & all_subs$city == "austin",])
     print("Mean Go Accuracy - S1 vs. S2 Unique")
     print(acc_uniq_a)

     sink()


     # AUSTIN AND HOUSTON SUBS
   
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/go_stats_ah.txt")

     rt_s1vc_ah = t.test(go_rt_mean ~ group2, data = all_subs[all_subs$group2 == "first" | all_subs$group2 == "control",])
     print("Mean Go RT - S1 vs. Controls")
     print(rt_s1vc_ah)

     acc_s1vc_ah = t.test(go_acc_mean ~ group2, data = all_subs[all_subs$group2 == "first" | all_subs$group2 == "control",])
     print("Mean Go Accuracy - S1 vs. Controls")
     print(acc_s1vc_ah)

     rt_s2vc_ah = t.test(go_rt_mean ~ group2, data = all_subs[all_subs$group2 == "second" | all_subs$group2 == "control",])
     print("Mean Go RT - S2 vs. Controls")
     print(rt_s2vc_ah)

     acc_s2vc_ah = t.test(go_acc_mean ~ group2, data = all_subs[all_subs$group2 == "second" | all_subs$group2 == "control",])
     print("Mean Go Accuracy - S2 vs. Controls")
     print(acc_s2vc_ah)

     rt_s1vs2_ah = t.test(go_rt_mean ~ group2, data = all_subs[all_subs$group2 == "first" | all_subs$group2 == "second",])
     print("Mean Go RT - S1 vs. s2")
     print(rt_s1vs2_ah)

     acc_s1vs2_ah = t.test(go_acc_mean ~ group2, data = all_subs[all_subs$group2 == "first" | all_subs$group2 == "second",])
     print("Mean Go Accuracy - S1 vs. S2")
     print(acc_s1vs2_ah)

     sink()

     # AUSTIN/HOUSTON REPEAT SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/go_stats_ah_repeat.txt")

     print(all_subs[all_subs$rep == "r",])

     rt_repeat_ah = t.test(go_rt_mean ~ group2, data = all_subs[all_subs$rep == "r",], paired=T)
     print("Mean Go RT - S1 vs. s2 Repeats")
     print(rt_repeat_ah)

     acc_repeat_ah = t.test(go_acc_mean ~ group2, data = all_subs[all_subs$rep == "r",], paired=T)
     print("Mean Go Accuracy - S1 vs. S2 Repeats")
     print(acc_repeat_ah)

     sink()


     # AUSTIN/HOUSTON UNIQUE SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/go_stats_ah_uniq.txt")

     print(all_subs[all_subs$group2!="control" & all_subs$rep == "u",])

     rt_uniq_ah = t.test(go_rt_mean ~ group2, data = all_subs[all_subs$group2!="control" & all_subs$rep == "u",])
     print("Mean Go RT - S1 vs. s2 Unique")
     print(rt_uniq_ah)

     acc_uniq_ah = t.test(go_acc_mean ~ group2, data = all_subs[all_subs$group2!="control" & all_subs$rep == "u",])
     print("Mean Go Accuracy - S1 vs. S2 Unique")
     print(acc_uniq_ah)

     sink()



# STOP STATS 

     # AUSTIN

     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/stop_stats_austin_feb16.txt")

     ssd_s1vc_a = t.test(ssd ~ group2, data = all_subs[all_subs$group2 == "first" & all_subs$city == "austin" | all_subs$group2 == "control" & all_subs$city == "austin",])
     print("Mean SSD - S1 vs. Controls")
     print(ssd_s1vc_a)

     ssrt_s1vc_a = t.test(SSRT_me ~ group2, data = all_subs[all_subs$group2 == "first" & all_subs$city == "austin" | all_subs$group2 == "control" & all_subs$city == "austin",])
     print("Mean SSRT - S1 vs. Controls")
     print(ssrt_s1vc_a)

     ssd_s2vc_a = t.test(ssd ~ group2, data = all_subs[all_subs$group2 == "second" & all_subs$city == "austin" | all_subs$group2 == "control" & all_subs$city == "austin",])
     print("Mean SSD - S2 vs. Controls")
     print(ssd_s2vc_a)

     ssrt_s2vc_a = t.test(SSRT_me ~ group2, data = all_subs[all_subs$group2 == "second" & all_subs$city == "austin" | all_subs$group2 == "control" & all_subs$city == "austin",])
     print("Mean SSRT - S2 vs. Controls")
     print(ssrt_s2vc_a)

     ssd_s1vs2_a = t.test(ssd ~ group2, data = all_subs[all_subs$group2 == "first" & all_subs$city == "austin" | all_subs$group2 == "second" & all_subs$city == "austin",])
     print("Mean SSD - S1 vs. s2")
     print(ssd_s1vs2_a)

     ssrt_s1vs2_a = t.test(SSRT_me ~ group2, data = all_subs[all_subs$group2 == "first" & all_subs$city == "austin" | all_subs$group2 == "second" & all_subs$city == "austin",])
     print("Mean SSRT - S1 vs. S2")
     print(ssrt_s1vs2_a)

     sink()


     # AUSTIN REPEAT SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/stop_stats_austin_repeat_feb16.txt")

     print(all_subs[all_subs$city=="austin" & all_subs$rep == "r",])

     ssd_repeat_a = t.test(ssd ~ group2, data = all_subs[all_subs$city=="austin" & all_subs$rep == "r",], paired=T)
     print("Mean SSD - S1 vs. s2 Repeats")
     print(ssd_repeat_a)

     ssrt_repeat_a = t.test(SSRT_me ~ group2, data = all_subs[all_subs$city=="austin" & all_subs$rep == "r",], paired=T)
     print("Mean SSRT - S1 vs. S2 Repeats")
     print(ssrt_repeat_a)

     sink()


     # AUSTIN UNIQUE SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/stop_stats_austin_uniq_feb16.txt")

     print(all_subs[all_subs$city=="austin" & all_subs$group2!="control" & all_subs$rep == "u",])

     ssd_uniq_a = t.test(ssd ~ group2, data = all_subs[all_subs$city=="austin" & all_subs$group2!="control" & all_subs$rep == "u",])
     print("Mean SSD - S1 vs. s2 Unique")
     print(ssd_uniq_a)

     ssd_uniq_a = t.test(SSRT_me ~ group2, data = all_subs[all_subs$city=="austin" & all_subs$group2!="control" & all_subs$rep == "u",])
     print("Mean SSRT  - S1 vs. S2 Unique")
     print(ssd_uniq_a)

     sink()


     # AUSTIN AND HOUSTON


     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/stop_stats_ah.txt")

     ssd_s1vc_ah = t.test(ssd ~ group2, data = all_subs[all_subs$group2 == "first" | all_subs$group2 == "control",])
     print("Mean SSD - S1 vs. Controls")
     print(ssd_s1vc_ah)

     ssrt_s1vc_ah = t.test(SSRT_me ~ group2, data = all_subs[all_subs$group2 == "first" | all_subs$group2 == "control",])
     print("Mean SSRT - S1 vs. Controls")
     print(ssrt_s1vc_ah)

     ssd_s2vc_ah = t.test(ssd ~ group2, data = all_subs[all_subs$group2 == "second" | all_subs$group2 == "control",])
     print("Mean SSD - S2 vs. Controls")
     print(ssd_s2vc_ah)

     ssrt_s2vc_ah = t.test(SSRT_me ~ group2, data = all_subs[all_subs$group2 == "second" | all_subs$group2 == "control",])
     print("Mean SSRT - S2 vs. Controls")
     print(ssrt_s2vc_ah)

     ssd_s1vs2_ah = t.test(ssd ~ group2, data = all_subs[all_subs$group2 == "first" | all_subs$group2 == "second",])
     print("Mean SSD - S1 vs. s2")
     print(ssd_s1vs2_ah)

     ssrt_s1vs2_ah = t.test(SSRT_me ~ group2, data = all_subs[all_subs$group2 == "first" | all_subs$group2 == "second",])
     print("Mean SSRT - S1 vs. S2")
     print(ssrt_s1vs2_ah)

     sink()


     # AUSTIN/HOUSTON REPEAT SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/stop_stats_ah_repeat.txt")

     print(all_subs[all_subs$rep == "r",])

     ssd_repeat_ah = t.test(ssd ~ group2, data = all_subs[all_subs$rep == "r",], paired=T)
     print("Mean SSD - S1 vs. s2 Repeats")
     print(ssd_repeat_ah)

     ssrt_repeat_ah = t.test(SSRT_me ~ group2, data = all_subs[all_subs$rep == "r",], paired=T)
     print("Mean SSRT - S1 vs. S2 Repeats")
     print(ssrt_repeat_ah)

     sink()


     # AUSTIN/HOUSTON UNIQUE SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/stop_stats_ah_uniq.txt")

     print(all_subs[all_subs$group2!="control" & all_subs$rep == "u",])

     ssd_uniq_ah = t.test(ssd ~ group2, data = all_subs[all_subs$group2!="control" & all_subs$rep == "u",])
     print("Mean SSD - S1 vs. s2 Unique")
     print(ssd_uniq_ah)

     ssd_uniq_ah = t.test(SSRT_me ~ group2, data = all_subs[all_subs$group2!="control" & all_subs$rep == "u",])
     print("Mean SSRT  - S1 vs. S2 Unique")
     print(ssd_uniq_ah)

     sink()




# FIGURES 

  # AUSTIN ONLY

    # GO RT

        A_goRT_box = ggplot(data = all_subs[all_subs$city=="austin",],aes(x = group2,y = go_rt_mean)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + geom_point(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(colod=ID2, size=2, shape=ID2),size=4.5) + geom_line(data=all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black"))
        ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_box_feb16.pdf",wd),width=5,height=5)

        #+ scale_color_discrete(name = "group2", breaks = c("A_control", "A_first", "A_second"), labels = c(sprintf("A_control (%d)", length(A_go_sub$groups[A_go_sub$groups == "A_control"])), sprintf("A_first (%d)",length(A_go_sub$group2[A_go_sub$group2 == "A_first"])), sprintf("A_second (%d)",length(A_go_sub$group2[A_go_sub$group2 == "A_second"]))))  + scale_shape_discrete(name = "group2", breaks = c("A_control", "A_first", "A_second"), labels = c(sprintf("A_control (%d)", length(A_go_sub$group2[A_go_sub$group2 == "A_control"])), sprintf("A_first (%d)",length(A_go_sub$group2[A_go_sub$group2 == "A_first"])), sprintf("A_second (%d)",length(A_go_sub$group2[A_go_sub$group2 == "A_second"])))) 
	             
 
        A_goRT_box_noline = ggplot(data = all_subs[all_subs$city=="austin",],aes(x = group2,y = go_rt_mean)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + geom_point(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(colod=ID2, size=2, shape=ID2),size=4.5) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black"))
        ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_box_noline_feb16.pdf",wd),width=5,height=5)


        A_goRT_box_nolegend = ggplot(data = all_subs[all_subs$city=="austin",],aes(x = group2,y = go_rt_mean)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + geom_point(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(colod=ID2, size=2, shape=ID2),size=4.5) + geom_line(data=all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black")) + guides(group = FALSE, color = FALSE, linetype=FALSE, size=FALSE, alpha=FALSE, shape=FALSE, fill=FALSE)
        ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_box_no_legend_feb16.pdf",wd),width=5,height=5)


       # AUSTIN REPEATS

        A_goRT_box_repeat = ggplot(data = all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(x = group2,y = go_rt_mean)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#CC6699", "#FFCCFF")) + geom_point(data= all_subs[all_subs$city=="austin" & all_subs$rep=="r",], aes(colod=ID2, size=2, shape=ID2),size=4.5) + geom_line(data=all_subs[all_subs$city=="austin" & all_subs$rep=="r",], aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black"))
        ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_box_repeat_feb16.pdf",wd),width=5,height=5)

        A_goRT_box_repeat_noline = ggplot(data = all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(x = group2,y = go_rt_mean)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#CC6699", "#FFCCFF")) + geom_point(data= all_subs[all_subs$city=="austin" & all_subs$rep=="r",], aes(colod=ID2, size=2, shape=ID2),size=4.5) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black"))
        ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_box_repeat_no_line_feb16.pdf",wd),width=5,height=5)

        A_goRT_box_repeat_nolegend = ggplot(data = all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(x = group2,y = go_rt_mean)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#CC6699", "#FFCCFF")) + geom_point(data= all_subs[all_subs$city=="austin" & all_subs$rep=="r",], aes(colod=ID2, size=2, shape=ID2),size=4.5) + geom_line(data=all_subs[all_subs$city=="austin" & all_subs$rep=="r",], aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black")) + guides(group = FALSE, color = FALSE, linetype=FALSE, size=FALSE, alpha=FALSE, shape=FALSE, fill=FALSE)
        ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_box_repeat_no_legend_feb16.pdf",wd),width=5,height=5)




    # GO ACC

        A_goACC_box = ggplot(data = all_subs[all_subs$city=="austin",],aes(x = group2,y = go_acc_mean)) + geom_boxplot(aes(fill=group2)) + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + geom_point(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(color=ID2, size=2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ geom_line(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),],aes(group = subind, color = ID2, linetype = ID2))+ xlab("Group") + ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
        ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_goACC_box_feb16.pdf",wd),width=5,height=5)

      #+ guides(group = FALSE, fill = FALSE, shape=FALSE, color=FALSE, linetype=FALSE) + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
      #+ legend.title = element_text(size = rel(1.5)), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10))  
	              

        A_goACC_box_noline = ggplot(data = all_subs[all_subs$city=="austin",],aes(x = group2,y = go_acc_mean)) + geom_boxplot(aes(fill=group2)) + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + geom_point(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(color=ID2, size=2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ xlab("Group") + ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_goACC_box_noline_feb16.pdf",wd),width=5,height=5)


        A_goACC_box_nolegend = ggplot(data = all_subs[all_subs$city=="austin",],aes(x = group2,y = go_acc_mean)) + geom_boxplot(aes(fill=group2)) + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + geom_point(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(color=ID2, size=2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ geom_line(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),],aes(group = subind, color = ID2, linetype = ID2))+ xlab("Group") + ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + guides(group = FALSE, fill = FALSE, shape=FALSE, color=FALSE, linetype=FALSE)
        ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_goACC_box_no_legend_feb16.pdf",wd),width=5,height=5)


       # AUSTIN REPEATS

        A_goACC_box_repeat = ggplot(data = all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(x = group2,y = go_acc_mean)) + geom_boxplot(aes(fill=group2)) + scale_fill_manual(values=c("#CC6699", "#FFCCFF")) + geom_point(data= all_subs[all_subs$city=="austin" & all_subs$rep=="r",], aes(color=ID2, size=2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ geom_line(data= all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(group = subind, color = ID2, linetype = ID2))+ xlab("Group") + ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
        ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_goACC_box_repeat_feb16.pdf",wd),width=5,height=5)

        A_goACC_box_repeat_noline = ggplot(data = all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(x = group2,y = go_acc_mean)) + geom_boxplot(aes(fill=group2)) + scale_fill_manual(values=c("#CC6699", "#FFCCFF")) + geom_point(data= all_subs[all_subs$city=="austin" & all_subs$rep=="r",], aes(color=ID2, size=2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ xlab("Group") + ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
        ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_goACC_box_repeat_no_line_feb16.pdf",wd),width=5,height=5)

        A_goACC_box_repeat_nolegend = ggplot(data = all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(x = group2,y = go_acc_mean)) + geom_boxplot(aes(fill=group2)) + scale_fill_manual(values=c("#CC6699", "#FFCCFF")) + geom_point(data= all_subs[all_subs$city=="austin" & all_subs$rep=="r",], aes(color=ID2, size=2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ geom_line(data= all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(group = subind, color = ID2, linetype = ID2))+ xlab("Group") + ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + guides(group = FALSE, fill = FALSE, shape=FALSE, color=FALSE, linetype=FALSE)
        ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_goACC_box_repeat_no_legend_feb16.pdf",wd),width=5,height=5)



    # STOP

        A_SSRT_box = ggplot(data = all_subs[all_subs$city=="austin",],aes(x = group2,y = SSRT_me)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + geom_point(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ geom_line(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + ylab("Mean SSRT (s)") 
	# + guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	             ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSRT_box_feb16.pdf",wd),width=5,height=5)

        A_SSRT_box_noline = ggplot(data = all_subs[all_subs$city=="austin",],aes(x = group2,y = SSRT_me)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + geom_point(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + ylab("Mean SSRT (s)") 
	# + guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	             ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSRT_box_noline_feb16.pdf",wd),width=5,height=5)


        A_SSRT_box_nolegend = ggplot(data = all_subs[all_subs$city=="austin",],aes(x = group2,y = SSRT_me)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + geom_point(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ geom_line(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + ylab("Mean SSRT (s)") + guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	             ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSRT_box_no_legend_feb16.pdf",wd),width=5,height=5)


        A_stopACC_box = ggplot(data = all_subs[all_subs$city=="austin",],aes(x = group2,y = stop_acc_me)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + geom_point(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + geom_line(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Stop Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
	#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_stopACC_box_feb16.pdf",wd),width=5,height=5)

        A_stopACC_box_noline = ggplot(data = all_subs[all_subs$city=="austin",],aes(x = group2,y = stop_acc_me)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + geom_point(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") + ylab("Mean Stop Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
	#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_stopACC_box_noline_feb16.pdf",wd),width=5,height=5)


        A_stopACC_box_nolegend = ggplot(data = all_subs[all_subs$city=="austin",],aes(x = group2,y = stop_acc_me)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + geom_point(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + geom_line(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Stop Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_stopACC_box_no_legend_feb16.pdf",wd),width=5,height=5)


        A_SSD_box = ggplot(data = all_subs[all_subs$city=="austin",],aes(x = group2,y = ssd)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + geom_point(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + geom_line(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean SSD (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSD_box_feb16.pdf",wd),width=5,height=5)

        A_SSD_box_noline = ggplot(data = all_subs[all_subs$city=="austin",],aes(x = group2,y = ssd)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + geom_point(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") + ylab("Mean SSD (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSD_box_noline_feb16.pdf",wd),width=5,height=5)

        A_SSD_box_nolegend = ggplot(data = all_subs[all_subs$city=="austin",],aes(x = group2,y = ssd)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + geom_point(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + geom_line(data= all_subs[((all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c") & all_subs$city=="austin"),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean SSD (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSD_box_no_legend_feb16.pdf",wd),width=5,height=5)




        # AUSTIN REPEATS

        A_SSRT_box_repeat = ggplot(data = all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(x = group2,y = SSRT_me)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#CC6699", "#FFCCFF")) + geom_point(data= all_subs[all_subs$city=="austin" & all_subs$rep=="r",], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ geom_line(data=all_subs[all_subs$city=="austin" & all_subs$rep=="r",] ,aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + ylab("Mean SSRT (s)") 
	# + guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	             ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSRT_box_repeat_feb16.pdf",wd),width=5,height=5)


        A_SSRT_box_repeat_noline = ggplot(data = all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(x = group2,y = SSRT_me)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#CC6699", "#FFCCFF")) + geom_point(data= all_subs[all_subs$city=="austin" & all_subs$rep=="r",], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + ylab("Mean SSRT (s)") 
	# + guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	             ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSRT_box_repeat_no_line_feb16.pdf",wd),width=5,height=5)

        A_SSRT_box_repeat_nolegend = ggplot(data = all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(x = group2,y = SSRT_me)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#CC6699", "#FFCCFF")) + geom_point(data= all_subs[all_subs$city=="austin" & all_subs$rep=="r",], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ geom_line(data=all_subs[all_subs$city=="austin" & all_subs$rep=="r",] ,aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + ylab("Mean SSRT (s)") + guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	             ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSRT_box_repeat_no_legend_feb16.pdf",wd),width=5,height=5)


        A_stopACC_box_repeat = ggplot(data = all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(x = group2,y = stop_acc_me)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#CC6699", "#FFCCFF")) + geom_point(data=all_subs[all_subs$city=="austin" & all_subs$rep=="r",], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + geom_line(data= all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Stop Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
	#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_stopACC_box_repeat_feb16.pdf",wd),width=5,height=5)

        A_stopACC_box_repeat_noline = ggplot(data = all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(x = group2,y = stop_acc_me)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#CC6699", "#FFCCFF")) + geom_point(data=all_subs[all_subs$city=="austin" & all_subs$rep=="r",], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") + ylab("Mean Stop Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
	#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_stopACC_box_repeat_no_line_feb16.pdf",wd),width=5,height=5)

        A_stopACC_box_repeat_nolegend = ggplot(data = all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(x = group2,y = stop_acc_me)) + geom_boxplot(aes(fill = group2)) + scale_fill_manual(values=c("#CC6699", "#FFCCFF")) + geom_point(data=all_subs[all_subs$city=="austin" & all_subs$rep=="r",], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + geom_line(data= all_subs[all_subs$city=="austin" & all_subs$rep=="r",],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Stop Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_stopACC_box_repeat_no_legend_feb16.pdf",wd),width=5,height=5)


  # AUSTIN AND HOUSTON COMBINED

    # GO

        All_goRT_box = ggplot(data = all_subs, aes(x = group2, y = go_rt_mean)) + geom_boxplot(data = all_subs, aes(fill = group2)) + geom_point(data=all_subs[(all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c"),], aes(colod=ID2, size=2, shape=ID2),size=4.5) + geom_line(data= all_subs[(all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c"),], aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black"))
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_goRT_box.pdf",wd),width=5,height=5)


        All_goRT_box_noline = ggplot(data = all_subs,aes(x = group2, y = go_rt_mean)) + geom_boxplot(data = all_subs,aes(fill = group2)) + geom_point(data=all_subs[(all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c"),], aes(colod=ID2, size=2, shape=ID2),size=4.5) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black"))
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_goRT_box_noline.pdf",wd),width=5,height=5)

        All_goACC_box = ggplot(data = all_subs,aes(x = group2,y = go_acc_mean)) + geom_boxplot(data = all_subs,aes(fill = group2)) + geom_point(data=all_subs[(all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c"),], aes(colod=ID2, size=2, shape=ID2),size=4.5) + geom_line(data= all_subs[(all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c"),], aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black"))
	                ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/All_goACC_box.pdf",wd),width=5,height=5)

        All_goACC_box_noline = ggplot(data = all_subs,aes(x = group2,y = go_acc_mean)) + geom_boxplot(data = all_subs,aes(fill = group2)) + geom_point(data=all_subs[(all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c"),], aes(colod=ID2, size=2, shape=ID2),size=4.5) + xlab("Group") + ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black"))
	                ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/All_goACC_box_noline.pdf",wd),width=5,height=5)


    # STOP

        All_SSRT_box = ggplot(data = all_subs,aes(x = group2,y = SSRT_me)) + geom_boxplot(aes(fill = group2)) + geom_point(data= all_subs[(all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + geom_line(data= all_subs[(all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c"),], aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean SSRT (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
	#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_SSRT_box.pdf",wd),width=5,height=5)
 

        All_SSRT_box_noline = ggplot(data = all_subs,aes(x = group2,y = SSRT_me)) + geom_boxplot(aes(fill = group2)) + geom_point(data= all_subs[(all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))  + xlab("Group") + ylab("Mean SSRT (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
	#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_SSRT_box_noline.pdf",wd),width=5,height=5)
 
        All_SSD_box = ggplot(data = all_subs,aes(x = group2,y = ssd)) + geom_boxplot(aes(fill = group2)) + geom_point(data= all_subs[(all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + geom_line(data= all_subs[(all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c"),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean SSD (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_SSD_box.pdf",wd),width=5,height=5)
   
        All_SSD_box_noline = ggplot(data = all_subs,aes(x = group2,y = ssd)) + geom_boxplot(aes(fill = group2)) + geom_point(data=all_subs[(all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") + ylab("Mean SSD (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_SSD_box_noline.pdf",wd),width=5,height=5)
   

        All_stopACC_box = ggplot(data = all_subs,aes(x = group2,y = stop_acc_me)) + geom_boxplot(aes(fill = group2)) + geom_point(data=all_subs[(all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + geom_line(data= all_subs[(all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c"),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Stop Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
	#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	                  ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/All_stopACC_box.pdf",wd),width=5,height=5)


        All_stopACC_box_noline = ggplot(data = all_subs,aes(x = group2,y = stop_acc_me)) + geom_boxplot(aes(fill = group2)) + geom_point(data=all_subs[(all_subs$ID2 =="0" | all_subs$ID2=="1" | all_subs$ID2 == "c"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") + ylab("Mean Stop Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
	#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	                  ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/All_stopACC_box_noline.pdf",wd),width=5,height=5)


























#EVERYTHING BELOW THIS LINE IS EXTRA AND SHOULD BE USED AS A REFERENCE FOR EXTRA CODING
--------------------------------------------------------------------------------------

  # INTERVENTION (compare 0 to 1/2)
    all_go_run$subind[all_go_run$groups=="A_second"]=substrLeft(all_go_run$subind[all_go_run$groups=="A_second"],5)  
    all_go_run$subind[all_go_run$groups=="A_third"]=substrLeft(all_go_run$subind[all_go_run$groups=="A_third"],5)
    all_go_run$subind[all_go_run$groups=="A_first2"]=substrRight(all_go_run$subind[all_go_run$groups=="A_first2"],5)  
    all_go_run$subind[all_go_run$groups=="H_second"]=substrLeft(all_go_run$subind[all_go_run$groups=="H_second"],6)  
    all_go_run$subind[all_go_run$groups=="H_third"]=substrLeft(all_go_run$subind[all_go_run$groups=="H_third"],6) 
    all_go_run$subind[all_go_run$groups=="H2_first"]=substrRight(all_go_run$subind[all_go_run$groups=="H2_first"],7) 
    all_go_run$subind[all_go_run$groups=="H_control_second"]=substrLeft(all_go_run$subind[all_go_run$groups=="H_control_second"],4)
    all_go_run$subind[all_go_run$groups=="H_control_third"]=substrLeft(all_go_run$subind[all_go_run$groups=="H_control_third"],4)
    all_go_run$ID = "c"
    all_go_run$ID[all_go_run$groups=="H_first"]=substrRight(all_go_run$subind[all_go_run$groups=="H_first"], 1)
    all_go_run$ID[all_go_run$groups=="H_second"]=substrRight(all_go_run$subind[all_go_run$groups=="H_second"], 1)
    all_go_run$ID[all_go_run$groups=="H_third"]=substrRight(all_go_run$subind[all_go_run$groups=="H_third"], 1)
    all_go_run$ID[all_go_run$groups=="H2_first"]=substrRight(all_go_run$subind[all_go_run$groups=="H2_first"], 1)
    all_go_run$ID[all_go_run$groups == "A_first"]  = substrLeft(all_go_run$subind[all_go_run$groups == "A_first"], 1)
    all_go_run$ID[all_go_run$groups == "A_second"]  = substrLeft(all_go_run$subind[all_go_run$groups == "A_second"], 1)
    all_go_run$ID[all_go_run$groups == "A_third"]  = substrLeft(all_go_run$subind[all_go_run$groups == "A_third"], 1)
    all_go_run$ID[all_go_run$groups == "A_first2"]  = substrLeft(all_go_run$subind[all_go_run$groups == "A_first2"], 1)

    # creates a column to be used to compare 0s to 1/2s
    all_go_run$ID2 = all_go_run$ID
    all_go_run$ID2[all_go_run$ID2 == 1 | all_go_run$ID2 == 2] = 1

    # RT
    int.mod1 = lme(go_rt_med ~ ID2, random = list(~1|subind, ~1|runnum), data = all_go_run[all_go_run$groups == "A_first" | all_go_run$groups == "A_first2",])
    summary(int.mod1)

    t.test(go_rt_med ~ ID2, data = all_go_run[all_go_run$groups == "A_first" | all_go_run$groups == "A_first2",])

    
    int.mod2 = lme(go_rt_med ~ ID2, random = list(~1|subind, ~1|runnum), data = all_go_run[all_go_run$groups == "A_second",])
    summary(int.mod2)

    t.test(go_rt_med ~ ID2, data = all_go_run[all_go_run$groups == "A_second",] )

    int.mod3 = lme(go_rt_med ~ ID2, random = list(~1|subind, ~1|runnum), data = all_go_run[all_go_run$groups == "A_third",])
    summary(int.mod3)
    
    # ACC
    int.mod4 = lme(go_acc ~ ID2, random = list(~1|subind, ~1|runnum), data = all_go_run[all_go_run$groups == "A_first" | all_go_run$groups == "A_first2",])
    summary(int.mod4)
 
    t.test(go_acc ~ ID2, data = all_go_run[all_go_run$groups == "A_first" | all_go_run$groups == "A_first2",])
   
    int.mod5 = lme(go_acc ~ ID2, random = list(~1|subind, ~1|runnum), data = all_go_run[all_go_run$groups == "A_second",])
    summary(int.mod5)

    t.test(go_acc ~ ID2, data = all_go_run[all_go_run$groups == "A_second",])

    int.mod6 = lme(go_acc ~ ID2, random = list(~1|subind, ~1|runnum), data = all_go_run[all_go_run$groups == "A_third",])
    summary(int.mod6)







# GO FIGURES 

  # BY SESSION

    # AUSTIN

        # BY SUB
        A_go_sub_fs = A_go_sub[A_go_sub$groups != "A_third",]

        A_go_sub_fs$groups = relevel(A_go_sub$groups, ref = "A_control")
        A_go_sub_fs$groups = relevel(A_go_sub$groups, ref = "A_first")
        A_go_sub_fs$groups = relevel(A_go_sub$groups, ref = "A_second")
        A.goACC.goRT.mod = lme(go_acc_mean ~ go_rt_mean*groups,  random = ~1 | subind, data = A_go_sub_fs, method = "ML")

        int.matrix = rbind("c.int"=c(1,0,0,0,0,0), "f.int"=c(1,0,1,0,0,0), "s.int"=c(1,0,0,1,0,0))
        colnames(int.matrix) = names(coef(A.goACC.goRT.mod)) 
        slp.matrix = rbind("c.slp"=c(0,1,0,0,0,0), "f.slp"=c(0,1,0,0,1,0), "s.slp"=c(0,1,0,0,0,1)) 
        colnames(slp.matrix) = names(coef(A.goACC.goRT.mod)) 

        #int.matrix = rbind("c.int"=c(1,0,0,0,0,0,0,0), "f.int"=c(1,0,1,0,0,0,0,0), "s.int"=c(1,0,0,1,0,0,0,0), "t.int"=c(1,0,0,0,1,0,0,0))
        #colnames(int.matrix) = names(coef(A.goACC.goRT.mod)) 
        #slp.matrix = rbind("c.slp"=c(0,1,0,0,0,0,0,0), "f.slp"=c(0,1,0,0,0,1,0,0), "s.slp"=c(0,1,0,0,0,0,1,0), "t.slp"=c(0,1,0,0,0,0,0,1)) 
        #colnames(slp.matrix) = names(coef(A.goACC.goRT.mod)) 

        summary(A.goACC.goRT.mod)$tTable
        intervals(A.goACC.goRT.mod)

        int.sum = data.frame(confint(summary(glht(A.goACC.goRT.mod, int.matrix), test = adjusted("none")))$confint)
        colnames(int.sum) = c("intercept", "int.lwr", "int.upr")
        slp.sum = data.frame(confint(summary(glht(A.goACC.goRT.mod, slp.matrix), test = adjusted("none")))$confint)
        colnames(slp.sum) = c("slope", "slp.lwr", "slp.upr")

        coef.dat = data.frame(groups=levels(A_go_sub_fs$groups), int.sum, slp.sum)  

        A_goACC_goRT_scat = ggplot(data = A_go_sub_fs, aes(x = go_rt_mean, y = go_acc_mean, group = groups)) + geom_point(aes(color = groups, shape = groups)) + geom_abline(data = coef.dat, aes(intercept=intercept, slope=slope, color = groups)) + xlab("Mean goRT") + ylab("Mean goACC") + ggtitle("Austin Go Correct ACC and RT by Sub") +  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black')) + scale_color_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", length(A_go_sub$groups[A_go_sub$groups == "A_control"])), sprintf("A_first (%d)",length(A_go_sub$groups[A_go_sub$groups == "A_first"])), sprintf("A_second (%d)",length(A_go_sub$groups[A_go_sub$groups == "A_second"])), sprintf("A_third (%d)",length(A_go_sub$groups[A_go_sub$groups == "A_third"]))))  + scale_shape_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", length(A_go_sub$groups[A_go_sub$groups == "A_control"])), sprintf("A_first (%d)",length(A_go_sub$groups[A_go_sub$groups == "A_first"])), sprintf("A_second (%d)",length(A_go_sub$groups[A_go_sub$groups == "A_second"])), sprintf("A_third (%d)",length(A_go_sub$groups[A_go_sub$groups == "A_third"]))))
                            ggsave(filename=sprintf("%s/figures/Project_4/SST/A_goACC_goRT_scat_sub_all_mri.png",wd),width=10,height=10)
        
       # BY RUN
        A.goACC.goRT.mod2 = lme(go_acc ~ go_rt_med*groups,  random = list(~1 | subind, ~1 | runnum), data = A_go_run, method = "ML")

        int.matrix2 = rbind("c.int"=c(1,0,0,0,0,0,0,0), "f.int"=c(1,0,1,0,0,0,0,0), "s.int"=c(1,0,0,1,0,0,0,0), "t.int"=c(1,0,0,0,1,0,0,0))
        colnames(int.matrix2) = names(coef(A.goACC.goRT.mod2)) 
        slp.matrix2 = rbind("c.slp"=c(0,1,0,0,0,0,0,0), "f.slp"=c(0,1,0,0,0,1,0,0), "s.slp"=c(0,1,0,0,0,0,1,0), "t.slp"=c(0,1,0,0,0,0,0,1)) 
        colnames(slp.matrix2) = names(coef(A.goACC.goRT.mod2)) 

        summary(A.goACC.goRT.mod2)$tTable
        int.sum2 = data.frame(confint(summary(glht(A.goACC.goRT.mod2, int.matrix2), test = adjusted("none")))$confint)
        colnames(int.sum2) = c("intercept", "int.lwr", "int.upr")
        slp.sum2 = data.frame(confint(summary(glht(A.goACC.goRT.mod2, slp.matrix2), test = adjusted("none")))$confint)
        colnames(slp.sum2) = c("slope", "slp.lwr", "slp.upr")

        coef.dat2 = data.frame(groups=levels(A_go_run$groups), int.sum2, slp.sum2)  

        A_goACC_goRT_scat2 = ggplot(data = A_go_run, aes(x = go_rt_med, y = go_acc, group = groups)) + geom_point(aes(color = groups, shape = groups)) + geom_abline(data = coef.dat2, aes(intercept=intercept, slope=slope, color = groups)) + xlab("Med goRT") + ylab("Mean goACC") + ggtitle("Austin Go Correct ACC and RT by Run") +  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))+ scale_color_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", length(A_go_sub$groups[A_go_sub$groups == "A_control"])), sprintf("A_first (%d)",length(A_go_sub$groups[A_go_sub$groups == "A_first"])), sprintf("A_second (%d)",length(A_go_sub$groups[A_go_sub$groups == "A_second"])), sprintf("A_third (%d)",length(A_go_sub$groups[A_go_sub$groups == "A_third"]))))  + scale_shape_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", length(A_go_sub$groups[A_go_sub$groups == "A_control"])), sprintf("A_first (%d)",length(A_go_sub$groups[A_go_sub$groups == "A_first"])), sprintf("A_second (%d)",length(A_go_sub$groups[A_go_sub$groups == "A_second"])), sprintf("A_third (%d)",length(A_go_sub$groups[A_go_sub$groups == "A_third"]))))
                            ggsave(filename=sprintf("%s/figures/Project_4/SST/A_goACC_goRT_scat_run_all_mri.png",wd),width=10,height=10)




      # HOUSTON


      # BOTH AUSTIN AND HOUSTON

        a1_h1_c_goRT_box = ggplot(data = a1_h1_c_go_sub,aes(x = groups,y = go_rt_mean, group = groups)) + geom_boxplot(aes(fill = groups, alpha = .2)) + geom_point(aes(color=ID2, size=2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,16)) + xlab("Session") + ylab("Mean RT (s)") + guides(group = FALSE, alpha = FALSE, size = FALSE, alpha = FALSE) + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
                           ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/a1_h1_c_goRT_box.pdf",wd),width=5,height=5)

        t.test(go_rt_mean ~ groups, data = a1_h1_c_go_sub[a1_h1_c_go_sub$groups=="A_first" | a1_h1_c_go_sub$groups=="A_first2",])
        t.test(go_rt_mean ~ groups, data = a1_h1_c_go_sub[a1_h1_c_go_sub$groups=="A_first" | a1_h1_c_go_sub$groups=="H2_first",])
        t.test(go_rt_mean ~ groups, data = a1_h1_c_go_sub[a1_h1_c_go_sub$groups=="A_first" | a1_h1_c_go_sub$groups=="A_control",])

        t.test(go_rt_mean ~ groups, data = a1_h1_c_go_sub[a1_h1_c_go_sub$groups=="A_first2" | a1_h1_c_go_sub$groups=="H2_first",]) #0.0584
        t.test(go_rt_mean ~ groups, data = a1_h1_c_go_sub[a1_h1_c_go_sub$groups=="A_first2" | a1_h1_c_go_sub$groups=="A_control",])

        t.test(go_rt_mean ~ groups, data = a1_h1_c_go_sub[a1_h1_c_go_sub$groups=="H2_first" | a1_h1_c_go_sub$groups=="A_control",])


        a1_h1_c_goACC_box = ggplot(data = a1_h1_c_go_sub,aes(x = groups,y = go_acc_mean, group = groups)) + geom_boxplot(aes(fill = groups, alpha = .2)) + geom_point(aes(color=ID2, size=2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,16)) + xlab("Session") + ylab("Mean ACC") + guides(group = FALSE, alpha = FALSE, size = FALSE, alpha = FALSE) + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
                           ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/a1_h1_c_goACC_box.pdf",wd),width=5,height=5)

 
        t.test(go_acc_mean ~ groups, data = a1_h1_c_go_sub[a1_h1_c_go_sub$groups=="A_first" | a1_h1_c_go_sub$groups=="A_first2",])
        t.test(go_acc_mean ~ groups, data = a1_h1_c_go_sub[a1_h1_c_go_sub$groups=="A_first" | a1_h1_c_go_sub$groups=="H2_first",])
        t.test(go_acc_mean ~ groups, data = a1_h1_c_go_sub[a1_h1_c_go_sub$groups=="A_first" | a1_h1_c_go_sub$groups=="A_control",])

        t.test(go_acc_mean ~ groups, data = a1_h1_c_go_sub[a1_h1_c_go_sub$groups=="A_first2" | a1_h1_c_go_sub$groups=="H2_first",])
        t.test(go_acc_mean ~ groups, data = a1_h1_c_go_sub[a1_h1_c_go_sub$groups=="A_first2" | a1_h1_c_go_sub$groups=="A_control",]) #0.0582

        t.test(go_acc_mean ~ groups, data = a1_h1_c_go_sub[a1_h1_c_go_sub$groups=="H2_first" | a1_h1_c_go_sub$groups=="A_control",]) #0.0608


    # BY INTERVENTION CODE

      # AUSTIN

        A_goRT_ID_box = ggplot(data = A_go_sub,aes(x = ID,y = go_rt_mean)) + geom_boxplot() + geom_point(aes(group = groups, color = groups),size = 2.5) + xlab("Intervention Code") +  ylab("Mean RT (s)") + ggtitle("Austin Go Correct RT by Intervention")+ guides(group = FALSE, size = FALSE)
	                ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_ID_box.png",wd),width=10,height=10)

        A_goRT_ID_bar = ggplot(data = A_go_ID, aes(x = ID, y = go_rt_me, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                            geom_errorbar(aes(y = go_rt_me, ymin = go_rt_me-go_rt_se, ymax = go_rt_me+go_rt_se), position = position_dodge(.9), width=.2) + xlab("Intervention Code") + ylab("Mean RT (s)") + 
                            ggtitle("Austin Go Correct RT by Intervention") +
                            scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", A_go_group$N[A_go_group$groups == "A_control"]), sprintf("A_first (%d)",
			      A_go_group$N[A_go_group$groups == "A_first"]), sprintf("A_second (%d)",A_go_group$N[A_go_group$groups == "A_second"]), sprintf("A_third (%d)",A_go_group$N[A_go_group$groups == "A_third"])))
                        ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_ID_bar.png",wd),width=10,height=10)


        A_goACC_ID_box = ggplot(data = A_go_sub,aes(x = ID,y = go_acc_mean)) + geom_boxplot() + geom_point(aes(group = groups, color = groups), size = 2.5) + xlab("Intervention Code") + ylab("Mean ACC") + ggtitle("Austin Go Correct ACC by Intervention")+ guides(group = FALSE,size = FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_goACC_ID_box.png",wd),width=10,height=10)


        A_goACC_ID_bar = ggplot(data = A_go_ID, aes(x = ID, y = go_acc_me, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                             geom_errorbar(aes(y = go_acc_me, ymin = go_acc_me-go_acc_se, ymax = go_acc_me+go_acc_se), position = position_dodge(.9), width=.2) + xlab("Session") + ylab("Mean ACC") + 
                             ggtitle("Austin Go Correct ACC by Intervention") +
                             scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", A_go_group$N[A_go_group$groups == "A_control"]), sprintf("A_first (%d)",
			      A_go_group$N[A_go_group$groups == "A_first"]), sprintf("A_second (%d)",A_go_group$N[A_go_group$groups == "A_second"]), sprintf("A_third (%d)",A_go_group$N[A_go_group$groups == "A_third"])))
                      ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_goACC_ID_bar.png",wd),width=10,height=10)


      # HOUSTON

        H_goRT_ID_box = ggplot(data = H_go_sub,aes(x = ID,y = go_rt_mean)) + geom_boxplot() + geom_point(aes(group = groups, color = groups, size = 2)) +  xlab("Intervention Code") + ylab("Mean RT (s)") + ggtitle("Houston Go Correct RT by Intervention")+ guides(group = FALSE, size = FALSE)
	                ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/H_goRT_ID_box.png",wd),width=10,height=10)
      
 
        H_goRT_ID_bar = ggplot(data=H_go_ID, aes(x = ID, y = go_rt_me, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                               geom_errorbar(aes(y=go_rt_me, ymin=go_rt_me-go_rt_se, ymax=go_rt_me+go_rt_se), position = position_dodge(.9), width=.2) + ylab("Mean RT") + xlab("Intervention Code") + 
                               ggtitle("Houston Go Correct RT by Intervention") +
                               scale_fill_discrete(name = "groups", breaks = c("H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("H_control_first (%d)", H_go_group$N[H_go_group$groups == "H_control_first"]), 
                                 sprintf("H_control_second (%d)", H_go_group$N[H_go_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", H_go_group$N[H_go_group$groups == "H_control_third"]), sprintf("H_first (%d)", H_go_group$N[H_go_group$groups == "H_first"]), 
                                 sprintf("H_second (%d)",H_go_group$N[H_go_group$groups == "H_second"]), sprintf("H_third (%d)",H_go_group$N[H_go_group$groups == "H_third"])))
                        ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/H_goRT_ID_bar.png",wd),width=10,height=10)


        H_goACC_ID_box = ggplot(data = H_go_sub,aes(x = ID,y = go_acc_mean)) + geom_boxplot() + geom_point(aes(group = groups, color = groups, size = 2)) + xlab("Intervention Code") + ylab("Mean ACC (s)") + ggtitle("Houston Go Correct ACC by Intervention")+ guides(group = FALSE, size = FALSE)
	                 ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/H_goACC_ID_box.png",wd),width=10,height=10)

        H_goACC_ID_bar = ggplot(data=H_go_ID, aes(x = ID, y = go_acc_me, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                                geom_errorbar(aes(y=go_acc_me, ymin=go_acc_me-go_acc_se, ymax=go_acc_me+go_acc_se), position = position_dodge(.9), width=.2) + ylab("Mean ACC") + xlab("Intervention Code") + 
                                ggtitle("Houston Go Correct ACC by Intervention") +
                                scale_fill_discrete(name = "groups", breaks = c("H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("H_control_first (%d)", H_go_group$N[H_go_group$groups == "H_control_first"]), 
                                 sprintf("H_control_second (%d)", H_go_group$N[H_go_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", H_go_group$N[H_go_group$groups == "H_control_third"]), sprintf("H_first (%d)", H_go_group$N[H_go_group$groups == "H_first"]), 
                                 sprintf("H_second (%d)",H_go_group$N[H_go_group$groups == "H_second"]), sprintf("H_third (%d)",H_go_group$N[H_go_group$groups == "H_third"])))
                         ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/H_goACC_ID_bar.png",wd),width=10,height=10)


      # BOTH AUSTIN AND HOUSTON


        All_goRT_ID_box = ggplot(data = all_go_sub,aes(x = ID,y = go_rt_mean)) + geom_boxplot() + geom_point(aes(group = groups, color = groups, size = 2)) + xlab("Intervention Code") + ylab("Mean RT (s)") + ggtitle("Austin and Houston Go Correct RT by Intervention")+ guides(group = FALSE,size = FALSE)
	                  ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_goRT_ID_box.png",wd),width=10,height=10)
   

        All_goRT_ID_bar = ggplot(data = all_go_ID, aes(x = ID, y = go_rt_me, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                                 geom_errorbar(aes(y = go_rt_me, ymin = go_rt_me-go_rt_se, ymax = go_rt_me+go_rt_se), position = position_dodge(.9), width=.2) + xlab("Intervention Code") + ylab("Mean Go Correct RT") + 
                                 ggtitle("Austin and Houston Go Correct RT by Intervention") +
                                 scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third", "H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("A_control (%d)", all_go_group$N[all_go_group$groups == "A_control"]), 
                                   sprintf("A_first (%d)", all_go_group$N[all_go_group$groups == 'A_first']), sprintf('A_second (%d)',all_go_group$N[all_go_group$groups == 'A_second']), sprintf('A_third (%d)',all_go_group$N[all_go_group$groups == 'A_third']),
                                   sprintf("H_control_first (%d)", all_go_group$N[all_go_group$groups == "H_control_first"]), sprintf("H_control_second (%d)", all_go_group$N[all_go_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", all_go_group$N[all_go_group$groups == "H_control_third"]), 
                                   sprintf("H_first (%d)", all_go_group$N[all_go_group$groups == "H_first"]), sprintf("H_second (%d)",all_go_group$N[all_go_group$groups == "H_second"]), sprintf("H_third (%d)",all_go_group$N[all_go_group$groups == "H_third"])))
                          ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_goRT_ID_bar.png",wd),width=10,height=10)


        All_goACC_ID_box = ggplot(data = all_go_sub,aes(x = ID,y = go_acc_mean)) + geom_boxplot() + geom_point(aes(group = groups, color = groups, size = 2)) + xlab("Intervention Code") + ylab("Mean Go Correct ACC (s)") + ggtitle("Austin and Houston Go Correct ACC by Intervention")+ 
                                  guides(group = FALSE, size = FALSE)
	                   ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/All_goACC_ID_box.png",wd),width=10,height=10)


        All_goACC_ID_bar = ggplot(data = all_go_ID, aes(x = ID, y = go_acc_me, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                                  geom_errorbar(aes(y = go_acc_me, ymin = go_acc_me-go_acc_se, ymax = go_acc_me+go_acc_se), position = position_dodge(.9), width=.2) + xlab("Intervention Code") + ylab("Mean Go Correct ACC") + 
                                  ggtitle("Austin and Houston Go Correct ACC by Intervention") +
                                  scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third", "H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("A_control (%d)", all_go_group$N[all_go_group$groups == "A_control"]), 
                                    sprintf("A_first (%d)", all_go_group$N[all_go_group$groups == 'A_first']), sprintf('A_second (%d)',all_go_group$N[all_go_group$groups == 'A_second']), sprintf('A_third (%d)',all_go_group$N[all_go_group$groups == 'A_third']),
                                    sprintf("H_control_first (%d)", all_go_group$N[all_go_group$groups == "H_control_first"]), sprintf("H_control_second (%d)", all_go_group$N[all_go_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", all_go_group$N[all_go_group$groups == "H_control_third"]), 
                                    sprintf("H_first (%d)", all_go_group$N[all_go_group$groups == "H_first"]), sprintf("H_second (%d)",all_go_group$N[all_go_group$groups == "H_second"]), sprintf("H_third (%d)",all_go_group$N[all_go_group$groups == "H_third"])))
                           ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/All_goACC_ID_bar.png",wd),width=10,height=10)



------------------

    # GOCORR VS. STOPCORR
    A_go_stop_sub = cbind(A_go_sub, A_stop_sub[A_stop_sub$subind == A_go_sub$subind,3:7])
        A_go_stop_sub$groups = relevel(A_go_stop_sub$groups, ref = "A_control")
        A_go_stop_sub$groups = relevel(A_go_stop_sub$groups, ref = "A_first")
        A_go_stop_sub$groups = relevel(A_go_stop_sub$groups, ref = "A_second")
        A.goACC.stopACC.mod = lme(go_acc_mean ~ stop_acc_me*groups,  random = ~1 | subind, data = A_go_stop_sub, method = "ML")

        int.matrix3 = rbind("c.int"=c(1,0,0,0,0,0,0,0), "f.int"=c(1,0,1,0,0,0,0,0), "s.int"=c(1,0,0,1,0,0,0,0), "t.int"=c(1,0,0,0,1,0,0,0))
        colnames(int.matrix3) = names(coef(A.goACC.stopACC.mod)) 
        slp.matrix3 = rbind("c.slp"=c(0,1,0,0,0,0,0,0), "f.slp"=c(0,1,0,0,0,1,0,0), "s.slp"=c(0,1,0,0,0,0,1,0), "t.slp"=c(0,1,0,0,0,0,0,1)) 
        colnames(slp.matrix3) = names(coef(A.goACC.stopACC.mod)) 

        summary(A.goACC.stopACC.mod)$tTable
        intervals(A.goACC.stopACC.mod)

        int.sum3 = data.frame(confint(summary(glht(A.goACC.stopACC.mod, int.matrix3), test = adjusted("none")))$confint)
        colnames(int.sum3) = c("intercept", "int.lwr", "int.upr")
        slp.sum3 = data.frame(confint(summary(glht(A.goACC.stopACC.mod, slp.matrix3), test = adjusted("none")))$confint)
        colnames(slp.sum3) = c("slope", "slp.lwr", "slp.upr")

        coef.dat3 = data.frame(groups=levels(A_go_stop_sub$groups), int.sum3, slp.sum3)  

        A_goACC_stopACC_scat_sub = ggplot(data = A_go_stop_sub, aes(x = stop_acc_me, y = go_acc_mean, group = groups)) + geom_point(aes(color = groups, shape = groups)) + geom_abline(data = coef.dat3, aes(intercept=intercept, slope=slope, color = groups)) + xlab("Mean stopACC") + ylab("Mean goACC") + ggtitle("Austin Stop Correct ACC and Go Correct ACC by Sub") +  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black')) + scale_color_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_control"])), sprintf("A_first (%d)",length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_first"])), sprintf("A_second (%d)",length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_second"])), sprintf("A_third (%d)",length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_third"]))))  + scale_shape_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_control"])), sprintf("A_first (%d)",length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_first"])), sprintf("A_second (%d)",length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_second"])), sprintf("A_third (%d)",length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_third"]))))
                            ggsave(filename=sprintf("%s/figures/Project_4/SST/A_goACC_stopACC_scat_sub_all_mri.png",wd),width=10,height=10)
        

     A_go_stop_run =  cbind(A_go_run, A_stop_run[A_stop_run$subind == A_go_run$subind,4:8])
        A.goACC.stopACC.mod2 = lme(go_acc ~ stop_acc*groups,  random = ~1 | subind, data = A_go_stop_run, method = "ML")

        int.matrix4 = rbind("c.int"=c(1,0,0,0,0,0,0,0), "f.int"=c(1,0,1,0,0,0,0,0), "s.int"=c(1,0,0,1,0,0,0,0), "t.int"=c(1,0,0,0,1,0,0,0))
        colnames(int.matrix4) = names(coef(A.goACC.stopACC.mod2)) 
        slp.matrix4 = rbind("c.slp"=c(0,1,0,0,0,0,0,0), "f.slp"=c(0,1,0,0,0,1,0,0), "s.slp"=c(0,1,0,0,0,0,1,0), "t.slp"=c(0,1,0,0,0,0,0,1)) 
        colnames(slp.matrix4) = names(coef(A.goACC.stopACC.mod2)) 

        int.sum4 = data.frame(confint(summary(glht(A.goACC.stopACC.mod2, int.matrix4), test = adjusted("none")))$confint)
        colnames(int.sum4) = c("intercept", "int.lwr", "int.upr")
        slp.sum4 = data.frame(confint(summary(glht(A.goACC.stopACC.mod2, slp.matrix4), test = adjusted("none")))$confint)
        colnames(slp.sum4) = c("slope", "slp.lwr", "slp.upr")

        coef.dat4 = data.frame(groups=levels(A_go_stop_run$groups), int.sum4, slp.sum4)     

        A_goACC_stopACC_scat_run = ggplot(data = A_go_stop_run, aes(x = stop_acc, y = go_acc, group = groups)) + geom_point(aes(color = groups, shape = groups)) + geom_abline(data = coef.dat4, aes(intercept=intercept, slope=slope, color = groups)) + xlab("Mean stopACC") + ylab("Mean goACC") + ggtitle("Austin Stop Correct ACC and Go Correct ACC by Run") +  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black')) + scale_color_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_control"])), sprintf("A_first (%d)",length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_first"])), sprintf("A_second (%d)",length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_second"])), sprintf("A_third (%d)",length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_third"]))))  + scale_shape_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_control"])), sprintf("A_first (%d)",length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_first"])), sprintf("A_second (%d)",length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_second"])), sprintf("A_third (%d)",length(A_go_stop_sub$groups[A_go_stop_sub$groups == "A_third"]))))
                            ggsave(filename=sprintf("%s/figures/Project_4/SST/A_goACC_stopACC_scat_run_all_mri.png",wd),width=10,height=10)
  


  # HOUSTON

# STOP STATS
  # INTERVENTION 1/0/2
    all_stop_run$subind[all_stop_run$groups=="H_second"]=substrLeft(all_stop_run$subind[all_stop_run$groups=="H_second"],6)  
    all_stop_run$subind[all_stop_run$groups=="H_third"]=substrLeft(all_stop_run$subind[all_stop_run$groups=="H_third"],6)
    all_stop_run$subind[all_stop_run$groups=="H2_first"]=substrRight(all_stop_run$subind[all_stop_run$groups=="H2_first"],7)
    all_stop_run$subind[all_stop_run$groups=="H_control_second"]=substrLeft(all_stop_run$subind[all_stop_run$groups=="H_control_second"],4)
    all_stop_run$subind[all_stop_run$groups=="H_control_third"]=substrLeft(all_stop_run$subind[all_stop_run$groups=="H_control_third"],4)
    all_stop_run$subind[all_stop_run$groups == 'A_second']=substrLeft(all_stop_run$subind[all_stop_run$groups == 'A_second'], 5)
    all_stop_run$subind[all_stop_run$groups == 'A_third']=substrLeft(all_stop_run$subind[all_stop_run$groups == 'A_third'], 5)
    all_stop_run$subind[all_stop_run$groups == 'A_first2']=substrRight(all_stop_run$subind[all_stop_run$groups == 'A_first2'], 5)


    all_stop_run$ID = 'c'
    all_stop_run$ID[all_stop_run$groups=="H_first"]=substrRight(all_stop_run$subind[all_stop_run$groups=="H_first"], 1)
    all_stop_run$ID[all_stop_run$groups=="H_second"]=substrRight(all_stop_run$subind[all_stop_run$groups=="H_second"], 1)
    all_stop_run$ID[all_stop_run$groups=="H_third"]=substrRight(all_stop_run$subind[all_stop_run$groups=="H_third"], 1)
    all_stop_run$ID[all_stop_run$groups=="H2_first"]=substrRight(all_stop_run$subind[all_stop_run$groups=="H2_first"], 1)
    all_stop_run$ID[all_stop_run$groups == 'A_first']  = substrLeft(all_stop_run$subind[all_stop_run$groups == 'A_first'], 1)
    all_stop_run$ID[all_stop_run$groups == 'A_second']  = substrLeft(all_stop_run$subind[all_stop_run$groups == 'A_second'], 1)
    all_stop_run$ID[all_stop_run$groups == 'A_third']  = substrLeft(all_stop_run$subind[all_stop_run$groups == 'A_third'], 1)
    all_stop_run$ID[all_stop_run$groups == 'A_first2']  = substrLeft(all_stop_run$subind[all_stop_run$groups == 'A_first2'], 1)

    all_stop_run$ID2 = all_stop_run$ID
    all_stop_run$ID2[all_stop_run$ID2 == 1 | all_stop_run$ID2 == 2] = 1


    #SSRT
    int.ssrt1 = lme(SSRT ~ ID2, random = list(~1|subind, ~1|runnum), data = all_stop_run[all_stop_run$group == 'A_first' | all_stop_run$group == 'A_first2',])
    summary(int.ssrt1)
    
    int.ssrt2 = lme(SSRT ~ ID2, random = list(~1|subind, ~1|runnum), data = all_stop_run[all_stop_run$group == 'A_second',])
    summary(int.ssrt2)

    int.ssrt3 = lme(SSRT ~ ID2, random = list(~1|subind, ~1|runnum), data = all_stop_run[all_stop_run$group == 'A_third',])
    summary(int.ssrt3)

    #SSD    
    int.ssd4 = lme(ssd_mean ~ ID2, random = list(~1|subind, ~1|runnum), data = all_stop_run[all_stop_run$group == 'A_first'| all_stop_run$group == 'A_first2',])
    summary(int.ssd4)
    
    int.ssd5 = lme(ssd_mean ~ ID2, random = list(~1|subind, ~1|runnum), data = all_stop_run[all_stop_run$group == 'A_second',])
    summary(int.ssd5)

    int.ssd6 = lme(ssd_mean ~ ID2, random = list(~1|subind, ~1|runnum), data = all_stop_run[all_stop_run$group == 'A_third',])
    summary(int.ssd6)

    A_stop_sub$groups2 = A_stop_sub$groups
    A_stop_sub$groups2[A_stop_sub$groups2 == "A_first2"] = "A_first"



# STOP FIGURES

    # BY SESSSION
      # HOUSTON

        H_SSRT_box = ggplot(data = H_stop_sub,aes(x = groups,y = SSRT_me)) + geom_boxplot() + geom_point() + geom_line(aes(group = subind, color = subind, linetype = ID))+ xlab("Session") + 
                            ylab("Mean SSRT (s)") + ggtitle("Houston SSRT")+ guides(group = FALSE,color = FALSE)
	             ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/H_SSRT_box.png",wd),width=10,height=10)
      
   
        H_SSRT_bar = ggplot(data=H_stop_group, aes(x = groups, y = SSRT_mean, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                            geom_errorbar(aes(y=SSRT_mean, ymin=SSRT_mean-SSRT_se, ymax=SSRT_mean+SSRT_se), position = position_dodge(.9), width=.2) + ylab("Mean SSRT") + xlab("Session") + 
                            ggtitle("Houston SSRT") +
                            scale_fill_discrete(name = "groups", breaks = c("H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("H_control_first (%d)", H_stop_group$N[H_stop_group$groups == "H_control_first"]), 
                              sprintf("H_control_second (%d)", H_stop_group$N[H_stop_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", H_stop_group$N[H_stop_group$groups == "H_control_third"]), sprintf("H_first (%d)", H_stop_group$N[H_stop_group$groups == "H_first"]), 
                              sprintf("H_second (%d)",H_stop_group$N[H_stop_group$groups == "H_second"]), sprintf("H_third (%d)",H_stop_group$N[H_stop_group$groups == "H_third"])))
                     ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/H_SSRT_bar.png",wd),width=10,height=10)


        H_stopACC_box = ggplot(data = H_stop_sub,aes(x = groups,y = stop_acc_me)) + geom_boxplot() + geom_point() + geom_line(aes(group = subind, color = subind, linetype = ID))+ xlab("Session") + 
                             ylab("Mean Stop ACC (s)") + ggtitle("Houston Stop ACC")+ guides(group = FALSE,color = FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/H_stopACC_box.png",wd),width=10,height=10)

        H_stopACC_bar = ggplot(data=H_stop_group, aes(x = groups, y = stop_acc_mean, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                            geom_errorbar(aes(y=stop_acc_mean, ymin=stop_acc_mean-stop_acc_se, ymax=stop_acc_mean+stop_acc_se), position = position_dodge(.9), width=.2) + ylab("Mean Stop ACC") + xlab("Session") + 
                            ggtitle("Houston Stop ACC") +
                            scale_fill_discrete(name = "groups", breaks = c("H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("H_control_first (%d)", H_stop_group$N[H_stop_group$groups == "H_control_first"]), 
                              sprintf("H_control_second (%d)", H_stop_group$N[H_stop_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", H_stop_group$N[H_stop_group$groups == "H_control_third"]), sprintf("H_first (%d)", H_stop_group$N[H_stop_group$groups == "H_first"]), 
                              sprintf("H_second (%d)",H_stop_group$N[H_stop_group$groups == "H_second"]), sprintf("H_third (%d)",H_stop_group$N[H_stop_group$groups == "H_third"])))
                      ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/H_stopACC_bar.png",wd),width=10,height=10)


      # BOTH AUSTIN AND HOUSTON


        all_stop_fail = melt(all_stop_group[, c("groups","N","stop_fail_cor_mean","stop_fail_incor_mean")], id=c("groups", "N"))
        colnames(all_stop_fail)= c("groups", "N", "error_type", "proportion")
        all_stop_fail$se=0
        all_stop_fail$se[all_stop_fail$error_type=="stop_fail_cor_mean"]=all_stop_group$stop_fail_cor_se
        all_stop_fail$se[all_stop_fail$error_type=="stop_fail_incor_mean"]=all_stop_group$stop_fail_incor_se


        All_stop_fail_bar = ggplot(data = all_stop_fail, aes(x = error_type, y = proportion, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                                 geom_errorbar(aes(y = proportion, ymin = proportion-se, ymax = proportion+se), position = position_dodge(.9), width=.2) +
                                 xlab("Stop Fail Type") + ylab("Proportion Stops") + ggtitle("Austin and Houston Stops") + theme_bw() +
                                 scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third", "H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("A_control (%d)", all_go_group$N[all_go_group$groups == "A_control"]), 
                                    sprintf("A_first (%d)", all_go_group$N[all_go_group$groups == 'A_first']), sprintf('A_second (%d)',all_go_group$N[all_go_group$groups == 'A_second']), sprintf('A_third (%d)',all_go_group$N[all_go_group$groups == 'A_third']),
                                    sprintf("H_control_first (%d)", all_go_group$N[all_go_group$groups == "H_control_first"]), sprintf("H_control_second (%d)", all_go_group$N[all_go_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", all_go_group$N[all_go_group$groups == "H_control_third"]), 
                                    sprintf("H_first (%d)", all_go_group$N[all_go_group$groups == "H_first"]), sprintf("H_second (%d)",all_go_group$N[all_go_group$groups == "H_second"]), sprintf("H_third (%d)",all_go_group$N[all_go_group$groups == "H_third"])))                                     
                          ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/All_stop_fail_bar.png",wd),width=10,height=10)


        a1_h1_c_SSRT_box  = ggplot(data = a1_h1_c_stop_sub, aes(x = groups,y = SSRT_me)) + geom_boxplot(aes(fill = groups, alpha=.2)) + geom_point(aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,16)) + xlab("Session") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + ylab("Mean SSRT (s)") + guides(group = FALSE, alpha=FALSE, color = FALSE)
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/a1_h1_c_SSRT_box.pdf",wd),width=5,height=5)

        t.test(SSRT_me ~ groups, data = a1_h1_c_stop_sub[a1_h1_c_stop_sub$groups=="A_first" | a1_h1_c_stop_sub$groups=="A_first2",])
        t.test(SSRT_me ~ groups, data = a1_h1_c_stop_sub[a1_h1_c_stop_sub$groups=="A_first" | a1_h1_c_stop_sub$groups=="H2_first",])
        t.test(SSRT_me ~ groups, data = a1_h1_c_stop_sub[a1_h1_c_stop_sub$groups=="A_first" | a1_h1_c_stop_sub$groups=="A_control",])

        t.test(SSRT_me ~ groups, data = a1_h1_c_stop_sub[a1_h1_c_stop_sub$groups=="A_first2" | a1_h1_c_stop_sub$groups=="H2_first",])
        t.test(SSRT_me ~ groups, data = a1_h1_c_stop_sub[a1_h1_c_stop_sub$groups=="A_first2" | a1_h1_c_stop_sub$groups=="A_control",])

        t.test(SSRT_me ~ groups, data = a1_h1_c_stop_sub[a1_h1_c_stop_sub$groups=="H2_first" | a1_h1_c_stop_sub$groups=="A_control",])

   
        a1_h1_c_stopACC_box  = ggplot(data = a1_h1_c_stop_sub, aes(x = groups,y = stop_acc_me)) + geom_boxplot(aes(fill = groups, alpha=.2)) + geom_point(aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,16)) + xlab("Session") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + ylab("Mean Stop ACC") + guides(group = FALSE, alpha=FALSE, color = FALSE)
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/a1_h1_c_stopACC_box.pdf",wd),width=5,height=5)

        t.test(stop_acc_me ~ groups, data = a1_h1_c_stop_sub[a1_h1_c_stop_sub$groups=="A_first" | a1_h1_c_stop_sub$groups=="A_first2",])
        t.test(stop_acc_me ~ groups, data = a1_h1_c_stop_sub[a1_h1_c_stop_sub$groups=="A_first" | a1_h1_c_stop_sub$groups=="H2_first",])
        t.test(stop_acc_me ~ groups, data = a1_h1_c_stop_sub[a1_h1_c_stop_sub$groups=="A_first" | a1_h1_c_stop_sub$groups=="A_control",])

        t.test(stop_acc_me ~ groups, data = a1_h1_c_stop_sub[a1_h1_c_stop_sub$groups=="A_first2" | a1_h1_c_stop_sub$groups=="H2_first",])
        t.test(stop_acc_me ~ groups, data = a1_h1_c_stop_sub[a1_h1_c_stop_sub$groups=="A_first2" | a1_h1_c_stop_sub$groups=="A_control",])

        t.test(stop_acc_me ~ groups, data = a1_h1_c_stop_sub[a1_h1_c_stop_sub$groups=="H2_first" | a1_h1_c_stop_sub$groups=="A_control",])



    # BY INTERVENTION CODE

      # AUSTIN

        A_SSRT_ID_box = ggplot(data = A_stop_sub,aes(x = ID,y = SSRT_me)) + geom_boxplot(aes(fill = groups)) + geom_point() + xlab("Intervention Code") + ylab("Mean SSRT (s)") + ggtitle("Austin SSRT by Intervention")+ 
                               guides(group = FALSE,color = FALSE)
	                ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSRT_ID_box.png",wd),width=10,height=10)
   

        A_SSRT_ID_bar = ggplot(data = A_stop_ID, aes(x = ID, y = SSRT_mean, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                               geom_errorbar(aes(y = SSRT_mean, ymin = SSRT_mean-SSRT_se, ymax = SSRT_mean+SSRT_se), position = position_dodge(.9), width=.2) + xlab("Intervention Code") + ylab("Mean SSRT") + 
                               ggtitle("Austin SSRT by Intervention") +
                               scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", A_stop_group$N[A_stop_group$groups == "A_control"]), sprintf("A_first (%d)",
			           A_stop_group$N[A_stop_group$groups == 'A_first']), sprintf('A_second (%d)',A_stop_group$N[A_stop_group$groups == 'A_second']), sprintf('A_third (%d)',A_stop_group$N[A_stop_group$groups == 'A_third'])))
                        ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSRT_ID_bar.png",wd),width=10,height=10)


        A_stopACC_ID_box = ggplot(data = A_stop_sub,aes(x = ID,y = stop_acc_me)) + geom_boxplot() + geom_point()+ xlab("Intervention Code") + ylab("Mean Stop ACC (s)") + ggtitle("Austin Stop ACC by Intervention")+ guides(group = FALSE,color = FALSE)
	                   ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_stopACC_ID_box.png",wd),width=10,height=10)


        A_stopACC_ID_bar = ggplot(data = A_stop_ID, aes(x = ID, y = stop_acc_mean, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                                  geom_errorbar(aes(y = stop_acc_mean, ymin = stop_acc_mean-stop_acc_se, ymax = stop_acc_mean+stop_acc_se), position = position_dodge(.9), width=.2) + xlab("Intervention Code") + ylab("Mean Stop ACC") + 
                                  ggtitle("Austin SSRT by Intervention") +
                                  scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", A_stop_group$N[A_stop_group$groups == "A_control"]), sprintf("A_first (%d)",
			            A_stop_group$N[A_stop_group$groups == 'A_first']), sprintf('A_second (%d)',A_stop_group$N[A_stop_group$groups == 'A_second']), sprintf('A_third (%d)',A_stop_group$N[A_stop_group$groups == 'A_third'])))
                        ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_stopACC_ID_bar.png",wd),width=10,height=10)



      # HOUSTON

        H_SSRT_ID_box = ggplot(data = H_stop_sub,aes(x = groups,y = SSRT_me)) + geom_boxplot() + geom_point() + geom_line(aes(group = subind, color = subind))+ xlab("Session") + 
                            ylab("Mean SSRT (s)") + ggtitle("Houston SSRT")+ guides(group = FALSE,color = FALSE)
	             ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/H_SSRT_ID_box.png",wd),width=10,height=10)
      
   
        H_SSRT_ID_bar = ggplot(data=H_stop_ID, aes(x = ID, y = SSRT_mean, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                            geom_errorbar(aes(y=SSRT_mean, ymin=SSRT_mean-SSRT_se, ymax=SSRT_mean+SSRT_se), position = position_dodge(.9), width=.2) + ylab("Mean SSRT") + xlab("Session") + 
                            ggtitle("Houston SSRT") +
                            scale_fill_discrete(name = "groups", breaks = c("H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("H_control_first (%d)", H_stop_group$N[H_stop_group$groups == "H_control_first"]), 
                              sprintf("H_control_second (%d)", H_stop_group$N[H_stop_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", H_stop_group$N[H_stop_group$groups == "H_control_third"]), sprintf("H_first (%d)", H_stop_group$N[H_stop_group$groups == "H_first"]), 
                              sprintf("H_second (%d)",H_stop_group$N[H_stop_group$groups == "H_second"]), sprintf("H_third (%d)",H_stop_group$N[H_stop_group$groups == "H_third"])))
                     ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/H_SSRT_ID_bar.png",wd),width=10,height=10)


        H_stopACC_ID_box = ggplot(data = H_stop_sub,aes(x = groups,y = stop_acc_me)) + geom_boxplot() + geom_point() + geom_line(aes(group = subind, color = subind))+ xlab("Session") + 
                             ylab("Mean Stop ACC (s)") + ggtitle("Houston Stop ACC")+ guides(group = FALSE,color = FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/H_stopACC_ID_box.png",wd),width=10,height=10)

        H_stopACC_ID_bar = ggplot(data=H_stop_group, aes(x = groups, y = stop_acc_mean, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                            geom_errorbar(aes(y=stop_acc_mean, ymin=stop_acc_mean-stop_acc_se, ymax=stop_acc_mean+stop_acc_se), position = position_dodge(.9), width=.2) + ylab("Mean Stop ACC") + xlab("Session") + 
                            ggtitle("Houston Stop ACC") +
                            scale_fill_discrete(name = "groups", breaks = c("H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("H_control_first (%d)", H_stop_group$N[H_stop_group$groups == "H_control_first"]), 
                              sprintf("H_control_second (%d)", H_stop_group$N[H_stop_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", H_stop_group$N[H_stop_group$groups == "H_control_third"]), sprintf("H_first (%d)", H_stop_group$N[H_stop_group$groups == "H_first"]), 
                              sprintf("H_second (%d)",H_stop_group$N[H_stop_group$groups == "H_second"]), sprintf("H_third (%d)",H_stop_group$N[H_stop_group$groups == "H_third"])))
                      ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/H_stopACC_ID_bar.png",wd),width=10,height=10)


      # BOTH AUSTIN AND HOUSTON


        All_SSRT_ID_box = ggplot(data = all_stop_sub,aes(x = ID,y = SSRT_me)) + geom_boxplot() + geom_point() + xlab("Intervention Code") + ylab("Mean SSRT (s)") + ggtitle("Austin and Houston SSRT by Intervention")+ guides(group = FALSE,color = FALSE)
	                  ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_SSRT_ID_box.png",wd),width=10,height=10)
   

        All_SSRT_ID_bar = ggplot(data = all_stop_ID, aes(x = ID, y = SSRT_mean, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                                 geom_errorbar(aes(y = SSRT_mean, ymin = SSRT_mean-SSRT_se, ymax = SSRT_mean+SSRT_se), position = position_dodge(.9), width=.2) + xlab("Intervention Code") + ylab("Mean SSRT") + 
                                 ggtitle("Austin and Houston SSRT by Intervention") +
                                 scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third", "H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("A_control (%d)", all_go_group$N[all_go_group$groups == "A_control"]), 
                                    sprintf("A_first (%d)", all_go_group$N[all_go_group$groups == 'A_first']), sprintf('A_second (%d)',all_go_group$N[all_go_group$groups == 'A_second']), sprintf('A_third (%d)',all_go_group$N[all_go_group$groups == 'A_third']),
                                    sprintf("H_control_first (%d)", all_go_group$N[all_go_group$groups == "H_control_first"]), sprintf("H_control_second (%d)", all_go_group$N[all_go_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", all_go_group$N[all_go_group$groups == "H_control_third"]), 
                                    sprintf("H_first (%d)", all_go_group$N[all_go_group$groups == "H_first"]), sprintf("H_second (%d)",all_go_group$N[all_go_group$groups == "H_second"]), sprintf("H_third (%d)",all_go_group$N[all_go_group$groups == "H_third"])))                                 
                          ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_SSRT_ID_bar.png",wd),width=10,height=10)


        All_stopACC_ID_box = ggplot(data = all_stop_sub,aes(x = ID,y = stop_acc_me)) + geom_boxplot() + geom_point() + xlab("Intervention Code") + ylab("Mean Stop ACC (s)") + ggtitle("Austin and Houston Stop ACC by Intervention")+ 
                                    guides(group = FALSE,color = FALSE)
	                     ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/All_stopACC_ID_box.png",wd),width=10,height=10)


        All_stopACC_ID_bar = ggplot(data = all_stop_ID, aes(x = ID, y = stop_acc_mean, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                                    geom_errorbar(aes(y = stop_acc_mean, ymin = stop_acc_mean-stop_acc_se, ymax = stop_acc_mean+stop_acc_se), position = position_dodge(.9), width=.2) + xlab("Intervention Code") + ylab("Mean Stop ACC") + 
                                    ggtitle("Austin and Houston Stop ACC by Intervention") +
                                 scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third", "H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("A_control (%d)", all_go_group$N[all_go_group$groups == "A_control"]), 
                                    sprintf("A_first (%d)", all_go_group$N[all_go_group$groups == 'A_first']), sprintf('A_second (%d)',all_go_group$N[all_go_group$groups == 'A_second']), sprintf('A_third (%d)',all_go_group$N[all_go_group$groups == 'A_third']),
                                    sprintf("H_control_first (%d)", all_go_group$N[all_go_group$groups == "H_control_first"]), sprintf("H_control_second (%d)", all_go_group$N[all_go_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", all_go_group$N[all_go_group$groups == "H_control_third"]), 
                                    sprintf("H_first (%d)", all_go_group$N[all_go_group$groups == "H_first"]), sprintf("H_second (%d)",all_go_group$N[all_go_group$groups == "H_second"]), sprintf("H_third (%d)",all_go_group$N[all_go_group$groups == "H_third"])))                                 
                            ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/All_stopACC_ID_bar.png",wd),width=10,height=10)







# END