# Authors: Leo Olmedo (First); Mary Abbe Roe (Dec.2013 - Feb. 2014)
# This script reads in SST behavioral data and calculates RT and ACC for all groups
# Graphs are located in SST_r_graphs.R


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

  group=c("ldrc_[0-9]_[0-9][0-9][0-9]", "ldrc_*second", "ldrc_*third", "ldrc2_[0-9]_[0-9][0-9][0-9]","ldrc2_*second*", "PHILIPS/H_LD*_[0-9]", "H_LD*_[0-9]_second", "H_LD*_[0-9]_third", "LDFHO*_[0-9]", "LDFHO*_second", "ldrc_c_[0-9][0-9][0-9]", "PHILIPS/H_LD*[0-9][0-9][0-9][0-9]", "H_LD*_c_second", "H_LD*_c_third")
  chars=c(10,17,16,11,18,13,20,19,12,19,10,11,20,19)

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
    dat_all_6$group = "H_first"
    dat_all_7$group = "H_second"
    dat_all_8$group = "H_third"
    dat_all_9$group = "H2_first"
    dat_all_10$group = "H2_second"
    dat_all_11$group = "A_control"
    dat_all_12$group = "H_control_first"
    dat_all_13$group = "H_control_second"
    dat_all_14$group = "H_control_third"

    dat = rbind(dat_all_1,dat_all_2, dat_all_3, dat_all_4, dat_all_5, dat_all_6, dat_all_7, dat_all_8, dat_all_9, dat_all_10, dat_all_11, dat_all_12, dat_all_13, dat_all_14)
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
	group="H_first"

        } else {

	if (k==7){
	dat_all=dat_all_7
	group="H_second"

	} else {

	if (k==8){
	dat_all=dat_all_8
	group="H_third"

	} else {

	if (k==9){
	dat_all=dat_all_9
	group="H2_first"

	} else {

	if (k==10){
	dat_all=dat_all_10
	group="H2_second"

	} else {

	if (k==11){
	dat_all=dat_all_11
	group="A_control"
	
	} else {

	if (k==12){
	dat_all=dat_all_12
	group="H_control_first"

	} else {

	if (k==13){
	dat_all=dat_all_13
	group="H_control_second"

        } else {

	if (k==14){
	dat_all=dat_all_14
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

  # GO CORRECT RT and ACC

    # EACH RUN FOR EACH SUB (# lines are cutoffs)
 
      go_run=ddply(dat_all, .(subind, runnum, group), summarise, N=length(subind), go_rt_med=median(RT[correct==1], na.rm=TRUE), go_rt_sd=sd(RT[correct==1], na.rm=TRUE), go_acc=length(correct[correct==1])/length(cond[cond==1|cond==0]), go_error=length(correct[correct==0])/length(cond[cond==1|cond==0]))
      #go_run$go_rt_med[go_run$go_acc < .60 | go_run$go_error > .1] = NA
      #go_run$go_acc[go_run$go_acc < .60 | go_run$go_error > .1] = NA 
      assign(paste("go_run",k,sep="_"),go_run)

    # ACROSS ALL RUNS FOR EACH SUB (# lines are ACC cutoffs) - useful for SSRT calculation
   
      go_sub=ddply(go_run, .(subind, group), summarise, N=length(subind), go_rt_mean=mean(go_rt_med, na.rm=TRUE), go_rt_se=sd(go_rt_med)/sqrt(N), go_acc_mean=mean(go_acc, na.rm=TRUE), go_error_mean=mean(go_error, na.rm=TRUE))
      assign(paste("go_sub",k,sep="_"),go_sub)

    # ACROSS SUBS FOR EACH RUN

      go_run_all=ddply(go_run, .(runnum, group), summarise, N=length(runnum), go_rt_me=mean(go_rt_med, na.rm=TRUE), go_rt_se=sd(go_rt_med, na.rm=TRUE)/sqrt(N), go_acc_me=mean(go_acc, na.rm=TRUE), go_error_me=mean(go_error, na.rm=TRUE))
      assign(paste("go_run_all",k,sep="_"),go_run_all)   


} # END GO LOOP





# STOP LOOP
group=c("ldrc_[0-9]_[0-9][0-9][0-9]", "ldrc_*second", "ldrc_*third", "ldrc2_[0-9]_[0-9][0-9][0-9]","ldrc2_*second", "PHILIPS/H_LD*_[0-9]", "H_LD*_[0-9]_second", "H_LD*_[0-9]_third", "LDFHO*_[0-9]", "LDFHO*_second", "ldrc_c_[0-9][0-9][0-9]", "PHILIPS/H_LD*[0-9][0-9][0-9][0-9]", "H_LD*_c_second", "H_LD*_c_third")
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
	group="H_first"

        } else {

	if (k==7){
	dat_all=dat_all_7
	group="H_second"

	} else {

	if (k==8){
	dat_all=dat_all_8
	group="H_third"

	} else {

	if (k==9){
	dat_all=dat_all_9
	group="H2_first"

	} else {

	if (k==10){
	dat_all=dat_all_10
	group="H2_second"

	} else {

	if (k==11){
	dat_all=dat_all_11
	group="A_control"
	
	} else {

	if (k==12){
	dat_all=dat_all_12
	group="H_control_first"

	} else {

	if (k==13){
	dat_all=dat_all_13
	group="H_control_second"

	} else {

	if (k==14){
	dat_all=dat_all_14
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
        RT_asc_run=data.frame(subind=dat_all$subind[dat_all$correct==1], runnum=dat_all$runnum[dat_all$correct==1], TrialNum=dat_all$TrialNum[dat_all$correct==1], correct=dat_all$correct[dat_all$correct==1], 
                              RT=dat_all$RT[dat_all$correct==1])
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

    stop_run_all=ddply(stop_run, .(runnum, group), summarise, N=length(runnum), stop_acc_me=mean(stop_acc, na.rm=TRUE), stop_acc_se=sd(stop_acc, na.rm=TRUE)/sqrt(N), stop_error_me=mean(stop_error, na.rm=TRUE), stop_error_se=sd(stop_error, na.rm=TRUE)/sqrt(N), ssd_me=mean(ssd_mean, na.rm=TRUE), ssd_se=sd(ssd_mean)/sqrt(N), quantileRT_me=mean(quantileRT, na.rm=TRUE), SSRT_me=mean(SSRT, na.rm=TRUE), SSRT_se=sd(SSRT)/sqrt(N))
    assign(paste("stop_run_all",k,sep="_"),stop_run_all)   


} # END STOP LOOP

    


# ALL GROUP DFs *** ONLY RUN ON FULL DATA SET, DO NOT RUN ON DATA THAT EXCLUDES SUBS FOR BEHAV PERFORMANCE ***

  # GO

    all_go_run=rbind(go_run_1, go_run_2, go_run_3, go_run_4, go_run_5, go_run_6, go_run_7, go_run_8, go_run_9, go_run_10, go_run_11, go_run_12)
    write.csv(all_go_run, file=sprintf("%s/data_frames/SST/GO/go_all_subs_groups_by_run.csv", wd), na="NA", row.names=FALSE)

    all_go_sub=rbind(go_sub_1, go_sub_2, go_sub_3, go_sub_4, go_sub_5, go_sub_6, go_sub_7, go_sub_8, go_sub_9, go_sub_10, go_sub_11,go_sub_12, go_sub_13, go_sub_14)
    write.csv(all_go_sub, file=sprintf("%s/data_frames/SST/GO/go_all_subs_groups_by_sub_m.csv", wd), na="NA", row.names=FALSE)

    go_run_allsub=rbind(go_run_all_1, go_run_all_2, go_run_all_3, go_run_all_4, go_run_all_5, go_run_all_6, go_run_all_7, go_run_all_8, go_run_all_9, go_run_all_10, go_run_all_11, go_run_all_12)
    write.csv(go_run_allsub, file=sprintf("%s/data_frames/SST/GO/go_runnum_across_groups.csv", wd), na="NA", row.names=FALSE)

  # STOP

    all_stop_run=rbind(stop_run_1, stop_run_2, stop_run_3, stop_run_4, stop_run_5, stop_run_6, stop_run_7, stop_run_8, stop_run_9, stop_run_10, stop_run_11, stop_run_12)
    write.csv(all_stop_run, file=sprintf("%s/data_frames/SST/STOP/stop_all_subs_groups_by_run.csv", wd), na="NA", row.names=FALSE)

    all_stop_sub=rbind(stop_sub_1, stop_sub_2, stop_sub_3, stop_sub_4, stop_sub_5, stop_sub_6, stop_sub_7, stop_sub_8, stop_sub_9, stop_sub_10, stop_sub_11, stop_sub_12, stop_sub_13, stop_sub_14)
    write.csv(all_stop_sub, file=sprintf("%s/data_frames/SST/STOP/stop_all_subs_groups_by_sub_m.csv", wd), na="NA", row.names=FALSE)

    stop_run_allsub=rbind(stop_run_all_1, stop_run_all_2, stop_run_all_3, stop_run_all_4, stop_run_all_5, stop_run_all_6, stop_run_all_7, stop_run_all_8, stop_run_all_9, stop_run_all_10, stop_run_all_11, stop_run_all_12)
    write.csv(stop_run_allsub, file=sprintf("%s/data_frames/SST/STOP/stop_runnum_across_groups.csv", wd), na="NA", row.names=FALSE)




----------------

# GO DATAFRAMES (FOR STATS AND GRAPHS)

  # AUSTIN SUBS
    A_go_sub = rbind(go_sub_1, go_sub_2, go_sub_3, go_sub_4, go_sub_5, go_sub_11)
    A_go_sub$group2 = A_go_sub$group
    A_go_sub$group2[A_go_sub$group2=="A_first"] = "first"
    A_go_sub$group2[A_go_sub$group2=="A2_first"] = "first"
    A_go_sub$group2[A_go_sub$group2=="A_second"] = "second"
    A_go_sub$group2[A_go_sub$group2=="A2_second"] = "second"
    A_go_sub$group2[A_go_sub$group2=="A_third"] = "third"
    A_go_sub$group2[A_go_sub$group2=="A_control"] = "control"
    A_go_sub$subind[A_go_sub$group=="A_first" | A_go_sub$group=="A_second"] = substrRight(A_go_sub$subind[A_go_sub$group=="A_first"],5)
    A_go_sub$subind[A_go_sub$group=="A_second"] = substrLeft(A_go_sub$subind[A_go_sub$group=="A_second"],10)
    A_go_sub$subind[A_go_sub$group=="A_second"] = substrRight(A_go_sub$subind[A_go_sub$group=="A_second"],5)
    A_go_sub$subind[A_go_sub$group=="A_third"] = substrLeft(A_go_sub$subind[A_go_sub$group=="A_third"],10)  
    A_go_sub$subind[A_go_sub$group=="A_third"] = substrRight(A_go_sub$subind[A_go_sub$group=="A_third"],5)
    A_go_sub$subind[A_go_sub$group=="A2_first"] = substrRight(A_go_sub$subind[A_go_sub$group=="A2_first"],5)
    A_go_sub$subind[A_go_sub$group=="A2_second"] = substrLeft(A_go_sub$subind[A_go_sub$group=="A2_second"],11)
    A_go_sub$subind[A_go_sub$group=="A2_second"] = substrRight(A_go_sub$subind[A_go_sub$group=="A2_second"],5)
    A_go_sub$ID = substrLeft(A_go_sub$subind, 1)
    A_go_sub$ID2 = "c"
    A_go_sub$ID2[A_go_sub$ID == "0"] = "0"
    A_go_sub$ID2[A_go_sub$ID == "1" | A_go_sub$ID == "2"] = "1"
    A_go_sub$INT=c("c")
    A_go_sub$INT[A_go_sub$ID == 0 |A_go_sub$ID == 1 | A_go_sub$ID == 2] = "i"
    A_go_sub$city = "Austin"
    A_go_sub = A_go_sub[A_go_sub$group2!="third",]
    A_go_sub$rep = 'u'
    A_go_sub$rep[(duplicated(A_go_sub['subind'])) | (duplicated(A_go_sub['subind'], fromLast = TRUE))] = 'r'


    as.character(A_go_sub$subind)

    A_go_sub = within (A_go_sub, { 
               group = factor(group)
               ID = factor(ID)
               city = factor(city)
               })

    A_go_group = ddply(A_go_sub, .(group), summarise, N=length(group), go_rt_me=(mean(go_rt_mean, na.rm=TRUE)), go_rt_se=sd(go_rt_mean, na.rm=TRUE)/sqrt(N), go_acc_me=(mean(go_acc_mean, na.rm=TRUE)), go_acc_se=sd(go_acc_mean, na.rm=TRUE)/sqrt(N))
    A_go_ID = ddply(A_go_sub, .(ID, group), summarise, N=length(ID), go_rt_me=(mean(go_rt_mean, na.rm=TRUE)), go_rt_se=sd(go_rt_mean, na.rm=TRUE)/sqrt(N), go_acc_me=(mean(go_acc_mean, na.rm=TRUE)), go_acc_se=sd(go_acc_mean, na.rm=TRUE)/sqrt(N))
    A_go_ID2 = ddply(A_go_sub, .(ID2, group), summarise, N=length(ID), go_rt_me=(mean(go_rt_mean, na.rm=TRUE)), go_rt_se=sd(go_rt_mean, na.rm=TRUE)/sqrt(N), go_acc_me=(mean(go_acc_mean, na.rm=TRUE)), go_acc_se=sd(go_acc_mean, na.rm=TRUE)/sqrt(N))



  # HOUSTON SUBS
    go_sub_14 = go_sub_14[go_sub_14$subind!="H_LDIMG8960_c_third",]    

    H_go_sub=rbind (go_sub_7, go_sub_8, go_sub_9, go_sub_10, go_sub_13, go_sub_14)
    H_go_sub$group2 = H_go_sub$group
    H_go_sub$group2[H_go_sub$group2=="H2_first"] = "first"
    H_go_sub$group2[H_go_sub$group2=="H_second" | H_go_sub$group2=="H2_second"] = "second"
    H_go_sub$group2[H_go_sub$group2=="H_third"] = "third"
    H_go_sub$group2[H_go_sub$group2=="H_control_second" | H_go_sub$group2=="H_control_third"] = "control"
    H_go_sub$subind[H_go_sub$group=="H_second" | H_go_sub$group=="H_control_second" | H_go_sub$group=="H_third" | H_go_sub$group=="H_control_second" | H_go_sub$group=="H_control_third"]=substrLeft(H_go_sub$subind[H_go_sub$group=="H_second" | H_go_sub$group=="H_control_second"| H_go_sub$group=="H_third" | H_go_sub$group=="H_control_second" | H_go_sub$group=="H_control_third"],13)
    H_go_sub$subind[H_go_sub$group=="H_second" | H_go_sub$group=="H_control_second"| H_go_sub$group=="H_third" | H_go_sub$group=="H_control_second" | H_go_sub$group=="H_control_third"]=substrRight(H_go_sub$subind[H_go_sub$group=="H_second" | H_go_sub$group=="H_control_second"| H_go_sub$group=="H_third" | H_go_sub$group=="H_control_second"| H_go_sub$group=="H_control_third" ],6) 
    H_go_sub$subind[H_go_sub$group=="H2_first"]=substrRight(H_go_sub$subind[H_go_sub$group=="H2_first"],7) 
    H_go_sub$subind[H_go_sub$group=="H2_second"]=substrLeft(H_go_sub$subind[H_go_sub$group=="H2_second"],12)
    H_go_sub$subind[H_go_sub$group=="H2_second"]=substrRight(H_go_sub$subind[H_go_sub$group=="H2_second"],7) 
    H_go_sub$ID=c("c")
    H_go_sub$ID=substrRight(H_go_sub$subind,1)
    H_go_sub$ID2 = "c"
    H_go_sub$ID2[H_go_sub$ID == "0"] = "0"
    H_go_sub$ID2[H_go_sub$ID == "1" | H_go_sub$ID == "2"] = "1" 
    H_go_sub$INT=c("c")
    H_go_sub$INT[H_go_sub$ID == 0 | H_go_sub$ID == 1 | H_go_sub$ID == 2] = "i"
    H_go_sub$city = "Houston"
    H_go_sub = H_go_sub[H_go_sub$group2!="third",]
    H_go_sub$rep = 'u'
    H_go_sub$rep[(duplicated(H_go_sub['subind'])) | (duplicated(H_go_sub['subind'], fromLast = TRUE))] = 'r'



    H_go_sub = within (H_go_sub, { 
               subind = factor(subind)
               group = factor(group)
               ID = factor(ID)
               city = factor(city)
               })

    H_go_group = ddply(H_go_sub, .(group), summarise, N=length(group), go_rt_me=(mean(go_rt_mean, na.rm=TRUE)), go_rt_se=sd(go_rt_mean, na.rm=TRUE)/sqrt(N), go_acc_me=(mean(go_acc_mean, na.rm=TRUE)), go_acc_se=sd(go_acc_mean, na.rm=TRUE)/sqrt(N))
    H_go_ID = ddply(H_go_sub, .(ID, group), summarise, N=length(ID), go_rt_me=(mean(go_rt_mean, na.rm=TRUE)), go_rt_se=sd(go_rt_mean, na.rm=TRUE)/sqrt(N), go_acc_me=(mean(go_acc_mean, na.rm=TRUE)), go_acc_se=sd(go_acc_mean, na.rm=TRUE)/sqrt(N))
    H_go_ID2 = ddply(H_go_sub, .(ID2, group), summarise, N=length(ID), go_rt_me=(mean(go_rt_mean, na.rm=TRUE)), go_rt_se=sd(go_rt_mean, na.rm=TRUE)/sqrt(N), go_acc_me=(mean(go_acc_mean, na.rm=TRUE)), go_acc_se=sd(go_acc_mean, na.rm=TRUE)/sqrt(N))


  # AUSTIN AND HOUSTON 
    all_go_sub = rbind(A_go_sub, H_go_sub)

    all_go_sub$dup1 = duplicated(all_go_sub$subind)
    all_go_sub$dup2 = duplicated(all_go_sub$subind, fromLast = TRUE)
    
    all_go_group = ddply(all_go_sub, .(group), summarise, N=length(group), go_rt_me=(mean(go_rt_mean, na.rm=TRUE)), go_rt_se=sd(go_rt_mean, na.rm=TRUE)/sqrt(N), go_acc_me=(mean(go_acc_mean, na.rm=TRUE)), go_acc_se=sd(go_acc_mean, na.rm=TRUE)/sqrt(N))
    all_go_ID = ddply(all_go_sub, .(ID, group), summarise, N=length(ID), go_rt_me=(mean(go_rt_mean, na.rm=TRUE)), go_rt_se=sd(go_rt_mean, na.rm=TRUE)/sqrt(N), go_acc_me=(mean(go_acc_mean, na.rm=TRUE)), go_acc_se=sd(go_acc_mean, na.rm=TRUE)/sqrt(N))


    a1_h1_c_go_sub = all_go_sub[all_go_sub$groups == "A_first" | all_go_sub$groups == "A_first2" | all_go_sub$groups == "H2_first" | all_go_sub$groups == "A_control" , ]
    a1_h1_c_go_group = ddply(a1_h1_c_go_sub, .(groups), summarise, N=length(groups), go_rt_me=(mean(go_rt_mean, na.rm=TRUE)), go_rt_se=sd(go_rt_mean, na.rm=TRUE)/sqrt(N), go_acc_me=(mean(go_acc_mean, na.rm=TRUE)), go_acc_se=sd(go_acc_mean, na.rm=TRUE)/sqrt(N))
      write.csv(a1_h1_c_go_group, file=sprintf("%s/data_frames/SST/GO/go_all_subs_groups_by_run.csv", wd), na="NA", row.names=FALSE)    
    a1_h1_c_go_ID = ddply(a1_h1_c_go_sub, .(ID, groups), summarise, N=length(ID), go_rt_me=(mean(go_rt_mean, na.rm=TRUE)), go_rt_se=sd(go_rt_mean, na.rm=TRUE)/sqrt(N), go_acc_me=(mean(go_acc_mean, na.rm=TRUE)), go_acc_se=sd(go_acc_mean, na.rm=TRUE)/sqrt(N))
    a1_h1_c_go_ID2 = ddply(a1_h1_c_go_sub, .(ID2, groups), summarise, N=length(ID), go_rt_me=(mean(go_rt_mean, na.rm=TRUE)), go_rt_se=sd(go_rt_mean, na.rm=TRUE)/sqrt(N), go_acc_me=(mean(go_acc_mean, na.rm=TRUE)), go_acc_se=sd(go_acc_mean, na.rm=TRUE)/sqrt(N))


# GO STATS

  # GO STATS for NIH AUG15

     # AUSTIN SUBS

     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/go_stats_austin_aug15.txt")

     rt_s1vc_a = t.test(go_rt_mean ~ group3, data = A_go_sub[A_go_sub$group3 == "first" | A_go_sub$group3 == "control",])
     print("Mean Go RT - S1 vs. Controls")
     print(rt_s1vc_a)

     acc_s1vc_a = t.test(go_acc_mean ~ group3, data = A_go_sub[A_go_sub$group3 == "first" | A_go_sub$group3 == "control",])
     print("Mean Go Accuracy - S1 vs. Controls")
     print(acc_s1vc_a)

     rt_s2vc_a = t.test(go_rt_mean ~ group3, data = A_go_sub[A_go_sub$group3 == "second" | A_go_sub$group3 == "control",])
     print("Mean Go RT - S2 vs. Controls")
     print(rt_s2vc_a)

     acc_s2vc_a = t.test(go_acc_mean ~ group3, data = A_go_sub[A_go_sub$group3 == "second" | A_go_sub$group3 == "control",])
     print("Mean Go Accuracy - S2 vs. Controls")
     print(acc_s2vc_a)

     rt_s1vs2_a = t.test(go_rt_mean ~ group3, data = A_go_sub[A_go_sub$group3 == "first" | A_go_sub$group3 == "second",])
     print("Mean Go RT - S1 vs. s2")
     print(rt_s1vs2_a)

     acc_s1vs2_a = t.test(go_acc_mean ~ group3, data = A_go_sub[A_go_sub$group3 == "first" | A_go_sub$group3 == "second",])
     print("Mean Go Accuracy - S1 vs. S2")
     print(acc_s1vs2_a)

     sink()

     # AUSTIN REPEAT SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/go_stats_austin_repeat_aug15.txt")

     print(all_go_sub[all_go_sub$rep == "r" & all_go_sub$city == "Austin",])

     rt_repeat_a = t.test(go_rt_mean ~ group2, data = all_go_sub[all_go_sub$rep == "r" & all_go_sub$city == "Austin",], paired=T)
     print("Mean Go RT - S1 vs. s2 Repeats")
     print(rt_repeat_a)

     acc_repeat_a = t.test(go_acc_mean ~ group2, data = all_go_sub[all_go_sub$rep == "r" & all_go_sub$city == "Austin",], paired=T)
     print("Mean Go Accuracy - S1 vs. S2 Repeats")
     print(acc_repeat_a)

     sink()


     # AUSTIN UNIQUE SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/go_stats_austin_uniq_aug15.txt")

     print(all_go_sub[all_go_sub$group2!="control" &  all_go_sub$rep == "u" & all_go_sub$city == "Austin",])

     rt_uniq_a = t.test(go_rt_mean ~ group2, data = all_go_sub[all_go_sub$group2!="control" & all_go_sub$rep == "u" & all_go_sub$city == "Austin",])
     print("Mean Go RT - S1 vs. s2 Unique")
     print(rt_uniq_a)

     acc_uniq_a = t.test(go_acc_mean ~ group2, data = all_go_sub[all_go_sub$group2!="control" & all_go_sub$rep == "u" & all_go_sub$city == "Austin",])
     print("Mean Go Accuracy - S1 vs. S2 Unique")
     print(acc_uniq_a)

     sink()


     # AUSTIN AND HOUSTON SUBS
   
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/go_stats_ah_aug15.txt")

     rt_s1vc_ah = t.test(go_rt_mean ~ group3, data = all_go_sub[all_go_sub$group3 == "first" | all_go_sub$group3 == "control",])
     print("Mean Go RT - S1 vs. Controls")
     print(rt_s1vc_ah)

     acc_s1vc_ah = t.test(go_acc_mean ~ group3, data = all_go_sub[all_go_sub$group3 == "first" | all_go_sub$group3 == "control",])
     print("Mean Go Accuracy - S1 vs. Controls")
     print(acc_s1vc_ah)

     rt_s2vc_ah = t.test(go_rt_mean ~ group3, data = all_go_sub[all_go_sub$group3 == "second" | all_go_sub$group3 == "control",])
     print("Mean Go RT - S2 vs. Controls")
     print(rt_s2vc_ah)

     acc_s2vc_ah = t.test(go_acc_mean ~ group3, data = all_go_sub[all_go_sub$group3 == "second" | all_go_sub$group3 == "control",])
     print("Mean Go Accuracy - S2 vs. Controls")
     print(acc_s2vc_ah)

     rt_s1vs2_ah = t.test(go_rt_mean ~ group3, data = all_go_sub[all_go_sub$group3 == "first" | all_go_sub$group3 == "second",])
     print("Mean Go RT - S1 vs. s2")
     print(rt_s1vs2_ah)

     acc_s1vs2_ah = t.test(go_acc_mean ~ group3, data = all_go_sub[all_go_sub$group3 == "first" | all_go_sub$group3 == "second",])
     print("Mean Go Accuracy - S1 vs. S2")
     print(acc_s1vs2_ah)

     sink()

     # AUSTIN/HOUSTON REPEAT SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/go_stats_ah_repeat_aug15.txt")

     print(all_go_sub[all_go_sub$rep == "r",])

     rt_repeat_ah = t.test(go_rt_mean ~ group2, data = all_go_sub[all_go_sub$rep == "r",], paired=T)
     print("Mean Go RT - S1 vs. s2 Repeats")
     print(rt_repeat_ah)

     acc_repeat_ah = t.test(go_acc_mean ~ group2, data = all_go_sub[all_go_sub$rep == "r",], paired=T)
     print("Mean Go Accuracy - S1 vs. S2 Repeats")
     print(acc_repeat_ah)

     sink()


     # AUSTIN/HOUSTON UNIQUE SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/go_stats_ah_uniq_aug15.txt")

     print(all_go_sub[all_go_sub$group2!="control" & all_go_sub$rep == "u",])

     rt_uniq_ah = t.test(go_rt_mean ~ group2, data = all_go_sub[all_go_sub$group2!="control" & all_go_sub$rep == "u",])
     print("Mean Go RT - S1 vs. s2 Unique")
     print(rt_uniq_ah)

     acc_uniq_ah = t.test(go_acc_mean ~ group2, data = all_go_sub[all_go_sub$group2!="control" & all_go_sub$rep == "u",])
     print("Mean Go Accuracy - S1 vs. S2 Unique")
     print(acc_uniq_ah)

     sink()






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


    A_go_sub$groups2 = A_go_sub$groups
    A_go_sub$groups2[A_go_sub$groups == "A_first2"] = "A_first"


  # AUSTIN VS. HOUSTON FIRST CONTROLS
    # ACC
      control2.goACC = lme(go_acc_mean ~ city, random = list(~1 | subind, ~1 | city), data = all_go_sub[all_go_sub$groups == "A_control" | all_go_sub$groups == "H_control_first", ], method = "ML")
      summary(control2.goACC)$tTable
       
    # RT
      control2.goRT = lme(go_rt_mean ~ city -1, random = list(~1 | subind, ~1 | city), data = all_go_sub[all_go_sub$groups == "A_control" | all_go_sub$groups == "H_control_first", ], method = "ML")
      summary(control2.goRT)$tTable


  # AUSTIN FIRST VS. AUSTIN CONTROLS
    # ACC   
      mod2.goACC = lme(go_acc_mean ~ groups, random = list(~1 | subind), data = A_go_sub[A_go_sub$groups2 == "A_first" | A_go_sub$groups2 == "A_control", ], method = "ML")
      summary(mod2.goACC)$tTable

      t.test(go_acc_mean ~ groups2, data = A_go_sub[A_go_sub$groups2 == "A_first" | A_go_sub$groups2 == "A_control", ])

    # RT
      mod2.goRT = lme(go_rt_mean ~ groups, random = list(~1 | subind), data = A_go_sub[A_go_sub$groups == "A_first" | A_go_sub$groups == "A_control", ], method = "ML")
      summary(mod2.goRT)$tTable

      t.test(go_rt_mean ~ groups2, data = A_go_sub[A_go_sub$groups2 == "A_first" | A_go_sub$groups2 == "A_control", ])


  # AUSTIN FIRST VS. AUSTIN & HOUSTON FIRST CONTROLS
    # ACC
      int.cont2.goACC = lme(go_acc_mean ~ INT, random = list(~1 | subind), data = all_go_sub[all_go_sub$groups == "A_first" | all_go_sub$groups == "A_control"| all_go_sub$groups == "H_control_first", ], method = "ML")
      summary(int.cont2.goACC)$tTable

    # RT
      int.cont4.goRT = lme(go_rt_mean ~ INT, random = list(~1 | subind), data = all_go_sub[all_go_sub$groups == "A_first" | all_go_sub$groups == "A_control" | all_go_sub$groups == "H_control_first", ], method = "ML")
      summary(int.cont4.goRT)$tTable


  # AUSTIN SECOND VS. AUSTIN CONTROLS
    # ACC
      mod4.goACC = lme(go_acc_mean ~ groups, random = list(~1 | subind), data = A_go_sub[A_go_sub$groups == "A_second" | A_go_sub$groups == "A_control", ], method = "ML")
      summary(mod4.goACC)$tTable

      t.test(go_acc_mean ~ groups, data =  A_go_sub[A_go_sub$groups == "A_second" | A_go_sub$groups == "A_control", ])

    # RT
      mod4.goRT = lme(go_rt_mean ~ groups, random = list(~1 | subind), data = A_go_sub[A_go_sub$groups == "A_second" | A_go_sub$groups == "A_control", ], method = "ML")
      summary(mod4.goRT)$tTable

      t.test(go_rt_mean ~ groups, data =  A_go_sub[A_go_sub$groups == "A_second" | A_go_sub$groups == "A_control", ])

  # AUSTIN SECOND VS. AUSTIN AND HOUSTON SECOND CONTROLS
    # ACC
      mod6.goACC = lme(go_acc_mean ~ groups, random = list(~1 | subind), data = A_go_sub[A_go_sub$groups == "A_second" | A_go_sub$groups == "A_control", ], method = "ML")
      summary(mod6.goACC)$tTable

    # RT
      mod4.goRT = lme(go_rt_mean ~ groups, random = list(~1 | subind), data = A_go_sub[A_go_sub$groups == "A_second" | A_go_sub$groups == "A_control", ], method = "ML")
      summary(mod4.goRT)$tTable


  # AUSTIN THIRD VS. AUSTIN CONTROLS
    # ACC
      mod4.goACC = lme(go_acc_mean ~ groups, random = list(~1 | subind), data = A_go_sub[A_go_sub$groups == "A_third" | A_go_sub$groups == "A_control", ], method = "ML")
      summary(mod4.goACC)$tTable

    # RT
      mod4.goRT = lme(go_rt_mean ~ groups, random = list(~1 | subind), data = A_go_sub[A_go_sub$groups == "A_third" | A_go_sub$groups == "A_control", ], method = "ML")
      summary(mod4.goRT)$tTable


  # AUSTIN FIRST VS. SECOND

    A_go_int = A_go_sub[(A_go_sub$groups2=="A_first" | A_go_sub$groups2=="A_second" | A_go_sub$groups2=="A_third"), ]

    A_go_int_fs = A_go_int[(A_go_sub$groups2=="A_first" | A_go_sub$groups2=="A_second"),]
    A_go_int_fs$dup1 = duplicated(A_go_int_fs$subind)
    A_go_int_fs$dup2 = duplicated(A_go_int_fs$subind, fromLast = TRUE)
    A_go_int_fs$rep = "r"
    A_go_int_fs$rep[A_go_int_fs$dup1 == "FALSE" & A_go_int_fs$dup2 == "FALSE"] = "u"

    A_go_repeat_fs_subs = (length(A_go_int_fs$subind[A_go_int_fs$dup1 == "TRUE" | A_go_int_fs$dup2 == "TRUE"]))/2

  # AUSTIN FIRST VS. THIRD

    A_go_int_ft = A_go_int[(A_go_sub$groups=="A_first" | A_go_sub$groups=="A_third"),]
    A_go_int_ft$dup1 = duplicated(A_go_int_ft$subind)
    A_go_int_ft$dup2 = duplicated(A_go_int_ft$subind, fromLast = TRUE)
    A_go_int_ft$rep = "r"
    A_go_int_ft$rep[A_go_int_ft$dup1 == "FALSE" & A_go_int_ft$dup2 == "FALSE"] = "u"

    A_go_repeat_ft_subs = (length(A_go_int_ft$subind[A_go_int_ft$dup1 == "TRUE" | A_go_int_ft$dup2 == "TRUE"]))/2

  # AUSTIN SECOND VS. THIRD

    A_go_int_st = A_go_int[(A_go_sub$groups=="A_second" | A_go_sub$groups=="A_third"),]
    A_go_int_st$dup1 = duplicated(A_go_int_st$subind)
    A_go_int_st$dup2 = duplicated(A_go_int_st$subind, fromLast = TRUE)
    A_go_int_st$rep = "r"
    A_go_int_st$rep[A_go_int_st$dup1 == "FALSE" & A_go_int_st$dup2 == "FALSE"] = "u"

    A_go_repeat_st_subs = (length(A_go_int_st$subind[A_go_int_st$dup1 == "TRUE" | A_go_int_st$dup2 == "TRUE"]))/2


    # 1 vs. 2
      # UNIQUE
        # ACC
          mod7.goACC = lme(go_acc_mean ~ 1, random = ~1 | subind, data = A_go_int_fs[A_go_int_fs$rep == "u",], method = "ML")
          mod8.goACC = lme(go_acc_mean ~ groups, random = ~1 | subind, data = A_go_int_fs[A_go_int_fs$rep == "u",], method = "ML")
          summary(mod8.goACC)$tTable

          t.test(go_acc_mean ~ groups2, data = A_go_int_fs[A_go_int_fs$rep == "u",])

        # RT
          mod7.goRT = lme(go_rt_mean ~ 1, random = ~1 | subind, data = A_go_int_fs[A_go_int_fs$rep == "u",], method = "ML")
          mod8.goRT = lme(go_rt_mean ~ groups, random = ~1 | subind, data = A_go_int_fs[A_go_int_fs$rep == "u",], method = "ML")
          summary(mod8.goRT)$tTable

          t.test(go_rt_mean ~ groups2, data=A_go_int_fs[A_go_int_fs$rep == "u",])

      # REPEAT
        # ACC
          mod13.goACC = lme(go_acc_mean ~ 1, random = ~1 | subind, data = A_go_int_fs[A_go_int_fs$rep == "r",], method = "ML")
          mod14.goACC = lme(go_acc_mean ~ groups, random = ~1 | subind, data = A_go_int_fs[A_go_int_fs$rep == "r",] , method = "ML")
          summary(mod14.goACC)$tTable

          t.test(go_acc_mean ~ groups, data = A_go_int_fs[A_go_int_fs$rep == "r",], paired=T)

        # RT
          mod13.goRT = lme(go_rt_mean ~ 1, random = ~1 | subind, data = A_go_int_fs[A_go_int_fs$rep == "r",], method = "ML")
          mod14.goRT = lme(go_rt_mean ~ groups, random = ~1 | subind, data = A_go_int_fs[A_go_int_fs$rep == "r",], method = "ML")
          summary(mod14.goRT)$tTable

          t.test(go_rt_mean ~ groups, A_go_int_fs[A_go_int_fs$rep == "r",], paired=T)

      # ALL
        # ACC
          mod.all.goACC = lme(go_acc_mean ~ groups, random = ~1 | subind, data = A_go_int_fs, method = "ML")
          summary(mod.all.goACC)$tTable

          t.test(go_acc_mean ~ groups2, data =  A_go_int_fs)

        # RT
          mod.all.goRT = lme(go_rt_mean ~ groups, random = ~1 | subind, data = A_go_int_fs, method = "ML")
          summary(mod.all.goRT)$tTable

          t.test(go_rt_mean ~ groups2, data =  A_go_int_fs)


      # REPEAT VS. UNIQUE
          #A_go_int_fs$groups = relevel(A_go_int_fs$groups, ref = "A_first")
          #ru.matrix = rbind("f.ur"=c(0,0,1,0), "r.sf"=c(0,1,0,0), "s.ur"=c(0,0,1,1), "u.sf"=c(0,1,0,1)) 
        # ACC
          #mod.all.goACC = lme(go_acc_mean ~ groups + rep, random = list(~1 | subind), data = A_go_int_fs, method = "ML")
          #summary(mod.all.goACC)$tTable
          #summary(glht(mod.all.goACC, ru.matrix), test = adjusted("none"))
          #mod.all.goACC.test = lme(go_acc_mean ~ groups*rep, random = list(~1 | subind), data = A_go_int_fs, method = "ML")
          #summary(mod.all.goACC.test)$tTable
          #summary(glht(mod.all.goACC.test, ru.matrix), test = adjusted("none"))
        # RT
          #mod.all.goRT = lme(go_rt_mean ~ groups + rep, random = list(~1 | subind), data = A_go_int_fs, method = "ML")
          #summary(mod.all.goRT)$tTable


    # 1 vs. 3
      # UNIQUE
        # ACC
          mod9.goACC = lme(go_acc_mean ~ 1, random = ~1 | subind, data = A_go_int_ft[A_go_int_ft$rep == "u",], method = "ML")
          mod10.goACC = lme(go_acc_mean ~ groups, random = ~1 | subind, data = A_go_int_ft[A_go_int_ft$rep == "u",], method = "ML")
          summary(mod10.goACC)$tTable

        # RT
          mod9.goRT = lme(go_rt_mean ~ 1, random = ~1 | subind, data = A_go_int_ft[A_go_int_ft$rep == "u",], method = "ML")
          mod10.goRT = lme(go_rt_mean ~ groups, random = ~1 | subind, data = A_go_int_ft[A_go_int_ft$rep == "u",], method = "ML")
          summary(mod10.goRT)$tTable

      # REPEAT
        # ACC
          mod15.goACC = lme(go_acc_mean ~ 1, random = ~1 | subind, data = A_go_int_ft[A_go_int_ft$rep == "r",], method = "ML")
          mod16.goACC = lme(go_acc_mean ~ groups, random = ~1 | subind, data = A_go_int_ft[A_go_int_ft$rep == "r",], method = "ML")
          summary(mod16.goACC)$tTable

        # RT
          mod15.goRT = lme(go_rt_mean ~ 1, random = list(~1 | subind), data = A_go_int_ft[A_go_int_ft$rep == "r",], method = "ML")
          mod16.goRT = lme(go_rt_mean ~ groups, random = ~1 | subind, data = A_go_int_ft[A_go_int_ft$rep == "r",], method = "ML")
          summary(mod16.goRT)$tTable

      # ALL
        # ACC
          mod17.goACC = lme(go_acc_mean ~ 1, random = ~1 | subind, data = A_go_int_ft, method = "ML")
          mod18.goACC = lme(go_acc_mean ~ groups, random = ~1 | subind, data = A_go_int_ft, method = "ML")
          summary(mod18.goACC)$tTable

        # RT
          mod17.goRT = lme(go_rt_mean ~ 1, random = ~1 | subind, data = A_go_int_ft, method = "ML")
          mod18.goRT = lme(go_rt_mean ~ groups, random = ~1 | subind, data = A_go_int_ft, method = "ML")
          summary(mod18.goRT)$tTable


    # 2 vs. 3
      # UNIQUE
        # ACC
          mod11.goACC = lme(go_acc_mean ~ 1, random = ~1 | subind, data = A_go_int_st[A_go_int_st$rep =="u", ], method = "ML")
          mod12.goACC = lme(go_acc_mean ~ groups, random = ~1 | subind, data = A_go_int_st[A_go_int_st$rep =="u", ], method = "ML")
          summary(mod12.goACC)$tTable

        # RT
          mod11.goRT = lme(go_rt_mean ~ 1, random = ~1 | subind, data = A_go_int_st[A_go_int_st$rep =="u", ], method = "ML")
          mod12.goRT = lme(go_rt_mean ~ groups, random = ~1 | subind, data = A_go_int_st[A_go_int_st$rep =="u", ], method = "ML")
          summary(mod12.goRT)$tTable


      # REPEAT
        # ACC
          mod19.goACC = lme(go_acc_mean ~ 1, random = ~1 | subind, data = A_go_int_st[A_go_int_st$rep =="r",], method = "ML")
          mod20.goACC = lme(go_acc_mean ~ groups, random = ~1 | subind, data = A_go_int_st[A_go_int_st$rep =="r",], method = "ML")
          summary(mod20.goACC)$tTable

        # RT
          mod19.goRT = lme(go_rt_mean ~ 1, random = ~1 | subind, data = A_go_int_st[A_go_int_st$rep =="r",], method = "ML")
          mod20.goRT = lme(go_rt_mean ~ groups, random = ~1 | subind, data = A_go_int_st[A_go_int_st$rep =="r",], method = "ML")
          summary(mod20.goRT)$tTable

      # ALL
        # ACC
          mod21.goACC = lme(go_acc_mean ~ 1, random = ~1 | subind, data = A_go_int_st, method = "ML")
          mod22.goACC = lme(go_acc_mean ~ groups, random = ~1 | subind, data = A_go_int_st, method = "ML")
          summary(mod22.goACC)$tTable

        # RT
          mod21.goRT = lme(go_rt_mean ~ 1, random = ~1 | subind, data = A_go_int_st, method = "ML")
          mod22.goRT = lme(go_rt_mean ~ groups, random = ~1 | subind, data = A_go_int_st, method = "ML")
          summary(mod22.goRT)$tTable




  # HOUSTON CONTROLS FIRST VS. SECOND

    H_go_control = H_go_sub[(H_go_sub$ID=="c"), ]
    H_go_control$dup1 = duplicated(H_go_control$subind)
    H_go_control$dup2 = duplicated(H_go_control$subind, fromLast = TRUE)

    H_go_control_unique = H_go_control[((H_go_control$dup2 == "FALSE" & H_go_control$groups == "H_control_first") | (H_go_control$dup1 == "FALSE" & H_go_control$groups == "H_control_second")), ]
    H_go_control_repeat = H_go_control[(H_go_control$dup1 == "TRUE" | H_go_control$dup2 == "TRUE"), ]

    # UNIQUE SUBS
      # ACC
        houston1.goACC = lme(go_acc_mean ~ 1, random = list(~1 | subind), data = H_go_control_unique, method = "ML")
        houston2.goACC = lme(go_acc_mean ~ groups, random = list(~1 | subind), data = H_go_control_unique, method = "ML")
        h1.h2.unique.goACC = summary(houston2.goACC)$tTable

        anova(houston1.goACC)
        anova(houston2.goACC)
        h.unique.goACC = anova(houston1.goACC, houston2.goACC)

      # RT
        houston1.goRT = lme(go_rt_mean ~ 1, random = list(~1 | subind), data = H_go_control_unique, method = "ML")
        houston2.goRT = lme(go_rt_mean ~ groups, random = list(~1 | subind), data = H_go_control_unique, method = "ML")
        h1.h2.unique.goRT = summary(houston2.goRT)$tTable

        anova(houston1.goRT)
        anova(houston2.goRT)
        h.unique.goRT = anova(houston1.goRT, houston2.goRT)

        t.test(H_go_control_unique$go_rt_mean[H_go_control_unique$groups == "H_control_first"], H_go_control_unique$go_rt_mean[H_go_control_unique$groups == "H_control_second"], alternative = "less") 


    # REPEAT SUBS
      # ACC
        houston3.goACC = lme(go_acc_mean ~ 1, random = list(~1 | subind), data = H_go_control_repeat, method = "ML")
        houston4.goACC = lme(go_acc_mean ~ groups, random = list(~1 | subind), data = H_go_control_repeat, method = "ML")
        h1.h2.repeat.goACC = summary(houston4.goACC)$tTable

        anova(houston3.goACC)
        anova(houston4.goACC)
        h.repeat.goACC = anova(houston3.goACC, houston4.goACC)

        t.test(H_go_control_repeat$go_acc_mean[H_go_control_repeat$groups == "H_control_first"], H_go_control_repeat$go_acc_mean[H_go_control_repeat$groups == "H_control_second"], alternative = "less", paired = TRUE) 

      # RT
        houston3.goRT = lme(go_rt_mean ~ 1, random = list(~1 | subind), data = H_go_control_repeat, method = "ML")
        houston4.goRT = lme(go_rt_mean ~ groups, random = list(~1 | subind), data = H_go_control_repeat, method = "ML")
        h1.h2.repeat.goRT = summary(houston4.goRT)$tTable

        anova(houston3.goRT)
        anova(houston4.goRT)
        h.repeat.goRT = anova(houston3.goRT, houston4.goRT)


  # GO STATS CSV'S

    goACC_stats = rbind(a.h.control.goACC, a1.ac.goACC, a1.ahc1.goACC, a2.ac.goACC, a1.a2.unique.goACC, a1.a2.repeat.goACC, h1.h2.unique.goACC, h1.h2.repeat.goACC)
    write.csv(goACC_stats, file=sprintf("%s/data_frames/SST/GO/goACC_stats.csv", wd), na="NA")

    goRT_stats = rbind(a.h.control.goRT, a1.ac.goRT, a1.ahc1.goRT, a2.ac.goRT, a1.a2.unique.goRT, a1.a2.repeat.goRT, h1.h2.unique.goRT, h1.h2.repeat.goRT)
    write.csv(goRT_stats, file=sprintf("%s/data_frames/SST/GO/goRT_stats.csv", wd), na="NA")



# GO FIGURES 

  # BY SESSION

    # AUSTIN

       yoPalette = c("#E69F00", "#F0E442")


        A_go_sub$groups2 = A_go_sub$groups
        A_go_sub$groups2[A_go_sub$groups == "A_first2"] = "A_first"


        A_goRT_box_aug15 = ggplot(data = A_go_sub[A_go_sub$group2!="A_third",],aes(x = group2,y = go_rt_mean)) + geom_boxplot(aes(fill = group2)) + geom_point(data= A_go_sub[((A_go_sub$ID2 =="0" | A_go_sub$ID2=="1" | A_go_sub$ID2 == "c") & A_go_sub$group2!="A_third"),], aes(colod=ID2, size=2, shape=ID2),size=4.5) + geom_line(data= A_go_sub[((A_go_sub$ID2 =="0" | A_go_sub$ID2=="1") & A_go_sub$group2!="A_third"),], aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black"))

   #+ scale_color_discrete(name = "group2", breaks = c("A_control", "A_first", "A_second"), labels = c(sprintf("A_control (%d)", length(A_go_sub$groups[A_go_sub$groups == "A_control"])), sprintf("A_first (%d)",length(A_go_sub$group2[A_go_sub$group2 == "A_first"])), sprintf("A_second (%d)",length(A_go_sub$group2[A_go_sub$group2 == "A_second"]))))  + scale_shape_discrete(name = "group2", breaks = c("A_control", "A_first", "A_second"), labels = c(sprintf("A_control (%d)", length(A_go_sub$group2[A_go_sub$group2 == "A_control"])), sprintf("A_first (%d)",length(A_go_sub$group2[A_go_sub$group2 == "A_first"])), sprintf("A_second (%d)",length(A_go_sub$group2[A_go_sub$group2 == "A_second"])))) 
        #+ guides(group = FALSE, color = FALSE, linetype=FALSE, size=FALSE, alpha=FALSE, shape=FALSE, fill=FALSE) + legend.title = (element_text(size = rel(1.5))), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10))                         
	             ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_box_aug15.pdf",wd),width=5,height=5)
 
        A_goRT_box_aug15_noline = ggplot(data = A_go_sub[A_go_sub$group2!="A_third",],aes(x = group2,y = go_rt_mean)) + geom_boxplot(aes(fill = group2)) + geom_point(data= A_go_sub[((A_go_sub$ID2 =="0" | A_go_sub$ID2=="1" | A_go_sub$ID2 == "c") & A_go_sub$group2!="A_third"),], aes(colod=ID2, size=2, shape=ID2),size=4.5) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black"))
        ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_box_aug15_noline.pdf",wd),width=5,height=5)



  
        A_goRT_box_all = ggplot(data = A_go_sub,aes(x = groups,y = go_rt_mean)) + geom_boxplot() + geom_point() + geom_line(aes(group = subind, color = ID, linetype = ID))+ xlab("Session") + 
                         ylab("Mean RT (s)") + ggtitle("Austin Go Correct RT")+ guides(group = FALSE) +
                         theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), 
                           axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
	             ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_box_all_mri.png",wd),width=10,height=10)



        A_goRT_bar_sf = ggplot(data = A_go_group[A_go_group$groups!="A_third",], aes(x = groups, y = go_rt_me, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                            geom_errorbar(aes(y = go_rt_me, ymin = go_rt_me-go_rt_se, ymax = go_rt_me+go_rt_se), position = position_dodge(.9), width=.2) + xlab("Session") + ylab("Mean RT") + 
                            ggtitle("Austin Go Correct RT") +
                            scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", A_go_group$N[A_go_group$groups == "A_control"]), sprintf("A_first (%d)",
			           A_go_group$N[A_go_group$groups == "A_first"]), sprintf("A_second (%d)",A_go_group$N[A_go_group$groups == "A_second"]), sprintf("A_third (%d)",A_go_group$N[A_go_group$groups == "A_third"]))) +
                         theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), 
                           axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
                     ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_bar_sf_mri.png",wd),width=10,height=10)



        A_goRT_bar_all = ggplot(data = A_go_group, aes(x = groups, y = go_rt_me, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                            geom_errorbar(aes(y = go_rt_me, ymin = go_rt_me-go_rt_se, ymax = go_rt_me+go_rt_se), position = position_dodge(.9), width=.2) + xlab("Session") + ylab("Mean RT") + 
                            ggtitle("Austin Go Correct RT") +
                            scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", A_go_group$N[A_go_group$groups == "A_control"]), sprintf("A_first (%d)",
			           A_go_group$N[A_go_group$groups == "A_first"]), sprintf("A_second (%d)",A_go_group$N[A_go_group$groups == "A_second"]), sprintf("A_third (%d)",A_go_group$N[A_go_group$groups == "A_third"]))) +
                         theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), 
                           axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
                     ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_bar_all_mri.png",wd),width=10,height=10)

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



        A_goACC_box_aug15 = ggplot(data = A_go_sub[A_go_sub$group2!="A_third",],aes(x = group2,y = go_acc_mean)) + geom_boxplot(aes(fill=group2)) + geom_point(data= A_go_sub[((A_go_sub$ID2 =="0" | A_go_sub$ID2=="1" | A_go_sub$ID2=="c") & A_go_sub$group2!="A_third"),], aes(color=ID2, size=2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ geom_line(data= A_go_sub[((A_go_sub$ID2 =="0" | A_go_sub$ID2=="1") & A_go_sub$group2!="A_third"),],aes(group = subind, color = ID2, linetype = ID2))+ xlab("Group") + ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 

      + guides(group = FALSE, fill = FALSE, shape=FALSE, color=FALSE, linetype=FALSE) + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
      #+ legend.title = element_text(size = rel(1.5)), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10))  
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_goACC_box_aug15.pdf",wd),width=5,height=5)


        A_goACC_box_aug15_noline = ggplot(data = A_go_sub[A_go_sub$group2!="A_third",],aes(x = group2,y = go_acc_mean)) + geom_boxplot(aes(fill=group2)) + geom_point(data= A_go_sub[((A_go_sub$ID2 =="0" | A_go_sub$ID2=="1" | A_go_sub$ID2=="c") & A_go_sub$group2!="A_third"),], aes(color=ID2, size=2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ xlab("Group") + ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_goACC_box_aug15_noline.pdf",wd),width=5,height=5)







        A_goACC_box_all = ggplot(data = A_go_sub,aes(x = groups,y = go_acc_mean)) + geom_boxplot() + geom_point() + geom_line(aes(group = subind, color = ID, linetype = ID))+ xlab("Session") + 
                          ylab("Mean ACC") + ggtitle("Austin Go Correct ACC")+ guides(group = FALSE) +
                          theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), 
                           axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_goACC_box_all_mri.png",wd),width=10,height=10)


        A_goACC_bar_sf = ggplot(data = A_go_group[A_go_group$groups!="A_third",], aes(x = groups, y = go_acc_me, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                             geom_errorbar(aes(y = go_acc_me, ymin = go_acc_me-go_acc_se, ymax = go_acc_me+go_acc_se), position = position_dodge(.9), width=.2) + xlab("Session") + ylab("Mean ACC") + 
                             ggtitle("Austin Go Correct ACC") +
                             scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", A_go_group$N[A_go_group$groups == "A_control"]), sprintf("A_first (%d)",
			          A_go_group$N[A_go_group$groups == "A_first"]), sprintf("A_second (%d)",A_go_group$N[A_go_group$groups == "A_second"]), sprintf("A_third (%d)",A_go_group$N[A_go_group$groups == "A_third"]))) +
                          theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), 
                           axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
                      ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_goACC_bar_sf_mri.png",wd),width=10,height=10)



        A_goACC_bar_all = ggplot(data = A_go_group, aes(x = groups, y = go_acc_me, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                             geom_errorbar(aes(y = go_acc_me, ymin = go_acc_me-go_acc_se, ymax = go_acc_me+go_acc_se), position = position_dodge(.9), width=.2) + xlab("Session") + ylab("Mean ACC") + 
                             ggtitle("Austin Go Correct ACC") +
                             scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", A_go_group$N[A_go_group$groups == "A_control"]), sprintf("A_first (%d)",
			          A_go_group$N[A_go_group$groups == "A_first"]), sprintf("A_second (%d)",A_go_group$N[A_go_group$groups == "A_second"]), sprintf("A_third (%d)",A_go_group$N[A_go_group$groups == "A_third"]))) +
                          theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), 
                           axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
                      ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_goACC_bar_all_mri.png",wd),width=10,height=10)



      # HOUSTON

        H_goRT_box = ggplot(data = H_go_sub,aes(x = groups,y = go_rt_mean)) + geom_boxplot() + geom_point() + geom_line(aes(group = subind, color = subind, linetype = ID))+ xlab("Session") + 
                            ylab("Mean RT (s)") + ggtitle("Houston Go Correct RT")+ guides(group = FALSE,color = FALSE)
	             ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/H_goRT_box.png",wd),width=10,height=10)
      
   
        H_goRT_bar = ggplot(data=H_go_group, aes(x = groups, y = go_rt_me, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                            geom_errorbar(aes(y=go_rt_me, ymin=go_rt_me-go_rt_se, ymax=go_rt_me+go_rt_se), position = position_dodge(.9), width=.2) + ylab("Mean RT") + xlab("Session") + 
                            ggtitle("Houston Go Correct RT") +
                            scale_fill_discrete(name = "groups", breaks = c("H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("H_control_first (%d)", H_go_group$N[H_go_group$groups == "H_control_first"]), 
                              sprintf("H_control_second (%d)", H_go_group$N[H_go_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", H_go_group$N[H_go_group$groups == "H_control_third"]), sprintf("H_first (%d)", H_go_group$N[H_go_group$groups == "H_first"]), 
                              sprintf("H_second (%d)",H_go_group$N[H_go_group$groups == "H_second"]), sprintf("H_third (%d)",H_go_group$N[H_go_group$groups == "H_third"])))
                     ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/H_goRT_bar.png",wd),width=10,height=10)


        H_goACC_box = ggplot(data = H_go_sub,aes(x = groups,y = go_acc_mean)) + geom_boxplot() + geom_point() + geom_line(aes(group = subind, color = subind, linetype = ID))+ xlab("Session") + 
                             ylab("Mean ACC (s)") + ggtitle("Houston Go Correct ACC")+ guides(group = FALSE,color = FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/H_goACC_box.png",wd),width=10,height=10)

        H_goACC_bar = ggplot(data=H_go_group, aes(x = groups, y = go_acc_me, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                             geom_errorbar(aes(y=go_acc_me, ymin=go_acc_me-go_acc_se, ymax=go_acc_me+go_acc_se), position = position_dodge(.9), width=.2) + ylab("Mean ACC") + xlab("Session") + 
                             ggtitle("Houston Go Correct ACC") +
                             scale_fill_discrete(name = "groups", breaks = c("H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("H_control_first (%d)", H_go_group$N[H_go_group$groups == "H_control_first"]), 
                              sprintf("H_control_second (%d)", H_go_group$N[H_go_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", H_go_group$N[H_go_group$groups == "H_control_third"]), sprintf("H_first (%d)", H_go_group$N[H_go_group$groups == "H_first"]), 
                              sprintf("H_second (%d)",H_go_group$N[H_go_group$groups == "H_second"]), sprintf("H_third (%d)",H_go_group$N[H_go_group$groups == "H_third"])))
                      ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/H_goACC_bar.png",wd),width=10,height=10)


      # BOTH AUSTIN AND HOUSTON

        All_goRT_box_aug15 = ggplot(data = all_go_sub[all_go_sub$group3!="third",],aes(x = group3, y = go_rt_mean)) + geom_boxplot(data = all_go_sub[all_go_sub$group3!="third",],aes(fill = group3)) + geom_point(data=all_go_sub[((all_go_sub$ID2 =="0" | all_go_sub$ID2=="1" | all_go_sub$ID2 == "c") & all_go_sub$group3!="third"),], aes(colod=ID2, size=2, shape=ID2),size=4.5) + geom_line(data= all_go_sub[((all_go_sub$ID2 =="0" | all_go_sub$ID2=="1") & all_go_sub$group3!="third"),], aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black"))
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_goRT_box_aug15.pdf",wd),width=5,height=5)


        All_goRT_box_aug15_noline = ggplot(data = all_go_sub[all_go_sub$group3!="third",],aes(x = group3, y = go_rt_mean)) + geom_boxplot(data = all_go_sub[all_go_sub$group3!="third",],aes(fill = group3)) + geom_point(data=all_go_sub[((all_go_sub$ID2 =="0" | all_go_sub$ID2=="1" | all_go_sub$ID2 == "c") & all_go_sub$group3!="third"),], aes(colod=ID2, size=2, shape=ID2),size=4.5) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black"))
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_goRT_box_aug15_noline.pdf",wd),width=5,height=5)





        All_goRT_bar = ggplot(data = all_go_group, aes(x = groups, y = go_rt_me, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                              geom_errorbar(aes(y = go_rt_me, ymin = go_rt_me-go_rt_se, ymax = go_rt_me+go_rt_se), position = position_dodge(.9), width=.2) + xlab("Session") + ylab("Mean RT") + 
                              ggtitle("Austin and Houston Go Correct RT") +
                              scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third", "H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("A_control (%d)", all_go_group$N[all_go_group$groups == "A_control"]), 
                               sprintf("A_first (%d)", all_go_group$N[all_go_group$groups == 'A_first']), sprintf('A_second (%d)',all_go_group$N[all_go_group$groups == 'A_second']), sprintf('A_third (%d)',all_go_group$N[all_go_group$groups == 'A_third']),
                               sprintf("H_control_first (%d)", all_go_group$N[all_go_group$groups == "H_control_first"]), sprintf("H_control_second (%d)", all_go_group$N[all_go_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", all_go_group$N[all_go_group$groups == "H_control_third"]), 
                               sprintf("H_first (%d)", all_go_group$N[all_go_group$groups == "H_first"]), sprintf("H_second (%d)",all_go_group$N[all_go_group$groups == "H_second"]), sprintf("H_third (%d)",all_go_group$N[all_go_group$groups == "H_third"])))
                       ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_goRT_bar.png",wd),width=10,height=10)


        All_goACC_box_aug15 = ggplot(data = all_go_sub[all_go_sub$group3!="third",],aes(x = group3,y = go_acc_mean)) + geom_boxplot(data = all_go_sub[all_go_sub$group3!="third",],aes(fill = group3)) + geom_point(data=all_go_sub[((all_go_sub$ID2 =="0" | all_go_sub$ID2=="1" | all_go_sub$ID2 == "c") & all_go_sub$group3!="third"),], aes(colod=ID2, size=2, shape=ID2),size=4.5) + geom_line(data= all_go_sub[((all_go_sub$ID2 =="0" | all_go_sub$ID2=="1") & all_go_sub$group3!="third"),], aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black"))
	                ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/All_goACC_box_aug15.pdf",wd),width=5,height=5)

        All_goACC_box_aug15_noline = ggplot(data = all_go_sub[all_go_sub$group3!="third",],aes(x = group3,y = go_acc_mean)) + geom_boxplot(data = all_go_sub[all_go_sub$group3!="third",],aes(fill = group3)) + geom_point(data=all_go_sub[((all_go_sub$ID2 =="0" | all_go_sub$ID2=="1" | all_go_sub$ID2 == "c") & all_go_sub$group3!="third"),], aes(colod=ID2, size=2, shape=ID2),size=4.5) + xlab("Group") + ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + scale_shape_manual(values=c(17,2,1)) + scale_colour_manual(values=c("black","black", "black"))
	                ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/All_goACC_box_aug15_noline.pdf",wd),width=5,height=5)




        All_goACC_bar = ggplot(data = all_go_group, aes(x = groups, y = go_acc_me, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                               geom_errorbar(aes(y = go_acc_me, ymin = go_acc_me-go_acc_se, ymax = go_acc_me+go_acc_se), position = position_dodge(.9), width=.2) + xlab("Session") + ylab("Mean ACC") + 
                               ggtitle("Austin and Houston Go Correct ACC") +
                               scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third", "H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("A_control (%d)", all_go_group$N[all_go_group$groups == "A_control"]), 
                               sprintf("A_first (%d)", all_go_group$N[all_go_group$groups == 'A_first']), sprintf('A_second (%d)',all_go_group$N[all_go_group$groups == 'A_second']), sprintf('A_third (%d)',all_go_group$N[all_go_group$groups == 'A_third']),
                               sprintf("H_control_first (%d)", all_go_group$N[all_go_group$groups == "H_control_first"]), sprintf("H_control_second (%d)", all_go_group$N[all_go_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", all_go_group$N[all_go_group$groups == "H_control_third"]), 
                               sprintf("H_first (%d)", all_go_group$N[all_go_group$groups == "H_first"]), sprintf("H_second (%d)",all_go_group$N[all_go_group$groups == "H_second"]), sprintf("H_third (%d)",all_go_group$N[all_go_group$groups == "H_third"])))
                        ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/All_goACC_bar.png",wd),width=10,height=10)


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

# STOP DATAFRAMES (STATS & GRAPHS)

  # AUSTIN
    A_stop_sub = rbind(stop_sub_1, stop_sub_2, stop_sub_3, stop_sub_4, stop_sub_5, stop_sub_11)
    A_stop_sub$group2 = A_stop_sub$group
    A_stop_sub$group2[A_stop_sub$group2=="A_first" | A_stop_sub$group2=="A2_first"] = "first"
    A_stop_sub$group2[A_stop_sub$group2=="A_second" | A_stop_sub$group2=="A2_second"] = "second"
    A_stop_sub$group2[A_stop_sub$group2=="A_third"] = "third"
    A_stop_sub$group2[A_stop_sub$group2=="A_control"] = "control"
    A_stop_sub$subind[A_stop_sub$group=="A_first"] = substrRight(A_stop_sub$subind[A_stop_sub$group=="A_first"],5)
    A_stop_sub$subind[A_stop_sub$group=="A_second"] = substrLeft(A_stop_sub$subind[A_stop_sub$group=="A_second"],10)
    A_stop_sub$subind[A_stop_sub$group=="A_second"] = substrRight(A_stop_sub$subind[A_stop_sub$group=="A_second"],5)
    A_stop_sub$subind[A_stop_sub$group=="A_third"] = substrLeft(A_stop_sub$subind[A_stop_sub$group=="A_third"],10)  
    A_stop_sub$subind[A_stop_sub$group=="A_third"] = substrRight(A_stop_sub$subind[A_stop_sub$group=="A_third"],5)
    A_stop_sub$subind[A_stop_sub$group=="A2_first"] = substrRight(A_stop_sub$subind[A_stop_sub$group=="A2_first"],5)
    A_stop_sub$subind[A_stop_sub$group=="A2_second"] = substrLeft(A_stop_sub$subind[A_stop_sub$group=="A2_second"],11)
    A_stop_sub$subind[A_stop_sub$group=="A2_second"] = substrRight(A_stop_sub$subind[A_stop_sub$group=="A2_second"],5)
    A_stop_sub$ID=substrLeft(A_stop_sub$subind, 1)
    A_stop_sub$ID2="c"
    A_stop_sub$ID2[A_stop_sub$ID=="0"] = "0"
    A_stop_sub$ID2[A_stop_sub$ID=="1" | A_stop_sub$ID=="2"] = "1"
    A_stop_sub$INT = "c"
    A_stop_sub$INT[A_stop_sub$ID == 0 | A_stop_sub$ID == 1 | A_stop_sub$ID == 2] = "i"
    A_stop_sub$city = "Austin"
    A_stop_sub=A_stop_sub[A_stop_sub$group2!="third",]
    A_stop_sub$rep = 'u'
    A_stop_sub$rep[(duplicated(A_stop_sub['subind'])) | (duplicated(A_stop_sub['subind'], fromLast = TRUE))] = 'r'

    A_stop_sub = within (A_stop_sub, { 
                 subind = factor(subind)
                 group = factor(group)
                 ID = factor(ID) 
                 city = factor(city)
                 })

   write.csv(A_stop_sub, file=sprintf("%s/data_frames/SST/STOP/A_stop_sub_df.csv", wd), na="NA")

    A_stop_group = ddply(A_stop_sub, .(groups), summarise, N=length(groups), SSRT_mean=(mean(SSRT_me, na.rm=TRUE)), SSRT_se=sd(SSRT_me, na.rm=TRUE)/sqrt(N), stop_acc_mean=(mean(stop_acc_me, na.rm=TRUE)), stop_acc_se=sd(stop_acc_me, na.rm=TRUE)/sqrt(N), ssd_mean = mean(ssd, na.rm=TRUE), ssd_se = sd(ssd, na.rm = TRUE)/sqrt(N))
    A_stop_ID = ddply(A_stop_sub, .(ID, groups), summarise, N=length(ID), SSRT_mean=(mean(SSRT_me, na.rm=TRUE)), SSRT_se=sd(SSRT_me, na.rm=TRUE)/sqrt(N), stop_acc_mean=(mean(stop_acc_me, na.rm=TRUE)), stop_acc_se=sd(stop_acc_me, na.rm=TRUE)/sqrt(N))
    A_stop_ID2 = ddply(A_stop_sub, .(ID2, groups), summarise, N=length(ID), SSRT_mean=(mean(SSRT_me, na.rm=TRUE)), SSRT_se=sd(SSRT_me, na.rm=TRUE)/sqrt(N), stop_acc_mean=(mean(stop_acc_me, na.rm=TRUE)), stop_acc_se=sd(stop_acc_me, na.rm=TRUE)/sqrt(N))


    A_stop_run = rbind(stop_run_1, stop_run_2, stop_run_3, stop_run_4, stop_run_9)
    A_stop_run$subind[A_stop_run$groups=="A_second"] = substrLeft(A_stop_run$subind[A_stop_run$groups=="A_second"],5)
    A_stop_run$subind[A_stop_run$groups=="A_third"] = substrLeft(A_stop_run$subind[A_stop_run$groups=="A_third"],5)
    A_stop_run$subind[A_stop_run$groups=="A_first2"]=substrRight(A_stop_run$subind[A_stop_run$groups=="A_first2"],5)
    A_stop_run$ID = substrLeft(A_stop_run$subind, 1)
    A_stop_run$ID2="c"
    A_stop_run$ID2[A_stop_run$ID=="0"] = "0"
    A_stop_run$ID2[A_stop_run$ID=="1" | A_stop_run$ID=="2"] = "1"
    A_stop_run$INT=c("c")
    A_stop_run$INT[A_stop_run$ID == 0 |A_stop_run$ID == 1 | A_stop_run$ID == 2] = "i"
    A_stop_run$city = "Austin"
    as.character(A_stop_run$subind)

    A_stop_run = within (A_stop_run, { 
               groups = factor(groups)
               ID = factor(ID)
               city = factor(city)
               })




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
    stop_sub_14 = stop_sub_14[stop_sub_14$subind!="H_LDIMG8960_c_third",]
    H_stop_sub = rbind(stop_sub_7, stop_sub_8, stop_sub_9, stop_sub_10,stop_sub_13, stop_sub_14)
    H_stop_sub$group2 = H_stop_sub$group
    H_stop_sub$group2[H_stop_sub$group2=="H2_first"] = "first"
    H_stop_sub$group2[H_stop_sub$group2=="H_second" | H_stop_sub$group2=="H2_second"] = "second"
    H_stop_sub$group2[H_stop_sub$group2=="H_third"] = "third"
    H_stop_sub$group2[H_stop_sub$group2=="H_control_second" | H_stop_sub$group2=="H_control_third"] = "control"
    H_stop_sub$subind[H_stop_sub$group=="H_second" | H_stop_sub$group=="H_control_second" | H_stop_sub$group=="H_third" | H_stop_sub$group=="H_control_second" | H_stop_sub$group=="H_control_third"]=substrLeft(H_stop_sub$subind[H_stop_sub$group=="H_second" | H_stop_sub$group=="H_control_second"| H_stop_sub$group=="H_third" | H_stop_sub$group=="H_control_second" | H_stop_sub$group=="H_control_third"],13)
    H_stop_sub$subind[H_stop_sub$group=="H_second" | H_stop_sub$group=="H_control_second"| H_stop_sub$group=="H_third" | H_stop_sub$group=="H_control_second" | H_stop_sub$group=="H_control_third"]=substrRight(H_stop_sub$subind[H_stop_sub$group=="H_second" | H_stop_sub$group=="H_control_second"| H_stop_sub$group=="H_third" | H_stop_sub$group=="H_control_second"| H_stop_sub$group=="H_control_third" ],6) 
    H_stop_sub$subind[H_stop_sub$group=="H2_first"]=substrRight(H_stop_sub$subind[H_stop_sub$group=="H2_first"],7) 
    H_stop_sub$subind[H_stop_sub$group=="H2_second"]=substrLeft(H_stop_sub$subind[H_stop_sub$group=="H2_second"],12)
    H_stop_sub$subind[H_stop_sub$group=="H2_second"]=substrRight(H_stop_sub$subind[H_stop_sub$group=="H2_second"],7) 
    H_stop_sub$ID=substrRight(H_stop_sub$subind, 1)
    H_stop_sub$ID2=H_stop_sub$ID
    H_stop_sub$ID2[H_stop_sub$ID=="0"] = "0"
    H_stop_sub$ID2[H_stop_sub$ID=="1" | H_stop_sub$ID=="2"] = "1"
    H_stop_sub$INT = "c"
    H_stop_sub$INT[H_stop_sub$ID == 0 | H_stop_sub$ID == 1 | H_stop_sub$ID == 2] = "i"
    H_stop_sub$city = "Houston"
    H_stop_sub = H_stop_sub[H_stop_sub$group2!="third",]
    H_stop_sub$rep = 'u'
    H_stop_sub$rep[(duplicated(H_stop_sub['subind'])) | (duplicated(H_stop_sub['subind'], fromLast = TRUE))] = 'r'


    H_stop_sub = within (H_stop_sub, { 
                 subind = factor(subind)
                 group = factor(group)
                 ID = factor(ID) 
                 city = factor(city)
                 })


    H_stop_group = ddply(H_stop_sub, .(groups), summarise, N=length(groups), SSRT_mean=(mean(SSRT_me, na.rm=TRUE)), SSRT_se=sd(SSRT_me, na.rm=TRUE)/sqrt(N), stop_acc_mean=(mean(stop_acc_me, na.rm=TRUE)), stop_acc_se=sd(stop_acc_me, na.rm=TRUE)/sqrt(N))
    H_stop_ID = ddply(H_stop_sub, .(ID, groups), summarise, N=length(ID), SSRT_mean=(mean(SSRT_me, na.rm=TRUE)), SSRT_se=sd(SSRT_me, na.rm=TRUE)/sqrt(N), stop_acc_mean=(mean(stop_acc_me, na.rm=TRUE)), stop_acc_se=sd(stop_acc_me, na.rm=TRUE)/sqrt(N))
    H_stop_ID2 = ddply(H_stop_sub, .(ID2, groups), summarise, N=length(ID), SSRT_mean=(mean(SSRT_me, na.rm=TRUE)), SSRT_se=sd(SSRT_me, na.rm=TRUE)/sqrt(N), stop_acc_mean=(mean(stop_acc_me, na.rm=TRUE)), stop_acc_se=sd(stop_acc_me, na.rm=TRUE)/sqrt(N))


  # AUSTIN AND HOUSTON
    all_stop_sub = rbind(A_stop_sub, H_stop_sub)

    all_stop_sub$dup1 = duplicated(all_stop_sub$subind)
    all_stop_sub$dup2 = duplicated(all_stop_sub$subind, fromLast = TRUE)

    all_stop_group = ddply(all_stop_sub, .(groups), summarise, N=length(groups), SSRT_mean=(mean(SSRT_me, na.rm=TRUE)), SSRT_se=sd(SSRT_me, na.rm=TRUE)/sqrt(N), stop_acc_mean=(mean(stop_acc_me, na.rm=TRUE)), stop_acc_se=sd(stop_acc_me, na.rm=TRUE)/sqrt(N), 
                           stop_fail_cor_mean=mean(stop_fail_cor_me, na.rm=TRUE), stop_fail_cor_se=sd(stop_fail_cor_me, na.rm=TRUE)/sqrt(N), stop_fail_incor_mean=mean(stop_fail_incor_me, na.rm=TRUE), stop_fail_incor_se=sd(stop_fail_incor_me, na.rm=TRUE)/sqrt(N))
    all_stop_ID = ddply(all_stop_sub, .(ID, groups), summarise, N=length(ID), SSRT_mean=(mean(SSRT_me, na.rm=TRUE)), SSRT_se=sd(SSRT_me, na.rm=TRUE)/sqrt(N), stop_acc_mean=(mean(stop_acc_me, na.rm=TRUE)), stop_acc_se=sd(stop_acc_me, na.rm=TRUE)/sqrt(N))

    a1_h1_c_stop_sub = all_stop_sub[all_stop_sub$groups == "A_control" | all_stop_sub$groups == "A_first" | all_stop_sub$groups == "A_first2" | all_stop_sub$groups == "H2_first", ]
    a1_h1_c_stop_group = ddply(a1_h1_c_stop_sub, .(groups), summarise, N=length(groups), SSRT_mean=(mean(SSRT_me, na.rm=TRUE)), SSRT_se=sd(SSRT_me, na.rm=TRUE)/sqrt(N), stop_acc_mean=(mean(stop_acc_me, na.rm=TRUE)), stop_acc_se=sd(stop_acc_me, na.rm=TRUE)/sqrt(N))
     write.csv(a1_h1_c_stop_group, file=sprintf("%s/data_frames/SST/STOP/a1_h1_c_stop_group.csv", wd), na="NA", row.names=FALSE)    

# STOP STATS


  # STOP STATS for NIH AUG15

     # AUSTIN

     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/stop_stats_austin_aug15.txt")

     ssd_s1vc_a = t.test(ssd ~ group3, data = A_stop_sub[A_stop_sub$group3 == "first" | A_stop_sub$group3 == "control",])
     print("Mean SSD - S1 vs. Controls")
     print(ssd_s1vc_a)

     ssrt_s1vc_a = t.test(SSRT_me ~ group3, data = A_stop_sub[A_stop_sub$group3 == "first" | A_stop_sub$group3 == "control",])
     print("Mean SSRT - S1 vs. Controls")
     print(ssrt_s1vc_a)

     ssd_s2vc_a = t.test(ssd ~ group3, data = A_stop_sub[A_stop_sub$group3 == "second" | A_stop_sub$group3 == "control",])
     print("Mean SSD - S2 vs. Controls")
     print(ssd_s2vc_a)

     ssrt_s2vc_a = t.test(SSRT_me ~ group3, data = A_stop_sub[A_stop_sub$group3 == "second" | A_stop_sub$group3 == "control",])
     print("Mean SSRT - S2 vs. Controls")
     print(ssrt_s2vc_a)

     ssd_s1vs2_a = t.test(ssd ~ group3, data = A_stop_sub[A_stop_sub$group3 == "first" | A_stop_sub$group3 == "second",])
     print("Mean SSD - S1 vs. s2")
     print(ssd_s1vs2_a)

     ssrt_s1vs2_a = t.test(SSRT_me ~ group3, data = A_stop_sub[A_stop_sub$group3 == "first" | A_stop_sub$group3 == "second",])
     print("Mean SSRT - S1 vs. S2")
     print(ssrt_s1vs2_a)

     sink()


     # AUSTIN REPEAT SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/stop_stats_austin_repeat_aug15.txt")

     print(all_stop_sub[all_stop_sub$city=="Austin" & all_stop_sub$rep == "r",])

     ssd_repeat_a = t.test(ssd ~ group2, data = all_stop_sub[all_stop_sub$city=="Austin" & all_stop_sub$rep == "r",], paired=T)
     print("Mean SSD - S1 vs. s2 Repeats")
     print(ssd_repeat_a)

     ssrt_repeat_a = t.test(SSRT_me ~ group2, data = all_stop_sub[all_stop_sub$city=="Austin" & all_stop_sub$rep == "r",], paired=T)
     print("Mean SSRT - S1 vs. S2 Repeats")
     print(ssrt_repeat_a)

     sink()


     # AUSTIN UNIQUE SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/stop_stats_austin_uniq_aug15.txt")

     print(all_stop_sub[all_stop_sub$city=="Austin" & all_stop_sub$group2!="control" & all_stop_sub$rep == "u",])

     ssd_uniq_a = t.test(ssd ~ group2, data = all_stop_sub[all_stop_sub$city=="Austin" & all_stop_sub$group2!="control" & all_stop_sub$rep == "u",])
     print("Mean SSD - S1 vs. s2 Unique")
     print(ssd_uniq_a)

     ssd_uniq_a = t.test(SSRT_me ~ group2, data = all_stop_sub[all_stop_sub$city=="Austin" & all_stop_sub$group2!="control" & all_stop_sub$rep == "u",])
     print("Mean SSRT  - S1 vs. S2 Unique")
     print(ssd_uniq_a)

     sink()


     # AUSTIN AND HOUSTON


     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/stop_stats_ah_aug15.txt")

     ssd_s1vc_ah = t.test(ssd ~ group3, data = all_stop_sub[all_stop_sub$group3 == "first" | all_stop_sub$group3 == "control",])
     print("Mean SSD - S1 vs. Controls")
     print(ssd_s1vc_ah)

     ssrt_s1vc_ah = t.test(SSRT_me ~ group3, data = all_stop_sub[all_stop_sub$group3 == "first" | all_stop_sub$group3 == "control",])
     print("Mean SSRT - S1 vs. Controls")
     print(ssrt_s1vc_ah)

     ssd_s2vc_ah = t.test(ssd ~ group3, data = all_stop_sub[all_stop_sub$group3 == "second" | all_stop_sub$group3 == "control",])
     print("Mean SSD - S2 vs. Controls")
     print(ssd_s2vc_ah)

     ssrt_s2vc_ah = t.test(SSRT_me ~ group3, data = all_stop_sub[all_stop_sub$group3 == "second" | all_stop_sub$group3 == "control",])
     print("Mean SSRT - S2 vs. Controls")
     print(ssrt_s2vc_ah)

     ssd_s1vs2_ah = t.test(ssd ~ group3, data = all_stop_sub[all_stop_sub$group3 == "first" | all_stop_sub$group3 == "second",])
     print("Mean SSD - S1 vs. s2")
     print(ssd_s1vs2_ah)

     ssrt_s1vs2_ah = t.test(SSRT_me ~ group3, data = all_stop_sub[all_stop_sub$group3 == "first" | all_stop_sub$group3 == "second",])
     print("Mean SSRT - S1 vs. S2")
     print(ssrt_s1vs2_ah)

     sink()


     # AUSTIN/HOUSTON REPEAT SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/stop_stats_ah_repeat_aug15.txt")

     print(all_stop_sub[all_stop_sub$rep == "r",])

     ssd_repeat_ah = t.test(ssd ~ group2, data = all_stop_sub[all_stop_sub$rep == "r",], paired=T)
     print("Mean SSD - S1 vs. s2 Repeats")
     print(ssd_repeat_ah)

     ssrt_repeat_ah = t.test(SSRT_me ~ group2, data = all_stop_sub[all_stop_sub$rep == "r",], paired=T)
     print("Mean SSRT - S1 vs. S2 Repeats")
     print(ssrt_repeat_ah)

     sink()


     # AUSTIN/HOUSTON UNIQUE SUBS
     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/stop_stats_ah_uniq_aug15.txt")

     print(all_stop_sub[all_stop_sub$group2!="control" & all_stop_sub$rep == "u",])

     ssd_uniq_ah = t.test(ssd ~ group2, data = all_stop_sub[all_stop_sub$group2!="control" & all_stop_sub$rep == "u",])
     print("Mean SSD - S1 vs. s2 Unique")
     print(ssd_uniq_ah)

     ssd_uniq_ah = t.test(SSRT_me ~ group2, data = all_stop_sub[all_stop_sub$group2!="control" & all_stop_sub$rep == "u",])
     print("Mean SSRT  - S1 vs. S2 Unique")
     print(ssd_uniq_ah)

     sink()









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


  # AUSTIN VS. HOUSTON FIRST CONTROLS
    # SSD
      control1.SSD = lme(ssd ~ 1, random = list(~1 | subind, ~1 | city), data = all_stop_sub[all_stop_sub$groups == "A_control" | all_stop_sub$groups == "H_control_first", ], method = "ML")
      control2.SSD = lme(ssd ~ city, random = list(~1 | subind, ~1 | city), data = all_stop_sub[all_stop_sub$groups == "A_control" | all_stop_sub$groups == "H_control_first", ], method = "ML")
      summary(control2.SSD)$tTable

      #anova(control1.SSD)
      #anova(control2.SSD)
      #controls.SSD = anova(control1.SSD, control2.SSD)

      #t.test(both_stop_control$ssd[both_stop_control$city == "Austin"], both_stop_control$ssd[both_stop_control$city == "Houston" & both_stop_control$dup == "FALSE"], alternative = "less") 

    # SSRT
      control1.SSRT = lme(SSRT_me ~ 1, random = list(~1 | subind, ~1 | city), data = all_stop_sub[all_stop_sub$groups == "A_control" | all_stop_sub$groups == "H_control_first", ], method = "ML")
      control2.SSRT = lme(SSRT_me ~ city, random = list(~1 | subind, ~1 | city), data = all_stop_sub[all_stop_sub$groups == "A_control" | all_stop_sub$groups == "H_control_first", ], method = "ML")
      summary(control2.SSRT)$tTable

      #anova(control1.SSRT)
      #anova(control2.SSRT)
      #controls.SSRT = anova(control1.SSRT, control2.SSRT)

      #t.test(both_stop_control$SSRT_me[both_stop_control$city == "Austin"], both_stop_control$SSRT_me[both_stop_control$city == "Houston" & both_stop_control$dup == "FALSE"], alternative = "greater") 


  # AUSTIN FIRST VS. AUSTIN CONTROLS
    # SSD
      mod1.SSD = lme(ssd ~ 1, random = list(~1 | subind), data = A_stop_sub[A_stop_sub$groups == "A_first" | A_stop_sub$groups == "A_control", ], method = "ML")
      mod2.SSD = lme(ssd ~ groups, random = list(~1 | subind), data = A_stop_sub[A_stop_sub$groups == "A_first" | A_stop_sub$groups == "A_control", ], method = "ML")
      summary(mod2.SSD)$tTable

      t.test(ssd ~ groups2, data = A_stop_sub[A_stop_sub$groups2 == "A_first" | A_stop_sub$groups2 == "A_control", ])

    # SSRT
      mod1.SSRT = lme(SSRT_me ~ 1, random = list(~1 | subind), data = A_stop_sub[A_stop_sub$groups == "A_first" | A_stop_sub$groups == "A_control", ], method = "ML")
      mod2.SSRT = lme(SSRT_me ~ groups, random = list(~1 | subind), data = A_stop_sub[A_stop_sub$groups == "A_first" | A_stop_sub$groups == "A_control", ], method = "ML")
      summary(mod2.SSRT)$tTable

      t.test(SSRT_me ~ groups2, A_stop_sub[A_stop_sub$groups2 == "A_first" | A_stop_sub$groups2 == "A_control", ])

  # AUSTIN FIRST VS. AUSTIN & HOUSTON FIRST CONTROLS
    # SSD
      int.cont1.SSD = lme(ssd ~ 1, random = list(~1 | subind), data = all_stop_sub[all_stop_sub$groups == "A_first" | all_stop_sub$groups == "A_control"| all_stop_sub$groups == "H_control_first", ], method = "ML")
      int.cont2.SSD = lme(ssd ~ INT, random = list(~1 | subind), data = all_stop_sub[all_stop_sub$groups == "A_first" | all_stop_sub$groups == "A_control"| all_stop_sub$groups == "H_control_first", ], method = "ML")
      summary(int.cont2.SSD)$tTable

      #anova(int.cont1.SSD)
      #anova(int.cont2.SSD)
      #int.cont.1.2.SSD = anova(int.cont1.SSD, int.cont2.SSD)

    # SSRT
      int.cont3.SSRT = lme(SSRT_me ~ 1, random = list(~1 | subind), data = all_stop_sub[all_stop_sub$groups == "A_first" | all_stop_sub$groups == "A_control"| all_stop_sub$groups == "H_control_first", ], method = "ML")
      int.cont4.SSRT = lme(SSRT_me ~ INT, random = list(~1 | subind), data = all_stop_sub[all_stop_sub$groups == "A_first" | all_stop_sub$groups == "A_control"| all_stop_sub$groups == "H_control_first", ], method = "ML")
      summary(int.cont4.SSRT)$tTable

      #anova(int.cont3.SSRT)
      #anova(int.cont3.SSRT)
      #mod1.mod2.SSRT = anova(int.cont3.SSRT, int.cont3.SSRT)


  # AUSTIN SECOND VS. CONTROLS
    # SSD    
      mod3.SSD = lme(ssd ~ 1, random = list(~1 | subind), data = A_stop_sub[A_stop_sub$groups == "A_second" | A_stop_sub$groups == "A_control", ], method = "ML")
      mod4.SSD = lme(ssd ~ groups, random = list(~1 | subind), data = A_stop_sub[A_stop_sub$groups == "A_second" | A_stop_sub$groups == "A_control", ], method = "ML")
      summary(mod4.SSD)$tTable
      
      t.test(ssd ~ groups, A_stop_sub[A_stop_sub$groups == "A_second" | A_stop_sub$groups == "A_control", ])

    # SSRT
      mod3.SSRT = lme(SSRT_me ~ 1, random = list(~1 | subind), data = A_stop_sub[A_stop_sub$groups == "A_second" | A_stop_sub$groups == "A_control", ], method = "ML")
      mod4.SSRT = lme(SSRT_me ~ groups, random = list(~1 | subind), data = A_stop_sub[A_stop_sub$groups == "A_second" | A_stop_sub$groups == "A_control", ], method = "ML")
      summary(mod4.SSRT)$tTable

      t.test(SSRT_me ~ groups, data= A_stop_sub[A_stop_sub$groups == "A_second" | A_stop_sub$groups == "A_control", ])

  # AUSTIN THIRD VS. CONTROLS
    # SSD    
      mod5.SSD = lme(ssd ~ 1, random = list(~1 | subind), data = A_stop_sub[A_stop_sub$groups == "A_third" | A_stop_sub$groups == "A_control", ], method = "ML")
      mod6.SSD = lme(ssd ~ groups, random = list(~1 | subind), data = A_stop_sub[A_stop_sub$groups == "A_third" | A_stop_sub$groups == "A_control", ], method = "ML")
      summary(mod6.SSD)$tTable


    # SSRT
      mod5.SSRT = lme(SSRT_me ~ 1, random = list(~1 | subind), data = A_stop_sub[A_stop_sub$groups == "A_third" | A_stop_sub$groups == "A_control", ], method = "ML")
      mod6.SSRT = lme(SSRT_me ~ groups, random = list(~1 | subind), data = A_stop_sub[A_stop_sub$groups == "A_third" | A_stop_sub$groups == "A_control", ], method = "ML")
      summary(mod6.SSRT)$tTable


  # AUSTIN FIRST VS. SECOND
    A_stop_int = A_stop_sub[(A_stop_sub$groups2=="A_first" | A_stop_sub$groups=="A_second"| A_stop_sub$groups=="A_third"), ]
    A_stop_int_fs = A_stop_int[(A_stop_sub$groups2=="A_first" | A_stop_sub$groups=="A_second"), ]
    A_stop_int_fs$dup1 = duplicated(A_stop_int_fs$subind)
    A_stop_int_fs$dup2 = duplicated(A_stop_int_fs$subind, fromLast = TRUE)
    A_stop_int_fs$rep = "r"
    A_stop_int_fs$rep[A_stop_int_fs$dup1 == "FALSE" & A_stop_int_fs$dup2 == "FALSE"] = "u"

    A_stop_int_fs = within (A_stop_int_fs, { 
                 subind = factor(subind)
                 groups = factor(groups)
                 groups2 = factor(groups2) 
                 ID = factor(ID) 
                 city = factor(city)
                 })

    A_stop_repeat_fs_subs = length(A_stop_int_fs$subind[A_stop_int_fs$dup1 == "TRUE" | A_stop_int_fs$dup2 == "TRUE"])/2


  # AUSTIN FIRST VS. THIRD
    A_stop_int_ft = A_stop_int[(A_stop_sub$groups=="A_first" | A_stop_sub$groups=="A_third"), ]
    A_stop_int_ft$dup1 = duplicated(A_stop_int_ft$subind)
    A_stop_int_ft$dup2 = duplicated(A_stop_int_ft$subind, fromLast = TRUE)
    A_stop_int_ft$rep = "r"
    A_stop_int_ft$rep[A_stop_int_ft$dup1 == "FALSE" & A_stop_int_ft$dup2 == "FALSE"] = "u"

    A_stop_int_ft = within (A_stop_int_ft, { 
                 subind = factor(subind)
                 groups = factor(groups) 
                 ID = factor(ID) 
                 city = factor(city)
                 })


    A_stop_repeat_ft_subs = length(A_stop_int_ft$subind[A_stop_int_ft$dup1 == "TRUE" | A_stop_int_ft$dup2 == "TRUE"])/2

  # AUSTIN SECOND VS. THIRD
    A_stop_int_st = A_stop_int[(A_stop_sub$groups=="A_second" | A_stop_sub$groups=="A_third"), ]
    A_stop_int_st$dup1 = duplicated(A_stop_int_st$subind)
    A_stop_int_st$dup2 = duplicated(A_stop_int_st$subind, fromLast = TRUE)
    A_stop_int_st$rep = "r"
    A_stop_int_st$rep[A_stop_int_st$dup1 == "FALSE" & A_stop_int_st$dup2 == "FALSE"] = "u"

    A_stop_int_st = within (A_stop_int_st, { 
                 subind = factor(subind)
                 groups = factor(groups) 
                 ID = factor(ID) 
                 city = factor(city)
                 })


    A_stop_repeat_st_subs = length(A_stop_int_st$subind[A_stop_int_st$dup1 == "TRUE" | A_stop_int_st$dup2 == "TRUE"])/2


    #  1 v. 2
      # UNIQUE
        # SSD
          mod7.SSD = lme(ssd ~ 1, random = ~1 | subind, data = A_stop_int_fs[A_stop_int_fs$rep =="u",], method = "ML")
          mod8.SSD = lme(ssd ~ groups, random = ~1 | subind, data = A_stop_int_fs[A_stop_int_fs$rep =="u",], method = "ML")
          summary(mod8.SSD)$tTable
         
          t.test(ssd ~ groups2,data = A_stop_int_fs[A_stop_int_fs$rep =="u",]) 

        # SSRT
          mod7.SSRT = lme(SSRT_me ~ 1, random = ~1 | subind, data = A_stop_int_fs[A_stop_int_fs$rep =="u",], method = "ML")
          mod8.SSRT = lme(SSRT_me ~ groups, random = ~1 | subind, data = A_stop_int_fs[A_stop_int_fs$rep =="u",], method = "ML")
          summary(mod8.SSRT)$tTable

          t.test(SSRT_me ~ groups2, data = A_stop_int_fs[A_stop_int_fs$rep =="u",])

      # REPEAT
        # SSD
          mod9.SSD = lme(ssd ~ 1, random = ~1 | subind, data = A_stop_int_fs[A_stop_int_fs$rep =="r",], method = "ML")
          mod10.SSD = lme(ssd ~ groups, random = ~1 | subind, data = A_stop_int_fs[A_stop_int_fs$rep =="r",], method = "ML")
          summary(mod10.SSD)$tTable

          t.test(ssd ~ groups2, data = A_stop_int_fs[A_stop_int_fs$rep =="r",], paired=T)

        # SSRT
          mod9.SSRT = lme(SSRT_me ~ 1, random = ~1 | subind, data = A_stop_int_fs[A_stop_int_fs$rep =="r",], method = "ML")
          mod10.SSRT = lme(SSRT_me ~ groups, random = ~1 | subind, data = A_stop_int_fs[A_stop_int_fs$rep =="r",], method = "ML")
          summary(mod10.SSRT)$tTable

          t.test(SSRT_me~groups2, data = A_stop_int_fs[A_stop_int_fs$rep =="r",], paired=T)

      # ALL
        # SSD
          mod11.SSD = lme(ssd ~ 1, random = ~1 | subind, data = A_stop_int_fs, method = "ML")
          mod12.SSD = lme(ssd ~ groups, random = ~1 | subind, data = A_stop_int_fs, method = "ML")
          summary(mod12.SSD)$tTable

          t.test(ssd ~ groups2, A_stop_int_fs)

        # SSRT
          mod11.SSRT = lme(SSRT_me ~ 1, random = ~1 | subind, data = A_stop_int_fs, method = "ML")
          mod12.SSRT = lme(SSRT_me ~ groups, random = ~1 | subind, data = A_stop_int_fs, method = "ML")
          summary(mod12.SSRT)$tTable

          t.test(SSRT_me ~ groups2, A_stop_int_fs)


    #  1 v. 3
      # UNIQUE
        # SSD
          mod13.SSD = lme(ssd ~ 1, random = ~1 | subind, data = A_stop_int_ft[A_stop_int_ft$rep == "u", ], method = "ML")
          mod14.SSD = lme(ssd ~ groups, random = ~1 | subind, data = A_stop_int_ft[A_stop_int_ft$rep == "u", ], method = "ML")
          summary(mod14.SSD)$tTable

        # SSRT
          mod13.SSRT = lme(SSRT_me ~ 1, random = ~1 | subind, data = A_stop_int_ft[A_stop_int_ft$rep == "u", ], method = "ML")
          mod14.SSRT = lme(SSRT_me ~ groups, random = ~1 | subind, data = A_stop_int_ft[A_stop_int_ft$rep == "u", ], method = "ML")
          summary(mod14.SSRT)$tTable


      # REPEAT
        # SSD
          mod15.SSD = lme(ssd ~ 1, random = ~1 | subind, data = A_stop_int_ft[A_stop_int_ft$rep == "r", ], method = "ML")
          mod16.SSD = lme(ssd ~ groups, random = ~1 | subind, data = A_stop_int_ft[A_stop_int_ft$rep == "r", ], method = "ML")
          summary(mod16.SSD)$tTable

        # SSRT
          mod15.SSRT = lme(SSRT_me ~ 1, random = ~1 | subind, data = A_stop_int_ft[A_stop_int_ft$rep == "r", ], method = "ML")
          mod16.SSRT = lme(SSRT_me ~ groups, random = ~1 | subind, data = A_stop_int_ft[A_stop_int_ft$rep == "r", ], method = "ML")
          summary(mod16.SSRT)$tTable


      # ALL
        # SSD
          mod23.SSD = lme(ssd ~ 1, random = ~1 | subind, data = A_stop_int_ft, method = "ML")
          mod24.SSD = lme(ssd ~ groups, random = ~1 | subind, data = A_stop_int_ft, method = "ML")
          summary(mod24.SSD)$tTable

        # SSRT
          mod23.SSRT = lme(SSRT_me ~ 1, random = ~1 | subind, data = A_stop_int_ft, method = "ML")
          mod24.SSRT = lme(SSRT_me ~ groups, random = ~1 | subind, data = A_stop_int_ft, method = "ML")
          summary(mod24.SSRT)$tTable


    # 2 v. 3
      # UNIQUE
        # SSD
          mod17.SSD = lme(ssd ~ 1, random = ~1 | subind, data = A_stop_int_st[A_stop_int_st$rep == "u", ], method = "ML")
          mod18.SSD = lme(ssd ~ groups, random = ~1 | subind, data = A_stop_int_st[A_stop_int_st$rep == "u", ], method = "ML")
          summary(mod18.SSD)$tTable

        # SSRT
          mod17.SSRT = lme(SSRT_me ~ 1, random = ~1 | subind, data = A_stop_int_st[A_stop_int_st$rep == "u", ], method = "ML")
          mod18.SSRT = lme(SSRT_me ~ groups, random = ~1 | subind, data = A_stop_int_st[A_stop_int_st$rep == "u", ], method = "ML")
          summary(mod18.SSRT)$tTable

      # REPEAT 
        # SSD
          mod19.SSD = lme(ssd ~ 1, random = ~1 | subind, data = A_stop_int_st[A_stop_int_st$rep == "r", ], method = "ML")
          mod20.SSD = lme(ssd ~ groups, random = ~1 | subind, data = A_stop_int_st[A_stop_int_st$rep == "r", ], method = "ML")
          summary(mod20.SSD)$tTable

        # SSRT
          mod19.SSRT = lme(SSRT_me ~ 1, random = ~1 | subind, data = A_stop_int_st[A_stop_int_st$rep == "r", ], method = "ML")
          mod20.SSRT = lme(SSRT_me ~ groups, random = ~1 | subind, data = A_stop_int_st[A_stop_int_st$rep == "r", ], method = "ML")
          summary(mod20.SSRT)$tTable

      # ALL 
        # SSD
          mod21.SSD = lme(ssd ~ 1, random = ~1 | subind, data = A_stop_int_st, method = "ML")
          mod22.SSD = lme(ssd ~ groups, random = ~1 | subind, data = A_stop_int_st, method = "ML")
          summary(mod22.SSD)$tTable

        # SSRT
          mod21.SSRT = lme(SSRT_me ~ 1, random = ~1 | subind, data = A_stop_int_st, method = "ML")
          mod22.SSRT = lme(SSRT_me ~ groups, random = ~1 | subind, data = A_stop_int_st, method = "ML")
          summary(mod22.SSRT)$tTable




    # HOUSTON UNIQUE CONTROL SUBS
      H_stop_control = H_stop_sub[(H_stop_sub$groups=="H_control_first" | H_stop_sub$groups=="H_control_second" | H_stop_sub$groups=="H_control_third"), ] 
      H_stop_control$dup1 = duplicated(H_stop_control$subind)
      H_stop_control$dup2 = duplicated(H_stop_control$subind, fromLast = TRUE)

      H_stop_control = within (H_stop_control, { 
                       subind = factor(subind)
                       groups = factor(groups) 
                       ID = factor(ID) 
                       city = factor(city)
                       })

      H_stop_control_repeat = H_stop_control[(H_stop_control$dup1 == "TRUE" | H_stop_control$dup2 == "TRUE"), ]

      # SSD
        houston1.SSD = lme(ssd ~ 1, random = list(~1 | subind), data = H_stop_control[((H_stop_control$dup2 == "FALSE" & H_stop_control$groups == "H_control_first") | (H_stop_control$dup1 == "FALSE" & H_stop_control$groups == "H_control_second")), ], method = "ML")
        houston2.SSD = lme(ssd ~ groups, random = list(~1 | subind), data = H_stop_control[((H_stop_control$dup2 == "FALSE" & H_stop_control$groups == "H_control_first") | (H_stop_control$dup1 == "FALSE" & H_stop_control$groups == "H_control_second")), ], method = "ML")        
        ummary(houston2.SSD)$tTable

        #anova(houston1.SSD)
        #anova(houston2.SSD)
        #h.unique.SSD = anova(houston1.SSD, houston2.SSD)

        #t.test(both_stop_control$ssd[both_stop_control$city == "Austin"], both_stop_control$ssd[both_stop_control$city == "Houston" & both_stop_control$dup == "FALSE"], alternative = "less") 

     # SSRT
       houston3.SSRT = lme(SSRT_me ~ 1, random = list(~1 | subind), data = H_stop_control[((H_stop_control$dup2 == "FALSE" & H_stop_control$groups == "H_control_first") | (H_stop_control$dup1 == "FALSE" & H_stop_control$groups == "H_control_second")), ], method = "ML")
       houston4.SSRT = lme(SSRT_me ~ groups, random = list(~1 | subind), data = H_stop_control[((H_stop_control$dup2 == "FALSE" & H_stop_control$groups == "H_control_first") | (H_stop_control$dup1 == "FALSE" & H_stop_control$groups == "H_control_second")), ], method = "ML")
       summary(houston4.SSRT)$tTable

       #anova(houston3.SSRT)
       #anova(houston4.SSRT)
       #h.unique.SSRT = anova(houston3.SSRT, houston4.SSRT)

    # HOUSTON REPEAT CONTROL SUBS
      # SSD
        houston3.SSD = lme(ssd ~ 1, random = list(~1 | subind), data = H_stop_control_repeat, method = "ML")
        houston4.SSD = lme(ssd ~ groups, random = list(~1 | subind), data = H_stop_control_repeat, method = "ML")
        summary(houston4.SSD)$tTable

        #anova(houston3.SSD)
        #anova(houston4.SSD)
        #h.repeat.SSD = anova(houston3.SSD, houston4.SSD)

      # SSRT
        houston5.SSRT = lme(SSRT_me ~ 1, random = list(~1 | subind), data = H_stop_control_repeat, method = "ML")
        houston6.SSRT = lme(SSRT_me ~ groups, random = list(~1 | subind), data = H_stop_control_repeat, method = "ML")
        summary(houston6.SSRT)$tTable

        #anova(houston5.SSRT)
        #anova(houston6.SSRT)
        #h.repeat.SSRT = anova(houston5.SSRT, houston6.SSRT)



 # SSD AND SSRT STAT OUTPUT

  SSD_stats = rbind(a.h.control.SSD, a1.ac.SSD, a1.ahc.SSD, a2.ac.SSD, a1.a2.unique.SSD, a1.a2.repeat.SSD, h1.h2.unique.SSD, h1.h2.repeat.SSD)
  write.csv(SSD_stats, file=sprintf("%s/data_frames/SST/STOP/SSD_stats.csv", wd), na="NA")

  SSRT_stats = rbind(a.h.control.SSRT, a1.ac.SSRT, a1.ahc.SSRT, a2.ac.SSRT, a1.a2.unique.SSRT, a1.a2.repeat.SSRT, h1.h2.unique.SSRT, h1.h2.repeat.SSRT)
  write.csv(SSRT_stats, file=sprintf("%s/data_frames/SST/STOP/SSRT_stats.csv", wd), na="NA")




# STOP FIGURES

    # BY SESSSION

      # AUSTIN

        A_SSRT_box_aug15 = ggplot(data = A_stop_sub[A_stop_sub$group2!="A_third",],aes(x = group2,y = SSRT_me)) + geom_boxplot(aes(fill = group2)) + geom_point(data= A_stop_sub[((A_stop_sub$ID2 =="0" | A_stop_sub$ID2=="1" | A_stop_sub$ID2=="c") & A_stop_sub$group2!="A_third"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ geom_line(data= A_stop_sub[((A_stop_sub$ID2 =="0" | A_stop_sub$ID2=="1") & A_stop_sub$group2!="A_third"),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + ylab("Mean SSRT (s)") 
	# + guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	             ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSRT_box_aug15.pdf",wd),width=5,height=5)

        A_SSRT_box_aug15_noline = ggplot(data = A_stop_sub[A_stop_sub$group2!="A_third",],aes(x = group2,y = SSRT_me)) + geom_boxplot(aes(fill = group2)) + geom_point(data= A_stop_sub[((A_stop_sub$ID2 =="0" | A_stop_sub$ID2=="1" | A_stop_sub$ID2=="c") & A_stop_sub$group2!="A_third"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + ylab("Mean SSRT (s)") 
	# + guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	             ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSRT_box_aug15_noline.pdf",wd),width=5,height=5)





   
        A_SSRT_box_all = ggplot(data = A_stop_sub,aes(x = groups,y = SSRT_me)) + geom_boxplot(aes(alpha = .2)) + geom_point() + geom_line(aes(group = subind, color = ID, linetype = ID, alpha = 1))+ xlab("Session") + 
                         ylab("Mean SSRT (s)") + ggtitle("Austin SSRT")+ guides(group = FALSE, alpha = FALSE) +
                         theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'),
                          axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14),panel.background = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
	             ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSRT_box_all_mri.png",wd),width=10,height=10)
  

        A_SSRT_bar_sf = ggplot(data = A_stop_group[A_stop_group$groups!="A_third",], aes(x = groups, y = SSRT_mean, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                            geom_errorbar(aes(y = SSRT_mean, ymin = SSRT_mean-SSRT_se, ymax = SSRT_mean+SSRT_se), position = position_dodge(.9), width=.2) + xlab("Session") + ylab("Mean SSRT (s)") + 
                            ggtitle("Austin SSRT") +
                            scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", A_stop_group$N[A_stop_group$groups == "A_control"]), sprintf("A_first (%d)",
			           A_stop_group$N[A_stop_group$groups == 'A_first']), sprintf('A_second (%d)',A_stop_group$N[A_stop_group$groups == 'A_second']), sprintf('A_third (%d)',A_stop_group$N[A_stop_group$groups == 'A_third']))) +
                         theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'),
                          axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
                     ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSRT_bar_sf_mri.png",wd),width=10,height=10)


        A_SSRT_bar_all = ggplot(data = A_stop_group, aes(x = groups, y = SSRT_mean, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                            geom_errorbar(aes(y = SSRT_mean, ymin = SSRT_mean-SSRT_se, ymax = SSRT_mean+SSRT_se), position = position_dodge(.9), width=.2) + xlab("Session") + ylab("Mean SSRT (s)") + 
                            ggtitle("Austin SSRT") +
                            scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", A_stop_group$N[A_stop_group$groups == "A_control"]), sprintf("A_first (%d)",
			           A_stop_group$N[A_stop_group$groups == 'A_first']), sprintf('A_second (%d)',A_stop_group$N[A_stop_group$groups == 'A_second']), sprintf('A_third (%d)',A_stop_group$N[A_stop_group$groups == 'A_third']))) +
                         theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'),
                          axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
                     ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSRT_bar_all_mri.png",wd),width=10,height=10)


        A_stopACC_box_aug15 = ggplot(data = A_stop_sub[A_stop_sub$group2!="A_third",],aes(x = group2,y = stop_acc_me)) + geom_boxplot(aes(fill = group2)) + geom_point(data= A_stop_sub[((A_stop_sub$ID2 =="0" | A_stop_sub$ID2=="1" | A_stop_sub$ID2=="c") & A_stop_sub$group2!="A_third"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + geom_line(data= A_stop_sub[((A_stop_sub$ID2 =="0" | A_stop_sub$ID2=="1") & A_stop_sub$group2!="A_third"),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Stop Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
	#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_stopACC_box_aug15.pdf",wd),width=5,height=5)


        A_stopACC_box_aug15_noline = ggplot(data = A_stop_sub[A_stop_sub$group2!="A_third",],aes(x = group2,y = stop_acc_me)) + geom_boxplot(aes(fill = group2)) + geom_point(data= A_stop_sub[((A_stop_sub$ID2 =="0" | A_stop_sub$ID2=="1" | A_stop_sub$ID2=="c") & A_stop_sub$group2!="A_third"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") + ylab("Mean Stop Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
	#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_stopACC_box_aug15_noline.pdf",wd),width=5,height=5)




        A_stopACC_box_all = ggplot(data = A_stop_sub,aes(x = groups,y = stop_acc_me)) + geom_boxplot() + geom_point() + geom_line(aes(group = subind, color = subind, linetype = ID))+ xlab("Session") + 
                            ylab("Mean Stop ACC") + ggtitle("Austin Stop ACC")+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE) +
                            theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'),
                              axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14),panel.background = element_blank(), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_stopACC_box_all.png",wd),width=10,height=10)


        A_stopACC_bar = ggplot(data = A_stop_group, aes(x = groups, y = stop_acc_mean, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                             geom_errorbar(aes(y = stop_acc_mean, ymin = stop_acc_mean-stop_acc_se, ymax = stop_acc_mean+stop_acc_se), position = position_dodge(.9), width=.2) + xlab("Session") + ylab("Mean Stop ACC") + 
                             ggtitle("Austin Stop ACC") +
                             scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", A_stop_group$N[A_stop_group$groups == "A_control"]), sprintf("A_first (%d)",
			           A_stop_group$N[A_stop_group$groups == 'A_first']), sprintf('A_second (%d)',A_stop_group$N[A_stop_group$groups == 'A_second']), sprintf('A_third (%d)',A_stop_group$N[A_stop_group$groups == 'A_third'])))
                      ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/A_stopACC_bar.png",wd),width=10,height=10)


        A_SSD_box_sf = ggplot(data = A_stop_sub[A_stop_sub$groups!="A_third",],aes(x = groups,y = ssd)) + geom_boxplot(aes(alpha = .2)) + geom_point() + geom_line(aes(group = subind, color = ID, linetype = ID, alpha = 1))+ xlab("Session") + 
                             ylab("Mean SSD (s)") + ggtitle("Austin SSD")+ guides(group = FALSE,alpha = FALSE, size = FALSE) +
                             theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'),
                               axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14),panel.background = element_blank(), panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSD_box_sf_mri.png",wd),width=10,height=10)
  
        A_SSD_box_aug15 = ggplot(data = A_stop_sub[A_stop_sub$group2!="A_third",],aes(x = group2,y = ssd)) + geom_boxplot(aes(fill = group2)) + geom_point(data= A_stop_sub[((A_stop_sub$ID2 =="0" | A_stop_sub$ID2=="1" | A_stop_sub$ID2=="c") & A_stop_sub$group2!="A_third"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + geom_line(data= A_stop_sub[((A_stop_sub$ID2 =="0" | A_stop_sub$ID2=="1") & A_stop_sub$group2!="A_third"),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean SSD (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSD_box_aug15.pdf",wd),width=5,height=5)

        A_SSD_box_aug15_noline = ggplot(data = A_stop_sub[A_stop_sub$group2!="A_third",],aes(x = group2,y = ssd)) + geom_boxplot(aes(fill = group2)) + geom_point(data= A_stop_sub[((A_stop_sub$ID2 =="0" | A_stop_sub$ID2=="1" | A_stop_sub$ID2=="c") & A_stop_sub$group2!="A_third"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") + ylab("Mean SSD (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	              ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSD_box_aug15_noline.pdf",wd),width=5,height=5)



 
        A_SSD_box_all = ggplot(data = A_stop_sub,aes(x = groups,y = ssd)) + geom_boxplot(aes(alpha = .2)) + geom_point() + geom_line(aes(group = subind, color = ID, linetype = ID, alpha = 1))+ xlab("Session") + 
                             ylab("Mean SSD (s)") + ggtitle("Austin SSD")+ guides(group = FALSE,alpha = FALSE, size = FALSE) +
                             theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'),
                               axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14),panel.background = element_blank(), panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSD_box_all_mri.png",wd),width=10,height=10)
   

        A_SSD_bar_sf = ggplot(data = A_stop_group[A_stop_group$groups!="A_third",], aes(x = groups, y = ssd_mean, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                            geom_errorbar(aes(y = ssd_mean, ymin = ssd_mean-ssd_se, ymax = ssd_mean+ssd_se), position = position_dodge(.9), width=.2) + xlab("Session") + ylab("Mean SSD (s)") + 
                            ggtitle("Austin SSD") +
                            scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", A_stop_group$N[A_stop_group$groups == "A_control"]), sprintf("A_first (%d)",
			           A_stop_group$N[A_stop_group$groups == 'A_first']), sprintf('A_second (%d)',A_stop_group$N[A_stop_group$groups == 'A_second']), sprintf('A_third (%d)',A_stop_group$N[A_stop_group$groups == 'A_third']))) +
                         theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'),
                          axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
                     ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSD_bar_sf_mri.png",wd),width=10,height=10)




        A_SSD_bar_all = ggplot(data = A_stop_group, aes(x = groups, y = ssd_mean, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                            geom_errorbar(aes(y = ssd_mean, ymin = ssd_mean-ssd_se, ymax = ssd_mean+ssd_se), position = position_dodge(.9), width=.2) + xlab("Session") + ylab("Mean SSD (s)") + 
                            ggtitle("Austin SSD") +
                            scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third"), labels = c(sprintf("A_control (%d)", A_stop_group$N[A_stop_group$groups == "A_control"]), sprintf("A_first (%d)",
			           A_stop_group$N[A_stop_group$groups == 'A_first']), sprintf('A_second (%d)',A_stop_group$N[A_stop_group$groups == 'A_second']), sprintf('A_third (%d)',A_stop_group$N[A_stop_group$groups == 'A_third']))) +
                         theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'),
                          axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
                     ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_SSD_bar_all_mri.png",wd),width=10,height=10)




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


        All_SSRT_box_aug15 = ggplot(data = all_stop_sub[all_stop_sub$group3!="third",],aes(x = group3,y = SSRT_me)) + geom_boxplot(aes(fill = group3)) + geom_point(data= all_stop_sub[((all_stop_sub$ID2 =="0" | all_stop_sub$ID2=="1" | all_stop_sub$ID2=="c") & all_stop_sub$group3!="third"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + geom_line(data= all_stop_sub[((all_stop_sub$ID2 =="0" | all_stop_sub$ID2=="1") & all_stop_sub$group3!="third"),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean SSRT (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
	#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_SSRT_box_aug15.pdf",wd),width=5,height=5)
 

        All_SSRT_box_aug15_noline = ggplot(data = all_stop_sub[all_stop_sub$group3!="third",],aes(x = group3,y = SSRT_me)) + geom_boxplot(aes(fill = group3)) + geom_point(data= all_stop_sub[((all_stop_sub$ID2 =="0" | all_stop_sub$ID2=="1" | all_stop_sub$ID2=="c") & all_stop_sub$group3!="third"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))  + xlab("Group") + ylab("Mean SSRT (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
	#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_SSRT_box_aug15_noline.pdf",wd),width=5,height=5)
 

  

        All_SSRT_bar = ggplot(data = all_stop_group, aes(x = groups, y = SSRT_mean, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                              geom_errorbar(aes(y = SSRT_mean, ymin = SSRT_mean-SSRT_se, ymax = SSRT_mean+SSRT_se), position = position_dodge(.9), width=.2) + xlab("Session") + ylab("Mean SSRT") + 
                              ggtitle("Austin and Houston SSRT") +
                              scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third", "H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("A_control (%d)", all_go_group$N[all_go_group$groups == "A_control"]), 
                                    sprintf("A_first (%d)", all_go_group$N[all_go_group$groups == 'A_first']), sprintf('A_second (%d)',all_go_group$N[all_go_group$groups == 'A_second']), sprintf('A_third (%d)',all_go_group$N[all_go_group$groups == 'A_third']),
                                    sprintf("H_control_first (%d)", all_go_group$N[all_go_group$groups == "H_control_first"]), sprintf("H_control_second (%d)", all_go_group$N[all_go_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", all_go_group$N[all_go_group$groups == "H_control_third"]), 
                                    sprintf("H_first (%d)", all_go_group$N[all_go_group$groups == "H_first"]), sprintf("H_second (%d)",all_go_group$N[all_go_group$groups == "H_second"]), sprintf("H_third (%d)",all_go_group$N[all_go_group$groups == "H_third"])))
                       ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_SSRT_bar.png",wd),width=10,height=10)
 
        All_SSD_box_aug15 = ggplot(data = all_stop_sub[all_stop_sub$group3!="third",],aes(x = group3,y = ssd)) + geom_boxplot(aes(fill = group3)) + geom_point(data= all_stop_sub[((all_stop_sub$ID2 =="0" | all_stop_sub$ID2=="1" | all_stop_sub$ID2=="c") & all_stop_sub$group3!="third"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + geom_line(data= all_stop_sub[((all_stop_sub$ID2 =="0" | all_stop_sub$ID2=="1") & all_stop_sub$group3!="third"),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean SSD (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_SSD_box_aug15.pdf",wd),width=5,height=5)
   
        All_SSD_box_aug15_noline = ggplot(data = all_stop_sub[all_stop_sub$group3!="third",],aes(x = group3,y = ssd)) + geom_boxplot(aes(fill = group3)) + geom_point(data= all_stop_sub[((all_stop_sub$ID2 =="0" | all_stop_sub$ID2=="1" | all_stop_sub$ID2=="c") & all_stop_sub$group3!="third"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") + ylab("Mean SSD (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	               ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/All_SSD_box_aug15_noline.pdf",wd),width=5,height=5)
   





        All_stopACC_box_aug15 = ggplot(data = all_stop_sub[all_stop_sub$group3!="third",],aes(x = group3,y = stop_acc_me)) + geom_boxplot(aes(fill = group3)) + geom_point(data= all_stop_sub[((all_stop_sub$ID2 =="0" | all_stop_sub$ID2=="1" | all_stop_sub$ID2=="c") & all_stop_sub$group3!="third"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + geom_line(data= all_stop_sub[((all_stop_sub$ID2 =="0" | all_stop_sub$ID2=="1") & all_stop_sub$group3!="third"),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Stop Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
	#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	                  ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/All_stopACC_box_aug15.pdf",wd),width=5,height=5)


        All_stopACC_box_aug15_noline = ggplot(data = all_stop_sub[all_stop_sub$group3!="third",],aes(x = group3,y = stop_acc_me)) + geom_boxplot(aes(fill = group3)) + geom_point(data= all_stop_sub[((all_stop_sub$ID2 =="0" | all_stop_sub$ID2=="1" | all_stop_sub$ID2=="c") & all_stop_sub$group3!="third"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") + ylab("Mean Stop Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
	#+ guides(group = FALSE, fill = FALSE, color = FALSE, shape = FALSE, linetype=FALSE)
	                  ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/All_stopACC_box_aug15_noline.pdf",wd),width=5,height=5)




        All_stopACC_bar = ggplot(data = all_stop_group, aes(x = groups, y = stop_acc_mean, group = groups, fill = groups)) + geom_bar(position = "dodge", stat = "identity") + 
                                 geom_errorbar(aes(y = stop_acc_mean, ymin = stop_acc_mean-stop_acc_se, ymax = stop_acc_mean+stop_acc_se), position = position_dodge(.9), width=.2) + xlab("Session") + ylab("Mean Stop ACC") + 
                                 ggtitle("Austin and Houston Stop ACC") +
                              scale_fill_discrete(name = "groups", breaks = c("A_control", "A_first", "A_second", "A_third", "H_control_first", "H_control_second", "H_control_third", "H_first", "H_second", "H_third"), labels = c(sprintf("A_control (%d)", all_go_group$N[all_go_group$groups == "A_control"]), 
                                    sprintf("A_first (%d)", all_go_group$N[all_go_group$groups == 'A_first']), sprintf('A_second (%d)',all_go_group$N[all_go_group$groups == 'A_second']), sprintf('A_third (%d)',all_go_group$N[all_go_group$groups == 'A_third']),
                                    sprintf("H_control_first (%d)", all_go_group$N[all_go_group$groups == "H_control_first"]), sprintf("H_control_second (%d)", all_go_group$N[all_go_group$groups == "H_control_second"]), sprintf("H_control_third (%d)", all_go_group$N[all_go_group$groups == "H_control_third"]), 
                                    sprintf("H_first (%d)", all_go_group$N[all_go_group$groups == "H_first"]), sprintf("H_second (%d)",all_go_group$N[all_go_group$groups == "H_second"]), sprintf("H_third (%d)",all_go_group$N[all_go_group$groups == "H_third"])))                                 
                          ggsave(filename=sprintf("%s/figures/Project_4/SST/ACC/All_stopACC_bar.png",wd),width=10,height=10)

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