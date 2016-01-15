# Authors: Leo Olmedo (first); Mary Abbe Roe & Joel Martinez Dec. 2013
# This script reads in behavioral data for SC and calculates RT and ACC


#### LIBRARIES, FUNCTIONS, READING IN DATA  ####
 
  library(ggplot2)
  library(plyr)

  wd=getwd()

 
# FUNCTIONS   
  
  # call summarySE function to use later in ggplot and grt mean, sd, and se


  summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    require(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary; it's not easy to understand...
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun= function(xx, col, na.rm) {
                           c( N    = length2(xx[,col], na.rm=na.rm),
                              mean = mean   (xx[,col], na.rm=na.rm),
                              sd   = sd     (xx[,col], na.rm=na.rm)
                              )
                          },
                    measurevar,
                    na.rm
             )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean"=measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: dat_all_5

    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
  }


  # function for extracting end of string
    substrRight=function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }

  # function for extracting start of string
    substrLeft=function(x, n){
    substr(x, 1, n)
  }



# READING IN DATA AND CREATING DATA FRAMES FOR EACH GROUP

  # For Houston: subdirs=Sys.glob("/corral-repl/utexas/ldrc/H_*MR1")
  # For Austin: subdirs=Sys.glob("/corral-repl/utexas/ldrc/ldrc_?_[0-9][0-9][0-9]") or 
  #             subdirs=Sys.glob("/corral-repl/utexas/ldrc/ldrc_*second")


  group=c("ldrc_[0-9]_[0-9][0-9][0-9]", "ldrc_*second", "PHILIPS/H_LD*_[0-9]", "H_LD*_[0-9]_second", 
          "ldrc_c_[0-9][0-9][0-9]", "PHILIPS/H_LD*[0-9][0-9][0-9][0-9]", "H_LD*_c_second")
  chars=c(5,12,6,13,5,4,13)


  # GROUP
  for (i in 1:length(group)){ 
    subdirs=Sys.glob(sprintf("/corral-repl/utexas/ldrc/%s" ,group[i]))
    rm("dat_all")

    # SUB
    for (sub in subdirs){
      behav_dirs=Sys.glob(sprintf("%s/behav/SC/SC_R*", sub))
      subnum=substrRight(sub, chars[i])

	# RUN
        for (run in behav_dirs){
          run_num=substrRight(run,1)
          Rfile=Sys.glob(sprintf("%s/*R.txt", run))

          if (length(Rfile)==0){
            warning(sprintf("cannot find R.txt file for %s", run))
	    next
          }

          dat_loop=read.table(Rfile, header=TRUE, sep="\t", na.strings="NaN")
          dat_loop$subind=rep(subnum, dim(dat_loop)[1])
          dat_loop$runnum=rep(run_num, dim(dat_loop)[1])

          # CREATE DAT_ALL IF DOESN'T EXIST
          if (exists("dat_all")==FALSE){
            dat_all=dat_loop 
          } 
          else{ 
            dat_all=rbind(dat_all, dat_loop)
          }
     
       } # END RUN LOOP
     }  # END SUB LOOP



    # Correct: 1=yes, 0=no, 3=mismatch or RT out of range, 4=no response
    # Cond: 1='active_s', 2='active_ns', 3='passive_s', 4='passive_ns'

    dat_all$cond_type=dat_all$Cond
    dat_all$cond_type[dat_all$Cond==1]='active_s'
    dat_all$cond_type[dat_all$Cond==2]='active_ns'
    dat_all$cond_type[dat_all$Cond==3]='passive_s'
    dat_all$cond_type[dat_all$Cond==4]='passive_ns'

    # PICKS OUT FIRST OF REPEAT TRIALS??? REVIEW!
    indicator=c(dat_all$TrialNum,0)-c(0, dat_all$TrialNum)
    indicator[indicator!=0]=1
    dat_all$first=indicator[1:(length(indicator)-1)]

    # FIRST CORRECT REPEAT TRIAL
    dat_all$correct_first=0*dat_all$correct
    dat_all$correct_first[dat_all$correct==1 & dat_all$first==1]=1

    # SHOULD IT BE CHANGED TO THE 0s, NOT CORRECT TRIALS? ONLY TIME USED?
    cond_first=dat_all$cond_type
    cond_first[dat_all$correct_first==1]=NA

    # CREATE SEPARATE DATA FRAME FOR EACH GROUP
    assign(paste("dat_all",i,sep="_"),dat_all)

  } # END GROUP LOOP


# DATA FRAMES CREATED  
  # dat_all_1 = Austin session 1 subs
  # dat_all_2 = Austin session 2 subs
  # dat_all_3 = Houston session 1 subs
  # dat_all_4 = Houston session 2 subs
  # dat_all_5 = Austin controls
  # dat_all_6 = Houston controls session 1
  # dat_all_7 = Houston controls session 2

  # to see what variables are in the data frame:  names(dat_all)
  # to see what subjects are here: table(dat_all$subind)


# SET LOWER CUTOFFS, SET TO 0 FOR NO CUTOFFS (prop_correct's = .6)

  run_RT_cutoff=0
  cond_RT_cutoff=0

  prop_correct_cutoff=0
  prop_correct_cond_cutoff=0


# CREATE SAMPLE SIZE VARIABLES (7 groups; sub names, type, number)

  sess_1_subs=names(table(dat_all_1$subind))
  sess_1_sub_num=length(sess_1_subs)
  sess_1_subs_type=substrLeft(names(table(dat_all_1$subind)),1)

  sess_2_subs=substrLeft(names(table(dat_all_2$subind)),5)
  sess_2_sub_num=length(sess_2_subs)
  sess_2_subs_type=substrLeft(names(table(dat_all_2$subind)),1)

  H_sess_1_subs=names(table(dat_all_3$subind))
  H_sess_1_sub_num=length(H_sess_1_subs)

  H_sess_2_subs=substrLeft(names(table(dat_all_4$subind)),11)
  H_sess_2_sub_num=length(H_sess_2_subs)

  control_subs=names(table(dat_all_5$subind))
  control_sub_num=length(control_subs)
  control_subs_type=substrLeft(names(table(dat_all_5$subind)),1)

  control_H_sess_1_subs=names(table(dat_all_6$subind))
  control_H_sess_1_sub_num=length(control_H_sess_1_subs)

  control_H_sess_2_subs=substrLeft(names(table(dat_all_7$subind)),4)
  control_H_sess_2_sub_num=length(control_H_sess_2_subs)

  all_sub_num=sum(sess_1_sub_num, sess_2_sub_num, H_sess_1_sub_num, H_sess_2_sub_num,
                  control_sub_num, control_H_sess_1_sub_num, control_H_sess_2_sub_num)



#######################################################################################

RT Analysis

#######################################################################################
# calculates median RT across runs, mean across runs for each sub, median by sent cond

# CREATE EMPTY MATRICES FOR ALL_COND (not in loop because don't want it to recreate)
  cond_RT_active_s=matrix(0,all_sub_num,1)
  cond_RT_active_ns=matrix(0,all_sub_num,1)
  cond_RT_passive_s=matrix(0,all_sub_num,1)
  cond_RT_passive_ns=matrix(0,all_sub_num,1)


# RT LOOP

  sessions=length(group)

  for (k in 1:sessions){

	if (k==1){
	dat_all=dat_all_1
	n="first"
	x=1:sess_1_sub_num

	} else {

	if (k==2){
	dat_all=dat_all_2
	n="second"
	x=(sess_1_sub_num+1):(sess_1_sub_num+sess_2_sub_num)

	} else {

	if (k==3){
	dat_all=dat_all_3
	n="H_first"
	x=(sess_1_sub_num+sess_2_sub_num+1):(sess_1_sub_num+sess_2_sub_num+H_sess_1_sub_num)

	} else {

	if (k==4){
	dat_all=dat_all_4
	n="H_second"
	x=(sess_1_sub_num+sess_2_sub_num+H_sess_1_sub_num+1):
          (sess_1_sub_num+sess_2_sub_num+H_sess_1_sub_num+H_sess_2_sub_num)

	} else {

	if (k==5){
	dat_all=dat_all_5
	n="control"
	x=(sess_1_sub_num+sess_2_sub_num+H_sess_1_sub_num+H_sess_2_sub_num+1):
          (sess_1_sub_num+sess_2_sub_num+H_sess_1_sub_num+H_sess_2_sub_num+control_sub_num)

	} else {

	if (k==6){
	dat_all=dat_all_6
	n="control_H_first"
	x=(sess_1_sub_num+sess_2_sub_num+H_sess_1_sub_num+H_sess_2_sub_num+control_sub_num+1):
          (sess_1_sub_num+sess_2_sub_num+H_sess_1_sub_num+H_sess_2_sub_num+control_sub_num+control_H_sess_1_sub_num)

        } else {

	if (k==7){
	dat_all=dat_all_7
	n="control_H_second"
	x=(sess_1_sub_num+sess_2_sub_num+H_sess_1_sub_num+H_sess_2_sub_num+control_sub_num+control_H_sess_1_sub_num+1):
           all_sub_num

	}
	}
	}
	}
        }
        }
        }

    rm("run_RT")
    rm("cond_RT")


    # MEDIAN RT FOR EACH RUN

      run_med_df=ddply(dat_all, .(subind, correct_first, runnum), summarise, N=length(subind), median=median(RT, na.rm=TRUE))
      run_med_df=run_med_df[!(run_med_df$correct_first==0),]      
      assign(paste("run_med_df",k,sep="_"),run_med_df)
    
      run_mean_df=ddply(run_med_df, .(runnum), summarise, N=length(runnum), mean=mean(median), se=sd(median)/sqrt(N))
      assign(paste("run_mean_df",k,sep="_"),run_mean_df)

    # MEDIAN RT BY SENTENCE TYPES

      cond_med_df=ddply(dat_all, .(subind, correct_first, cond_type), summarise, N=length(subind), median=median(RT, na.rm=TRUE))
      cond_med_df=cond_med_df[!(cond_med_df$correct_first==0),]
      assign(paste("cond_med_df",k,sep="_"),cond_med_df)

      cond_mean_df=ddply(cond_med_df, .(cond_type), summarise, N=length(cond_type), mean=mean(median), se=sd(median)/sqrt(N))
      assign(paste("cond_mean_df",k,sep="_"),cond_mean_df)

    # MEDIAN RT ACROSS ALL TRIALS FOR EACH SUB

      sub_med_df=ddply(dat_all, .(subind, correct_first), summarise, N=length(subind), median=median(RT, na.rm=TRUE))
      sub_med_df=sub_med_df[!(sub_med_df$correct_first==0),]
      assign(paste("sub_med_df",k,sep="_"),sub_med_df)





    # CREATE VARIABLES AND EMPTY DATA FRAME
    sub_nums=names(table(dat_all$subind))
    run_nums=c("1", "2", "3")
    sent_names=c("active_s", "active_ns", "passive_s", "passive_ns")

    run_RT=data.frame(matrix(0,nrow=length(sub_nums), ncol=length(run_nums)))
    colnames(run_RT)=c("Run_1", "Run_2", "Run_3")
    rownames(run_RT)=c(sub_nums)

    # FILL run_RT DF WITH EACH MEDIAM RT FOR EACH RUN
    for (i in 1:length(sub_nums)){    
      for (j in 1:length(run_nums)){
        run_RT[i,j]=
        c( median(dat_all$RT[dat_all$correct_first==1 & (dat_all$cond_type=="active_s" | dat_all$cond_type=="active_ns"| 
           dat_all$cond_type=="passive_s" | dat_all$cond_type=="passive_ns") & dat_all$subind==sub_nums[i] & dat_all$run==run_nums[j]], na.rm=TRUE))
      }
    }


    # MEDIAN ACROSS RUNS FOR EACH SUB   
    sub_median=c("median")    

    med_run_RT=data.frame(matrix(0, nrow=length(sub_nums), ncol=1))  
    colnames(med_run_RT)=c(sub_median)
    rownames(med_run_RT)=c(sub_nums)

    for (i in 1:length(sub_nums)){    
      for (j in 1:length(sub_median)){
        med_run_RT[i,j]=c(median(dat_all$RT[dat_all$correct_first==1 & dat_all$subind==sub_nums[i]]))
      }
    }

    run_RT=cbind(run_RT, med_run_RT)

    # APPLY RUN_RT CUTOFF

    #  if (run_RT_cutoff==0){
    #    assign(paste("run_RT",n,sep="_"),run_RT)
    #  } else {        
    #      if (run_RT_cutoff>0){
    #        run_RT_cutoff_mask=run_RT<run_RT_cutoff

    #	     run_RT[run_RT_cutoff_mask]=NA
    # 	     run_RT$median <- rowMeans(run_RT, na.rm = TRUE)

    #	     assign(paste("run_RT",n,sep="_"),run_RT)
    #      }
    #  }


  # MEDIAN RT BY SENTENCE CONDITION (Use cond_RT for figures. Session 1 = cond_RT_first and Session 2 = cond_RT_second)

    # CREATE EMPTY cond_RT DATA FRAME
    cond_RT=data.frame(matrix(0,nrow=length(sub_nums), ncol=4))
    names(cond_RT)=sent_names 
    rownames(cond_RT)=c(sub_nums)


    # FILL IN cond_RT
    # use all first trials for this sub within this sent type - WHY? ; use dat_loop[,7] because RT is column 7
    for (i in 1:length(sub_nums)){
      for (sent in c(1:4)){
        dat_loop=dat_all[(dat_all$subind==sub_nums[i] & dat_all$first==1 & dat_all$cond_type==sent_names[sent])& dat_all$correct==1 , ]
        cond_RT[i,sent]= median(dat_loop[,7])
      }
    }


    # MEDIAN ACROSS SENTENCE TYPES
    sub_median=c("median")    

    med_cond_RT=data.frame(matrix(0, nrow=length(sub_nums), ncol=1))  
    colnames(med_cond_RT)=c(sub_median)
    rownames(med_cond_RT)=c(sub_nums)

    for (i in 1:length(sub_nums)){    
      for (j in 1:length(sub_median)){
        med_cond_RT[i,j]=c(median(dat_all$RT[dat_all$first==1 & dat_all$subind==sub_nums[i] & dat_all$correct==1]))
      }
    }

    cond_RT=cbind(cond_RT, med_cond_RT)



    # APPLY COND_RT CUTOFF

    #  if (cond_RT_cutoff==0){
    #    assign(paste("cond_RT",n,sep="_"),cond_RT)
    #  } else {
    #    if (cond_RT_cutoff>0){	  
    #      remove_row=matrix(1:length(cond_RT$mean))

    #	  cond_RT_cutoff_mask=cond_RT$mean<cond_RT_cutoff
    #	  remove_row=remove_row[cond_RT_cutoff_mask]

    #	  cond_RT[remove_row,]=NA
    #	  cond_RT$mean <- rowMeans(cond_RT, na.rm = TRUE)

    #	  assign(paste("cond_RT",n,sep="_"),cond_RT)
    #    }
    #    }



  # CONDITION VECTORS FOR ALL SUBJECTS; ADDS NEXT GROUP AS GOES THROUGH LOOP; MEDIAN VALUES FOR EACH SUB FOR EACH SENTENCE TYPE

    cond_RT_active_s[x]=cond_RT$active_s
    cond_RT_active_ns[x]=cond_RT$active_ns
    cond_RT_passive_s[x]=cond_RT$passive_s
    cond_RT_passive_ns[x]=cond_RT$passive_ns


  } # END OF RT LOOP



# to check data frames for RT (in R in terminal); data frames use for figures
  # by condition:  cond_RT* where * is _control, _control_H_first,_control_H_second,_first, _second, _H_first, _H_second
  # by run:   run_RT* where * is _control, _control_H_first,_control_H_second,_first, _second, _H_first, _H_second


############################ ACCURACY ANALYSIS  #############################



############## RT FIGURES ################

##### Make identifiers for type of session

sess_1=rep("session_1",length(run_RT_first$mean))
sess_2=rep("session_2",length(run_RT_second$mean))
controls=rep("session_control",length(run_RT_control$mean))
H_sess_1=rep("H_session_1",length(run_RT_H_first$mean))
H_sess_2=rep("H_session_2",length(run_RT_H_second$mean))

A_all_sess_RT=c(run_RT_first$mean,run_RT_second$mean,run_RT_control$mean)
A_all_sess_subs=c(sess_1_subs,sess_2_subs,control_subs)
A_all_sess_type=c(sess_1,sess_2,controls)
A_all_subs_type=c(sess_1_subs_type,sess_2_subs_type,control_subs_type)

H_all_sess_RT=c(run_RT_H_first$mean,run_RT_H_second$mean,run_RT_control$mean)
H_all_sess_subs=c(H_sess_1_subs,H_sess_2_subs,control_subs)
H_all_sess_type=c(H_sess_1,H_sess_2,controls)


all_sess_RT=c(run_RT_first$mean,run_RT_second$mean,run_RT_H_first$mean,run_RT_H_second$mean,run_RT_control$mean)
all_sess_subs=c(sess_1_subs,sess_2_subs,H_sess_1_subs,H_sess_2_subs,control_subs)
all_sess_type=c(sess_1,sess_2,H_sess_1,H_sess_2,controls)

comb_sess_1=c(run_RT_first$mean,run_RT_H_first$mean)
comb_sess_2=c(run_RT_second$mean,run_RT_H_second$mean)

comb_sess_RT=c(comb_sess_1,comb_sess_2,run_RT_control$mean)
comb_sess_subs=c(sess_1_subs,H_sess_1_subs,sess_2_subs,H_sess_2_subs,control_subs)
comb_sess_type=c(rep("comb_sess_1",sum(length(run_RT_first$mean),length(run_RT_H_first$mean))),rep("comb_sess_2",sum(length(run_RT_second$mean),length(run_RT_H_second$mean))),controls)

 


#### AVG RT BOX PLOTS

	#### AUSTIN SUBJS

		dat_A_all_sess_RT=data.frame(A_all_sess_RT,A_all_sess_subs,A_all_sess_type)

		A_all_sess_RT_plot=ggplot(dat_A_all_sess_RT,aes(x=A_all_sess_type,y=A_all_sess_RT)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("AUSTIN AVG SC RT")+ guides(group=FALSE,color=FALSE)

		ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/RT/AVG_RT/avg_rt_group_comparison.png",wd),width=10,height=10)


	#### HOUSTON SUBJS

		dat_H_all_sess_RT=data.frame(H_all_sess_RT,H_all_sess_subs,H_all_sess_type)

		H_all_sess_RT_plot=ggplot(dat_H_all_sess_RT,aes(x=H_all_sess_type,y=H_all_sess_RT)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("HOUSTON AVG SC RT")+ guides(group=FALSE,color=FALSE)

		ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/RT/AVG_RT/avg_rt_group_comparison.png",wd),width=10,height=10)


	#### ALL SUBJS


		dat_all_sess_RT=data.frame(all_sess_RT,all_sess_subs,all_sess_type)

		all_sess_RT_plot=ggplot(dat_all_sess_RT,aes(x=all_sess_type,y=all_sess_RT)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("ALL AVG SC RT")+ guides(group=FALSE,color=FALSE)

		ggsave(filename=sprintf("%s/figures/Project_4/All/SC/RT/AVG_RT/avg_rt_group_comparison.png",wd),width=10,height=10)


	#### COMBINED SUBJECTS

		
		dat_comb_sess_RT=data.frame(comb_sess_RT,comb_sess_subs,comb_sess_type)

		comb_sess_RT_plot=ggplot(dat_comb_sess_RT,aes(x=comb_sess_type,y=comb_sess_RT)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_sess_subs,color=comb_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("COMBINED AVG SC RT")+ guides(group=FALSE,color=FALSE)

		ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/RT/AVG_RT/avg_rt_group_comparison.png",wd),width=10,height=10)


#### AVG RT BAR GRAPHS W/ ERROR BARS

	#### AUSTIN SUBJS

		dat_A_all_sess_RT=data.frame(A_all_sess_RT,A_all_sess_subs,A_all_sess_type)

		dat_A_all_sess_se_RT=summarySE(dat_A_all_sess_RT, measurevar="A_all_sess_RT", groupvars=c("A_all_sess_type"))	

		ggplot(dat_A_all_sess_se_RT, aes(x=A_all_sess_type, y=A_all_sess_RT, fill=A_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=A_all_sess_RT-se, ymax=A_all_sess_RT+se),width=.2, position=position_dodge(.9))

		ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/RT/AVG_RT/avg_rt_se_group_comparison.png",wd),width=10,height=10)


	#### HOUSTON SUBJS

		dat_H_all_sess_RT=data.frame(H_all_sess_RT,H_all_sess_subs,H_all_sess_type)

		dat_H_all_sess_se_RT=summarySE(dat_H_all_sess_RT, measurevar="H_all_sess_RT", groupvars=c("H_all_sess_type"))	

		ggplot(dat_H_all_sess_se_RT, aes(x=H_all_sess_type, y=H_all_sess_RT, fill=H_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=H_all_sess_RT-se, ymax=H_all_sess_RT+se),width=.2, position=position_dodge(.9))

		ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/RT/AVG_RT/avg_rt_se_group_comparison.png",wd),width=10,height=10)


	#### ALL SUBJS

		dat_all_sess_RT=data.frame(all_sess_RT,all_sess_subs,all_sess_type)

		dat_all_sess_se_RT=summarySE(dat_all_sess_RT, measurevar="all_sess_RT", groupvars=c("all_sess_type"))	

		ggplot(dat_all_sess_se_RT, aes(x=all_sess_type, y=all_sess_RT, fill=all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=all_sess_RT-se, ymax=all_sess_RT+se),width=.2, position=position_dodge(.9))

		ggsave(filename=sprintf("%s/figures/Project_4/All/SC/RT/AVG_RT/avg_rt_se_group_comparison.png",wd),width=10,height=10)


	#### COMBINED SUBJS

		dat_comb_sess_RT=data.frame(comb_sess_RT,comb_sess_subs,comb_sess_type)

		dat_comb_sess_se_RT=summarySE(dat_comb_sess_RT, measurevar="comb_sess_RT", groupvars=c("comb_sess_type"))	

		ggplot(dat_comb_sess_se_RT, aes(x=comb_sess_type, y=comb_sess_RT, fill=comb_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=comb_sess_RT-se, ymax=comb_sess_RT+se),width=.2, position=position_dodge(.9))

		ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/RT/AVG_RT/avg_rt_se_group_comparison.png",wd),width=10,height=10)


##### AUSTIN FIGURES


### RT by condition dataframe - BOX PLOTS & BAR GRAPHS

	##Active Sensible
	
		## BOX PLOT
	
			A_cond_RT_active_s=cond_RT_active_s[all_sess_type=="session_1" | all_sess_type=="session_2" | all_sess_type=="session_control"]
			dat_A_cond_RT_active_s=data.frame(A_cond_RT_active_s,A_all_sess_subs,A_all_sess_type)

			A_cond_RT_active_s_plot=ggplot(dat_A_cond_RT_active_s,aes(x=A_all_sess_type,y=A_cond_RT_active_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("AUSTIN AVG ACTIVE S RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/RT/COND_RT/A_active_s_rt_group_comparison.png",wd),width=10,height=10)


		## BAR GRAPH

			dat_A_cond_se_active_s_RT=summarySE(dat_A_cond_RT_active_s, measurevar="A_cond_RT_active_s", groupvars=c("A_all_sess_type"))

			ggplot(dat_A_cond_se_active_s_RT, aes(x=A_all_sess_type, y=A_cond_RT_active_s, fill=A_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=A_cond_RT_active_s-se, ymax=A_cond_RT_active_s+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/RT/COND_RT/A_active_s_rt_se_group_comparison.png",wd),width=10,height=10)

	##Active Non-sensible

		## BOX PLOT

			A_cond_RT_active_ns=cond_RT_active_ns[all_sess_type=="session_1" | all_sess_type=="session_2" | all_sess_type=="session_control"]
			dat_A_cond_RT_active_ns=data.frame(A_cond_RT_active_ns,A_all_sess_subs,A_all_sess_type)

			A_cond_RT_active_ns_plot=ggplot(dat_A_cond_RT_active_ns,aes(x=A_all_sess_type,y=A_cond_RT_active_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("AUSTIN AVG ACTIVE NS RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/RT/COND_RT/A_active_ns_rt_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH

			dat_A_cond_se_active_ns_RT=summarySE(dat_A_cond_RT_active_ns, measurevar="A_cond_RT_active_ns", groupvars=c("A_all_sess_type"))

			ggplot(dat_A_cond_se_active_ns_RT, aes(x=A_all_sess_type, y=A_cond_RT_active_ns, fill=A_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=A_cond_RT_active_ns-se, ymax=A_cond_RT_active_ns+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/RT/COND_RT/A_active_ns_rt_se_group_comparison.png",wd),width=10,height=10)

	##Passive Sensible

		## BOX PLOT

			A_cond_RT_passive_s=cond_RT_passive_s[all_sess_type=="session_1" | all_sess_type=="session_2" | all_sess_type=="session_control"]
			dat_A_cond_RT_passive_s=data.frame(A_cond_RT_passive_s,A_all_sess_subs,A_all_sess_type)

			A_cond_RT_passive_s_plot=ggplot(dat_A_cond_RT_passive_s,aes(x=A_all_sess_type,y=A_cond_RT_passive_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("AUSTIN AVG PASSIVE S RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/RT/COND_RT/A_passive_s_rt_group_comparison.png",wd),width=10,height=10)


		## BAR GRAPH

			dat_A_cond_se_passive_s_RT=summarySE(dat_A_cond_RT_passive_s, measurevar="A_cond_RT_passive_s", groupvars=c("A_all_sess_type"))

			ggplot(dat_A_cond_se_passive_s_RT, aes(x=A_all_sess_type, y=A_cond_RT_passive_s, fill=A_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=A_cond_RT_passive_s-se, ymax=A_cond_RT_passive_s+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/RT/COND_RT/A_passive_s_rt_se_group_comparison.png",wd),width=10,height=10)

	##Passive Non-sensible
		
		## BOX PLOT
	
			A_cond_RT_passive_ns=cond_RT_passive_ns[all_sess_type=="session_1" | all_sess_type=="session_2" | all_sess_type=="session_control"]
			dat_A_cond_RT_passive_ns=data.frame(A_cond_RT_passive_ns,A_all_sess_subs,A_all_sess_type)

			A_cond_RT_passive_ns_plot=ggplot(dat_A_cond_RT_passive_ns,aes(x=A_all_sess_type,y=A_cond_RT_passive_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("AUSTIN AVG PASSIVE NS RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/RT/COND_RT/A_passive_ns_rt_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH

			dat_A_cond_se_passive_ns_RT=summarySE(dat_A_cond_RT_passive_ns, measurevar="A_cond_RT_passive_ns", groupvars=c("A_all_sess_type"))

			ggplot(dat_A_cond_se_passive_ns_RT, aes(x=A_all_sess_type, y=A_cond_RT_passive_ns, fill=A_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=A_cond_RT_passive_ns-se, ymax=A_cond_RT_passive_ns+se),width=.2, position=position_dodge(.9))
			
			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/RT/COND_RT/A_passive_ns_rt_se_group_comparison.png",wd),width=10,height=10)



###### HOUSTON SUBJS


### RT by condition dataframe

	##Active Sensible

		## BOX PLOT

			H_cond_RT_active_s=cond_RT_active_s[all_sess_type=="H_session_1" | all_sess_type=="H_session_2" | all_sess_type=="session_control"]
			dat_H_cond_RT_active_s=data.frame(H_cond_RT_active_s,H_all_sess_subs,H_all_sess_type)

			H_cond_RT_active_s_plot=ggplot(dat_H_cond_RT_active_s,aes(x=H_all_sess_type,y=H_cond_RT_active_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("HOUSTON AVG ACTIVE S RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/RT/COND_RT/H_active_s_rt_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH

			dat_H_cond_se_active_s_RT=summarySE(dat_H_cond_RT_active_s, measurevar="H_cond_RT_active_s", groupvars=c("H_all_sess_type"))

			ggplot(dat_H_cond_se_active_s_RT, aes(x=H_all_sess_type, y=H_cond_RT_active_s, fill=H_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=H_cond_RT_active_s-se, ymax=H_cond_RT_active_s+se),width=.2, position=position_dodge(.9))
			
			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/RT/COND_RT/H_active_s_rt_se_group_comparison.png",wd),width=10,height=10)


	##Active Non-sensible

		## BOX PLOT	

			H_cond_RT_active_ns=cond_RT_active_ns[all_sess_type=="H_session_1" | all_sess_type=="H_session_2" | all_sess_type=="session_control"]
			dat_H_cond_RT_active_ns=data.frame(H_cond_RT_active_ns,H_all_sess_subs,H_all_sess_type)

			H_cond_RT_active_ns_plot=ggplot(dat_H_cond_RT_active_ns,aes(x=H_all_sess_type,y=H_cond_RT_active_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("HOUSTON AVG ACTIVE NS RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/RT/COND_RT/H_active_ns_rt_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH
			
			dat_H_cond_se_active_ns_RT=summarySE(dat_H_cond_RT_active_ns, measurevar="H_cond_RT_active_ns", groupvars=c("H_all_sess_type"))

			ggplot(dat_H_cond_se_active_ns_RT, aes(x=H_all_sess_type, y=H_cond_RT_active_ns, fill=H_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=H_cond_RT_active_ns-se, ymax=H_cond_RT_active_ns+se),width=.2, position=position_dodge(.9))
			
			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/RT/COND_RT/H_active_ns_rt_se_group_comparison.png",wd),width=10,height=10)



	##Passive Sensible

		## BOX PLOT
		
			H_cond_RT_passive_s=cond_RT_passive_s[all_sess_type=="H_session_1" | all_sess_type=="H_session_2" | all_sess_type=="session_control"]
			dat_H_cond_RT_passive_s=data.frame(H_cond_RT_passive_s,H_all_sess_subs,H_all_sess_type)

			H_cond_RT_passive_s_plot=ggplot(dat_H_cond_RT_passive_s,aes(x=H_all_sess_type,y=H_cond_RT_passive_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("HOUSTON AVG PASSIVE S RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/RT/COND_RT/H_passive_s_rt_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH

			dat_H_cond_se_passive_s_RT=summarySE(dat_H_cond_RT_passive_s, measurevar="H_cond_RT_passive_s", groupvars=c("H_all_sess_type"))

			ggplot(dat_H_cond_se_passive_s_RT, aes(x=H_all_sess_type, y=H_cond_RT_passive_s, fill=H_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=H_cond_RT_passive_s-se, ymax=H_cond_RT_passive_s+se),width=.2, position=position_dodge(.9))
			
			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/RT/COND_RT/H_passive_s_rt_se_group_comparison.png",wd),width=10,height=10)


	##Passive Non-sensible

		## BOX PLOT

			H_cond_RT_passive_ns=cond_RT_passive_ns[all_sess_type=="H_session_1" | all_sess_type=="H_session_2" | all_sess_type=="session_control"]
			dat_H_cond_RT_passive_ns=data.frame(H_cond_RT_passive_ns,H_all_sess_subs,H_all_sess_type)
	
			H_cond_RT_passive_ns_plot=ggplot(dat_H_cond_RT_passive_ns,aes(x=H_all_sess_type,y=H_cond_RT_passive_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("HOUSTON AVG PASSIVE NS RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/RT/COND_RT/H_passive_ns_rt_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH

			dat_H_cond_se_passive_ns_RT=summarySE(dat_H_cond_RT_passive_ns, measurevar="H_cond_RT_passive_ns", groupvars=c("H_all_sess_type"))

			ggplot(dat_H_cond_se_passive_ns_RT, aes(x=H_all_sess_type, y=H_cond_RT_passive_ns, fill=H_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=H_cond_RT_passive_ns-se, ymax=H_cond_RT_passive_ns+se),width=.2, position=position_dodge(.9))
			
			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/RT/COND_RT/H_passive_ns_rt_se_group_comparison.png",wd),width=10,height=10)



###### ALL SUBJS FIGURES


### RT by condition dataframe

	##Active Sensible

		## BOX PLOT

			dat_cond_RT_active_s=data.frame(cond_RT_active_s,all_sess_subs,all_sess_type)

			cond_RT_active_s_plot=ggplot(dat_cond_RT_active_s,aes(x=all_sess_type,y=cond_RT_active_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("ALL AVG ACTIVE S RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/RT/COND_RT/all_active_s_rt_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH
			
			dat_cond_se_active_s_RT=summarySE(dat_cond_RT_active_s, measurevar="cond_RT_active_s", groupvars=c("all_sess_type"))

			ggplot(dat_cond_se_active_s_RT, aes(x=all_sess_type, y=cond_RT_active_s, fill=all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=cond_RT_active_s-se, ymax=cond_RT_active_s+se),width=.2, position=position_dodge(.9))
			
			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/RT/COND_RT/all_active_s_rt_se_group_comparison.png",wd),width=10,height=10)



	##Active Non-sensible

		## BOX PLOT

			dat_cond_RT_active_ns=data.frame(cond_RT_active_ns,all_sess_subs,all_sess_type)
	
			cond_RT_active_ns_plot=ggplot(dat_cond_RT_active_ns,aes(x=all_sess_type,y=cond_RT_active_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("ALL AVG ACTIVE NS RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/RT/COND_RT/all_active_ns_rt_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH

			dat_cond_se_active_ns_RT=summarySE(dat_cond_RT_active_ns, measurevar="cond_RT_active_ns", groupvars=c("all_sess_type"))

			ggplot(dat_cond_se_active_ns_RT, aes(x=all_sess_type, y=cond_RT_active_ns, fill=all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=cond_RT_active_ns-se, ymax=cond_RT_active_ns+se),width=.2, position=position_dodge(.9))
			
			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/RT/COND_RT/all_active_ns_rt_se_group_comparison.png",wd),width=10,height=10)

	##Passive Sensible

		## BOX PLOT

			dat_cond_RT_passive_s=data.frame(cond_RT_passive_s,all_sess_subs,all_sess_type)

			cond_RT_passive_s_plot=ggplot(dat_cond_RT_passive_s,aes(x=all_sess_type,y=cond_RT_passive_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("ALL AVG PASSIVE S RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/RT/COND_RT/all_passive_s_rt_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH

			dat_cond_se_passive_s_RT=summarySE(dat_cond_RT_passive_s, measurevar="cond_RT_passive_s", groupvars=c("all_sess_type"))

			ggplot(dat_cond_se_passive_s_RT, aes(x=all_sess_type, y=cond_RT_passive_s, fill=all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=cond_RT_passive_s-se, ymax=cond_RT_passive_s+se),width=.2, position=position_dodge(.9))
			
			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/RT/COND_RT/all_passive_s_rt_se_group_comparison.png",wd),width=10,height=10)
			

	##Passive Non-sensible
		
		## BOX PLOT

			dat_cond_RT_passive_ns=data.frame(cond_RT_passive_ns,all_sess_subs,all_sess_type)

			cond_RT_passive_ns_plot=ggplot(dat_cond_RT_passive_ns,aes(x=all_sess_type,y=cond_RT_passive_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("ALL_AVG PASSIVE NS RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/RT/COND_RT/all_passive_ns_rt_group_comparison.png",wd),width=10,height=10)


		## BAR GRAPH
	
			dat_cond_se_passive_ns_RT=summarySE(dat_cond_RT_passive_ns, measurevar="cond_RT_passive_ns", groupvars=c("all_sess_type"))

			ggplot(dat_cond_se_passive_ns_RT, aes(x=all_sess_type, y=cond_RT_passive_ns, fill=all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=cond_RT_passive_ns-se, ymax=cond_RT_passive_ns+se),width=.2, position=position_dodge(.9))
			
			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/RT/COND_RT/all_passive_ns_rt_se_group_comparison.png",wd),width=10,height=10)
			



###### COMBINED SUBJS


### RT by condition dataframe

	##Active Sensible

		## BOX PLOT

			comb_cond_RT_active_s=c((cond_RT_active_s[all_sess_type=="session_1" | all_sess_type=="H_session_1"]), (cond_RT_active_s[all_sess_type=="session_2" | all_sess_type=="H_session_2"]),cond_RT_active_s[all_sess_type=="session_control"])
			dat_comb_cond_RT_active_s=data.frame(comb_cond_RT_active_s,comb_sess_subs,comb_sess_type)

			comb_cond_RT_active_s_plot=ggplot(dat_comb_cond_RT_active_s,aes(x=comb_sess_type,y=comb_cond_RT_active_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_sess_subs,color=comb_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("COMBINED AVG ACTIVE S RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/RT/COND_RT/comb_active_s_rt_group_comparison.png",wd),width=10,height=10)
		
		## BAR GRAPH

			dat_comb_cond_se_active_s_RT=summarySE(dat_comb_cond_RT_active_s, measurevar="comb_cond_RT_active_s", groupvars=c("comb_sess_type"))

			ggplot(dat_comb_cond_se_active_s_RT, aes(x=comb_sess_type, y=comb_cond_RT_active_s, fill=comb_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=comb_cond_RT_active_s-se, ymax=comb_cond_RT_active_s+se),width=.2, position=position_dodge(.9))
			
			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/RT/COND_RT/comb_active_s_rt_se_group_comparison.png",wd),width=10,height=10)


	##Active Non-sensible

		## BOX PLOT

			comb_cond_RT_active_ns=c((cond_RT_active_ns[all_sess_type=="session_1" | all_sess_type=="H_session_1"]), (cond_RT_active_ns[all_sess_type=="session_2" | all_sess_type=="H_session_2"]),cond_RT_active_ns[all_sess_type=="session_control"])
			dat_comb_cond_RT_active_ns=data.frame(comb_cond_RT_active_ns,comb_sess_subs,comb_sess_type)

			comb_cond_RT_active_ns_plot=ggplot(dat_comb_cond_RT_active_ns,aes(x=comb_sess_type,y=comb_cond_RT_active_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_sess_subs,color=comb_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("COMBINED AVG ACTIVE NS RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/RT/COND_RT/comb_active_ns_rt_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH

			dat_comb_cond_se_active_ns_RT=summarySE(dat_comb_cond_RT_active_ns, measurevar="comb_cond_RT_active_ns", groupvars=c("comb_sess_type"))

			ggplot(dat_comb_cond_se_active_ns_RT, aes(x=comb_sess_type, y=comb_cond_RT_active_ns, fill=comb_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=comb_cond_RT_active_ns-se, ymax=comb_cond_RT_active_ns+se),width=.2, position=position_dodge(.9))
			
			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/RT/COND_RT/comb_active_ns_rt_se_group_comparison.png",wd),width=10,height=10)


	##Passive Sensible
		## BOX PLOT

			comb_cond_RT_passive_s=c((cond_RT_passive_s[all_sess_type=="session_1" | all_sess_type=="H_session_1"]), (cond_RT_passive_s[all_sess_type=="session_2" | all_sess_type=="H_session_2"]),cond_RT_passive_s[all_sess_type=="session_control"])
			dat_comb_cond_RT_passive_s=data.frame(comb_cond_RT_passive_s,comb_sess_subs,comb_sess_type)

			comb_cond_RT_passive_s_plot=ggplot(dat_comb_cond_RT_passive_s,aes(x=comb_sess_type,y=comb_cond_RT_passive_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_sess_subs,color=comb_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("COMBINED AVG PASSIVE S RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/RT/COND_RT/comb_passive_s_rt_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH

			dat_comb_cond_se_passive_s_RT=summarySE(dat_comb_cond_RT_passive_s, measurevar="comb_cond_RT_passive_s", groupvars=c("comb_sess_type"))

			ggplot(dat_comb_cond_se_passive_s_RT, aes(x=comb_sess_type, y=comb_cond_RT_passive_s, fill=comb_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=comb_cond_RT_passive_s-se, ymax=comb_cond_RT_passive_s+se),width=.2, position=position_dodge(.9))
			
			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/RT/COND_RT/comb_passive_s_rt_se_group_comparison.png",wd),width=10,height=10)


	##Passive Non-sensible

		## BOX PLOT

			comb_cond_RT_passive_ns=c((cond_RT_passive_ns[all_sess_type=="session_1" | all_sess_type=="H_session_1"]), (cond_RT_passive_ns[all_sess_type=="session_2" | all_sess_type=="H_session_2"]),cond_RT_passive_ns[all_sess_type=="session_control"])
			dat_comb_cond_RT_passive_ns=data.frame(comb_cond_RT_passive_ns,comb_sess_subs,comb_sess_type)

			comb_cond_RT_passive_ns_plot=ggplot(dat_comb_cond_RT_passive_ns,aes(x=comb_sess_type,y=comb_cond_RT_passive_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_sess_subs,color=comb_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (S)") + ggtitle("COMBINED AVG PASSIVE NS RT")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/RT/COND_RT/comb_passive_ns_rt_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH

			dat_comb_cond_se_passive_ns_RT=summarySE(dat_comb_cond_RT_passive_ns, measurevar="comb_cond_RT_passive_ns", groupvars=c("comb_sess_type"))

			ggplot(dat_comb_cond_se_passive_ns_RT, aes(x=comb_sess_type, y=comb_cond_RT_passive_ns, fill=comb_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=comb_cond_RT_passive_ns-se, ymax=comb_cond_RT_passive_ns+se),width=.2, position=position_dodge(.9))
			
			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/RT/COND_RT/comb_passive_ns_rt_se_group_comparison.png",wd),width=10,height=10)



################################################################################################################################################################

Accuracy Analysis

################################################################################################################################################################



############ START ACC LOOP #############


# vector for all_cond matrix 

cond_acc_active_s=matrix(0,all_sub_num,1)
cond_acc_active_ns=matrix(0,all_sub_num,1)
cond_acc_passive_s=matrix(0,all_sub_num,1)
cond_acc_passive_ns=matrix(0,all_sub_num,1)


sessions=length(group)

for (k in 1:sessions){

	if (k==1){
	dat_all=dat_all_1
	n="first"
	x=1:sess_1_sub_num

	} else {

	if (k==2){
	dat_all=dat_all_2
	n="second"
	x=(sess_1_sub_num+1):(sess_1_sub_num+sess_2_sub_num)

	} else {

	if (k==3){
	dat_all=dat_all_3
	n="H_first"
	x=(sess_1_sub_num+sess_2_sub_num+1):(sess_1_sub_num+sess_2_sub_num+H_sess_1_sub_num)

	} else {

	if (k==4){
	dat_all=dat_all_4
	n="H_second"
	x=(sess_1_sub_num+sess_2_sub_num+H_sess_1_sub_num+1):(sess_1_sub_num+sess_2_sub_num+H_sess_1_sub_num+H_sess_2_sub_num)


	} else {

	if (k==5){
	dat_all=dat_all_5
	n="control"
	x=(sess_1_sub_num+sess_2_sub_num+H_sess_1_sub_num+H_sess_2_sub_num+1):all_sub_num

	}
	}
	}
	}
	}


#Find accuracy of subject per run
 

sub_nums=names(table(dat_all$subind))
run_nums=names(table(dat_all$run))
prop_correct=data.frame(matrix(0,nrow=length(sub_nums), ncol=3))
sent_names=c("active_s", "active_ns", "passive_s", "passive_ns")

colnames(prop_correct)=c("Run_1", "Run_2", "Run_3")
rownames(prop_correct)=c(sub_nums)

for (i in 1:length(sub_nums)){
for (j in 1:length(run_nums)){

      #get all first trials for this sub within this sent type
      dat_loop=dat_all[(dat_all$subind==sub_nums[i] & dat_all$first==1 & ( dat_all$cond_type==sent_names[1] |dat_all$cond_type==sent_names[2] | dat_all$cond_type==sent_names[3] |dat_all$cond_type==sent_names[4] ) & dat_all$run==run_nums[j]),]
      prop_correct[i,j]=sum(dat_loop$correct==1)/dim(dat_loop)[1]


}
}

### Take the mean across runs for each subject

prop_correct$mean=rowMeans(prop_correct,na.rm=TRUE)



#### apply prop_correct cutoff

if (prop_correct_cutoff==0){


	assign(paste("prop_correct",n,sep="_"),prop_correct)
	} else {

	if (prop_correct_cutoff>0){

	prop_correct_cutoff_mask=prop_correct<prop_correct_cutoff

	prop_correct[prop_correct_cutoff_mask]=NA
	prop_correct$mean <- rowMeans(prop_correct, na.rm = TRUE)

	assign(paste("prop_correct",n,sep="_"),prop_correct)

}
}

#######



prop_correct_cond=data.frame(matrix(0,nrow=length(sub_nums), ncol=4))
rownames(prop_correct_cond)=c(sub_nums)
names(prop_correct_cond)=sent_names
 
for (i in 1:length(sub_nums)){
    for (sent in c(1:4)){
      #get all first trials for this sub within this sent type
      dat_loop=dat_all[(dat_all$subind==sub_nums[i] & dat_all$first==1 & dat_all$cond_type==sent_names[sent]) ,]
      prop_correct_cond[i,sent]=sum(dat_loop$correct==1)/dim(dat_loop)[1]

      
}
}

prop_correct_cond$mean=rowMeans(prop_correct_cond,na.rm=TRUE)



### apply prop_correct_cond cutoff

if (prop_correct_cond_cutoff==0){


	assign(paste("prop_correct_cond",n,sep="_"),prop_correct_cond)

	} else {

	if (prop_correct_cond_cutoff>0){


	remove_row=matrix(1:length(prop_correct_cond$mean))

	prop_correct_cond_cutoff_mask=prop_correct_cond$mean<prop_correct_cond_cutoff
	remove_row=remove_row[prop_correct_cond_cutoff_mask]

	prop_correct_cond[remove_row,]=NA

	prop_correct_cond$mean=rowMeans(prop_correct_cond,na.rm=TRUE)


	assign(paste("prop_correct_cond",n,sep="_"),prop_correct_cond)
}
}

#######






### Condition vectors for all

cond_acc_active_s[x]=prop_correct_cond$active_s
cond_acc_active_ns[x]=prop_correct_cond$active_ns
cond_acc_passive_s[x]=prop_correct_cond$passive_s
cond_acc_passive_ns[x]=prop_correct_cond$passive_ns



} #end of loop




# in R in terminal, you can check to see percentage correct for each run for each group by looking at the following

  # prop_correct_control
  # prop_correct_first
  # prop_correct_second
  # prop_correct_H_first
  # prop_correct_H_second

# Check percentage correct for each condition for each group by looking at

  # prop_correct_cond_control
  # prop_correct_cond_first
  # prop_correct_cond_second
  # prop_correct_cond_H_first
  # prop_correct_cond_H_second


############# END ACC LOOP ##############




############# ACC FIGURES ###############


##### Make identifiers for type of session

sess_1=rep("session_1",length(prop_correct_first$mean))
sess_2=rep("session_2",length(prop_correct_second$mean))
controls=rep("session_control",length(prop_correct_control$mean))
H_sess_1=rep("H_session_1",length(prop_correct_H_first$mean))
H_sess_2=rep("H_session_2",length(prop_correct_H_second$mean))

A_all_sess_acc=c(prop_correct_first$mean,prop_correct_second$mean,prop_correct_control$mean)
A_all_sess_subs=c(sess_1_subs,sess_2_subs,control_subs)
A_all_sess_type=c(sess_1,sess_2,controls)


H_all_sess_acc=c(prop_correct_H_first$mean,prop_correct_H_second$mean,prop_correct_control$mean)
H_all_sess_subs=c(H_sess_1_subs,H_sess_2_subs,control_subs)
H_all_sess_type=c(H_sess_1,H_sess_2,controls)


all_sess_acc=c(prop_correct_first$mean,prop_correct_second$mean,prop_correct_H_first$mean,prop_correct_H_second$mean,prop_correct_control$mean)
all_sess_subs=c(sess_1_subs,sess_2_subs,H_sess_1_subs,H_sess_2_subs,control_subs)
all_sess_type=c(sess_1,sess_2,H_sess_1,H_sess_2,controls)

comb_all_sess_acc=c(c(prop_correct_first$mean,prop_correct_H_first$mean),c(prop_correct_second$mean,prop_correct_H_second$mean),prop_correct_control$mean)
comb_all_sess_subs=c(sess_1_subs,H_sess_1_subs,sess_2_subs,H_sess_2_subs,control_subs)
comb_all_sess_type=c(rep("comb_sess_1",sum(length(prop_correct_first$mean),length(prop_correct_H_first$mean))),rep("comb_sess_2",sum(length(prop_correct_second$mean),length(prop_correct_H_second$mean))),controls)





##### AVG ACC BOX PLOTS


### AUSTIN
	
	dat_A_all_sess_correct=data.frame(A_all_sess_acc,A_all_sess_subs,A_all_sess_type)

	A_all_sess_correct_plot=ggplot(dat_A_all_sess_correct,aes(x=A_all_sess_type,y=A_all_sess_acc)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ guides(group=FALSE,color=FALSE)+ xlab("SESSION TYPE") + ylab("%CORRECT") + ggtitle("AUSTIN AVG ACCURACY")
	ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/ACC/AVG_ACC/A_all_avg_acc.png",wd),width=10,height=10)



### HOUSTON

	dat_H_all_sess_correct=data.frame(H_all_sess_acc,H_all_sess_subs,H_all_sess_type)

	H_all_sess_correct_plot=ggplot(dat_H_all_sess_correct,aes(x=H_all_sess_type,y=H_all_sess_acc)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ guides(group=FALSE,color=FALSE)+ xlab("SESSION TYPE") + ylab("%CORRECT") + ggtitle("HOUSTON AVG ACCURACY")
	ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/ACC/AVG_ACC/H_all_avg_acc.png",wd),width=10,height=10)



### ALL

	dat_all_sess_correct=data.frame(all_sess_acc,all_sess_subs,all_sess_type)

	all_sess_correct_plot=ggplot(dat_all_sess_correct,aes(x=all_sess_type,y=all_sess_acc)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ guides(group=FALSE,color=FALSE)+ xlab("SESSION TYPE") + ylab("%CORRECT") + ggtitle("ALL AVG ACCURACY")
	ggsave(filename=sprintf("%s/figures/Project_4/All/SC/ACC/AVG_ACC/all_avg_acc.png",wd),width=10,height=10)


### COMBINED

	dat_comb_all_sess_correct=data.frame(comb_all_sess_acc,comb_all_sess_subs,comb_all_sess_type)

	comb_all_sess_correct_plot=ggplot(dat_comb_all_sess_correct,aes(x=comb_all_sess_type,y=comb_all_sess_acc)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_all_sess_subs,color=comb_all_sess_subs))+ guides(group=FALSE,color=FALSE)+ xlab("SESSION TYPE") + ylab("%CORRECT") + ggtitle("COMBINED AVG ACCURACY")
	ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/ACC/AVG_ACC/comb_all_avg_acc.png",wd),width=10,height=10)



#### AVG ACC BAR GRAPHS W/ ERROR BARS

	#### AUSTIN SUBJS

		dat_A_all_sess_correct=data.frame(A_all_sess_acc,A_all_sess_subs,A_all_sess_type)

		dat_A_all_sess_se_correct=summarySE(dat_A_all_sess_correct, measurevar="A_all_sess_acc", groupvars=c("A_all_sess_type"))	

		ggplot(dat_A_all_sess_se_correct, aes(x=A_all_sess_type, y=A_all_sess_acc, fill=A_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=A_all_sess_acc-se, ymax=A_all_sess_acc+se),width=.2, position=position_dodge(.9))

		ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/ACC/AVG_ACC/A_all_se_avg_acc.png",wd),width=10,height=10)


	#### HOUSTON SUBJS

		dat_H_all_sess_correct=data.frame(H_all_sess_acc,H_all_sess_subs,H_all_sess_type)

		dat_H_all_sess_se_correct=summarySE(dat_H_all_sess_correct, measurevar="H_all_sess_acc", groupvars=c("H_all_sess_type"))	

		ggplot(dat_H_all_sess_se_correct, aes(x=H_all_sess_type, y=H_all_sess_acc, fill=H_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=H_all_sess_acc-se, ymax=H_all_sess_acc+se),width=.2, position=position_dodge(.9))

		ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/ACC/AVG_ACC/H_all_se_avg_acc.png",wd),width=10,height=10)


	#### ALL SUBJS

		dat_all_sess_correct=data.frame(all_sess_acc,all_sess_subs,all_sess_type)

		dat_all_sess_se_correct=summarySE(dat_all_sess_correct, measurevar="all_sess_acc", groupvars=c("all_sess_type"))	

		ggplot(dat_all_sess_se_correct, aes(x=all_sess_type, y=all_sess_acc, fill=all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=all_sess_acc-se, ymax=all_sess_acc+se),width=.2, position=position_dodge(.9))

		ggsave(filename=sprintf("%s/figures/Project_4/All/SC/ACC/AVG_ACC/all_se_avg_acc.png",wd),width=10,height=10)


	#### COMBINED SUBJS

		dat_comb_all_sess_correct=data.frame(comb_all_sess_acc,comb_all_sess_subs,comb_all_sess_type)

		dat_comb_all_sess_se_correct=summarySE(dat_comb_all_sess_correct, measurevar="comb_all_sess_acc", groupvars=c("comb_all_sess_type"))	

		ggplot(dat_comb_all_sess_se_correct, aes(x=comb_all_sess_type, y=comb_all_sess_acc, fill=comb_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=comb_all_sess_acc-se, ymax=comb_all_sess_acc+se),width=.2, position=position_dodge(.9))

		ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/ACC/AVG_ACC/comb_all_se_avg_acc.png",wd),width=10,height=10)



##### ACC by condition dataframe



##### AUSTIN SUBJS


	##Active Sensible

		## BOX PLOT
		
			A_cond_acc_active_s=cond_acc_active_s[all_sess_type=="session_1" | all_sess_type=="session_2" | all_sess_type=="session_control"]
			dat_A_cond_acc_active_s=data.frame(A_cond_acc_active_s,A_all_sess_subs,A_all_sess_type)

			A_cond_acc_active_s_plot=ggplot(dat_A_cond_acc_active_s,aes(x=A_all_sess_type,y=A_cond_acc_active_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("AUSTIN AVG ACTIVE S ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/ACC/COND_ACC/A_active_s_acc_group_comparison.png",wd),width=10,height=10)
	
		## BAR GRAPH
		
			dat_A_cond_acc_se_active_s=summarySE(dat_A_cond_acc_active_s, measurevar="A_cond_acc_active_s", groupvars=c("A_all_sess_type"))

			ggplot(dat_A_cond_acc_se_active_s, aes(x=A_all_sess_type, y=A_cond_acc_active_s, fill=A_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=A_cond_acc_active_s-se, ymax=A_cond_acc_active_s+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/ACC/COND_ACC/A_active_s_acc_se_group_comparison.png",wd),width=10,height=10)



	##Active Non-sensible

		## BOX PLOT

			A_cond_acc_active_ns=cond_acc_active_ns[all_sess_type=="session_1" | all_sess_type=="session_2" | all_sess_type=="session_control"]
			dat_A_cond_acc_active_ns=data.frame(A_cond_acc_active_ns,A_all_sess_subs,A_all_sess_type)

			A_cond_acc_active_ns_plot=ggplot(dat_A_cond_acc_active_ns,aes(x=A_all_sess_type,y=A_cond_acc_active_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("AUSTIN AVG ACTIVE NS ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/ACC/COND_ACC/A_active_ns_acc_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH

			dat_A_cond_acc_se_active_ns=summarySE(dat_A_cond_acc_active_ns, measurevar="A_cond_acc_active_ns", groupvars=c("A_all_sess_type"))

			ggplot(dat_A_cond_acc_se_active_ns, aes(x=A_all_sess_type, y=A_cond_acc_active_ns, fill=A_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=A_cond_acc_active_ns-se, ymax=A_cond_acc_active_ns+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/ACC/COND_ACC/A_active_ns_acc_se_group_comparison.png",wd),width=10,height=10)



	##Passive Sensible
		
		## BOX PLOT

			A_cond_acc_passive_s=cond_acc_passive_s[all_sess_type=="session_1" | all_sess_type=="session_2" | all_sess_type=="session_control"]
			dat_A_cond_acc_passive_s=data.frame(A_cond_acc_passive_s,A_all_sess_subs,A_all_sess_type)

			A_cond_acc_passive_s_plot=ggplot(dat_A_cond_acc_passive_s,aes(x=A_all_sess_type,y=A_cond_acc_passive_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("AUSTIN AVG PASSIVE S ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/ACC/COND_ACC/A_passive_s_acc_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH

			dat_A_cond_acc_se_passive_s=summarySE(dat_A_cond_acc_passive_s, measurevar="A_cond_acc_passive_s", groupvars=c("A_all_sess_type"))

			ggplot(dat_A_cond_acc_se_passive_s, aes(x=A_all_sess_type, y=A_cond_acc_passive_s, fill=A_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=A_cond_acc_passive_s-se, ymax=A_cond_acc_passive_s+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/ACC/COND_ACC/A_passive_s_acc_se_group_comparison.png",wd),width=10,height=10)


	##Passive Non-sensible
		
		## BOX PLOT
	
			A_cond_acc_passive_ns=cond_acc_passive_ns[all_sess_type=="session_1" | all_sess_type=="session_2" | all_sess_type=="session_control"]
			dat_A_cond_acc_passive_ns=data.frame(A_cond_acc_passive_ns,A_all_sess_subs,A_all_sess_type)

			A_cond_acc_passive_ns_plot=ggplot(dat_A_cond_acc_passive_ns,aes(x=A_all_sess_type,y=A_cond_acc_passive_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("AUSTIN AVG PASSIVE NS ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/ACC/COND_ACC/A_passive_ns_acc_group_comparison.png",wd),width=10,height=10)


		## BAR GRAPH

			dat_A_cond_acc_se_passive_ns=summarySE(dat_A_cond_acc_passive_ns, measurevar="A_cond_acc_passive_ns", groupvars=c("A_all_sess_type"))

			ggplot(dat_A_cond_acc_se_passive_ns, aes(x=A_all_sess_type, y=A_cond_acc_passive_ns, fill=A_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=A_cond_acc_passive_ns-se, ymax=A_cond_acc_passive_ns+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/ACC/COND_ACC/A_passive_ns_acc_se_group_comparison.png",wd),width=10,height=10)





###### HOUSTON SUBJS


### acc by condition dataframe

	##Active Sensible

		## BOX PLOT

			H_cond_acc_active_s=cond_acc_active_s[all_sess_type=="H_session_1" | all_sess_type=="H_session_2" | all_sess_type=="session_control"]
			dat_H_cond_acc_active_s=data.frame(H_cond_acc_active_s,H_all_sess_subs,H_all_sess_type)

			H_cond_acc_active_s_plot=ggplot(dat_H_cond_acc_active_s,aes(x=H_all_sess_type,y=H_cond_acc_active_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("HOUSTON AVG ACTIVE S ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/ACC/COND_ACC/H_active_s_acc_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH
		
			dat_H_cond_acc_se_active_s=summarySE(dat_H_cond_acc_active_s, measurevar="H_cond_acc_active_s", groupvars=c("H_all_sess_type"))

			ggplot(dat_H_cond_acc_se_active_s, aes(x=H_all_sess_type, y=H_cond_acc_active_s, fill=H_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=H_cond_acc_active_s-se, ymax=H_cond_acc_active_s+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/ACC/COND_ACC/H_active_s_acc_se_group_comparison.png",wd),width=10,height=10)


	##Active Non-sensible

		## BOX PLOTS

			H_cond_acc_active_ns=cond_acc_active_ns[all_sess_type=="H_session_1" | all_sess_type=="H_session_2" | all_sess_type=="session_control"]
			dat_H_cond_acc_active_ns=data.frame(H_cond_acc_active_ns,H_all_sess_subs,H_all_sess_type)

			H_cond_acc_active_ns_plot=ggplot(dat_H_cond_acc_active_ns,aes(x=H_all_sess_type,y=H_cond_acc_active_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("HOUSTON AVG ACTIVE NS ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/ACC/COND_ACC/H_active_ns_acc_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH
		
			dat_H_cond_acc_se_active_ns=summarySE(dat_H_cond_acc_active_ns, measurevar="H_cond_acc_active_ns", groupvars=c("H_all_sess_type"))

			ggplot(dat_H_cond_acc_se_active_ns, aes(x=H_all_sess_type, y=H_cond_acc_active_ns, fill=H_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=H_cond_acc_active_ns-se, ymax=H_cond_acc_active_ns+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/ACC/COND_ACC/H_active_ns_acc_se_group_comparison.png",wd),width=10,height=10)


	##Passive Sensible

		## BOX PLOT
		
			H_cond_acc_passive_s=cond_acc_passive_s[all_sess_type=="H_session_1" | all_sess_type=="H_session_2" | all_sess_type=="session_control"]
			dat_H_cond_acc_passive_s=data.frame(H_cond_acc_passive_s,H_all_sess_subs,H_all_sess_type)

			H_cond_acc_passive_s_plot=ggplot(dat_H_cond_acc_passive_s,aes(x=H_all_sess_type,y=H_cond_acc_passive_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("HOUSTON AVG PASSIVE S ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/ACC/COND_ACC/H_passive_s_acc_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH
		
			dat_H_cond_acc_se_passive_s=summarySE(dat_H_cond_acc_passive_s, measurevar="H_cond_acc_passive_s", groupvars=c("H_all_sess_type"))

			ggplot(dat_H_cond_acc_se_passive_s, aes(x=H_all_sess_type, y=H_cond_acc_passive_s, fill=H_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=H_cond_acc_passive_s-se, ymax=H_cond_acc_passive_s+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/ACC/COND_ACC/H_passive_s_acc_se_group_comparison.png",wd),width=10,height=10)


	##Passive Non-sensible

		## BOX PLOT

			H_cond_acc_passive_ns=cond_acc_passive_ns[all_sess_type=="H_session_1" | all_sess_type=="H_session_2" | all_sess_type=="session_control"]
			dat_H_cond_acc_passive_ns=data.frame(H_cond_acc_passive_ns,H_all_sess_subs,H_all_sess_type)

			H_cond_acc_passive_ns_plot=ggplot(dat_H_cond_acc_passive_ns,aes(x=H_all_sess_type,y=H_cond_acc_passive_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("HOUSTON AVG PASSIVE NS ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/ACC/COND_ACC/H_passive_ns_acc_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH
		
			dat_H_cond_acc_se_passive_ns=summarySE(dat_H_cond_acc_passive_ns, measurevar="H_cond_acc_passive_ns", groupvars=c("H_all_sess_type"))

			ggplot(dat_H_cond_acc_se_passive_ns, aes(x=H_all_sess_type, y=H_cond_acc_passive_ns, fill=H_all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=H_cond_acc_passive_ns-se, ymax=H_cond_acc_passive_ns+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/ACC/COND_ACC/H_passive_ns_acc_se_group_comparison.png",wd),width=10,height=10)



###### ALL SUBJS FIGURES


### acc by condition dataframe

	##Active Sensible

		## BOX PLOT

			dat_cond_acc_active_s=data.frame(cond_acc_active_s,all_sess_subs,all_sess_type)
	
			cond_acc_active_s_plot=ggplot(dat_cond_acc_active_s,aes(x=all_sess_type,y=cond_acc_active_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("ALL AVG ACTIVE S ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/ACC/COND_ACC/all_active_s_acc_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH
		
			dat_cond_acc_se_active_s=summarySE(dat_cond_acc_active_s, measurevar="cond_acc_active_s", groupvars=c("all_sess_type"))

			ggplot(dat_cond_acc_se_active_s, aes(x=all_sess_type, y=cond_acc_active_s, fill=all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=cond_acc_active_s-se, ymax=cond_acc_active_s+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/ACC/COND_ACC/all_active_s_acc_se_group_comparison.png",wd),width=10,height=10)


	##Active Non-sensible

		## BOX PLOT

			dat_cond_acc_active_ns=data.frame(cond_acc_active_ns,all_sess_subs,all_sess_type)

			cond_acc_active_ns_plot=ggplot(dat_cond_acc_active_ns,aes(x=all_sess_type,y=cond_acc_active_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("ALL AVG ACTIVE NS ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/ACC/COND_ACC/all_active_ns_acc_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH
		
			dat_cond_acc_se_active_ns=summarySE(dat_cond_acc_active_ns, measurevar="cond_acc_active_ns", groupvars=c("all_sess_type"))

			ggplot(dat_cond_acc_se_active_ns, aes(x=all_sess_type, y=cond_acc_active_ns, fill=all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=cond_acc_active_ns-se, ymax=cond_acc_active_ns+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/ACC/COND_ACC/all_active_ns_acc_se_group_comparison.png",wd),width=10,height=10)


	##Passive Sensible

		## BOX PLOT

			dat_cond_acc_passive_s=data.frame(cond_acc_passive_s,all_sess_subs,all_sess_type)

			cond_acc_passive_s_plot=ggplot(dat_cond_acc_passive_s,aes(x=all_sess_type,y=cond_acc_passive_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("ALL AVG PASSIVE S ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/ACC/COND_ACC/all_passive_s_acc_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH
		
			dat_cond_acc_se_passive_s=summarySE(dat_cond_acc_passive_s, measurevar="cond_acc_passive_s", groupvars=c("all_sess_type"))

			ggplot(dat_cond_acc_se_passive_s, aes(x=all_sess_type, y=cond_acc_passive_s, fill=all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=cond_acc_passive_s-se, ymax=cond_acc_passive_s+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/ACC/COND_ACC/all_passive_s_acc_se_group_comparison.png",wd),width=10,height=10)


	##Passive Non-sensible

		## BOX PLOT

			dat_cond_acc_passive_ns=data.frame(cond_acc_passive_ns,all_sess_subs,all_sess_type)

			cond_acc_passive_ns_plot=ggplot(dat_cond_acc_passive_ns,aes(x=all_sess_type,y=cond_acc_passive_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("ALL_AVG PASSIVE NS ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/ACC/COND_ACC/all_passive_ns_acc_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH
		
			dat_cond_acc_se_passive_ns=summarySE(dat_cond_acc_passive_ns, measurevar="cond_acc_passive_ns", groupvars=c("all_sess_type"))

			ggplot(dat_cond_acc_se_passive_ns, aes(x=all_sess_type, y=cond_acc_passive_ns, fill=all_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=cond_acc_passive_ns-se, ymax=cond_acc_passive_ns+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/All/SC/ACC/COND_ACC/all_passive_ns_acc_se_group_comparison.png",wd),width=10,height=10)


###### COMBINED SUBJS


### acc by condition dataframe

	##Active Sensible
		
		## BOX PLOTS

			comb_cond_acc_active_s=c((cond_acc_active_s[all_sess_type=="session_1" | all_sess_type=="H_session_1"]), (cond_acc_active_s[all_sess_type=="session_2" | all_sess_type=="H_session_2"]),cond_acc_active_s[all_sess_type=="session_control"])
			dat_comb_cond_acc_active_s=data.frame(comb_cond_acc_active_s,comb_sess_subs,comb_sess_type)

			comb_cond_acc_active_s_plot=ggplot(dat_comb_cond_acc_active_s,aes(x=comb_sess_type,y=comb_cond_acc_active_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_sess_subs,color=comb_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("COMBINED AVG ACTIVE S ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/ACC/COND_ACC/comb_active_s_acc_group_comparison.png",wd),width=10,height=10)
	
		## BAR GRAPH
		
			dat_comb_cond_acc_se_active_s=summarySE(dat_comb_cond_acc_active_s, measurevar="comb_cond_acc_active_s", groupvars=c("comb_sess_type"))

			ggplot(dat_comb_cond_acc_se_active_s, aes(x=comb_sess_type, y=comb_cond_acc_active_s, fill=comb_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=comb_cond_acc_active_s-se, ymax=comb_cond_acc_active_s+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/ACC/COND_ACC/comb_active_s_acc_se_group_comparison.png",wd),width=10,height=10)


	##Active Non-sensible

		## BOX PLOTS

			comb_cond_acc_active_ns=c((cond_acc_active_ns[all_sess_type=="session_1" | all_sess_type=="H_session_1"]), (cond_acc_active_ns[all_sess_type=="session_2" | all_sess_type=="H_session_2"]),cond_acc_active_ns[all_sess_type=="session_control"])
			dat_comb_cond_acc_active_ns=data.frame(comb_cond_acc_active_ns,comb_sess_subs,comb_sess_type)

			comb_cond_acc_active_ns_plot=ggplot(dat_comb_cond_acc_active_ns,aes(x=comb_sess_type,y=comb_cond_acc_active_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_sess_subs,color=comb_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("COMBINED AVG ACTIVE NS ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/ACC/COND_ACC/comb_active_ns_acc_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH
		
			dat_comb_cond_acc_se_active_ns=summarySE(dat_comb_cond_acc_active_ns, measurevar="comb_cond_acc_active_ns", groupvars=c("comb_sess_type"))

			ggplot(dat_comb_cond_acc_se_active_ns, aes(x=comb_sess_type, y=comb_cond_acc_active_ns, fill=comb_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=comb_cond_acc_active_ns-se, ymax=comb_cond_acc_active_ns+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/ACC/COND_ACC/comb_active_ns_acc_se_group_comparison.png",wd),width=10,height=10)


	##Passive Sensible

		## BOX PLOT
		
			comb_cond_acc_passive_s=c((cond_acc_passive_s[all_sess_type=="session_1" | all_sess_type=="H_session_1"]), (cond_acc_passive_s[all_sess_type=="session_2" | all_sess_type=="H_session_2"]),cond_acc_passive_s[all_sess_type=="session_control"])
			dat_comb_cond_acc_passive_s=data.frame(comb_cond_acc_passive_s,comb_sess_subs,comb_sess_type)

			comb_cond_acc_passive_s_plot=ggplot(dat_comb_cond_acc_passive_s,aes(x=comb_sess_type,y=comb_cond_acc_passive_s)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_sess_subs,color=comb_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("COMBINED AVG PASSIVE S ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/ACC/COND_ACC/comb_passive_s_acc_group_comparison.png",wd),width=10,height=10)

		## BAR GRAPH
		
			dat_comb_cond_acc_se_passive_s=summarySE(dat_comb_cond_acc_passive_s, measurevar="comb_cond_acc_passive_s", groupvars=c("comb_sess_type"))

			ggplot(dat_comb_cond_acc_se_passive_s, aes(x=comb_sess_type, y=comb_cond_acc_passive_s, fill=comb_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=comb_cond_acc_passive_s-se, ymax=comb_cond_acc_passive_s+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/ACC/COND_ACC/comb_passive_s_acc_se_group_comparison.png",wd),width=10,height=10)		

	##Passive Non-sensible

		## BOX PLOT

			comb_cond_acc_passive_ns=c((cond_acc_passive_ns[all_sess_type=="session_1" | all_sess_type=="H_session_1"]), (cond_acc_passive_ns[all_sess_type=="session_2" | all_sess_type=="H_session_2"]),cond_acc_passive_ns[all_sess_type=="session_control"])
			dat_comb_cond_acc_passive_ns=data.frame(comb_cond_acc_passive_ns,comb_sess_subs,comb_sess_type)

			comb_cond_acc_passive_ns_plot=ggplot(dat_comb_cond_acc_passive_ns,aes(x=comb_sess_type,y=comb_cond_acc_passive_ns)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_sess_subs,color=comb_sess_subs))+ xlab("SESSION TYPE") + ylab("% Correct") + ggtitle("COMBINED AVG PASSIVE NS ACC")+ guides(group=FALSE,color=FALSE)

			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/ACC/COND_ACC/comb_passive_ns_acc_group_comparison.png",wd),width=10,height=10)


		## BAR GRAPH
		
			dat_comb_cond_acc_se_passive_ns=summarySE(dat_comb_cond_acc_passive_ns, measurevar="comb_cond_acc_passive_ns", groupvars=c("comb_sess_type"))

			ggplot(dat_comb_cond_acc_se_passive_ns, aes(x=comb_sess_type, y=comb_cond_acc_passive_ns, fill=comb_sess_type)) + geom_bar(position=position_dodge(), stat="identity") +geom_errorbar(aes(ymin=comb_cond_acc_passive_ns-se, ymax=comb_cond_acc_passive_ns+se),width=.2, position=position_dodge(.9))

			ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/ACC/COND_ACC/comb_passive_ns_acc_se_group_comparison.png",wd),width=10,height=10)













############################## SCATTER PLOTS

##Austin

dat_A_all_sess_avg_scatter=data.frame(A_all_sess_acc,A_all_sess_RT,A_all_sess_subs,A_all_sess_type,A_all_subs_type)


ggplot(dat_A_all_sess_avg_scatter, aes(x=A_all_sess_RT, y=A_all_sess_acc,shape=A_all_subs_type,color=A_all_sess_type)) +geom_point(size=3)+ geom_line(aes(group=A_all_sess_subs),color="#990000",linetype="dashed")+ ggtitle("SC RT VS SC %CORRECT")  + xlab("RT (s)") + ylab("% CORRECT")+guides(group=FALSE)






