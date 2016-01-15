# Authors: Leo Olmedo (First); Mary Abbe Roe Dec. 2013
# This script reads in SST behavioral data and calculates RT and ACC for all groups


#### LIBRARIES, FUNCTIONS, READING IN DATA ####

library(ggplot2)
library(plyr)
library(miscTools)

wd=getwd()


# FUNCTIONS

  #call summarySE function to use later in ggplot and grt mean, sd, and se
  summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    require(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This is does the summary; it's not easy to understand...
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
    # Calculate t-statistic for confidence interval: 
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

  # For Austin: subdirs=Sys.glob("/corral-repl/utexas/ldrc/ldrc_0[0-9][0-9]") or subdirs=Sys.glob("/corral-repl/utexas/ldrc/ldrc_*second")
  # For Houston: subdirs=Sys.glob("/corral-repl/utexas/ldrc/H_*MR1")

  group=c("ldrc_[0-9]_[0-9][0-9][0-9]", "ldrc_*second", "PHILIPS/H_LD*_[0-9]", "H_LD*_[0-9]_second", "ldrc_c_[0-9][0-9][0-9]", "PHILIPS/H_LD*[0-9][0-9][0-9][0-9]", "H_LD*_c_second")
  chars=c(5,12,6,13,5,4,13)

  # GROUP
  for (i in 1:length(group)){ 
    subdirs=Sys.glob(sprintf("/corral-repl/utexas/ldrc/%s" ,group[i]))
    rm("dat_all")

    # SUB
    for (sub in subdirs){
      behav_dirs=Sys.glob(sprintf("%s/behav/SST/SST_R*", sub))
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
       
         # CREATE DAT_ALL IF DOESN'T ALREADY EXIST
         if (exists("dat_all")==FALSE){
           dat_all=dat_loop 
         } 
         else { 
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
  dat=rbind(dat_all_1,dat_all_2, dat_all_3, dat_all_4, dat_all_5, dat_all_6, dat_all_7)


# DFs CREATED:  
  # dat_all_1 = Austin session 1 subs
  # dat_all_2 = Austin session 2 subs
  # dat_all_3 = Houston session 1 subs
  # dat_all_4 = Houston session 2 subs
  # dat_all_5 = Austin controls
  # dat_all_6 = Houston controls session 1
  # dat_all_7 = Houston controls session 2
  # dat = all groups


# to see variables in the data frame:  	names(dat_all)
# to see all variables created: 	ls()
# to see subjects: 			table(dat_all$subind)


# CODES:
  # dat_all$cond: 	0= arrow points left, 1= arrow points right, 2= arrow points left and stop , 3= arrow points right and stop
  # dat_all$Stim: 	0= left arrow, 1= right arrow
  # dat_all$Resp: 	0=no button press , 1=left button , 4=right button
  # dat_all$correct: 	0=go_incorrect, 1=go_correct, 2=stop_fail_incorrect, 3=stop_fail_correct, 4=stop_correct, 5=go_omission (4 and 5 are trials that have no response)


# SET LOWER CUTOFFS, SET TO 0 FOR NO CUTOFFS (go cutoff <.75; stop cutoff >.65)

  ssd_cutoff=0
  gort_med_cutoff=0

  go_correct_cutoff=0
  stop_correct_cutoff=0


# CREATE SAMPLE SIZE VARIABLES (7 groups; sub names, type, number)

  sess_1_subs=names(table(dat_all_1$subind))
  sess_1_subs_type=substrLeft(names(table(dat_all_1$subind)),1)
  sess_1_sub_num=length(sess_1_subs)

  sess_2_subs=substrLeft(names(table(dat_all_2$subind)),5)
  sess_2_subs_type=substrLeft(names(table(dat_all_2$subind)),1)
  sess_2_sub_num=length(sess_2_subs)

  H_sess_1_subs=names(table(dat_all_3$subind))
  H_sess_1_subs_type=substrRight(names(table(dat_all_3$subind)),1)
  H_sess_1_sub_num=length(H_sess_1_subs)

  H_sess_2_subs=substrLeft(names(table(dat_all_4$subind)),6)
  H_sess_2_subs_type=substrRight(names(table(dat_all_4$subind)),1)
  H_sess_2_sub_num=length(H_sess_2_subs)

  control_subs=names(table(dat_all_5$subind))
  control_subs_type=substrLeft(names(table(dat_all_5$subind)),1)
  control_sub_num=length(control_subs)

  control_H_sess_1_subs=names(table(dat_all_6$subind))
  control_H_sess_1_sub_num=length(control_H_sess_1_subs)

  control_H_sess_2_subs=substrLeft(names(table(dat_all_7$subind)),4)
  control_H_sess_2_sub_num=length(control_H_sess_2_subs)

  all_sub_num=sum(sess_1_sub_num, sess_2_sub_num, H_sess_1_sub_num, H_sess_2_sub_num, control_sub_num, control_H_sess_1_sub_num, control_H_sess_2_sub_num)


#####################################################################################

RT Analysis

#####################################################################################


sessions=length(group)

for (k in 1:sessions){

	if (k==1){
	dat_all=dat_all_1
	n="first"

	} else {

	if (k==2){
	dat_all=dat_all_2
	n="second"

	} else {

	if (k==3){
	dat_all=dat_all_3
	n="H_first"

	} else {

	if (k==4){
	dat_all=dat_all_4
	n="H_second"

	} else {

	if (k==5){
	dat_all=dat_all_5
	n="control"

	} else {

	if (k==6){
	dat_all=dat_all_6
	n="control_H_first"

	} else {

	if (k==7){
	dat_all=dat_all_7
	n="control_H_second"
	
	}
	}
	}
	}
	}
        }
        }


  # GO CORRECT RT and ACC

    # MEDIAN
 
      go_rt_med=ddply(dat_all, .(subind, runnum), summarise, N=length(subind), rt_median=median(RT[correct==1], na.rm=TRUE))
 

    # MEAN ACROSS SUBS FOR EACH RUN

      go_rt_mean=ddply(go_rt_med, .(runnum), summarise, N=length(runnum), rt_mean=mean(rt_median), rt_se=sd(rt_median)/sqrt(N))
 



    sub_nums=names(table(dat_all$subind))
    run_nums=c("1", "2")

    gort_med=data.frame(matrix(0,nrow=length(sub_nums), ncol=length(run_nums)))
    colnames(gort_med)=c("Run_1", "Run_2")
    rownames(gort_med)=c(sub_nums)

    for (i in 1:length(sub_nums)){
      for (j in 1:length(run_nums)){
   	gort_med[i,j]=
     	c( median(dat_all$RT[dat_all$correct==1 & dat_all$subind==sub_nums[i]  & dat_all$run==run_nums[j]]))
      }	
    }

    gort_med$mean <- rowMeans(gort_med, na.rm = TRUE)


# GO RT_MED CUTOFF

  if (gort_med_cutoff==0){
    assign(paste("gort_med",n,sep="_"),gort_med)
  } else {
    if (gort_med_cutoff>0){
      gort_med_cutoff_mask=gort_med<gort_med_cutoff

      gort_med[gort_med_cutoff_mask]=NA
      gort_med$mean <- rowMeans(gort_med, na.rm = TRUE)

      assign(paste("gort_med",n,sep="_"),gort_med)

    }
  }



# SSD Calculation per subject

  ssd=data.frame(matrix(0,nrow=length(sub_nums), ncol=j))
  colnames(ssd)=c("Run_1", "Run_2")
  rownames(ssd)=c(sub_nums)

  for (i in 1:length(sub_nums)){
    for (j in 1:length(run_nums)){
      ssd[i,j]=
      c( mean(dat_all$SSD[dat_all$correct==4 & dat_all$subind==sub_nums[i]  & dat_all$run==run_nums[j]]))
    }	
  }

  ssd$mean <- rowMeans(ssd, na.rm = TRUE)


# SSD CUTOFF

  if (ssd_cutoff==0){
    assign(paste("ssd",n,sep="_"),ssd)
  } else {
    if (ssd_cutoff>0){
      ssd_cutoff_mask=ssd<ssd_cutoff
      ssd[ssd_cutoff_mask]=NA
      ssd$mean <- rowMeans(ssd, na.rm = TRUE)

      assign(paste("ssd",n,sep="_"),ssd)
    }
  }


# SSRT

  SSRT=gort_med$mean-ssd$mean
  assign(paste("SSRT",n,sep="_"),SSRT)



} # END RT LOOP




# DFs TO CHECK
  # gort_med* with _first, _second, _control, H_first, H_second, control_H_first, or control_H_second
  # ssd* with _first, _second, _control, H_first, H_second, control_H_first, or control_H_second


######### START RT FIGURES ###########


##### Make identifiers for type of session

sess_1=rep("session_1",length(gort_med_first$mean))
sess_2=rep("session_2",length(gort_med_second$mean))
controls=rep("session_control",length(gort_med_control$mean))
H_sess_1=rep("H_session_1",length(gort_med_H_first$mean))
H_sess_2=rep("H_session_2",length(gort_med_H_second$mean))


A_all_sess_subs=c(sess_1_subs,sess_2_subs,control_subs)
A_all_sess_type=c(sess_1,sess_2,controls)
A_all_subs_type=c(sess_1_subs_type,sess_2_subs_type,control_subs_type)

H_all_sess_subs=c(H_sess_1_subs,H_sess_2_subs,control_subs)
H_all_sess_type=c(H_sess_1,H_sess_2,controls)

all_sess_subs=c(sess_1_subs,H_sess_1_subs,sess_2_subs,H_sess_2_subs,control_subs)
all_sess_type=c(sess_1,H_sess_1,sess_2,H_sess_2,controls)

comb_sess_subs=c(sess_1_subs,H_sess_1_subs,sess_2_subs,H_sess_2_subs,control_subs)
comb_sess_type=c(rep("comb_sess_1",sum(length(gort_med_first$mean),length(gort_med_H_first$mean))),rep("comb_sess_2",sum(length(gort_med_second$mean),length(gort_med_H_second$mean))),controls)

########### code to find sub type

#grep_0=glob2rx("0_*")
#grep_1=glob2rx("1_*")
#grep_2=glob2rx("2_*")
#grep_c=glob2rx("c_*")

#subj_0_mask=grep(grep_0,dat_A_all_sess_gort$A_all_sess_subs)
#subj_1_mask=grep(grep_1,dat_A_all_sess_gort$A_all_sess_subs)
#subj_2_mask=grep(grep_2,dat_A_all_sess_gort$A_all_sess_subs)
#subj_c_mask=grep(grep_c,dat_A_all_sess_gort$A_all_sess_subs)

#dat_A_inter_1_gort=c(A_all_sess_gort,A_all_sess_subs,A_all_sess_type

#######	

#### AUSTIN SUBJECT FIGURES


#GO RT

	A_all_sess_gort=c(gort_med_first$mean,gort_med_second$mean,gort_med_control$mean)
	dat_A_all_sess_gort=data.frame(A_all_sess_gort,A_all_sess_subs,A_all_sess_type,A_all_subs_type)

	A_all_sess_gort=ggplot(dat_A_all_sess_gort,aes(x=A_all_sess_type,y=A_all_sess_gort)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (s)") + ggtitle("AUSTIN CORRECT GO RT")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/Austin/SST/RT/GO_RT/A_go_rt_group_comparison.png",wd),width=10,height=10)

### GO RT by Intervention code

	A_all_sess_gort=c(gort_med_first$mean,gort_med_second$mean,gort_med_control$mean)
	dat_A_all_sess_gort=data.frame(A_all_sess_gort,A_all_sess_subs,A_all_sess_type

#SSRT

	A_all_sess_SSRT=c(SSRT_first,SSRT_second,SSRT_control)
	dat_A_all_sess_SSRT=data.frame(A_all_sess_SSRT,A_all_sess_subs,A_all_sess_type)

	A_all_sess_SSRT_plot=ggplot(dat_A_all_sess_gort,aes(x=A_all_sess_type,y=A_all_sess_SSRT)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (s)") + ggtitle("AUSTIN SSRT")+guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/Austin/SST/RT/SSRT/A_ssrt_group_comparison.png",wd),width=10,height=10)


# SSD

	A_all_sess_ssd=c(ssd_first$mean,ssd_second$mean,ssd_control$mean)
	dat_A_all_sess_ssd=data.frame(A_all_sess_ssd,A_all_sess_subs,A_all_sess_type)

	A_all_sess_ssd_plot=ggplot(dat_A_all_sess_ssd,aes(x=A_all_sess_type,y=A_all_sess_ssd)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (s)") + ggtitle("AUSTIN SSD")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/Austin/SST/RT/SSD/A_ssd_group_comparison.png",wd),width=10,height=10)




#### HOUSTON SUBJECT FIGURES

#GO RT

	H_all_sess_gort=c(gort_med_H_first$mean,gort_med_H_second$mean,gort_med_control$mean)
	dat_H_all_sess_gort=data.frame(H_all_sess_gort,H_all_sess_subs,H_all_sess_type)

	H_all_sess_gort_plot=ggplot(dat_H_all_sess_gort,aes(x=H_all_sess_type,y=H_all_sess_gort)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (s)") + ggtitle("HOUSTON CORRECT GO RT")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/Houston/SST/RT/GO_RT/H_go_rt_group_comparison.png",wd),width=10,height=10)


#SSRT

	H_all_sess_SSRT=c(SSRT_H_first,SSRT_H_second,SSRT_control)
	dat_H_all_sess_SSRT=data.frame(H_all_sess_SSRT,H_all_sess_subs,H_all_sess_type)

	H_all_sess_SSRT_plot=ggplot(dat_H_all_sess_gort,aes(x=H_all_sess_type,y=H_all_sess_SSRT)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (s)") + ggtitle("HOUSTON SSRT")+guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/Houston/SST/RT/SSRT/H_ssrt_group_comparison.png",wd),width=10,height=10)


# SSD

	H_all_sess_ssd=c(ssd_H_first$mean,ssd_H_second$mean,ssd_control$mean)
	dat_H_all_sess_ssd=data.frame(H_all_sess_ssd,H_all_sess_subs,H_all_sess_type)

	H_all_sess_ssd_plot=ggplot(dat_H_all_sess_ssd,aes(x=H_all_sess_type,y=H_all_sess_ssd)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (s)") + ggtitle("HOUSTON SSD")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/Houston/SST/RT/SSD/H_ssd_group_comparison.png",wd),width=10,height=10)



#### ALL SUBJECT FIGURES

#GO RT

	all_sess_gort=c(gort_med_first$mean,gort_med_H_first$mean,gort_med_second$mean,gort_med_H_second$mean,gort_med_control$mean)
	dat_all_sess_gort=data.frame(all_sess_gort,all_sess_subs,all_sess_type)

	all_sess_gort_plot=ggplot(dat_all_sess_gort,aes(x=all_sess_type,y=all_sess_gort)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (s)") + ggtitle("ALL CORRECT GO RT")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/All/SST/RT/GO_RT/all_go_rt_group_comparison.png",wd),width=10,height=10)


#SSRT

	all_sess_SSRT=c(SSRT_first,SSRT_H_first,SSRT_second,SSRT_H_second,SSRT_control)
	dat_all_sess_SSRT=data.frame(all_sess_SSRT,all_sess_subs,all_sess_type)

	all_sess_SSRT_plot=ggplot(dat_all_sess_gort,aes(x=all_sess_type,y=all_sess_SSRT)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (s)") + ggtitle("ALL SSRT")+guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/All/SST/RT/SSRT/all_ssrt_group_comparison.png",wd),width=10,height=10)


# SSD

	all_sess_ssd=c(ssd_first$mean,ssd_H_first$mean,ssd_second$mean,ssd_H_second$mean,ssd_control$mean)
	dat_all_sess_ssd=data.frame(all_sess_ssd,all_sess_subs,all_sess_type)

	all_sess_ssd_plot=ggplot(dat_all_sess_ssd,aes(x=all_sess_type,y=all_sess_ssd)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (s)") + ggtitle("ALL SSD")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/All/SST/RT/SSD/all_ssd_group_comparison.png",wd),width=10,height=10)


#### Combined SUBJECT FIGURES

#GO RT

	comb_sess_gort=c(gort_med_first$mean,gort_med_H_first$mean,gort_med_second$mean,gort_med_H_second$mean,gort_med_control$mean)
	dat_comb_sess_gort=data.frame(comb_sess_gort,comb_sess_subs,comb_sess_type)

	comb_sess_gort_plot=ggplot(dat_comb_sess_gort,aes(x=comb_sess_type,y=comb_sess_gort)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_sess_subs,color=comb_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (s)") + ggtitle("ALL CORRECT GO RT")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/Combined/SST/RT/GO_RT/comb_go_rt_group_comparison.png",wd),width=10,height=10)


#SSRT

	comb_sess_SSRT=c(SSRT_first,SSRT_H_first,SSRT_second,SSRT_H_second,SSRT_control)
	dat_comb_sess_SSRT=data.frame(comb_sess_SSRT,comb_sess_subs,comb_sess_type)

	comb_sess_SSRT_plot=ggplot(dat_comb_sess_gort,aes(x=comb_sess_type,y=comb_sess_SSRT)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_sess_subs,color=comb_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (s)") + ggtitle("ALL SSRT")+guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/Combined/SST/RT/SSRT/comb_ssrt_group_comparison.png",wd),width=10,height=10)


# SSD

	comb_sess_ssd=c(ssd_first$mean,ssd_H_first$mean,ssd_second$mean,ssd_H_second$mean,ssd_control$mean)
	dat_comb_sess_ssd=data.frame(comb_sess_ssd,comb_sess_subs,comb_sess_type)

	comb_sess_ssd_plot=ggplot(dat_comb_sess_ssd,aes(x=comb_sess_type,y=comb_sess_ssd)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_sess_subs,color=comb_sess_subs))+ xlab("SESSION TYPE") + ylab("RT (s)") + ggtitle("ALL SSD")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/Combined/SST/RT/SSD/comb_ssd_group_comparison.png",wd),width=10,height=10)



############ END RT FIGURES #############




################################################################################################################################################################

Accuracy Analysis

################################################################################################################################################################

sessions=length(group)

for (k in 1:sessions){

	if (k==1){
	dat_all=dat_all_1
	n="first"

	} else {

	if (k==2){
	dat_all=dat_all_2
	n="second"

	} else {

	if (k==3){
	dat_all=dat_all_3
	n="H_first"

	} else {

	if (k==4){
	dat_all=dat_all_4
	n="H_second"

	} else {

	if (k==5){
	dat_all=dat_all_5
	n="control"

	} else {

	if (k==6){
	dat_all=dat_all_6
	n="control_H_first"

	} else {

	if (k==7){
	dat_all=dat_all_7
	n="control_H_second"
	
	}
	}
	}
	}
	}
        }
        }
	

#### GO ACC ####

# Find Go accuracy when correct==1

  sub_nums=names(table(dat_all$subind))
  run_nums=names(table(dat_all$run))

  go_correct=data.frame(matrix(0,nrow=length(sub_nums), ncol=2))
  colnames(go_correct)=c("Run_1", "Run_2")
  rownames(go_correct)=c(sub_nums)

  for (i in 1:length(sub_nums)){
    for (j in 1:length(run_nums)){
      dat_loop=dat_all[(dat_all$subind==sub_nums[i] &  dat_all$run==run_nums[j]),]
      go_correct[i,j]=sum(dat_loop$correct==1)/sum(dat_loop$cond==1|dat_loop$cond==0)
    }
  }

  go_correct$mean <- rowMeans(go_correct, na.rm = TRUE)



# GO_CORRECT CUTOFF

  if (go_correct_cutoff==0){
    assign(paste("go_correct",n,sep="_"),go_correct) 
  } else {
    if (go_correct_cutoff>0){
      go_correct_cutoff_mask=go_correct<go_correct_cutoff

      go_correct[go_correct_cutoff_mask]=NA
      go_correct$mean <- rowMeans(go_correct, na.rm = TRUE)

      assign(paste("go_correct",n,sep="_"),go_correct)
    }
  }



#### STOP ACC ####

#Find Stop accuracy when correct==4

sub_nums=names(table(dat_all$subind))
run_nums=names(table(dat_all$run))

stop_correct=data.frame(matrix(0,nrow=length(sub_nums), ncol=2))

colnames(stop_correct)=c("Run_1", "Run_2")
rownames(stop_correct)=c(sub_nums)

for (i in 1:length(sub_nums)){
for (j in 1:length(run_nums)){

      #get all first trials for this sub within this sent type
      dat_loop=dat_all[(dat_all$subind==sub_nums[i] &  dat_all$run==run_nums[j]),]
      stop_correct[i,j]=sum(dat_loop$correct==4)/sum(dat_loop$cond==2|dat_loop$cond==3)

}
}

stop_correct$mean <- rowMeans(stop_correct, na.rm = TRUE)



#### apply stop_correct cutoff

if (stop_correct_cutoff==0){


assign(paste("stop_correct",n,sep="_"),stop_correct)

} else {

if (stop_correct_cutoff>0){

stop_correct_cutoff_mask=stop_correct<stop_correct_cutoff

stop_correct[stop_correct_cutoff_mask]=NA
stop_correct$mean <- rowMeans(stop_correct, na.rm = TRUE)

assign(paste("stop_correct",n,sep="_"),stop_correct)

}
}


} # END OF ACC LOOP




# DFs TO CHECK
  # go_correct* with _first, _second, _control, H_first, H_second, control_H_first, or control_H_second
  # stop_correct* with _first, _second, _control, H_first, H_second, control_H_first, or control_H_second




######### START ACC FIGURES #########


##### Make identifiers for type of session

sess_1=rep("session_1",length(go_correct_first$mean))
sess_2=rep("session_2",length(go_correct_second$mean))
controls=rep("control",length(go_correct_control$mean))
H_sess_1=rep("session_1",length(go_correct_H_first$mean))
H_sess_2=rep("session_2",length(go_correct_H_second$mean))

A_all_sess_subs=c(sess_1_subs,sess_2_subs,control_subs)
A_all_sess_type=c(sess_1,sess_2,controls)
A_all_subs_type=c(sess_1_subs_type,sess_2_subs_type,control_subs_type)

H_all_sess_subs=c(H_sess_1_subs,H_sess_2_subs,control_subs)
H_all_sess_type=c(H_sess_1,H_sess_2,controls)

all_sess_subs=c(sess_1_subs,H_sess_1_subs,sess_2_subs,H_sess_2_subs,control_subs)
all_sess_type=c(sess_1,H_sess_1,sess_2,H_sess_2,controls)

comb_sess_subs=c(sess_1_subs,H_sess_1_subs,sess_2_subs,H_sess_2_subs,control_subs)
comb_sess_type=c(rep("comb_sess_1",sum(length(go_correct_first$mean),length(go_correct_H_first$mean))),rep("comb_sess_2",sum(length(go_correct_second$mean),length(go_correct_H_second$mean))),controls)


#### AUSTIN SUBJS ACC FIGURES 

#GO CORRECT

	A_all_sess_go_correct=c(go_correct_first$mean,go_correct_second$mean,go_correct_control$mean)
	dat_A_all_sess_go_correct=data.frame(A_all_sess_go_correct,A_all_sess_subs,A_all_sess_type,A_all_subs_type)


	A_all_sess_go_correct_plot=ggplot(dat_A_all_sess_go_correct,aes(x=A_all_sess_type,y=A_all_sess_go_correct)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + ylab("% CORRECT") + ggtitle("GO ACCURACY")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/Austin/SST/ACC/GO_CORRECT/A_go_correct_group_comparison.png",wd),width=10,height=10)

#STOP CORRECT

	A_all_sess_stop_correct=c(stop_correct_first$mean,stop_correct_second$mean,stop_correct_control$mean)
	dat_A_all_sess_stop_correct=data.frame(A_all_sess_go_correct,A_all_sess_subs,A_all_sess_type)


	A_all_sess_stop_correct_plot=ggplot(dat_A_all_sess_stop_correct,aes(x=A_all_sess_type,y=A_all_sess_stop_correct)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + ylab("% CORRECT") + ggtitle("STOP ACCURACY")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/Austin/SST/ACC/STOP_CORRECT/A_stop_correct_group_comparison.png",wd),width=10,height=10)



#### HOUSTON SUBJS FIGURES

#GO CORRECT

	H_all_sess_go_correct=c(go_correct_H_first$mean,go_correct_H_second$mean,go_correct_control$mean)
	dat_H_all_sess_go_correct=data.frame(H_all_sess_go_correct,H_all_sess_subs,H_all_sess_type)


	H_all_sess_go_correct_plot=ggplot(dat_H_all_sess_go_correct,aes(x=H_all_sess_type,y=H_all_sess_go_correct)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ xlab("SESSION TYPE") + ylab("% CORRECT") + ggtitle("GO ACCURACY")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/Houston/SST/ACC/GO_CORRECT/H_go_correct_group_comparison.png",wd),width=10,height=10)

#STOP CORRECT

	H_all_sess_stop_correct=c(stop_correct_H_first$mean,stop_correct_H_second$mean,stop_correct_control$mean)
	dat_H_all_sess_stop_correct=data.frame(H_all_sess_go_correct,H_all_sess_subs,H_all_sess_type)


	H_all_sess_stop_correc_plot=ggplot(dat_H_all_sess_stop_correct,aes(x=H_all_sess_type,y=H_all_sess_stop_correct)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ xlab("SESSION TYPE") + ylab("% CORRECT") + ggtitle("STOP ACCURACY")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/Houston/SST/ACC/STOP_CORRECT/H_stop_correct_group_comparison.png",wd),width=10,height=10)


###### ALL SUBJS ACC FIGURES

#GO CORRECT

	all_sess_go_correct=c(go_correct_first$mean,go_correct_H_first$mean,go_correct_second$mean,go_correct_H_second$mean,go_correct_control$mean)
	dat_all_sess_go_correct=data.frame(all_sess_go_correct,all_sess_subs,all_sess_type)


	all_sess_go_correct_plot=ggplot(dat_all_sess_go_correct,aes(x=all_sess_type,y=all_sess_go_correct)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ xlab("SESSION TYPE") + ylab("% CORRECT") + ggtitle("GO ACCURACY")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/All/SST/ACC/GO_CORRECT/all_go_correct_group_comparison.png",wd),width=10,height=10)

#STOP CORRECT

	all_sess_stop_correct=c(stop_correct_first$mean,stop_correct_H_first$mean,stop_correct_second$mean,stop_correct_H_second$mean,stop_correct_control$mean)
	dat_all_sess_stop_correct=data.frame(all_sess_go_correct,all_sess_subs,all_sess_type)


	all_sess_stop_correct_plot=ggplot(dat_all_sess_stop_correct,aes(x=all_sess_type,y=all_sess_stop_correct)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ xlab("SESSION TYPE") + ylab("% CORRECT") + ggtitle("STOP ACCURACY")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/All/SST/ACC/STOP_CORRECT/all_stop_correct_group_comparison.png",wd),width=10,height=10)
	
	
	
	
	
###### COMBINED SUBJS ACC FIGURES

#GO CORRECT

	comb_sess_go_correct=c(go_correct_first$mean,go_correct_H_first$mean,go_correct_second$mean,go_correct_H_second$mean,go_correct_control$mean)
	dat_comb_sess_go_correct=data.frame(comb_sess_go_correct,comb_sess_subs,comb_sess_type)


	comb_sess_go_correct_plot=ggplot(dat_comb_sess_go_correct,aes(x=comb_sess_type,y=comb_sess_go_correct)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_sess_subs,color=comb_sess_subs))+ xlab("SESSION TYPE") + ylab("% CORRECT") + ggtitle("GO ACCURACY")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/Combined/SST/ACC/GO_CORRECT/comb_go_correct_group_comparison.png",wd),width=10,height=10)

#STOP CORRECT

	comb_sess_stop_correct=c(stop_correct_first$mean,stop_correct_H_first$mean,stop_correct_second$mean,stop_correct_H_second$mean,stop_correct_control$mean)
	dat_comb_sess_stop_correct=data.frame(comb_sess_stop_correct,comb_sess_subs,comb_sess_type)


	comb_sess_stop_correct_plot=ggplot(dat_comb_sess_stop_correct,aes(x=comb_sess_type,y=comb_sess_stop_correct)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_sess_subs,color=comb_sess_subs))+ xlab("SESSION TYPE") + ylab("% CORRECT") + ggtitle("STOP ACCURACY")+ guides(group=FALSE,color=FALSE)
	ggsave(filename=sprintf("%s/figures/Project_4/Combined/SST/ACC/STOP_CORRECT/comb_stop_correct_group_comparison.png",wd),width=10,height=10)



############################## SCATTER PLOTS

##Austin

dat_A_all_sess_go_scatter=data.frame(A_all_sess_go_correct,A_all_sess_gort,A_all_sess_subs,A_all_sess_type,A_all_subs_type)

ggplot(dat_A_all_sess_go_scatter, aes(x=A_all_sess_gort, y=A_all_sess_go_correct,shape=A_all_subs_type,color=A_all_sess_type)) +geom_point(size=3)+ geom_line(aes(group=A_all_sess_subs),color="#990000",linetype="dashed")+ ggtitle("GO RT VS GO %CORRECT")  + xlab("RT (s)") + ylab("% CORRECT")+guides(group=FALSE)


##if you want consistent coordinate plane 

# + coord_cartesian(ylim=c(0,1.05),xlim=c(.2,1.2)) 






















#after_stop_1_go=data.frame(matrix(0,nrow=length(sub_nums), ncol=length(run_nums)))





### AVG ACROSS ALL SUBJS PER SESSION TYPE (IN PROGRESS)


sub_nums=names(table(dat_all$subind))


m=1

for (i in 1:length(sub_nums)){

run_nums=names(table(dat_all$run[dat_all$subind==sub_nums[i]]))

for (j in 1:length(run_nums)){







vec_num=length(dat_all$correct)


vec_pos=matrix(1:vec_num,nrow=vec_num, ncol=1)


vec_stop_mask=dat_all$correct==4 & dat_all$subind==sub_nums[i]  & dat_all$run==run_nums[j]
vec_go_mask=dat_all$correct==1 & dat_all$subind==sub_nums[i]  & dat_all$run==run_nums[j]


vec_pos_stop=vec_pos[vec_stop_mask]
vec_pos_go=vec_pos[vec_go_mask]

vec_pos_stop_num=length(vec_pos_stop)
vec_pos_go_num=length(vec_pos_go)

# reset matrix
total_num_go_trials_after_stop=matrix(0,nrow=vec_pos_stop_num,ncol=1)

for (b in 1:vec_pos_stop_num){

	start=vec_pos_stop[b]
	stop=vec_pos_stop[b+1]

	go_pos_after_stop=vec_pos_go[(vec_pos_go > start) & (vec_pos_go < stop)]

	num_go_trials_after_stop=go_pos_after_stop-start
	total_num_go_trials_after_stop[b]=length(num_go_trials_after_stop)

}



sum_total_num_go_trials_after_stop=sum(total_num_go_trials_after_stop)

total_num_go_trials_after_stop_RT=matrix(0,nrow=sum_total_num_go_trials_after_stop, ncol=1)

RT_pos=matrix(0,nrow=vec_pos_stop_num,ncol=1)

pos_num_go_trials_after_stop=matrix(0,nrow=sum_total_num_go_trials_after_stop, ncol=1)

for (b in 1:vec_pos_stop_num){

	start=vec_pos_stop[b]
	
	if (b==vec_pos_stop_num){
	
	stop=vec_pos_go[vec_pos_go_num]+1
	
	} else {

	stop=vec_pos_stop[b+1]
	
	}

	go_pos_after_stop=vec_pos_go[(vec_pos_go > start) & (vec_pos_go < stop)]

	num_go_trials_after_stop=go_pos_after_stop-start
	
	RT_pos[b]=sum(total_num_go_trials_after_stop[1:b])

	if (b==1){

		if (RT_pos[b]!=0){

		total_num_go_trials_after_stop_RT[1:RT_pos[b]]=dat_all$RT[go_pos_after_stop]

		} else {
		
		}
	

	} else {	

		if (RT_pos[b-1]==RT_pos[b]){
	
		} else {

		total_num_go_trials_after_stop_RT[(RT_pos[b-1]+1):RT_pos[b]]=dat_all$RT[go_pos_after_stop]

		}
	

	
	}


	if (b==1){

		pos_num_go_trials_after_stop[1:RT_pos[b]]=1:total_num_go_trials_after_stop[b]

	} else if (total_num_go_trials_after_stop[b]!=0){

		pos_num_go_trials_after_stop[(RT_pos[b-1]+1):RT_pos[b]]=1:total_num_go_trials_after_stop[b]

	} else {	
	

	}


	
	

sub=sub_nums[i]
	


dat_after_pos_RT=data.frame(pos_num_go_trials_after_stop,total_num_go_trials_after_stop_RT,sub)

assign(pastedag("dat_after_pos_RT",n,m,sep="_"),dat_after_pos_RT)



} ### end of after stop loop



} ##end of run loop
m=m+j


	
} ##end of sub loop 







print (m)
print("run done")


print(m)
print("sub done")


####









} #end session loop

