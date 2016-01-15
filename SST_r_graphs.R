# Authors: Mary Abbe Roe (Dec. 2013 - Jan. 2014)
# This script uses the SST_r_analysis.R script to create graphs for SST behavioral data


# LIBRARIES


library(gtools)
library(ggplot2)
library(plyr)
library(miscTools)

wd=getwd()



# CREATE SAMPLE SIZE VARIABLES (OLD)

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


# Make identifiers for type of session (OLD)

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

#dat_A_inter_1_gort=c(A_all_sess_gort,A_all_sess_subs,A_all_sess_type)






# AUSTIN

  # GO RT BY SESSION



	A_all_sess_gort=c(gort_med_first$mean,gort_med_second$mean,gort_med_control$mean)
	dat_A_all_sess_gort=data.frame(A_all_sess_gort,A_all_sess_subs,A_all_sess_type,A_all_subs_type)

	A_all_sess_gort=ggplot(dat_A_all_sess_gort,aes(x=A_all_sess_type,y=A_all_sess_gort)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + 
                               ylab("RT (s)") + ggtitle("AUSTIN CORRECT GO RT")+ guides(group=FALSE,color=FALSE)
	                       ggsave(filename=sprintf("%s/figures/Project_4/Austin/SST/RT/GO_RT/A_go_rt_group_comparison.png",wd),width=10,height=10)

  # GO RT by Intervention code

	A_all_sess_gort=c(gort_med_first$mean,gort_med_second$mean,gort_med_control$mean)
	dat_A_all_sess_gort=data.frame(A_all_sess_gort,A_all_sess_subs,A_all_sess_type

  # SSRT

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

