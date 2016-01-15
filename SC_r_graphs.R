# Authors: Mary Abbe Roe (Dec. 2013 - Jan. 2013)
# This script uses the SC_r_analysis.R script to create graphs for SC behavioral data


# LIBRARIES

  library(ggplot2)
  library(plyr)

  wd=getwd()


# SOURCE THE SC_r_analysis.R SCRIPT

  source('/corral-repl/utexas/ldrc/SCRIPTS/SC_r_analysis.R')



# CREATE SAMPLE SIZE VARIABLES (OLD)

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

  all_sub_num=sum(sess_1_sub_num, sess_2_sub_num, H_sess_1_sub_num, H_sess_2_sub_num, control_sub_num, control_H_sess_1_sub_num, control_H_sess_2_sub_num)



# MAKE IDENTIFIERS FOR EACH TYPE OF SESSION (OLD)

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

 


# RT AND ACC FIGURES BY SESSION (BOX PLOTS, BAR GRAPHS)

  # AUSTIN

    A_sub_med = rbind(sub_med_1, sub_med_2, sub_med_5)
    A_sess_mean = ddply(A_sub_med, .(groups), summarise, N=length(groups), A_rt_mean = mean(rt_median, na.rm = TRUE), A_rt_se = sd(rt_median, na.rm=TRUE)/sqrt(N), 
                        A_acc_mean = mean(acc_mean, na.rm = TRUE), A_acc_se = sd(acc_mean, na.rm=TRUE)/sqrt(N))


    A_sess_RT_bplot = ggplot(A_sess_mean, aes(x = groups, y = A_rt_mean)) + geom_boxplot() + xlab("Session") + ylab("Mean RT") + ggtitle("Austin Average SC RT")
                        ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/RT/AVG_RT/avg_rt_sess_comparison.png",wd),width=10,height=10)

    A_sess_RT_bgraph = ggplot(A_sess_mean, aes(x = groups, y = A_rt_mean, group = groups, fill = groups)) + geom_bar(position = 'dodge', stat = 'identity') +
                               geom_errorbar(aes(y = A_rt_mean, ymin = A_rt_mean - A_rt_se, ymax = A_rt_mean + A_rt_se), position = position_dodge(.9), width = .2) +
                               ylab("Mean RT") + xlab("Session") + ggtitle("Austin AVERAGE SC RT")
                        ggsave( )

    A_sess_acc_bplot = ggplot(A_sess_mean, aes(x = groups, y = A_acc_mean)) + geom_boxplot() + xlab("Session") + ylab("Mean ACC") + ggtitle("Austin Average SC ACC")
                         ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/ACC/AVG_ACC/avg_acc_sess_comparison.png",wd),width=10,height=10)

    A_sess_acc_bgraph = ggplot(A_sess_mean, aes(x = groups, y = A_acc_mean, group = groups, fill = groups)) + geom_bar(position = 'dodge', stat = 'identity') +
                               geom_errorbar(aes(y = A_acc_mean, ymin = A_acc_mean - A_acc_se, ymax = A_acc_mean + A_acc_se), position = position_dodge(.9), width = .2) +
                               ylab("Mean RT") + xlab("Session") + ggtitle("Austin AVERAGE SC ACC")
                         ggsave( )




    # OLD

      dat_A_all_sess_RT=data.frame(A_all_sess_RT,A_all_sess_subs,A_all_sess_type)

      A_all_sess_RT_plot=ggplot(dat_A_all_sess_RT,aes(x=A_all_sess_type,y=A_all_sess_RT)) +geom_boxplot()+geom_point()+geom_line(aes(group=A_all_sess_subs,color=A_all_sess_subs))+ xlab("SESSION TYPE") + 
                                ylab("RT (S)") + ggtitle("AUSTIN AVG SC RT")+ guides(group=FALSE,color=FALSE)
                         ggsave(filename=sprintf("%s/figures/Project_4/Austin/SC/RT/AVG_RT/avg_rt_group_comparison.png",wd),width=10,height=10)

        


  # HOUSTON

    H_sub_med = rbind(sub_med_3, sub_med_4, sub_med_6, sub_med_7)
    H_sess_mean = ddply(H_sub_med, .(groups), summarise, N=length(groups), H_rt_mean = mean(rt_median, na.rm = TRUE), H_rt_se = sd(rt_median, na.rm=TRUE)/sqrt(N), 
                        H_acc_mean = mean(acc_mean, na.rm = TRUE), H_acc_se = sd(acc_mean, na.rm=TRUE)/sqrt(N))





    # OLD

      dat_H_all_sess_RT=data.frame(H_all_sess_RT,H_all_sess_subs,H_all_sess_type)

      H_all_sess_RT_plot=ggplot(dat_H_all_sess_RT,aes(x=H_all_sess_type,y=H_all_sess_RT)) +geom_boxplot()+geom_point()+geom_line(aes(group=H_all_sess_subs,color=H_all_sess_subs))+ xlab("SESSION TYPE") + 
                                ylab("RT (S)") + ggtitle("HOUSTON AVG SC RT")+ guides(group=FALSE,color=FALSE)
                         ggsave(filename=sprintf("%s/figures/Project_4/Houston/SC/RT/AVG_RT/avg_rt_group_comparison.png",wd),width=10,height=10)


  # ALL GROUPS

    all_sub_med = rbind(sub_med_1, sub_med_2, sub_med_3, sub_med_4, sub_med_5, sub_med_6, sub_med_7)
    all_sess_mean = ddply(all_sub_med, .(groups), summarise, N=length(groups), all_rt_mean = mean(rt_median, na.rm = TRUE), all_rt_se = sd(rt_median, na.rm=TRUE)/sqrt(N), 
                        all_acc_mean = mean(acc_mean, na.rm = TRUE), all_acc_se = sd(acc_mean, na.rm=TRUE)/sqrt(N))



    # OLD

      dat_all_sess_RT=data.frame(all_sess_RT,all_sess_subs,all_sess_type)

      all_sess_RT_plot=ggplot(dat_all_sess_RT,aes(x=all_sess_type,y=all_sess_RT)) +geom_boxplot()+geom_point()+geom_line(aes(group=all_sess_subs,color=all_sess_subs))+ xlab("SESSION TYPE") + 
                              ylab("RT (S)") + ggtitle("ALL AVG SC RT")+ guides(group=FALSE,color=FALSE)
		       ggsave(filename=sprintf("%s/figures/Project_4/All/SC/RT/AVG_RT/avg_rt_group_comparison.png",wd),width=10,height=10)


  # AUSTIN AND HOUSTON SUBS COMBINED

    comb_sub_med = rbind(sub_med_1, sub_med_2, sub_med_3, sub_med_4, sub_med_5, sub_med_6, sub_med_7)
    comb_sess_mean = ddply(all_sub_med, .(groups), summarise, N=length(groups), all_rt_mean = mean(rt_median, na.rm = TRUE), all_rt_se = sd(rt_median, na.rm=TRUE)/sqrt(N), 
                        all_acc_mean = mean(acc_mean, na.rm = TRUE), all_acc_se = sd(acc_mean, na.rm=TRUE)/sqrt(N))

		
    # OLD

      dat_comb_sess_RT=data.frame(comb_sess_RT,comb_sess_subs,comb_sess_type)

      comb_sess_RT_plot=ggplot(dat_comb_sess_RT,aes(x=comb_sess_type,y=comb_sess_RT)) +geom_boxplot()+geom_point()+geom_line(aes(group=comb_sess_subs,color=comb_sess_subs))+ xlab("SESSION TYPE") + 
                               ylab("RT (S)") + ggtitle("COMBINED AVG SC RT")+ guides(group=FALSE,color=FALSE)
                        ggsave(filename=sprintf("%s/figures/Project_4/Combined/SC/RT/AVG_RT/avg_rt_group_comparison.png",wd),width=10,height=10)






# FIGURES BY SENTENCE CONDITION (BOX PLOTS, BAR GRAPHS)

  # AUSTIN  

  # HOUSTON

  # AUSTIN AND HOUSTON

  # COMBINED 




##### EVERYTHING BELOW OLD


# BAR GRAPHS W/ ERROR BARS

  # AUSTIN SUBJS




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






