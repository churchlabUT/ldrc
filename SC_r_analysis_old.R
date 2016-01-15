
# Authors: Leo Olmedo (first); Mary Abbe Roe & Joel Martinez (Dec. - Jan. 2013)
# This script reads in behavioral data for SC and calculates RT and ACC
# Graphs are located in SC_r_graphs.R



#### LIBRARIES, FUNCTIONS, READING IN DATA ####

# LIBRARIES
 
  library(ggplot2)
  library(plyr)
  library(nlme)
  library(grid)

  wd = getwd()

 
# FUNCTIONS   
 
  # function for extracting end of string
    substrRight = function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }

  # function for extracting start of string
    substrLeft = function(x, n){
    substr(x, 1, n)
  }


# READING IN DATA AND CREATING DATA FRAMES FOR EACH GROUP

  # For Houston: subdirs=Sys.glob("/corral-repl/utexas/ldrc/H_*MR1")
  # For Austin: subdirs=Sys.glob("/corral-repl/utexas/ldrc/ldrc_?_[0-9][0-9][0-9]") or subdirs=Sys.glob("/corral-repl/utexas/ldrc/ldrc_*second")


  group = c("ldrc_[0-9]_[0-9][0-9][0-9]", "ldrc_*second", "ldrc_*third", "ldrc2_[0-9]_[0-9][0-9][0-9]","ldrc2_*second", "PHILIPS/H_LD*_[0-9]", "H_LD*_[0-9]_second", "H_LD*_[0-9]_third", "LDFHO*_[0-9]", "LDFHO*_second","ldrc_c_[0-9][0-9][0-9]", "PHILIPS/H_LD*[0-9][0-9][0-9][0-9]", "H_LD*_c_second", "H_LD*_c_third")
  chars=c(10,17,16,11,18,13,20,19,12,19,10,11,20,19)
  #chars = c(5,12,11,11,12,6,13,12,13,14,5,4,13,12)

  # GROUP
  for (i in 1:length(group)){ 
    subdirs = Sys.glob(sprintf("/corral-repl/utexas/ldrc/%s" ,group[i]))
    rm("dat_all")
    print(group[i])
    # SUB
    for (sub in subdirs){
      behav_dirs = c(Sys.glob(sprintf("%s/behav/SC/SC_R*", sub))) #Sys.glob(sprintf("%s/behav/SC/m_SC_R*", sub)), Sys.glob(sprintf("%s/behav/SC/x_SC_R*", sub)), Sys.glob(sprintf("%s/behav/x_SC/SC_R*", sub)))
      subnum = substrRight(sub, chars[i])

	# RUN
        for (run in behav_dirs){
          run_num = substrRight(run,1)
          Rfile = Sys.glob(sprintf("%s/*R.txt", run))

          if (length(Rfile)==0){
            warning(sprintf("cannot find R.txt file for %s", run))
	    next
          }

          dat_loop = read.table(Rfile, header=TRUE, sep="\t", na.strings="NaN")
          dat_loop$subind = rep(subnum, dim(dat_loop)[1])
          dat_loop$runnum = rep(run_num, dim(dat_loop)[1])


          # dat_loop$correct: 1=yes, 0=no, 3=mismatch or RT out of range, 4=no response
          # dat_loop$Cond: 1='active_s', 2='active_ns', 3='passive_s', 4='passive_ns'

          dat_loop$cond_type = dat_loop$Cond
          dat_loop$cond_type[dat_loop$Cond==1] = 'active_s'
          dat_loop$cond_type[dat_loop$Cond==2] = 'active_ns'
          dat_loop$cond_type[dat_loop$Cond==3] = 'passive_s'
          dat_loop$cond_type[dat_loop$Cond==4] = 'passive_ns'

          dat_loop$rep=duplicated(dat_loop$Sentence)
          dat_loop$corr_first=0
          dat_loop$corr_first[dat_loop$correct==1 & dat_loop$rep==FALSE] = 1

          # CREATE DAT_ALL IF DOESN'T EXIST
          if (exists("dat_all")==FALSE){
            dat_all = dat_loop 
          } 
          else{ 
            dat_all = rbind(dat_all, dat_loop)
          }
     
       } # END RUN LOOP

     }  # END SUB LOOP

    # ADD GROUP IDENTIFIER
      dat_all$group_num= i

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

    dat_all_1=dat_all_1[dat_all_1$rep==FALSE,]
    dat_all_2=dat_all_2[dat_all_2$rep==FALSE,]
    dat_all_3=dat_all_3[dat_all_3$rep==FALSE,]
    dat_all_4=dat_all_4[dat_all_4$rep==FALSE,]
    dat_all_5=dat_all_5[dat_all_5$rep==FALSE,]
    dat_all_6=dat_all_6[dat_all_6$rep==FALSE,]
    dat_all_7=dat_all_7[dat_all_7$rep==FALSE,]
    dat_all_8=dat_all_8[dat_all_8$rep==FALSE,]
    dat_all_9=dat_all_9[dat_all_9$rep==FALSE,]
    dat_all_10=dat_all_10[dat_all_10$rep==FALSE,]
    dat_all_11=dat_all_11[dat_all_11$rep==FALSE,]
    dat_all_12=dat_all_12[dat_all_12$rep==FALSE,]
    dat_all_13=dat_all_13[dat_all_13$rep==FALSE,]
    dat_all_14=dat_all_14[dat_all_14$rep==FALSE,]
    

    dat = rbind(dat_all_1,dat_all_2, dat_all_3, dat_all_4, dat_all_5, dat_all_6, dat_all_7, dat_all_8, dat_all_9, dat_all_10, dat_all_11, dat_all_12, dat_all_13, dat_all_14)
    write.csv(dat, file="/corral-repl/utexas/ldrc/SCRIPTS/sc_behav_data_clean.csv")
    dat_int = rbind(dat_all_1,dat_all_2, dat_all_3, dat_all_4, dat_all_5, dat_all_6)

  # DFs CREATED  
    # dat_all_1 = Austin session 1 subs
    # dat_all_2 = Austin session 2 subs
    # dat_all_3 = Houston session 1 subs
    # dat_all_4 = Houston session 2 subs
    # dat_all_5 = Austin controls
    # dat_all_6 = Houston controls session 1
    # dat_all_7 = Houston controls session 2
    # dat       = all groups

    # to see variables in the data frame:  	names(dat_all_1)
    # to see all variables created: 		ls()
    # to see subjects: 				table(dat_all$subind)


#CHECK LIST ORDER

sent = ddply(dat, .(subind,runnum, TrialNum, Sentence), summarize, N = length(subind))
sent = sent[sent$TrialNum == 0,]
sent$order = 'N.A.'
sent$order[sent$Sentence == 'The soup was caught by a ring.'] = 'SC_L1_O1'
sent$order[sent$Sentence == 'The key opened the carrot.'] = 'SC_L1_O2'
sent$order[sent$Sentence == 'A cousin wanted a pillow.'] = 'SC_L1_O3'
sent$order[sent$Sentence == 'A hand was built by the ant.'] = 'SC_L2_O1'
sent$order[sent$Sentence == 'A pillow scratched the fish.'] = 'SC_L2_O2'
sent$order[sent$Sentence == 'A cowgirl caught a snake.'] = 'SC_L2_O3'
sent$order[sent$Sentence == 'Pasta was waved by a fish.'] = 'SC_L3_O1'
sent$order[sent$Sentence == 'A fence walked to the mother.'] = 'SC_L3_O2'
sent$order[sent$Sentence == 'A bird dropped the worm.'] = 'SC_L3_O3'
sent$order[sent$Sentence == 'A test was cooked by a bat.'] = 'SC_L4_O1'
sent$order[sent$Sentence == 'A plate stained a kitten.'] = 'SC_L4_O2'
sent$order[sent$Sentence == 'A woman bought a pizza.'] = 'SC_L4_O3'
sent$order[sent$Sentence == 'A worm was jumped by a dress.'] = 'SC_L5_O1'
sent$order[sent$Sentence == 'The house chewed on the phone.'] = 'SC_L5_O2'
sent$order[sent$Sentence == 'The friend borrowed a book.'] = 'SC_L5_O3'
sent$order[sent$Sentence == 'A wolf was found by a fan.'] = 'SC_L6_O1'
sent$order[sent$Sentence == 'The money counted the singer.'] = 'SC_L6_O2'
sent$order[sent$Sentence == 'The child peeled the banana.'] = 'SC_L6_O3'
write.csv(sent, file=sprintf("%s/data_frames/SC/sentence_list_order.csv", wd), na="NA", row.names=FALSE)

#### RT and ACC ANALYSIS ####


sessions = length(group)

for (k in 1:sessions){

  if (k==1){
  dat_all = dat_all_1
  group = "A_first"

  } else {

  if (k==2){
  dat_all = dat_all_2
  group = "A_second"

  } else {

  if (k==3){
  dat_all = dat_all_3
  group = "A_third"

  } else {

  if (k==4){
  dat_all = dat_all_4
  group = "A2_first"

  } else {

  if (k==5){
  dat_all = dat_all_5
  group = "A2_second"

  } else {

  if (k==6){
  dat_all = dat_all_6
  group = "H_first"

  } else {

  if (k==7){
  dat_all = dat_all_7
  group = "H_second"

  } else {

  if (k==8){
  dat_all = dat_all_8
  group = "H_third"

  } else {

  if (k==9){
  dat_all = dat_all_9
  group = "H2_first"

  } else {

  if (k==10){
  dat_all = dat_all_10
  group = "H2_second"

  } else {

  if (k==11){
  dat_all = dat_all_11
  group = "A_control"

  } else {

  if (k==12){
  dat_all = dat_all_12
  group = "H_control_first"

  } else {

  if (k==13){
  dat_all = dat_all_13
  group = "H_control_second"
  
  } else {

  if (k==14){
  dat_all = dat_all_14
  group = "H_control_third"
  
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

  # RT AND ACC BY RUN

    # MEDIAN (third line is acc cutoff)
 
      run_sub_med = ddply(dat_all, .(subind, runnum, group), summarise, N = length(runnum), rt_median = median(RT[corr_first==1], na.rm = TRUE), acc_avg = mean(corr_first, na.rm = TRUE))
      #run_sub_med$rt_median[run_sub_med$acc_avg < .6] = NA
      #run_sub_med$acc_avg[run_sub_med$acc_avg < .6] = NA
      assign(paste("run_sub_med",k,sep="_"),run_sub_med)

      sub_mean = ddply(run_sub_med, .(subind, group), summarise, N = length(subind), rt_mean = mean(rt_median, na.rm = TRUE), rt_se = sd(rt_median, na.rm = TRUE)/sqrt(N), acc_mean = mean(acc_avg, na.rm = TRUE), acc_se = sd(acc_avg, na.rm = TRUE)/sqrt(N))
      assign(paste("sub_mean",k,sep="_"),sub_mean)

     
    # MEAN

      run_group_mean = ddply(run_sub_med, .(runnum, group), summarise, N = length(runnum), rt_mean = mean(rt_median), rt_se = sd(rt_median, na.rm = TRUE)/sqrt(N), acc_mean = mean(acc_avg), 
                       acc_se = sd(acc_avg, na.rm = TRUE)/sqrt(N))
      assign(paste("run_group_mean",k,sep="_"),run_group_mean)


  # RT AND ACC BY SENTENCE TYPES

    # MEDIAN (third line is acc cutoff)
      bad_runs = run_sub_med[is.na(run_sub_med$rt_median),]

      cond_sub_med = ddply(dat_all, .(subind, runnum, cond_type, group), summarise, N = length(subind), rt_median = median(RT[corr_first==1], na.rm = TRUE), acc_avg = mean(corr_first, na.rm = TRUE))
      
      #Fills in NA for the bad runs
      for(b in 1:dim(bad_runs)[1]){
      	cond_sub_med$rt_median[cond_sub_med$subind == bad_runs$subind[b] & cond_sub_med$runnum == bad_runs$runnum[b]] = NA
      	cond_sub_med$acc_avg[cond_sub_med$subind == bad_runs$subind[b] & cond_sub_med$runnum == bad_runs$runnum[b]] = NA
      }
      
      assign(paste("cond_sub_med",k,sep="_"),cond_sub_med)

    # MEAN

      cond_group_mean = ddply(cond_sub_med, .(cond_type, group), summarise, N = length(cond_type), rt_mean = mean(rt_median, na.rm = T), rt_se = sd(rt_median, na.rm = TRUE)/sqrt(N), acc_mean = mean(acc_avg, na.rm = T), acc_se = sd(acc_avg, na.rm = TRUE)/sqrt(N))
      assign(paste("cond_group_mean",k,sep="_"),cond_group_mean)

} # END OF LOOP





# ALL GROUPS DFs

  # RUN 
    #RT med and acc avg per subject per run
    all_run_med = rbind(run_sub_med_1, run_sub_med_2, run_sub_med_3, run_sub_med_4, run_sub_med_5, run_sub_med_6, run_sub_med_7, run_sub_med_8, run_sub_med_9, run_sub_med_10, run_sub_med_11, run_sub_med_12, run_sub_med_13, run_sub_med_14)
    write.csv(all_run_med, file=sprintf("%s/data_frames/SC/all_subs_groups_by_run_med_m.csv", wd), na="NA", row.names=FALSE)
    
    #rt mean and acc mean per subject across runs
    all_sub_mean = rbind(sub_mean_1, sub_mean_2, sub_mean_3, sub_mean_4, sub_mean_5, sub_mean_6, sub_mean_7, sub_mean_8, sub_mean_9, sub_mean_10, sub_mean_11, sub_mean_12, sub_mean_13, sub_mean_14)
    write.csv(all_sub_mean, file=sprintf("%s/data_frames/SC/all_subs_groups_mean_m.csv", wd), na="NA", row.names=FALSE)

  # CONDITION
    #rt med and acc avg per subject per run per condition
    all_sub_cond_med = rbind(cond_sub_med_1, cond_sub_med_2, cond_sub_med_3, cond_sub_med_4, cond_sub_med_5, cond_sub_med_6, cond_sub_med_7, cond_sub_med_8, cond_sub_med_9, cond_sub_med_10, cond_sub_med_11, cond_sub_med_12, cond_sub_med_13)
    write.csv(all_sub_cond_med, file=sprintf("%s/data_frames/SC/all_subs_groups_by_cond_med_m.csv", wd), na="NA", row.names=FALSE)

    #rt mean and acc mean peer condition across subjects and runs
    all_cond_mean = rbind(cond_group_mean_1, cond_group_mean_2, cond_group_mean_3, cond_group_mean_4, cond_group_mean_5, cond_group_mean_6, cond_group_mean_7, cond_group_mean_8, cond_group_mean_9, cond_group_mean_10, cond_group_mean_11, cond_group_mean_12, cond_group_mean_13)
    write.csv(all_run_med, file=sprintf("%s/data_frames/SC/all_subs_groups_by_cond_mean_m.csv", wd), na="NA", row.names=FALSE)



# FIGURES

  # ACC/RT INTERVENTION PERFORMANCE (AUSTIN AND HOUSTON)

    # BY RUN 
    all_run_med_int = rbind(run_sub_med_1, run_sub_med_2, run_sub_med_3, run_sub_med_4, run_sub_med_5, run_sub_med_6)
  
    acc.rt.run.all = ggplot(all_run_med_int, aes(x = rt_median, y = acc_avg)) + geom_point(aes(colour = groups, size = 0.5, alpha = 0.7), position="identity") + scale_x_discrete(limit = c("1", "2", "3", "4", "5", "6", "7", "8")) + xlab("RT (s)") + ylab("ACC") + ggtitle("Austin and Houston Intervention - SC (ACC and RT)")+ guides(group = FALSE, size = FALSE, alpha = FALSE)+ theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'),axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black')) + scale_colour_brewer(palette="Set1")
                 ggsave(filename=sprintf("%s/figures/Project_4/SC/A_H_int_acc_rt_run_plot.png",wd),width=10,height=10)

    # BY TRIAL
    dat_int_corr = dat_int[dat_int$correct==1 & dat_int$correct_first==1, ]
    dat_int_corr$group[dat_int_corr$group_num==1] = "A_first"
    dat_int_corr$group[dat_int_corr$group_num==2] = "A_second"
    dat_int_corr$group[dat_int_corr$group_num==3] = "A_third"
    dat_int_corr$group[dat_int_corr$group_num==4] = "H_first"
    dat_int_corr$group[dat_int_corr$group_num==5] = "H_second"
    dat_int_corr$group[dat_int_corr$group_num==6] = "H_third"


    rt.corr.trial.all = ggplot(dat_int_corr, aes(x = RT, y = group)) + geom_point(aes(colour = group, size = 0.2, alpha = 0.3), position="identity") + scale_x_continuous(limit=c(0,11), breaks=seq(0,11,1)) + xlab("RT (s)") + ylab("Group") + ggtitle("Austin and Houston Intervention - SC RTs for Correct Trials")+ guides(group = FALSE, size = FALSE, alpha = FALSE, colour = FALSE)+ theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'),axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black')) + scale_colour_brewer(palette="Set1")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/A_H_int_rt_trial_plot.png",wd),width=10,height=10)



  # AUSTIN & HOUSTON INTERVENTION OMISSIONS
    


  # RT AND ACC FIGURES BY SESSION (BOX PLOTS, BAR GRAPHS)
    
  #LARGE DATA FRAMES
    #ALL DATA
    all = rbind(dat_all_1, dat_all_2, dat_all_3, dat_all_4, dat_all_5, dat_all_6, dat_all_7, dat_all_8, dat_all_9, dat_all_10, dat_all_11, dat_all_12, dat_all_13, dat_all_14))
    write.csv(all, file=sprintf("%s/data_frames/SC/JM_all_data.csv", wd), na="NA", row.names=FALSE) 
    
    sents = ddply(all, .(Sentence, cond_type, Resp), summarise, N = length(Resp))
     write.csv(sents, file=sprintf("%s/data_frames/SC/JM_sentences.csv", wd), na="NA", row.names=FALSE) 
     
     sentnum = ddply(all, .(Sentence), summarise, N = length(Sentence))

  
    #GROUPS
      all_sub_mean = rbind(sub_mean_1, sub_mean_2, sub_mean_3, sub_mean_4, sub_mean_5, sub_mean_6, sub_mean_7, sub_mean_8, sub_mean_9, sub_mean_10, sub_mean_11, sub_mean_12, sub_mean_13, sub_mean_14)
      all_sub_mean$subind[all_sub_mean$group=="A_first"]=substrRight(all_sub_mean$subind[all_sub_mean$group=="A_first"],5)
      all_sub_mean$subind[all_sub_mean$group=="A_second"]=substrLeft(all_sub_mean$subind[all_sub_mean$group=="A_second"],10)
      all_sub_mean$subind[all_sub_mean$group=="A_second"]=substrRight(all_sub_mean$subind[all_sub_mean$group=="A_second"],5)
      all_sub_mean$subind[all_sub_mean$group=="A_third"]=substrLeft(all_sub_mean$subind[all_sub_mean$group=="A_third"],10)
      all_sub_mean$subind[all_sub_mean$group=="A_third"]=substrRight(all_sub_mean$subind[all_sub_mean$group=="A_third"],5)
      all_sub_mean$subind[all_sub_mean$group=="A_third"]=substrRight(all_sub_mean$subind[all_sub_mean$group=="A_third"],5)
      all_sub_mean$subind[all_sub_mean$group=="A2_first"]=substrRight(all_sub_mean$subind[all_sub_mean$group=="A2_first"],5)
      all_sub_mean$subind[all_sub_mean$group=="A2_second"]=substrLeft(all_sub_mean$subind[all_sub_mean$group=="A2_second"],11)
      all_sub_mean$subind[all_sub_mean$group=="A2_second"]=substrRight(all_sub_mean$subind[all_sub_mean$group=="A2_second"],5) 
      all_sub_mean$subind[all_sub_mean$group=="H_first"]=substrRight(all_sub_mean$subind[all_sub_mean$group=="H_first"],6) 
      all_sub_mean$subind[all_sub_mean$group=="H_second"]=substrLeft(all_sub_mean$subind[all_sub_mean$group=="H_second"],13)
      all_sub_mean$subind[all_sub_mean$group=="H_second"]=substrRight(all_sub_mean$subind[all_sub_mean$group=="H_second"],6) 
      all_sub_mean$subind[all_sub_mean$group=="H_third"]=substrLeft(all_sub_mean$subind[all_sub_mean$group=="H_third"],13)
      all_sub_mean$subind[all_sub_mean$group=="H_third"]=substrRight(all_sub_mean$subind[all_sub_mean$group=="H_third"],6)  
      all_sub_mean$subind[all_sub_mean$group=="H2_first"]=substrRight(all_sub_mean$subind[all_sub_mean$group=="H2_first"],7) 
      all_sub_mean$subind[all_sub_mean$group=="H2_second"]=substrLeft(all_sub_mean$subind[all_sub_mean$group=="H2_second"],12)
      all_sub_mean$subind[all_sub_mean$group=="H2_second"]=substrRight(all_sub_mean$subind[all_sub_mean$group=="H2_second"],7)  
      all_sub_mean$subind[all_sub_mean$group=="H_control_second"]=substrLeft(all_sub_mean$subind[all_sub_mean$group=="H_control_second"],13)   
      all_sub_mean$subind[all_sub_mean$group=="H_control_third"]=substrLeft(all_sub_mean$subind[all_sub_mean$group=="H_control_third"],13) 
      all_sub_mean$group2 = all_sub_mean$group
      all_sub_mean$group2[grep('first', all_sub_mean$group)] = "first"
      all_sub_mean$group2[grep('second', all_sub_mean$group)] = "second"
      all_sub_mean$group2[grep('third', all_sub_mean$group)] = "third"
      all_sub_mean$group2[grep('control', all_sub_mean$group)] = "control"
      all_sub_mean$city = 'austin'
      all_sub_mean$city[grep('H_', all_sub_mean$group)] = 'houston'
      all_sub_mean$city[grep('H2_', all_sub_mean$group)] = 'houston'
      all_sub_mean$ID = substrLeft(all_sub_mean$subind, 1)
      all_sub_mean$ID[all_sub_mean$city == 'houston'] = substrRight(all_sub_mean$subind[all_sub_mean$city == 'houston'], 1)
      all_sub_mean$ID[grep('H_control', all_sub_mean$group)] = 'c'
      all_sub_mean$ID2 = all_sub_mean$ID
      all_sub_mean$ID2[all_sub_mean$ID == "2"] = "1"

      #to not include thirds and houston firsts in data frame
      all_sub_mean = all_sub_mean[(all_sub_mean$group2!="third" & all_sub_mean$group!="H_first" & all_sub_mean$group!="H_control_first" & all_sub_mean$group!="H_control_third"),]

      all_sub_mean$rep = 'u'
      all_sub_mean$rep[(duplicated(all_sub_mean['subind'])) | (duplicated(all_sub_mean['subind'], fromLast = TRUE))] = 'r'		# this line of code takes into account Austin Thirds and labels firsts or seconds repeats if they exists as thirds too
     write.csv(all_sub_mean, file=sprintf("%s/data_frames/SC/JM_all_subs_mean.csv", wd), na="NA", row.names=FALSE) 
     
     
     subnum = ddply(all_sub_mean, .(groups), summarise, N = length(groups))
      
    
    #CONDITION
      all_sub_cond_med$subind[all_sub_cond_med$groups=="A_second"]=substrLeft(all_sub_cond_med$subind[all_sub_cond_med$groups=="A_second"],5) 
      all_sub_cond_med$subind[all_sub_cond_med$groups=="A_third"]=substrLeft(all_sub_cond_med$subind[all_sub_cond_med$groups=="A_third"],5) 
      all_sub_cond_med$subind[all_sub_cond_med$groups=="H_second"]=substrLeft(all_sub_cond_med$subind[all_sub_cond_med$groups=="H_second"],6)
      all_sub_cond_med$subind[all_sub_cond_med$groups=="H_third"]=substrLeft(all_sub_cond_med$subind[all_sub_cond_med$groups=="H_third"],6)  
      all_sub_cond_med$subind[all_sub_cond_med$groups=="H_control_second"]=substrLeft(all_sub_cond_med$subind[all_sub_cond_med$groups=="H_control_second"],4)   
      all_sub_cond_med$subind[all_sub_cond_med$groups=="H_control_third"]=substrLeft(all_sub_cond_med$subind[all_sub_cond_med$groups=="H_control_third"],4)   
      all_sub_cond_med$city = 'austin'
      all_sub_cond_med$city[grep('H_', all_sub_cond_med$groups)] = 'houston'
      all_sub_cond_med$sense = 'sense'
      all_sub_cond_med$sense[grep('ns', all_sub_cond_med$cond_type)] = 'nonsense'
      all_sub_cond_med$active = 'active'
      all_sub_cond_med$active[grep('passive', all_sub_cond_med$cond_type)] = 'passive'
      all_sub_cond_med$ID = substrLeft(all_sub_cond_med$subind, 1)
      all_sub_cond_med$ID[all_sub_cond_med$city == 'houston'] = substrRight(all_sub_cond_med$subind[all_sub_cond_med$city == 'houston'], 1)
      all_sub_cond_med$ID[grep('H_control', all_sub_cond_med$groups)] = 'c'
      all_sub_cond_med$subtype = 'c'
      all_sub_cond_med$subtype[!(all_sub_cond_med$ID == 'c')] = 'i'
	dups = data.frame(subind = all_sub_mean$subind, groups = all_sub_mean$groups, rep = all_sub_mean$rep)
	all_sub_cond_med = merge(all_sub_cond_med, dups, by = c('subind', 'groups'))
	all_sub_cond_med$city.sense = as.factor(sprintf("%s%s", all_sub_cond_med$city, all_sub_cond_med$sense))
	all_sub_cond_med$city.active = as.factor(sprintf("%s%s", all_sub_cond_med$city, all_sub_cond_med$active))
	all_sub_cond_med$subtype.sense = as.factor(sprintf("%s%s", all_sub_cond_med$subtype, all_sub_cond_med$sense))
	all_sub_cond_med$groups.sense = as.factor(sprintf("%s%s", all_sub_cond_med$groups, all_sub_cond_med$sense))
	all_sub_cond_med$ID2 = all_sub_cond_med$ID
	all_sub_cond_med$ID2[all_sub_cond_med$ID2 ==2] = 1
      write.csv(all_sub_cond_med, file=sprintf("%s/data_frames/SC/JM_all_sub_cond_med.csv", wd), na="NA", row.names=FALSE) 
     
     #All Runs
     	all_run = rbind(run_sub_med_1, run_sub_med_2, run_sub_med_3, run_sub_med_4, run_sub_med_5, run_sub_med_6, run_sub_med_7, run_sub_med_8, run_sub_med_9, run_sub_med_10)
	all_run$subind[all_run$groups=="A_second"]=substrLeft(all_run$subind[all_run$groups=="A_second"],5)
 	all_run$subind[all_run$groups=="A_third"]=substrLeft(all_run$subind[all_run$groups=="A_third"],5)
	all_run$subind[all_run$groups=="H_second"]=substrLeft(all_run$subind[all_run$groups=="H_second"],6)
 	all_run$subind[all_run$groups=="H_third"]=substrLeft(all_run$subind[all_run$groups=="H_third"],6)
	all_run$subind[all_run$groups=="H_control_second"]=substrLeft(all_run$subind[all_run$groups=="H_control_second"],4)
	all_run$subind[all_run$groups=="H_control_third"]=substrLeft(all_run$subind[all_run$groups=="H_control_third"],4)   
	all_run$city = 'austin'
	all_run$city[grep('H_', all_run$groups)] = 'houston'
	all_run$ID = substrLeft(all_run$subind, 1)
	all_run$ID[all_run$city == 'houston'] = substrRight(all_run$subind[all_run$city == 'houston'], 1)
	all_run$ID[grep('H_control', all_run$groups)] = 'c'
	all_run$subtype = 'c'
	all_run$subtype[!(all_run$ID == 'c')] = 'i'
	

      

    # AUSTIN

      A_sub_mean = all_sub_mean[all_sub_mean$city == 'austin',]
      A_sub_mean_fs = A_sub_mean[(A_sub_mean$group2!="third"), ]
      A_sub_mean_fs$rep2 = 'u'
      A_sub_mean_fs$rep2[(duplicated(A_sub_mean_fs['subind'])) | (duplicated(A_sub_mean_fs['subind'], fromLast = TRUE))] = 'r'		
 
      write.csv(A_sub_mean_fs, file=sprintf("%s/data_frames/SC/A_subs_mean.csv", wd), na="NA", row.names=FALSE)
    
      A_group_mean = ddply(A_sub_mean, .(group), summarise, N=length(group), A_rt_mean = mean(rt_mean, na.rm = TRUE), A_rt_se = sd(rt_mean, na.rm=TRUE)/sqrt(N), 
                     A_acc_mean = mean(acc_mean, na.rm = TRUE), A_acc_se = sd(acc_mean, na.rm=TRUE)/sqrt(N))

      A_group_mean_fs$groups[A_group_mean_fs$group == "A_control"] = "Typical Readers"
      A_group_mean_fs$groups[A_group_mean_fs$group == "A_first"] = "Struggling Readers Pre"
      A_group_mean_fs$groups[A_group_mean_fs$group == "A_second"] = "Struggling Readers Post"

      A_sess_RT_box = ggplot(A_sub_mean[A_sub_mean$groups!="A_first2",], aes(x = groups, y = rt_mean)) + geom_boxplot(aes(fill = groups)) + geom_point(data= A_sub_mean[(A_sub_mean$ID2 =="0" | A_sub_mean$ID2=="1"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black")) + scale_shape_manual(values=c(17,2))  + geom_line(data= A_sub_mean[(A_sub_mean$ID2 =="0" | A_sub_mean$ID2=="1"),],aes(group = subind, color = ID2, linetype = ID2))+ xlab("Groups") + ylab("Mean RT (s)") + ggtitle("Sentence Comprehension Mean RT") + guides(size=FALSE) + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size = rel(1.5)), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10))
                        ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/A_avg_rt_sess_box_mri.png",wd),width=10,height=10)

      A_sess_RT_box_aug15 = ggplot(A_sub_mean_fs, aes(x = group2, y = rt_mean)) + geom_boxplot(aes(fill = group2)) + geom_point(data= A_sub_mean_fs[((A_sub_mean_fs$ID2 =="0" | A_sub_mean_fs$ID2=="1" | A_sub_mean_fs$ID2=="c")),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ geom_line(data= A_sub_mean_fs[((A_sub_mean_fs$ID2 =="0" | A_sub_mean_fs$ID2=="1")),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
#legend.title = element_text(size = rel(1.5)), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10)) + + guides(group = FALSE, fill = FALSE, color=FALSE, shape=FALSE, linetype=FALSE)
                        ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/A_avg_rt_box_aug15.pdf",wd),width=5,height=5)

      A_sess_RT_box_aug15_noline = ggplot(A_sub_mean_fs, aes(x = group2, y = rt_mean)) + geom_boxplot(aes(fill = group2)) + geom_point(data= A_sub_mean_fs[((A_sub_mean_fs$ID2 =="0" | A_sub_mean_fs$ID2=="1" | A_sub_mean_fs$ID2=="c")),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
#legend.title = element_text(size = rel(1.5)), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10)) + + guides(group = FALSE, fill = FALSE, color=FALSE, shape=FALSE, linetype=FALSE)
                        ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/A_avg_rt_box_aug15_noline.pdf",wd),width=5,height=5)








      A_sess_RT_bar_fs = ggplot(A_group_mean_fs, aes(x = groups, y = A_rt_mean, group = groups, fill = groups)) + geom_bar(position = 'dodge', stat = 'identity') +
                                 geom_errorbar(aes(y = A_rt_mean, ymin = A_rt_mean - A_rt_se, ymax = A_rt_mean + A_rt_se), position = position_dodge(.9), width = .2) +
                                ylab("Mean Response Time (s)") + xlab("Group") + #ggtitle("Sentence Comprehension Response Time")
				 + scale_fill_discrete(name = 'Group', breaks = c('Typical Readers', 'Struggling Readers Pre', 'Struggling Readers Post'), labels = c(sprintf('Typical Readers (%d)',
				 A_group_mean_fs$N[A_group_mean_fs$groups == 'Typical Readers']),sprintf('Struggling Readers Pre (%d)',
			         A_group_mean_fs$N[A_group_mean_fs$groups == 'Struggling Readers Pre']), sprintf('Struggling Readers Post (%d)',A_group_mean_fs$N[A_group_mean_fs$groups == 'Struggling Readers Post']))) + scale_y_continuous(expand=c(0,0)) +
                                 theme_classic() + theme(axis.title.y = element_text(size = rel(1.5), vjust=0.3), axis.title.x = element_text(size = rel(1.5)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size = rel(1.2)), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10))
                         ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/A_avg_rt_sess_bar_fs.png",wd),width=10,height=10)

      A_sess_RT_bar_fs2 = ggplot(A_group_mean_fs, aes(x = groups, y = A_rt_mean, group = groups, fill = groups)) + geom_bar(colour= 'black', position = 'dodge', stat = 'identity', width = .8) +
                                 geom_errorbar(aes(y = A_rt_mean, ymin = A_rt_mean - A_rt_se, ymax = A_rt_mean + A_rt_se), position = position_dodge(.9), width = .2) +
                                ylab("Mean Response Time (s)") + xlab("Group") + guides(fill=FALSE)+
                                 theme_classic() + theme(axis.title.y = element_text(size = rel(1.5), vjust=0.3), axis.title.x = element_text(size = rel(1.5)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
                         ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/A_avg_rt_sess_bar_fs2.png",wd),width=10,height=10)



      A_sess_ACC_box = ggplot(A_sub_mean[A_sub_mean$groups!="A_first2",], aes(x = groups, y = acc_mean)) + geom_boxplot(aes(fill = groups)) + geom_point(data= A_sub_mean[(A_sub_mean$ID2 =="0" | A_sub_mean$ID2=="1"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black")) + scale_shape_manual(values=c(17,2))  + geom_line(data= A_sub_mean[(A_sub_mean$ID2 =="0" | A_sub_mean$ID2=="1"),],aes(group = subind, color = ID2, linetype = ID2))+ xlab("Groups") + ylab("Mean Accuracy") + ggtitle("Sentence Comprehension Mean Accuracy") + guides(size=FALSE) + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size = rel(1.5)), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10)) 
                         ggsave(filename=sprintf("%s/figures/Project_4/SC/ACC/A_avg_acc_sess_box_mri.png",wd),width=10,height=10)

      A_sess_ACC_box_aug15 = ggplot(A_sub_mean_fs, aes(x = group2, y = acc_mean)) + geom_boxplot(aes(fill =group2)) + geom_point(data= A_sub_mean_fs[(A_sub_mean_fs$ID2 =="0" | A_sub_mean_fs$ID2=="1" |  A_sub_mean_fs$ID2=="c"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ geom_line(data= A_sub_mean_fs[(A_sub_mean_fs$ID2 =="0" | A_sub_mean_fs$ID2=="1"),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") +  ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
#legend.title = element_text(size = rel(1.5)), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10)) + + guides(group = FALSE,fill = FALSE, shape=FALSE, color=FALSE, linetype=FALSE)
                         ggsave(filename=sprintf("%s/figures/Project_4/SC/ACC/A_avg_acc_box_aug15.pdf",wd),width=5,height=5)


      A_sess_ACC_box_aug15_noline = ggplot(A_sub_mean_fs, aes(x = group2, y = acc_mean)) + geom_boxplot(aes(fill =group2)) + geom_point(data= A_sub_mean_fs[(A_sub_mean_fs$ID2 =="0" | A_sub_mean_fs$ID2=="1" |  A_sub_mean_fs$ID2=="c"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") +  ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
#legend.title = element_text(size = rel(1.5)), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10)) + + guides(group = FALSE,fill = FALSE, shape=FALSE, color=FALSE, linetype=FALSE)
                         ggsave(filename=sprintf("%s/figures/Project_4/SC/ACC/A_avg_acc_box_aug15_noline.pdf",wd),width=5,height=5)





     A_sess_ACC_bar_fs = ggplot(A_group_mean_fs, aes(x = groups, y = A_acc_mean, group = groups, fill = groups)) + geom_bar(colour='black', position = 'dodge', stat = 'identity', width = .8) +
                               geom_errorbar(aes(y = A_acc_mean, ymin = A_acc_mean - A_acc_se, ymax = A_acc_mean + A_acc_se), position = position_dodge(.9), width = .2) +
                               ylab("Mean Accuracy") + xlab("Group") + guides(fill=FALSE) + #ggtitle("Sentence Comprehension Accuracy") +
			       #+ scale_fill_discrete(name = 'groups', breaks = c('A_control','A_first', 'A_second'), labels = c(sprintf('A_control (%d)',
			       #A_group_mean$N[A_group_mean$groups == 'A_control']), sprintf('A_first (%d)',
			       # A_group_mean$N[A_group_mean$groups == 'A_first']), sprintf('A_second (%d)',A_group_mean$N[A_group_mean$groups == 'A_second']))) + scale_y_continuous(expand=c(0,0)) + 
                               theme_classic()  + theme(axis.title.y = element_text(size = rel(1.5), vjust=0.3), axis.title.x = element_text(size = rel(1.5)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size = rel(1.2)), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10))

                          ggsave(filename=sprintf("%s/figures/Project_4/SC/ACC/A_avg_acc_sess_bar_fs.png",wd),width=10,height=10)
	
	#CONDITION	  
	A_sub_cond = all_sub_cond_med[all_sub_cond_med$city == 'austin' &  all_sub_cond_med$groups !='A_first2',]
	A_sub_cond$year = substrLeft(A_sub_cond$subind, 1)
	
	A_sub_cond_mean = ddply(A_sub_cond, .(subind, cond_type, year, ID), summarise, N = length(cond_type), rt_avg = mean(rt_median, na.rm = T), rt_se = sd(rt_median, na.rm = T)/sqrt(N), acc_mean = mean(acc_avg, na.rm = T), acc_se = sd(acc_avg, na.rm = T))

	A_sub_cond_mean.t = ddply(A_sub_cond[A_sub_cond$groups!="A_third",], .(subind, sense, active, year, ID), summarise, N = length(cond_type), rt_avg = mean(rt_median, na.rm = T), rt_se = sd(rt_median, na.rm = T)/sqrt(N), acc_mean = mean(acc_avg, na.rm = T), acc_se = sd(acc_avg, na.rm = T))

	A_sub_cond_mean2 = ddply(A_sub_cond[A_sub_cond$groups!="A_third",], .(subind, cond_type, groups, ID), summarise, N = length(cond_type), rt_avg = mean(rt_median, na.rm = T), rt_se = sd(rt_median, na.rm = T)/sqrt(N), acc_mean = mean(acc_avg, na.rm = T), acc_se = sd(acc_avg, na.rm = T))
        write.csv(A_sub_cond_mean2,file=sprintf("%s/data_frames/SC/A_sub_cond_mean.csv", wd), na="NA", row.names=FALSE)

        A_sub_cond_mean2$groups = as.factor(A_sub_cond_mean2$groups)
        A_sub_cond_mean2$cond_type = as.factor(A_sub_cond_mean2$cond_type)

        A_sub_cond_mean2$groups = relevel(A_sub_cond_mean2$groups, ref = "A_control")
        A_sub_cond_mean2$groups = relevel(A_sub_cond_mean2$groups, ref = "A_first")
        A_sub_cond_mean2$groups = relevel(A_sub_cond_mean2$groups, ref = "A_second")

        A_sub_cond_mean2$cond_type = relevel(A_sub_cond_mean2$cond_type, ref = "active_ns")
        A_sub_cond_mean2$cond_type = relevel(A_sub_cond_mean2$cond_type, ref = "active_s")
        A_sub_cond_mean2$cond_type = relevel(A_sub_cond_mean2$cond_type, ref = "passive_ns")
        A_sub_cond_mean2$cond_type = relevel(A_sub_cond_mean2$cond_type, ref = "passive_s")


        rt.by.sense = t.test(rt_avg ~ sense, data=A_sub_cond_mean.t)
        acc.by.sense = t.test(acc_mean ~ sense,  data=A_sub_cond_mean.t)

        rt.by.active = t.test(rt_avg ~ active, data=A_sub_cond_mean.t)
        acc.by.active = t.test(acc_mean ~ active,  data=A_sub_cond_mean.t)



        mod.anova.rt1 = lme(rt_avg ~ cond_type*groups, random=~1|subind, data=A_sub_cond_mean2, method="ML")
        rt.cond.groups.int = anova(mod.anova.rt1)
	rt.cond.groups.int.sum = summary(mod.anova.rt1)$tTable

        write.csv(rt.cond.groups.int,file=sprintf("%s/data_frames/SC/anov_rt_cond_groups_int2.csv", wd), na="NA", row.names=TRUE)
        write.csv(rt.cond.groups.int.sum,file=sprintf("%s/data_frames/SC/anov_rt_cond_groups_int_summary2.csv", wd), na="NA", row.names=TRUE)

        mod.anova.rt2 = lme(rt_avg ~ cond_type+groups, random=~1|subind, data=A_sub_cond_mean2[A_sub_cond_mean2$groups!="A_third",], method="ML")
        rt.cond.groups.me = anova(mod.anova.rt2)
        rt.cond.groups.me.sum = summary(mod.anova.rt2)$tTable

        write.csv(rt.cond.groups.me,file=sprintf("%s/data_frames/SC/anov_rt_cond_groups_me4.csv", wd), na="NA", row.names=TRUE)
        write.csv(rt.cond.groups.me.sum,file=sprintf("%s/data_frames/SC/anov_rt_cond_groups_me_summary4.csv", wd), na="NA", row.names=TRUE)

        mod.anova.acc1 = lme(acc_mean ~ cond_type*groups, random=~1|subind, data=A_sub_cond_mean2[A_sub_cond_mean2$groups!="A_third",], method="ML")
        acc.cond.groups.int = anova(mod.anova.acc1)
        acc.cond.groups.int.sum = summary(mod.anova.acc1)$tTable

        write.csv(acc.cond.groups.int,file=sprintf("%s/data_frames/SC/anov_acc_cond_groups_int2.csv", wd), na="NA", row.names=TRUE)
        write.csv(acc.cond.groups.int.sum,file=sprintf("%s/data_frames/SC/anov_acc_cond_groups_int_summary2.csv", wd), na="NA", row.names=TRUE)

        mod.anova.acc2 = lme(acc_mean ~ cond_type+groups, random=~1|subind, data=A_sub_cond_mean2[A_sub_cond_mean2$groups!="A_third",], method="ML")
        acc.cond.groups.me = anova(mod.anova.acc2)
        acc.cond.groups.me.sum = summary(mod.anova.acc2)$tTable

        write.csv(acc.cond.groups.me,file=sprintf("%s/data_frames/SC/anov_acc_cond_groups_me4.csv", wd), na="NA", row.names=TRUE)
        write.csv(acc.cond.groups.me.sum,file=sprintf("%s/data_frames/SC/anov_acc_cond_groups_me_summary4.csv", wd), na="NA", row.names=TRUE)

				
	A_cond_rt_box =  ggplot(A_sub_cond_mean, aes(x = groups, y = rt_avg)) + geom_boxplot(aes(fill =groups)) + geom_point(aes(group = subind,
	                        color = ID)) + xlab("Sentence Type") + geom_line(aes(group = subind,
	                        color = ID)) + ylab("Median RT (s)") + ggtitle("Austin SC Condition RT") + theme_bw()
			 ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/A_cond_rt_box.png",wd),width=10,height=10)

        library(grid)

        A_cond_rt_box_fs = ggplot(data = A_sub_cond_mean2[A_sub_cond_mean2$groups!="A_third",], aes(x = cond_type, y = rt_avg)) + geom_boxplot(aes(fill = cond_type)) + guides(fill=F)+ xlab("Groups") + ylab("Mean RT (s)") +  scale_fill_manual(values=c("plum3","chocolate3", "mistyrose2","goldenrod1" )) +facet_grid(.~groups) +theme_bw() + theme(axis.title.y = element_text(size = rel(2), vjust=0.3), axis.title.x = element_text(size = rel(2)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
			 ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/A_cond_rt_box_fs.pdf",wd),width=5,height=5)	
				
	A_cond_acc_box =  ggplot(A_sub_cond_mean, aes(x = cond_type, y = acc_mean)) + geom_boxplot()  + geom_point(aes(group = subind,
	                         color = year)) + xlab("Session") + geom_line(aes(group = subind,
	                         color = year)) + 
                                 ylab("Mean Accuracy") + ggtitle("Austin SC Condition ACC") + theme_classic()
			  ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/A_cond_acc_box.png",wd),width=10,height=10)

        A_cond_acc_box_fs = ggplot(data = A_sub_cond_mean2[A_sub_cond_mean2$groups!="A_third",], aes(x = cond_type, y = acc_mean)) + geom_boxplot(aes(fill = cond_type)) + xlab("Groups") + ylab("Mean Accuracy") +  scale_fill_manual(values=c("plum3","chocolate3", "mistyrose2","goldenrod1" )) + facet_grid(.~groups) +theme_bw() + theme(axis.title.y = element_text(size = rel(2), vjust=0.3), axis.title.x = element_text(size = rel(2)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size = rel(1.5)), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10))
			 ggsave(filename=sprintf("%s/figures/Project_4/SC/ACC/A_cond_acc_box_fs_legend.pdf",wd),width=5,height=5)	
	


    # HOUSTON

      H_sub_mean = all_sub_mean[all_sub_mean$city == 'houston',]
      H_sub_mean = H_sub_mean[(H_sub_mean$group2!="third" & H_sub_mean$group!="H_first" & H_sub_mean$group!="H_control_first"),]
    
      H_group_mean =   ddply(H_sub_mean, .(group), summarise, N = length(group), H_rt_mean = mean(rt_mean, na.rm = T), H_rt_se = sd(rt_mean, na.rm = T)/sqrt(N), 
      			H_acc_mean = mean(acc_mean, na.rm = T), H_acc_se = sd(acc_mean, na.rm = T)/sqrt(N))


      H_sess_RT_box = ggplot(H_sub_mean, aes(x = groups, y = rt_mean)) + geom_boxplot() + geom_point() + geom_line(aes(group = subind, color = subind))+ xlab("Session") + 
                               ylab("Mean RT (s)") + ggtitle("Houston SC RT")+ guides(group = FALSE,color = FALSE)
                        ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/H_avg_rt_sess_box.png",wd),width=10,height=10)
			
      H_sess_RT_bar = ggplot(H_group_mean, aes(x = groups, y = H_rt_mean, group = groups, fill = groups)) + geom_bar(position = 'dodge', stat = 'identity') +
      			geom_errorbar(aes(y = H_rt_mean, ymin = H_rt_mean - H_rt_se, ymax = H_rt_mean + H_rt_se), position = position_dodge(.9), width = .2) + 
			ylab("Mean RT (s)") + xlab('Session') + ggtitle('Houston SC RT') + 
			scale_fill_discrete(name = 'groups', breaks = c('H_control_first', 'H_control_second', 'H_control_third','H_first', 'H_second', 'H_third'), labels =
			  c(sprintf('H_control_first (%d)', H_group_mean$N[H_group_mean$groups == 'H_control_first']), sprintf('H_control_second (%d)',
			  H_group_mean$N[H_group_mean$groups == 'H_control_second']), sprintf('H_control_third (%d)',
			  H_group_mean$N[H_group_mean$groups == 'H_control_third']), sprintf('H_First (%d)', H_group_mean$N[H_group_mean$groups == 'H_first']),
			  sprintf('H_second (%d)', H_group_mean$N[H_group_mean$groups == 'H_second']), sprintf('H_third (%d)', H_group_mean$N[H_group_mean$groups == 'H_third'])))
			ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/H_avg_rt_sess_bar.png",wd),width=10,height=10)


      H_sess_ACC_box = ggplot(H_sub_mean, aes(x = groups, y = acc_mean)) + geom_boxplot() + geom_point() + geom_line(aes(group = subind, color = subind))+ xlab("Session") + 
                               ylab("Mean Accuracy") + ggtitle("Houston SC ACC")+ guides(group = FALSE,color = FALSE)
                         ggsave(filename=sprintf("%s/figures/Project_4/SC/ACC/H_avg_acc_sess_box.png",wd),width=10,height=10)
   
      H_sess_ACC_bar = ggplot(H_group_mean, aes(x = groups, y = H_acc_mean, group = groups, fill = groups)) + geom_bar(position = 'dodge', stat = 'identity') +
      			geom_errorbar(aes(y = H_acc_mean, ymin = H_acc_mean - H_acc_se, ymax = H_acc_mean + H_acc_se), position = position_dodge(.9), width = .2) + 
			ylab("Mean Accuracy") + xlab('Session') + ggtitle('Houston SC ACC') +
			scale_fill_discrete(name = 'groups', breaks = c('H_control_first', 'H_control_second', 'H_control_third', 'H_first', 'H_second', 'H_third'), labels =
			  c(sprintf('H_control_first (%d)', H_group_mean$N[H_group_mean$groups == 'H_control_first']), sprintf('H_control_second (%d)',
			  H_group_mean$N[H_group_mean$groups == 'H_control_second']), sprintf('H_control_third (%d)',
			  H_group_mean$N[H_group_mean$groups == 'H_control_third']), sprintf('H_first (%d)', H_group_mean$N[H_group_mean$groups == 'H_first']),
			  sprintf('H_second (%d)', H_group_mean$N[H_group_mean$groups == 'H_second']), sprintf('H_third (%d)', H_group_mean$N[H_group_mean$groups == 'H_third'])))
			ggsave(filename=sprintf("%s/figures/Project_4/SC/ACC/H_avg_acc_sess_bar.png",wd),width=10,height=10)



    # ALL GROUPS

      all_sub_mean_fs = all_sub_mean[(all_sub_mean$group2!="third" & all_sub_mean$group!="H_first" & all_sub_mean$group!="H_control_first" & all_sub_mean$group!="H_control_third"),]







      
      all_group_mean = ddply(all_sub_mean[(all_sub_mean$group2!="third" & all_sub_mean$group!="H_first" & all_sub_mean$group!="H_control_first" & all_sub_mean$group!="H_control_third"),], .(group), summarise, N = length(group), all_rt_mean = mean(rt_mean, na.rm = T), all_rt_se = sd(rt_mean, na.rm =T)/sqrt(N),all_acc_mean = mean(acc_mean, na.rm = T), all_acc_se = sd(acc_mean, na.rm = T)/sqrt(N))

      all_comb_mean = ddply(all_sub_mean[(all_sub_mean$group2!="third" & all_sub_mean$group!="H_first" & all_sub_mean$group!="H_control_first" & all_sub_mean$group!="H_control_third"),], .(group2), summarise, N = length(group2), all_rt_mean = mean(rt_mean, na.rm = T), all_rt_se = sd(rt_mean, na.rm =T)/sqrt(N),all_acc_mean = mean(acc_mean, na.rm = T), all_acc_se = sd(acc_mean, na.rm = T)/sqrt(N))

      All_sess_RT_box_aug15 = ggplot(all_sub_mean_fs, aes(x = group2, y = rt_mean)) + geom_boxplot(aes(fill = group2)) + geom_point(data= all_sub_mean_fs[((all_sub_mean_fs$ID2 =="0" | all_sub_mean_fs$ID2=="1" | all_sub_mean_fs$ID2=="c")),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ geom_line(data= all_sub_mean_fs[((all_sub_mean_fs$ID2 =="0" | all_sub_mean_fs$ID2=="1")),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
#legend.title = element_text(size = rel(1.5)), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10)) + + guides(group = FALSE, fill = FALSE, color=FALSE, shape=FALSE, linetype=FALSE)
                          ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/All_avg_rt_sess_box_aug15.pdf",wd),width=5,height=5)

      All_sess_RT_box_aug15_noline = ggplot(all_sub_mean_fs, aes(x = group2, y = rt_mean)) + geom_boxplot(aes(fill = group2)) + geom_point(data= all_sub_mean_fs[((all_sub_mean_fs$ID2 =="0" | all_sub_mean_fs$ID2=="1" | all_sub_mean_fs$ID2=="c")),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) 
#legend.title = element_text(size = rel(1.5)), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10)) + + guides(group = FALSE, fill = FALSE, color=FALSE, shape=FALSE, linetype=FALSE)
                          ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/All_avg_rt_sess_box_aug15_noline.pdf",wd),width=5,height=5)
	




			  
      All_sess_RT_bar = ggplot(all_group_mean, aes(x = groups, y = all_rt_mean, group = groups, fill = groups)) + geom_bar(position = 'dodge', stat = 'identity') +
      			  geom_errorbar(aes(y = all_rt_mean, ymin = all_rt_mean - all_rt_se, ymax = all_rt_mean + all_rt_se), position = position_dodge(.9), width = .2) + 
			  ylab("Mean RT (s)") + xlab('Session') + ggtitle('Austin & Houston SC RT') +
			 scale_fill_discrete(name = 'groups', breaks = c('A_control','A_first', 'A_second', 'A_third', 'H_control_first', 'H_control_second', 'H_control_third','H_first', 'H_second', 'H_third'), 
			    labels = c(sprintf('A_control (%d)',A_group_mean$N[A_group_mean$groups == 'A_control']), sprintf('A_first (%d)',A_group_mean$N[A_group_mean$groups == 'A_first']), 
			    sprintf('A_second (%d)', A_group_mean$N[A_group_mean$groups == 'A_second']),sprintf('A_third (%d)', A_group_mean$N[A_group_mean$groups == 'A_third']),sprintf('H_control_first(%d)',H_group_mean$N[H_group_mean$groups ==
			    'H_control_first']), 
			    sprintf('H_control_second (%d)',H_group_mean$N[H_group_mean$groups == 'H_control_second']), sprintf('H_control_third (%d)',H_group_mean$N[H_group_mean$groups == 'H_control_third']), sprintf('H_First (%d)', H_group_mean$N[H_group_mean$groups == 'H_first']), 
			    sprintf('H_second (%d)', H_group_mean$N[H_group_mean$groups == 'H_second']), sprintf('H_third (%d)', H_group_mean$N[H_group_mean$groups == 'H_third'])))			  
			ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/All_avg_rt_sess_bar.png",wd),width=10,height=10)


      All_sess_ACC_box_aug15 = ggplot(all_sub_mean_fs, aes(x = group2, y = acc_mean)) + geom_boxplot(aes(fill = group2)) + geom_point(data= all_sub_mean_fs[((all_sub_mean_fs$ID2 =="0" | all_sub_mean_fs$ID2=="1" | all_sub_mean_fs$ID2=="c")),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1))+ geom_line(data= all_sub_mean_fs[((all_sub_mean_fs$ID2 =="0" | all_sub_mean_fs$ID2=="1")),],aes(group = subind, color = ID2, linetype = ID2)) + xlab("Group") + ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
                          ggsave(filename=sprintf("%s/figures/Project_4/SC/ACC/All_avg_ACC_sess_box_aug15.pdf",wd),width=5,height=5)
	
      All_sess_ACC_box_aug15_noline = ggplot(all_sub_mean_fs, aes(x = group2, y = acc_mean)) + geom_boxplot(aes(fill = group2)) + geom_point(data= all_sub_mean_fs[((all_sub_mean_fs$ID2 =="0" | all_sub_mean_fs$ID2=="1" | all_sub_mean_fs$ID2=="c")),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,1)) + xlab("Group") + ylab("Mean Accuracy") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))
                          ggsave(filename=sprintf("%s/figures/Project_4/SC/ACC/All_avg_ACC_sess_box_aug15_noline.pdf",wd),width=5,height=5)





		  
      All_sess_ACC_bar = ggplot(all_group_mean, aes(x = groups, y = all_acc_mean, group = groups, fill = groups)) + geom_bar(position = 'dodge', stat = 'identity') +
      			  geom_errorbar(aes(y = all_acc_mean, ymin = all_acc_mean - all_acc_se, ymax = all_acc_mean + all_acc_se), position = position_dodge(.9), width = .2) + 
			  ylab("Mean Accuracy") + xlab('Session') + ggtitle('Austin & Houston SC ACC') +
			 scale_fill_discrete(name = 'groups', breaks = c('A_control','A_first', 'A_second', 'A_third', 'H_control_first', 'H_control_second', 'H_control_third','H_first', 'H_second', 'H_third'), 
			    labels = c(sprintf('A_control (%d)',A_group_mean$N[A_group_mean$groups == 'A_control']), sprintf('A_first (%d)',A_group_mean$N[A_group_mean$groups == 'A_first']), 
			    sprintf('A_second (%d)', A_group_mean$N[A_group_mean$groups == 'A_second']),sprintf('A_third (%d)', A_group_mean$N[A_group_mean$groups == 'A_third']),sprintf('H_control_first(%d)',H_group_mean$N[H_group_mean$groups ==
			    'H_control_first']), 
			    sprintf('H_control_second (%d)',H_group_mean$N[H_group_mean$groups == 'H_control_second']), sprintf('H_control_third (%d)',H_group_mean$N[H_group_mean$groups == 'H_control_third']), sprintf('H_First (%d)', H_group_mean$N[H_group_mean$groups == 'H_first']), 
			    sprintf('H_second (%d)', H_group_mean$N[H_group_mean$groups == 'H_second']), sprintf('H_third (%d)', H_group_mean$N[H_group_mean$groups == 'H_third'])))
			ggsave(filename=sprintf("%s/figures/Project_4/SC/ACC/All_avg_acc_sess_bar.png",wd),width=10,height=10)
			
	all_sub_cond_mean = ddply(all_sub_cond_med, .(subind, cond_type, groups, city, sense, active, ID, subtype), summarise, N = length(subind), rt_mean =mean(rt_median, na.rm = T),
	 acc_mean = mean(acc_avg, na.rm = T))
	write.csv(all_sub_cond_mean, file=sprintf("%s/data_frames/SC/JM_all_sub_cond_mean.csv", wd), na="NA", row.names=FALSE) 
 
, panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor =
			 element_blank(), axis.line = element_line(color = 'black')


        all_firsts_mean = all_sub_mean[all_sub_mean$groups == "A_control" | all_sub_mean$groups == "A_first" | all_sub_mean$groups == "A_first2" | all_sub_mean$groups == "H_first2", ]

        all_firsts_RT_box = ggplot(all_firsts_mean, aes(x = groups, y = rt_mean)) + geom_boxplot(aes(fill = groups)) + geom_point(data= all_firsts_mean[(all_firsts_mean$ID2 =="0" | all_firsts_mean$ID2=="1"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,16)) + xlab("Group") + ylab("Mean Response Time (s)") + guides(group = FALSE, color=FALSE) + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size = rel(1.5)), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10))
                            ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/all_firsts_rt_box.pdf",wd),width=5,height=5)


         t.test(rt_mean ~ groups, data = all_firsts_mean[all_firsts_mean$groups=="A_control" | all_firsts_mean$groups=="A_first",])
         t.test(rt_mean ~ groups, data = all_firsts_mean[all_firsts_mean$groups=="A_control" | all_firsts_mean$groups=="A_first2",])
         t.test(rt_mean ~ groups, data = all_firsts_mean[all_firsts_mean$groups=="A_control" | all_firsts_mean$groups=="H_first2",])

         t.test(rt_mean ~ groups, data = all_firsts_mean[all_firsts_mean$groups=="A_first" | all_firsts_mean$groups=="A_first2",])
         t.test(rt_mean ~ groups, data = all_firsts_mean[all_firsts_mean$groups=="A_first" | all_firsts_mean$groups=="H_first2",])

         t.test(rt_mean ~ groups, data = all_firsts_mean[all_firsts_mean$groups=="A_first2" | all_firsts_mean$groups=="H_first2",])


        all_firsts_acc_box = ggplot(all_firsts_mean, aes(x = groups, y = acc_mean)) + geom_boxplot(aes(fill = groups)) + geom_point(data= all_firsts_mean[(all_firsts_mean$ID2 =="0" | all_firsts_mean$ID2=="1"),], aes(color=ID2, shape=ID2),size=4.5) + scale_colour_manual(values=c("black","black","black")) + scale_shape_manual(values=c(17,2,16)) + xlab("Group") + ylab("Mean Accuracy") + guides(group = FALSE, color=FALSE) + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15), legend.title = element_text(size = rel(1.5)), legend.key.height = unit(2, "line"), legend.key.width = unit(2, "line"), legend.text = element_text(size=10))
                            ggsave(filename=sprintf("%s/figures/Project_4/SC/ACC/all_firsts_acc_box.pdf",wd),width=5,height=5)

         t.test(acc_mean ~ groups, data = all_firsts_mean[all_firsts_mean$groups=="A_control" | all_firsts_mean$groups=="A_first",])
         t.test(acc_mean ~ groups, data = all_firsts_mean[all_firsts_mean$groups=="A_control" | all_firsts_mean$groups=="A_first2",])
         t.test(acc_mean ~ groups, data = all_firsts_mean[all_firsts_mean$groups=="A_control" | all_firsts_mean$groups=="H_first2",])

         t.test(acc_mean ~ groups, data = all_firsts_mean[all_firsts_mean$groups=="A_first" | all_firsts_mean$groups=="A_first2",])
         t.test(acc_mean ~ groups, data = all_firsts_mean[all_firsts_mean$groups=="A_first" | all_firsts_mean$groups=="H_first2",])

         t.test(acc_mean ~ groups, data = all_firsts_mean[all_firsts_mean$groups=="A_first2" | all_firsts_mean$groups=="H_first2",])





       all_firsts_group_mean = ddply(all_firsts_mean, .(groups), summarise, N = length(groups), gr_rt_mean = mean(rt_mean, na.rm = T), gr_rt_se = sd(rt_mean, na.rm =T)/sqrt(N),
      			gr_acc_mean = mean(acc_mean, na.rm = T), gr_acc_se = sd(acc_mean, na.rm = T)/sqrt(N))
       write.csv(all_firsts_group_mean, file=sprintf("%s/data_frames/SC/all_firsts_group_mean.csv", wd), na="NA", row.names=FALSE) 


	#CONDITION ALL

        all_cond_rt_hist = ggplot(data = all_sub_cond_mean, aes(x = rt_mean)) + geom_histogram(aes(x = rt_mean, fill = ..count..), binwidth = .1) + scale_fill_gradient("Count", low = "black", high = "grey") + geom_density(size = 1.2, aes(x = rt_mean, y = ..count..*..density.., color = cond_type), position = "dodge") + scale_x_continuous(breaks = 1:7) + theme_bw() + ggtitle("SC Mean RTs Histogram")
			   ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/All_mean_rt_histogram.png",wd),width=10,height=10)

        group_all_cond_rt_hist = ggplot(data = all_sub_cond_mean, aes(x = rt_mean)) + ylim(0,30) + geom_histogram(aes(x = rt_mean, fill = ..count..), binwidth = .2, alpha = .5) + geom_density(size = 1.2, aes(x = rt_mean,
	y = ..count..*..density.., color = cond_type, group = interaction(cond_type, subtype)), position = "dodge") + scale_fill_gradient('Count', low = "black", high = "grey") +
	scale_x_continuous(breaks = 1:7) + scale_color_discrete(name='Sentence Type') + theme_bw() + ggtitle("SC Mean RTs by Group and Sentence Type Histogram") + guides(alpha = FALSE) + facet_grid(. ~
	subtype) + scale_y_continuous(expand=c(0,0))+ xlab('RT Mean')
                                 ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/All_group_cond_rt_histogram.png",wd),width=10,height=10)

        group_active_rt_hist = ggplot(data = all_sub_cond_mean, aes(x = rt_mean))  + geom_histogram(aes(x = rt_mean, fill = ..count..), binwidth = .2, alpha = .5) + geom_density(size = 1.2, aes(x = rt_mean, y =
	..count..*..density.., color = active, group = interaction(active, subtype)), position = "dodge") + scale_fill_gradient('Count', low = "black", high = "grey") +
	scale_x_continuous(breaks = 1:7) + theme_bw() + ggtitle("SC Mean RTs by Group and Active Type Histogram") + guides(alpha = FALSE) + facet_grid(. ~ subtype) + scale_y_continuous(expand=c(0,0)) +
	xlab('RT Mean')
			       ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/All_group_active_rt_histogram.png",wd),width=10,height=10)


	all_cond_rt_box = ggplot(data = all_sub_cond_mean, aes(x = cond_type, y = rt_mean)) + geom_boxplot(aes(color = cond_type, fill = city, alpha = .2)) + theme_classic() +
			geom_point(alpha = 1) + guides(alpha = F, size = F)+ xlab("Groups") + ylab("Mean RT (s)") + ggtitle("Sentence Comprehension RT")+
   			theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'),
			 axis.text.x = element_text(size = 12, angle=90, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), 
			 axis.title.y = element_text(size = 14)) + scale_colour_brewer(palette="Set1") +facet_grid(.~groups)
			 ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/All_avg_rt_cond_box.png",wd),width=20,height=10)

	all_cond_acc_box = ggplot(data = all_sub_cond_mean, aes(x = interaction(cond_type, groups), y = acc_mean)) + geom_boxplot(aes(color = cond_type, fill = city, alpha = .2)) +
			geom_point(alpha = 1) + guides(alpha = F, size = F)+ xlab("Groups") + ylab("Mean ACC") + ggtitle("Sentence Comprehension Accuracy")+
   			theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'),
			 axis.text.x = element_text(size = 12, angle=90, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), 
			 axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor =
			 element_blank(), axis.line = element_line(color = 'black')) + scale_colour_brewer(palette="Set1")
			 ggsave(filename=sprintf("%s/figures/Project_4/SC/ACC/All_avg_acc_cond_box.png",wd),width=20,height=10)
			 
	all_cond_rt_connect_box = ggplot(data = all_sub_cond_mean, aes(x = interaction(groups, cond_type), y = rt_mean)) + geom_boxplot(aes(color = cond_type, fill = city, alpha = .2)) +
			geom_point() + geom_line(aes(group = interaction(subind, cond_type), alpha = 1, linetype = ID)) + guides(alpha = F, size = F)+ xlab("Sentence Type") + ylab("Mean RT (s)") + ggtitle("Sentence Comprehension RT")+
   			theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'),
			 axis.text.x = element_text(size = 12, angle=90, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), 
			 axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor =
			 element_blank(), axis.line = element_line(color = 'black')) + scale_colour_brewer(palette="Set1")
			 ggsave(filename=sprintf("%s/figures/Project_4/SC/RT/All_avg_rt_connect_cond_box.png",wd),width=20,height=10)
			 
	all_cond_acc_connect_box = ggplot(data = all_sub_cond_mean, aes(x = interaction(groups, cond_type), y = acc_mean)) + geom_boxplot(aes(color = cond_type, fill = city, alpha = .2)) +
			geom_point() + geom_line(aes(group = interaction(subind, cond_type), alpha = 1, linetype = ID)) + guides(alpha = F, size = F)+
			xlab("Sentence Type") + ylab("Mean ACC") + ggtitle("Sentence Comprehension Accuracy")+
   			theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'),
			 axis.text.x = element_text(size = 12, angle=90, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), 
			 axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor =
			 element_blank(), axis.line = element_line(color = 'black')) + scale_colour_brewer(palette="Set1")
			 ggsave(filename=sprintf("%s/figures/Project_4/SC/ACC/All_avg_acc_connect_cond_box.png",wd),width=20,height=10)


	#Sentences
	ggplot(sents, aes(x = Sentence, y = N, fill = Resp)) + geom_bar(stat = 'identity') + theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'),
			 axis.text.x = element_text(size = 10, angle=90, vjust=.7, color = 'black'), axis.title.x = element_text(size = 14), 
			 axis.title.y = element_text(size = 14))
	

    # AUSTIN AND HOUSTON COMBINED


# T-TEST stats FOR SRCD	
     A_sub_mean$groups2 = A_sub_mean$groups
     A_sub_mean$groups2[A_sub_mean$groups2 == "A_first2"] = "A_first"	
 
     t.test(rt_mean ~ groups2, data = A_sub_mean[A_sub_mean$groups2 == "A_first" | A_sub_mean$groups2 == "A_control",])
     t.test(acc_mean ~ groups2, data = A_sub_mean[A_sub_mean$groups2 == "A_first" | A_sub_mean$groups2 == "A_control",])


     t.test(rt_mean ~ groups2, data = A_sub_mean[A_sub_mean$groups2 == "A_second" | A_sub_mean$groups2 == "A_control",])
     t.test(acc_mean ~ groups2, data = A_sub_mean[A_sub_mean$groups2 == "A_second" | A_sub_mean$groups2 == "A_control",])

     t.test(rt_mean ~ groups2, data = A_sub_mean[A_sub_mean$groups2 == "A_first" | A_sub_mean$groups2 == "A_second",])
     t.test(acc_mean ~ groups2, data = A_sub_mean[A_sub_mean$groups2 == "A_first" | A_sub_mean$groups2 == "A_second",])

# T-TEST stats FOR NIH AUG15	
     # AUSTIN

     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SC/rt_acc_austin_aug15.txt")

     rt_s1vc_a = t.test(rt_mean ~ group2, data = A_sub_mean_fs[A_sub_mean_fs$group2 == "first" | A_sub_mean_fs$group2 == "control",])
     print("Mean RT - S1 vs. Controls")
     print(rt_s1vc_a)

     acc_s1vc_a = t.test(acc_mean ~ group2, data = A_sub_mean_fs[A_sub_mean_fs$group2 == "first" | A_sub_mean_fs$group2 == "control",])
     print("Mean Accuracy - S1 vs. Controls")
     print(acc_s1vc_a)

     rt_s2vc_a = t.test(rt_mean ~ group2, data = A_sub_mean_fs[A_sub_mean_fs$group2 == "second" | A_sub_mean_fs$group2 == "control",])
     print("Mean RT - S2 vs. Controls")
     print(rt_s2vc_a)

     acc_s2vc_a = t.test(acc_mean ~ group2, data = A_sub_mean_fs[A_sub_mean_fs$group2 == "second" | A_sub_mean_fs$group2 == "control",])
     print("Mean Accuracy - S2 vs. Controls")
     print(acc_s2vc_a)

     rt_s1vs2_a = t.test(rt_mean ~ group2, data = A_sub_mean_fs[A_sub_mean_fs$group2 == "first" | A_sub_mean_fs$group2 == "second",])
     print("Mean RT - S1 vs. s2")
     print(rt_s1vs2_a)

     acc_s1vs2_a = t.test(acc_mean ~ group2, data = A_sub_mean_fs[A_sub_mean_fs$group2 == "first" | A_sub_mean_fs$group2 == "second",])
     print("Mean Accuracy - S1 vs. S2")
     print(acc_s1vs2_a)

     sink()

     # REPEAT and UNIQUE subs AUSTIN

     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SC/sc_rt_acc_austin_repeats_aug15.txt")

     print(A_sub_mean[A_sub_mean$rep=="r",])

     rt_repeat_a = t.test(rt_mean ~ group2, data = A_sub_mean[A_sub_mean$rep=="r",], paired=T)
     print("Mean RT - S1 vs. S2 Repeats")
     print(rt_repeat_a)

     acc_repeat_a = t.test(acc_mean ~ group2, data = A_sub_mean[A_sub_mean$rep=="r",], paired=T)
     print("Mean Accuracy - S1 vs. S2 Repeats")
     print(acc_repeat_a)
  
     sink()

     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SC/sc_rt_acc_austin_uniq_aug15.txt")

     print(A_sub_mean[A_sub_mean$rep=="u" & A_sub_mean$group2!="control",])

     rt_uniq_a = t.test(rt_mean ~ group2, data = A_sub_mean[A_sub_mean$rep=="u" & A_sub_mean$group2!="control",])
     print("Mean RT - S1 vs. S2 Unique")
     print(rt_uniq_a)

     acc_uniq_a = t.test(acc_mean ~ group2, data = A_sub_mean[A_sub_mean$rep=="u" & A_sub_mean$group2!="control",])
     print("Mean Accuracy - S1 vs. S2 Unique")
     print(acc_uniq_a)
  
     sink()

    # AUSTIN and HOUSTON

     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SC/rt_acc_ah_aug15.txt")

     rt_s1vc_ah = t.test(rt_mean ~ group2, data = all_sub_mean_fs[all_sub_mean_fs$group2 == "first" | all_sub_mean_fs$group2 == "control",])
     print("Mean RT - S1 vs. Controls")
     print(rt_s1vc_ah)

     acc_s1vc_ah = t.test(acc_mean ~ group2, data = all_sub_mean_fs[all_sub_mean_fs$group2 == "first" | all_sub_mean_fs$group2 == "control",])
     print("Mean Accuracy - S1 vs. Controls")
     print(acc_s1vc_ah)

     rt_s2vc_ah = t.test(rt_mean ~ group2, data = all_sub_mean_fs[all_sub_mean_fs$group2 == "second" | all_sub_mean_fs$group2 == "control",])
     print("Mean RT - S2 vs. Controls")
     print(rt_s2vc_ah)

     acc_s2vc_ah = t.test(acc_mean ~ group2, data = all_sub_mean_fs[all_sub_mean_fs$group2 == "second" | all_sub_mean_fs$group2 == "control",])
     print("Mean Accuracy - S2 vs. Controls")
     print(acc_s2vc_ah)

     rt_s1vs2_ah = t.test(rt_mean ~ group2, data = all_sub_mean_fs[all_sub_mean_fs$group2 == "first" | all_sub_mean_fs$group2 == "second",])
     print("Mean RT - S1 vs. s2")
     print(rt_s1vs2_ah)

     acc_s1vs2_ah = t.test(acc_mean ~ group2, data = all_sub_mean_fs[all_sub_mean_fs$group2 == "first" | all_sub_mean_fs$group2 == "second",])
     print("Mean Accuracy - S1 vs. S2")
     print(acc_s1vs2_ah)

     sink()

     # AUSTIN AND HOUSTON REPEAT and UNIQUE subs 

     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SC/sc_rt_acc_ah_repeats_aug15.txt")

     print(all_sub_mean[all_sub_mean$rep=="r",])

     rt_repeat_ah = t.test(rt_mean ~ group2, data = all_sub_mean[all_sub_mean$rep=="r",], paired=T)
     print("Mean RT - S1 vs. S2 Repeats")
     print(rt_repeat_ah)

     acc_repeat_ah = t.test(acc_mean ~ group2, data = all_sub_mean[all_sub_mean$rep=="r",], paired=T)
     print("Mean Accuracy - S1 vs. S2 Repeats")
     print(acc_repeat_ah)
  
     sink()

     sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SC/sc_rt_acc_ah_uniq_aug15.txt")

     print(all_sub_mean[all_sub_mean$rep=="u" & all_sub_mean$group2!="control",])

     rt_uniq_ah = t.test(rt_mean ~ group2, data = all_sub_mean[all_sub_mean$rep=="u" & all_sub_mean$group2!="control",])
     print("Mean RT - S1 vs. S2 Unique")
     print(rt_uniq_ah)

     acc_uniq_ah = t.test(acc_mean ~ group2, data = all_sub_mean[all_sub_mean$rep=="u" & all_sub_mean$group2!="control",])
     print("Mean Accuracy - S1 vs. S2 Unique")
     print(acc_uniq_ah)
  
     sink()




#RT
#Analysis 1
#Are sites comparables within controls?

all_sub_cond_med$city.sense = as.factor(sprintf("%s%s", all_sub_cond_med$city, all_sub_cond_med$sense))
all_sub_cond_med$city.active = as.factor(sprintf("%s%s", all_sub_cond_med$city, all_sub_cond_med$active))
all_sub_cond_med$cond_type = as.factor(all_sub_cond_med$cond_type)

	city.mod1 = lme(rt_median ~  active + city + sense, random = list(~1|subind, ~1|city,
	~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' |
	 all_sub_cond_med$groups == 'H_control_first',]) #main effects
	 summary(city.mod1)
	
	city.mod2 = lme(rt_median ~ city + sense * active, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' |
	 all_sub_cond_med$groups == 'H_control_first',]) #interaction of sense & active
	 summary(city.mod2)
	
	city.mod3 = lme(rt_median ~ city * sense + city *active, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #2 by 2 interaction
	summary(city.mod3)

	city.mod4 = lme(rt_median ~ city * sense *active, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #three way interaction
	summary(city.mod4)
	
	
	#to see what is driving the difference in the city x sense difference.
	city.mod5 = lme(rt_median ~ city.sense - 1, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #barplot
	summary(city.mod5)
	means = city.mod5$coeff$fixed
	se = sqrt(diag(vcov(city.mod5)))
	csplot = barplot(means, ylab = 'Mean RT (s)', ylim = c(0,5))
	segments(csplot, means - se, csplot, means + se, lwd = 2) #vertical bar
	segments(csplot - .1, means - se, csplot + .1, means - se, , lwd = 2) #lower bar
	segments(csplot - .1, means + se, csplot + .1, means + se, , lwd = 2) #upper bar
	
	
	#rerun model to get the ttest pvalues. Just keep changin gthe baseline to get all the comparisons. 
	city.mod6 = lme(rt_median ~ city.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to A Ns
	
	levels(all_sub_cond_med$city.sense) #use to check reference for levels
	
	all_sub_cond_med = within(all_sub_cond_med, city.sense <- relevel(city.sense, ref = 3))
	
	city.mod6.1 = lme(rt_median ~ city.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to H Ns
	
	all_sub_cond_med = within(all_sub_cond_med, city.sense <- relevel(city.sense, ref = 3))
	
	city.mod6.2 = lme(rt_median ~ city.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to A S
	
	all_sub_cond_med = within(all_sub_cond_med, city.sense <- relevel(city.sense, ref = 4))
	
	city.mod6.3 = lme(rt_median ~ city.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to H S
	
	
	
	#to see what is driving the difference in the city x active difference.
	city.mod7 = lme(rt_median ~ city.active - 1, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #barplot
	summary(city.mod7)
	means = city.mod7$coeff$fixed
	se = sqrt(diag(vcov(city.mod7)))
	caplot = barplot(means, ylab = 'Mean RT (s)', ylim = c(0,5))
	segments(caplot, means - se, caplot, means + se, lwd = 2) #vertical bar
	segments(caplot - .1, means - se, caplot + .1, means - se, , lwd = 2) #lower bar
	segments(caplot - .1, means + se, caplot + .1, means + se, , lwd = 2) #upper bar
	
	
	
	
	#rerun model to get the ttest pvalues. Just keep changin gthe baseline to get all the comparisons. 
	city.mod8 = lme(rt_median ~ city.active, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to A A
	summary(city.mod8)
	
	levels(all_sub_cond_med$city.active) #use to check reference for levels
	
	all_sub_cond_med = within(all_sub_cond_med, city.active <- relevel(city.active, ref = 3))
	
	city.mod8.1 = lme(rt_median ~ city.active, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to H A
	
	all_sub_cond_med = within(all_sub_cond_med, city.active <- relevel(city.active, ref = 3))
	
	city.mod8.2 = lme(rt_median ~ city.active, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to A P
	
	all_sub_cond_med = within(all_sub_cond_med, city.active <- relevel(city.active, ref = 4))
	
	city.mod8.3 = lme(rt_median ~ city.active, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to H P
	
	
#Analysis 2
#Intervention differences
all_sub_cond_med$ID2 = as.factor(as.character(all_sub_cond_med$ID2))
library(multcomp)

	pre.int.mod1 = lme(rt_median ~  ID2 + sense + active, random = list(~1|subind, ~1|city, ~1|runnum), data =
	all_sub_cond_med[all_sub_cond_med$city == 'austin' & all_sub_cond_med$groups == 'A_first',])
	summary(pre.int.mod1)
	contr = rbind('0:' = c(1,0,0,0), '1' =c(1,1,0,0))
	confint(summary(glht(pre.int.mod1, contr), test = adjusted('none')))
	
	comp = rbind('1-0:' =c(0,1,0,0))
	confint(summary(glht(pre.int.mod1, comp), test = adjusted('none')))
	
	pre.int.mod2 = lme(rt_median ~ ID2 + sense  + active, random = list(~1|subind, ~1|city, ~1|runnum), data =
	all_sub_cond_med[all_sub_cond_med$city == 'austin' & all_sub_cond_med$groups == 'A_second',])
	summary(pre.int.mod2)
	confint(summary(glht(pre.int.mod2, contr), test = adjusted('none')))	
	confint(summary(glht(pre.int.mod2, comp), test = adjusted('none')))
	
	pre.int.mod3 = lme(rt_median ~ ID2 + sense + active, random = list(~1|subind, ~1|city, ~1|runnum), data =
	all_sub_cond_med[all_sub_cond_med$city == 'austin' & all_sub_cond_med$groups == 'A_third',])
	summary(pre.int.mod3)
	confint(summary(glht(pre.int.mod3, contr), test = adjusted('none')))
	confint(summary(glht(pre.int.mod3, comp), test = adjusted('none')))
	

#Analysis 3
# Are Austin struggling readers different than controls in both sites?
	
	sent.mod1 = lme(rt_median ~  subtype + sense +  active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control',]) #main effects
	summary(sent.mod1)
	contr3 = rbind('Control:'=c(1,0,0,0), 'Intervention:'=c(1,1,0,0))
	confint(summary(glht(sent.mod1, contr3), test = adjusted('none')))
	
	comp3 = rbind('Int - Control:' =c(0,1,0,0))
	confint(summary(glht(sent.mod1, comp3), test = adjusted('none')))
	
	
	
	test = lme(rt_median ~  groups, random =  list(~1|subind),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_second' | all_sub_cond_med$groups == 'A_control',]) #main effects
	summary(test)
	contr3 = rbind('Control:'=c(1,0,0), 'Intervention:'=c(1,1,0), 'Second' =c(1,0,1))
	confint(summary(glht(test, contr3), test = adjusted('none')))
	
	comp3 = rbind('Int - Control:' =c(0,1,0,0))
	confint(summary(glht(test, comp3), test = adjusted('none')))
	
	
	
	
	
	sent.mod1.2 = lme(rt_median ~  subtype + sense +  active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_second' | all_sub_cond_med$groups == 'A_control',]) #main effects
	summary(sent.mod1.2)
	confint(summary(glht(sent.mod1.2, contr3), test = adjusted('none')))
	confint(summary(glht(sent.mod1.2, comp3), test = adjusted('none')))
	
	sent.mod1.3 = lme(rt_median ~  subtype + sense +  active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_third' | all_sub_cond_med$groups == 'A_control',]) #main effects
	summary(sent.mod1.3)
	confint(summary(glht(sent.mod1.3, contr3), test = adjusted('none')))
	confint(summary(glht(sent.mod1.3, comp3), test = adjusted('none')))
	
	sent.mod1.1 = lme(rt_median ~ subtype + sense * active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #main effects
	summary(sent.mod1.1)
	
	sent.mod2 = lme(rt_median ~ subtype * sense + subtype * active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) # 2 way interaction
	summary(sent.mod2)
	
	sent.mod3 = lme(rt_median ~ subtype * sense * active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #3 way nteraction
	summary(sent.mod3)
	
	
#Analysis 4
#Are session 1 vs session 2 intervention different?

	sub.mod1 = lme(rt_median ~  groups + sense + active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second' | all_sub_cond_med$groups == 'A_third',]) #main effects
	summary(sub.mod1)
	contr4 = rbind('First:' = c(1,0,0,0,0), 'Second:' =c(1,1,0,0,0), 'Third'=c(1,0,1,0,0))
	confint(summary(glht(sub.mod1, contr4), test = adjusted('none')))
	
	comp4 = rbind('Second - First' = c(0,1,0,0,0), 'Third - First' =c(0,0,1,0,0), 'Third - Second'=c(0,-1,1,0,0))
	summary(glht(sub.mod1, comp4), test = adjusted('none'))
	confint(summary(glht(sub.mod1, comp4), test = adjusted('none')))
	
	
	sub.mod1.1 = lme(rt_median ~ groups + sense * active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #main effects
	summary(sub.mod1.1)
	
	sub.mod2 = lme(rt_median ~ groups * sense + groups * active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #interaction effects
	summary(sub.mod2)
	
	sub.mod3 = lme(rt_median ~ groups * sense * active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #three way effects
	summary(sub.mod3)

#Analysis 5
#intervention repeats
	
	one = all_sub_mean[all_sub_mean$groups =='A_first' | all_sub_mean$groups =='A_second',]
	one.list = one[(duplicated(one['subind']) | duplicated(one['subind'], fromLast = TRUE)),][,c(1,7)]
	rep1.2 = merge(all_sub_cond_med, one.list, by=c('subind', 'groups'))	
	
	two = all_sub_mean[all_sub_mean$groups =='A_second' | all_sub_mean$groups =='A_third',]
	two.list = two[(duplicated(two['subind']) | duplicated(two['subind'], fromLast = TRUE)),][,c(1,7)]
	rep2.3 = merge(all_sub_cond_med, two.list, by=c('subind', 'groups'))	
	
	three = all_sub_mean[all_sub_mean$groups =='A_first' | all_sub_mean$groups =='A_third',]
	three.list = three[(duplicated(three['subind']) | duplicated(three['subind'], fromLast = TRUE)),][,c(1,7)]
	rep1.3 = merge(all_sub_cond_med, three.list, by=c('subind', 'groups'))
	
	
	rep.mod1 = lme(rt_median ~ groups + sense + active, random = list(~1|subind, ~1|city, ~1|runnum), data= rep1.2)
	summary(rep.mod1)
	
	contr5 = rbind('First:' =c(1,0,0,0), 'Second:'=c(1,1,0,0))
	confint(summary(glht(rep.mod1, contr5), test = adjusted('none')))
	
	comp5 = rbind('Second - First:'=c(0,1,0,0))
	confint(summary(glht(rep.mod1, comp5), test = adjusted('none')))
	
	
	rep.mod2 = lme(rt_median ~ groups + sense + active, random = list(~1|subind, ~1|city, ~1|runnum), data= rep2.3)
	summary(rep.mod2)
	
	contr5 = rbind('Second:' =c(1,0,0,0), 'Third:'=c(1,1,0,0))
	confint(summary(glht(rep.mod2, contr5), test = adjusted('none')))
	
	comp5 = rbind('Third - Second:'=c(0,1,0,0))
	confint(summary(glht(rep.mod2, comp5), test = adjusted('none')))
	
	
	rep.mod3 = lme(rt_median ~ groups + sense + active, random = list(~1|subind, ~1|city, ~1|runnum), data= rep1.3)
	summary(rep.mod3)
	
	contr5 = rbind('First:' =c(1,0,0,0), 'Third:'=c(1,1,0,0))
	confint(summary(glht(rep.mod3, contr5), test = adjusted('none')))
	
	comp5 = rbind('Third - First:'=c(0,1,0,0))
	confint(summary(glht(rep.mod3, comp5), test = adjusted('none')))
	
	
	rep.mod2 = lme(rt_median ~ groups + sense * active, random = list(~1|subind, ~1|city, ~1|runnum), data =
	all_sub_cond_med[all_sub_cond_med$city == 'austin' & (all_sub_cond_med$dup1 == 'TRUE' | all_sub_cond_med$dup2 == 'TRUE'),])
	summary(rep.mod2) #one way interaction
	
	
	rep.mod3 = lme(rt_median ~ groups * sense + groups * active, random = list(~1|subind, ~1|city, ~1|runnum), data =
	all_sub_cond_med[all_sub_cond_med$city == 'austin' & (all_sub_cond_med$dup1 == 'TRUE' | all_sub_cond_med$dup2 == 'TRUE'),])
	summary(rep.mod3)
	
	rep.mod4 = lme(rt_median ~ groups * sense * active, random = list(~1|subind, ~1|city, ~1|runnum), data =
	all_sub_cond_med[all_sub_cond_med$city == 'austin' & (all_sub_cond_med$dup1 == 'TRUE' | all_sub_cond_med$dup2 == 'TRUE'),])
	summary(rep.mod4)





#ACCURACY
#Analysis 1
#Are sites comparables within controls?

	
	
	city.mod1a = lme(acc_avg ~ active + city + sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' |
	 all_sub_cond_med$groups == 'H_control_first',]) #main effects
	 summary(city.mod1a)
	
	city.mod2a = lme(acc_avg ~ city + sense * active, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' |
	 all_sub_cond_med$groups == 'H_control_first',]) #interaction of sense & active
	 summary(city.mod2a)
	 
	#to see what is driving the difference in the city x sense difference.
	city.mod2a.1 = lme(acc_avg ~ cond_type - 1, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #barplot
	summary(city.mod2a.1)
	means = city.mod2a.1$coeff$fixed
	se = sqrt(diag(vcov(city.mod2a.1)))
	csplot = barplot(means, ylab = 'Mean ACC', ylim = c(0, 1.1))
	segments(csplot, means - se, csplot, means + se, lwd = 2) #vertical bar
	segments(csplot - .1, means - se, csplot + .1, means - se, , lwd = 2) #lower bar
	segments(csplot - .1, means + se, csplot + .1, means + se, , lwd = 2) #upper bar
	
	
	#rerun model to get the ttest pvalues. Just keep changin gthe baseline to get all the comparisons. 
	city.mod2a.2 = lme(acc_avg ~ cond_type, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to A NS
	summary(city.mod2a.2)
	
	levels(all_sub_cond_med$cond_type) #use to check reference for levels
	
	all_sub_cond_med = within(all_sub_cond_med, cond_type <- relevel(cond_type, ref = 3))
	
	city.mod2a.3 = lme(acc_avg ~ cond_type, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to P NS
	
	all_sub_cond_med = within(all_sub_cond_med, cond_type <- relevel(cond_type, ref = 3))
	
	city.mod2a.4 = lme(acc_avg ~ cond_type, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to A S
	
	all_sub_cond_med = within(all_sub_cond_med, cond_type <- relevel(cond_type, ref = 4))
	
	city.mod2a.5 = lme(acc_avg ~ cond_type, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to P S
	 
	
	city.mod3a = lme(acc_avg ~ city * sense + city *active, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #2 by 2 interaction
	summary(city.mod3a)

	city.mod4a = lme(acc_avg ~ city * sense *active, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #three way interaction
	summary(city.mod4a)
	
	
	#to see what is driving the difference in the city x sense difference.
	city.mod5a = lme(acc_avg ~ city.sense - 1, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #barplot
	summary(city.mod5a)
	means = city.mod5a$coeff$fixed
	se = sqrt(diag(vcov(city.mod5a)))
	csplot = barplot(means, ylab = 'Mean ACC', ylim = c(0, 1.1))
	segments(csplot, means - se, csplot, means + se, lwd = 2) #vertical bar
	segments(csplot - .1, means - se, csplot + .1, means - se, , lwd = 2) #lower bar
	segments(csplot - .1, means + se, csplot + .1, means + se, , lwd = 2) #upper bar
	
	
	
	#rerun model to get the ttest pvalues. Just keep changin gthe baseline to get all the comparisons. 
	city.mod6a = lme(acc_avg ~ city.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to A Ns
	summary(city.mod6a)
	
	levels(all_sub_cond_med$city.sense) #use to check reference for levels
	
	all_sub_cond_med = within(all_sub_cond_med, city.sense <- relevel(city.sense, ref = 3))
	
	city.mod6a.1 = lme(acc_avg ~ city.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to H Ns
	
	all_sub_cond_med = within(all_sub_cond_med, city.sense <- relevel(city.sense, ref = 3))
	
	city.mod6a.2 = lme(acc_avg ~ city.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to A S
	
	all_sub_cond_med = within(all_sub_cond_med, city.sense <- relevel(city.sense, ref = 4))
	
	city.mod6a.3 = lme(acc_avg ~ city.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_control' | 
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to H S


#Analysis 2
#Intervention differences

	pre.int.mod1a = lme(acc_avg*100 ~  ID2 + active + sense, random = list(~1|subind, ~1|city, ~1|runnum), data =
	all_sub_cond_med[all_sub_cond_med$city == 'austin' & all_sub_cond_med$groups == 'A_first',])
	summary(pre.int.mod1a)
	
	acontr2 = rbind('0:'=c(1,0,0,0), '1:'=c(1,1,0,0))
	confint(summary(glht(pre.int.mod1a, acontr2), test = adjusted('none')))
	
	acomp2 = rbind('1 - 0:'=c(0,1,0,0))
	confint(summary(glht(pre.int.mod1a, acomp2), test = adjusted('none')))
	
	pre.int.mod2a = lme(acc_avg*100 ~ ID2 + active +sense, random = list(~1|subind, ~1|city, ~1|runnum), data =
	all_sub_cond_med[all_sub_cond_med$city == 'austin' & all_sub_cond_med$groups == 'A_second',])
	summary(pre.int.mod2a)	
	
	confint(summary(glht(pre.int.mod2a, acontr2), test = adjusted('none')))
	confint(summary(glht(pre.int.mod2a, acomp2), test = adjusted('none')))
	
	pre.int.mod3a = lme(acc_avg*100 ~ ID2 + active +sense, random = list(~1|subind, ~1|city, ~1|runnum), data =
	all_sub_cond_med[all_sub_cond_med$city == 'austin' & all_sub_cond_med$groups == 'A_third',])
	summary(pre.int.mod3a)	
	
	confint(summary(glht(pre.int.mod3a, acontr2), test = adjusted('none')))
	confint(summary(glht(pre.int.mod3a, acomp2), test = adjusted('none')))
	
#Analysis 3
# Are Austin struggling readers different than controls in both sites?
	
	sent.mod1a = lme(acc_avg*100 ~ subtype + active + sense, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control',]) #main effects
	summary(sent.mod1a)
	
	acontr3 = rbind('Controls:'=c(1,0,0,0), 'Int:'=c(1,1,0,0))
	acomp3 = rbind('Intervention - Controls:'=c(0,1,0,0))
	
	confint(summary(glht(sent.mod1a, acontr3), test = adjusted('none')))
	confint(summary(glht(sent.mod1a, acomp3), test = adjusted('none')))
	
	sent.mod1b = lme(acc_avg*100 ~ subtype + active + sense, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_second' | all_sub_cond_med$groups == 'A_control',]) #main effects
	summary(sent.mod1b)
	
	confint(summary(glht(sent.mod1b, acontr3), test = adjusted('none')))
	confint(summary(glht(sent.mod1b, acomp3), test = adjusted('none')))
	
	sent.mod1c = lme(acc_avg*100 ~ subtype + active + sense, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_third' | all_sub_cond_med$groups == 'A_control',]) #main effects
	summary(sent.mod1c)
	
	confint(summary(glht(sent.mod1c, acontr3), test = adjusted('none')))
	confint(summary(glht(sent.mod1c, acomp3), test = adjusted('none')))
	
	
	sent.mod1a.1 = lme(acc_avg ~ subtype + sense * active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #main effects
	summary(sent.mod1a.1)
	
	#to see what is driving the difference in the active x sense difference.
	sent.mod1a.2 = lme(acc_avg ~ cond_type - 1, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #barplot
	summary(sent.mod1a.2)
	means = sent.mod1a.2$coeff$fixed
	se = sqrt(diag(vcov(sent.mod1a.2)))
	csplot = barplot(means, ylab = 'Mean ACC', ylim = c(0, 1))
	segments(csplot, means - se, csplot, means + se, lwd = 2) #vertical bar
	segments(csplot - .1, means - se, csplot + .1, means - se, , lwd = 2) #lower bar
	segments(csplot - .1, means + se, csplot + .1, means + se, , lwd = 2) #upper bar
	
	#rerun model to get the ttest pvalues. Just keep changin gthe baseline to get all the comparisons. 
	sent.mod1a.3 = lme(acc_avg ~ cond_type, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to P S
	summary(sent.mod1a.3)
	
	levels(all_sub_cond_med$cond_type) #use to check reference for levels
	
	all_sub_cond_med = within(all_sub_cond_med, cond_type <- relevel(cond_type, ref = 3))
	
	sent.mod1a.4 = lme(acc_avg ~ cond_type, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to P Ns
	
	all_sub_cond_med = within(all_sub_cond_med, cond_type <- relevel(cond_type, ref = 3))
	
	sent.mod1a.5 = lme(acc_avg ~ cond_type, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to A S
	
	all_sub_cond_med = within(all_sub_cond_med, cond_type <- relevel(cond_type, ref = 4))
	
	sent.mod1a.6 = lme(acc_avg ~ cond_type, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to A NS
	
	
	
	
	sent.mod2a = lme(acc_avg ~ subtype * sense + subtype * active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) # 2 way interaction
	summary(sent.mod2a)
	
	sent.mod3a = lme(acc_avg ~ subtype * sense * active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #3 way nteraction
	summary(sent.mod3a)
	
	
	
	#to see what is driving the difference in the city x sense difference.
	sent.mod5a = lme(acc_avg ~ subtype.sense - 1, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #barplot
	summary(sent.mod5a)
	means = sent.mod5a$coeff$fixed
	se = sqrt(diag(vcov(sent.mod5a)))
	csplot = barplot(means, ylab = 'Mean ACC', ylim = c(0, 1))
	segments(csplot, means - se, csplot, means + se, lwd = 2) #vertical bar
	segments(csplot - .1, means - se, csplot + .1, means - se, , lwd = 2) #lower bar
	segments(csplot - .1, means + se, csplot + .1, means + se, , lwd = 2) #upper bar
	
	
	
	#rerun model to get the ttest pvalues. Just keep changin gthe baseline to get all the comparisons. 
	sent.mod6a = lme(acc_avg ~ subtype.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to C Ns
	summary(sent.mod6a)
	
	levels(all_sub_cond_med$subtype.sense) #use to check reference for levels
	
	all_sub_cond_med = within(all_sub_cond_med, subtype.sense <- relevel(subtype.sense, ref = 3))
	
	sent.mod6a.1 = lme(acc_avg ~ subtype.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to I Ns
	
	all_sub_cond_med = within(all_sub_cond_med, subtype.sense <- relevel(subtype.sense, ref = 3))
	
	sent.mod6a.2 = lme(acc_avg ~ subtype.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to C S
	
	all_sub_cond_med = within(all_sub_cond_med, subtype.sense <- relevel(subtype.sense, ref = 4))
	
	sent.mod6a.3 = lme(acc_avg ~ subtype.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' | all_sub_cond_med$groups == 'A_control' |
	all_sub_cond_med$groups == 'H_control_first',]) #t pvalues compared to I S
	
#analysis 4
#Are session 1 vs session 2 intervention different?

	sub.mod1a = lme(acc_avg*100 ~ groups + active +sense, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second' | all_sub_cond_med$groups == 'A_third',]) #main effects
	summary(sub.mod1a)
	
	acontr4 = rbind('First:'=c(1,0,0,0,0), 'Second:'=c(1,1,0,0,0), 'Third:'=c(1,0,1,0,0))
	acomp4 = rbind('Second - First:'=c(0,1,0,0,0), 'Third - First:'=c(0,0,1,0,0), 'Third - Second:'=c(0,-1,1,0,0))
	
	confint(summary(glht(sub.mod1a, acontr4), test = adjusted('none')))
	confint(summary(glht(sub.mod1a, acomp4), test = adjusted('none')))
		
	
	acontr4 = rbind('First:'=c(1,0,0,0), 'Third:'=c(1,1,0,0))
	acomp4 = rbind('Third - First:'=c(0,1,0,0))
	confint(summary(glht(sub.mod1c, acontr4), test = adjusted('none')))
	confint(summary(glht(sub.mod1c, acomp4), test = adjusted('none')))
	
	sub.mod1a.1 = lme(acc_avg ~ groups + sense * active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #interaction effect
	summary(sub.mod1a.1)
	
	
	#to see what is driving the difference in the active x sense difference.
	sub.mod1a.2 = lme(acc_avg ~ cond_type - 1, random = list(~1|subind, ~1|city, ~1|runnum), all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #barplot
	summary(sub.mod1a.2)
	means = sub.mod1a.2$coeff$fixed
	se = sqrt(diag(vcov(sub.mod1a.2)))
	csplot = barplot(means, ylab = 'Mean ACC', ylim = c(0, 1))
	segments(csplot, means - se, csplot, means + se, lwd = 2) #vertical bar
	segments(csplot - .1, means - se, csplot + .1, means - se, , lwd = 2) #lower bar
	segments(csplot - .1, means + se, csplot + .1, means + se, , lwd = 2) #upper bar
	
	#rerun model to get the ttest pvalues. Just keep changin gthe baseline to get all the comparisons. 
	sub.mod1a.3 = lme(acc_avg ~ cond_type, random = list(~1|subind, ~1|city, ~1|runnum), all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #t pvalues compared to A NS
	summary(sub.mod1a.3)
	
	levels(all_sub_cond_med$cond_type) #use to check reference for levels
	
	all_sub_cond_med = within(all_sub_cond_med, cond_type <- relevel(cond_type, ref = 3))
	
	sub.mod1a.4 = lme(acc_avg ~ cond_type, random = list(~1|subind, ~1|city, ~1|runnum), all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #t pvalues compared to P NS
	
	all_sub_cond_med = within(all_sub_cond_med, cond_type <- relevel(cond_type, ref = 3))
	
	sub.mod1a.5 = lme(acc_avg ~ cond_type, random = list(~1|subind, ~1|city, ~1|runnum), all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #t pvalues compared to A S
	
	all_sub_cond_med = within(all_sub_cond_med, cond_type <- relevel(cond_type, ref = 4))
	
	sub.mod1a.6 = lme(acc_avg ~ cond_type, random = list(~1|subind, ~1|city, ~1|runnum), all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #t pvalues compared to P S
	
	
	sub.mod2a = lme(acc_avg ~ groups * sense + groups * active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #2 x2 interaction effects
	summary(sub.mod2a)
	
	sub.mod3a = lme(acc_avg ~ groups * sense * active, random =  list(~1|subind, ~1|city, ~1|runnum),data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #three way effects
	summary(sub.mod3a)
	
	
	#to see what is driving the difference in the groups x sense difference.
	sub.mod5a = lme(acc_avg ~ groups.sense - 1, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #barplot
	summary(sub.mod5a)
	means = sub.mod5a$coeff$fixed
	se = sqrt(diag(vcov(sub.mod5a)))
	csplot = barplot(means, ylab = 'Mean ACC', ylim = c(0, 1))
	segments(csplot, means - se, csplot, means + se, lwd = 2) #vertical bar
	segments(csplot - .1, means - se, csplot + .1, means - se, , lwd = 2) #lower bar
	segments(csplot - .1, means + se, csplot + .1, means + se, , lwd = 2) #upper bar
		
		
		
	sub.mod6a = lme(acc_avg ~ groups.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #t pvalues compared to AC Ns
	summary(sub.mod6a)
	
	levels(all_sub_cond_med$groups.sense) #use to check reference for levels
	
	all_sub_cond_med = within(all_sub_cond_med, groups.sense <- relevel(groups.sense, ref = 3))
	
	sub.mod6.1a = lme(acc_avg ~ groups.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #t pvalues compared to AF Ns
	summary(sub.mod6.1a)
	
	all_sub_cond_med = within(all_sub_cond_med, groups.sense <- relevel(groups.sense, ref = 7))
	
	sub.mod6.2a = lme(acc_avg ~ groups.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #t pvalues compared to HF Ns
	summary(sub.mod6.2a)
	
	
	all_sub_cond_med = within(all_sub_cond_med, groups.sense <- relevel(groups.sense, ref = 4))
	
	sub.mod6.3a = lme(acc_avg ~ groups.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #t pvalues compared to AC S
	summary(sub.mod6.3a)
	
	all_sub_cond_med = within(all_sub_cond_med, groups.sense <- relevel(groups.sense, ref = 5))
	
	sub.mod6.4a = lme(acc_avg ~ groups.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #t pvalues compared to AF S
	summary(sub.mod6.4a)

	all_sub_cond_med = within(all_sub_cond_med, groups.sense <- relevel(groups.sense, ref = 8))
	
	sub.mod6.5a = lme(acc_avg ~ groups.sense, random = list(~1|subind, ~1|city, ~1|runnum), data = all_sub_cond_med[all_sub_cond_med$groups == 'A_first' |
	all_sub_cond_med$groups == 'A_second',]) #t pvalues compared to AF S
	summary(sub.mod6.5a)
	
	
#Analysis 5
#intervention repeats
	
	one = all_sub_mean[all_sub_mean$groups =='A_first' | all_sub_mean$groups =='A_second',]
	one.list = one[(duplicated(one['subind']) | duplicated(one['subind'], fromLast = TRUE)),][,c(1,7)]
	rep1.2 = merge(all_sub_cond_med, one.list, by=c('subind', 'groups'))	
	
	two = all_sub_mean[all_sub_mean$groups =='A_second' | all_sub_mean$groups =='A_third',]
	two.list = two[(duplicated(two['subind']) | duplicated(two['subind'], fromLast = TRUE)),][,c(1,7)]
	rep2.3 = merge(all_sub_cond_med, two.list, by=c('subind', 'groups'))	
	
	three = all_sub_mean[all_sub_mean$groups =='A_first' | all_sub_mean$groups =='A_third',]
	three.list = three[(duplicated(three['subind']) | duplicated(three['subind'], fromLast = TRUE)),][,c(1,7)]
	rep1.3 = merge(all_sub_cond_med, three.list, by=c('subind', 'groups'))
	
	
	
	a.rep.mod1a = lme(acc_avg*100 ~ groups + active + sense, random = list(~1|subind, ~1|city, ~1|runnum), data =
	rep1.2)
	summary(a.rep.mod1a)
	acontr5 = rbind('First:'=c(1,0,0,0), 'Second:'=c(1,1,0,0))
	acomp5 = rbind('Second - First:'=c(0,1,0,0))
	confint(summary(glht(a.rep.mod1a, acontr5), test = adjusted('none')))
	confint(summary(glht(a.rep.mod1a, acomp5), test = adjusted('none')))
	
	a.rep.mod1b = lme(acc_avg*100 ~ groups + active + sense, random = list(~1|subind, ~1|city, ~1|runnum), data =
	rep2.3)
	summary(a.rep.mod1b)
	acontr5 = rbind('Second:'=c(1,0,0,0), 'Third:'=c(1,1,0,0))
	acomp5 = rbind('Third - Second:'=c(0,1,0,0))
	confint(summary(glht(a.rep.mod1b, acontr5), test = adjusted('none')))
	confint(summary(glht(a.rep.mod1b, acomp5), test = adjusted('none')))
	
	a.rep.mod1c = lme(acc_avg*100 ~ groups + active + sense, random = list(~1|subind, ~1|city, ~1|runnum), data =
	rep1.3)
	summary(a.rep.mod1c)
	acontr5 = rbind('First:'=c(1,0,0,0), 'Third:'=c(1,1,0,0))
	acomp5 = rbind('Third - First:'=c(0,1,0,0))
	confint(summary(glht(a.rep.mod1c, acontr5), test = adjusted('none')))
	confint(summary(glht(a.rep.mod1c, acomp5), test = adjusted('none')))
	
	
	a.rep.mod2a = lme(acc_avg ~ groups + sense * active, random = list(~1|subind, ~1|city, ~1|runnum), data =
	all_sub_cond_med[all_sub_cond_med$city == 'austin' & (all_sub_cond_med$dup1 == 'TRUE' | all_sub_cond_med$dup2 == 'TRUE'),])
	summary(a.rep.mod2a) #one way interaction
	
	a.rep.mod3a = lme(acc_avg ~ groups * sense + groups * active, random = list(~1|subind, ~1|city, ~1|runnum), data =
	all_sub_cond_med[all_sub_cond_med$city == 'austin' & (all_sub_cond_med$dup1 == 'TRUE' | all_sub_cond_med$dup2 == 'TRUE'),])
	summary(a.rep.mod3a)
	
	
	#to see what is driving the difference in the groups x sense difference.
	a.rep.mod5a = lme(acc_avg ~ groups.sense - 1, random = list(~1|subind, ~1|city, ~1|runnum), data =
	all_sub_cond_med[all_sub_cond_med$city == 'austin' & (all_sub_cond_med$dup1 == 'TRUE' | all_sub_cond_med$dup2 == 'TRUE'),]) #barplot
	summary(a.rep.mod5a)
	means = a.rep.mod5a$coeff$fixed
	se = sqrt(diag(vcov(a.rep.mod5a)))
	csplot = barplot(means, ylab = 'Mean ACC', ylim = c(0, 1))
	segments(csplot, means - se, csplot, means + se, lwd = 2) #vertical bar
	segments(csplot - .1, means - se, csplot + .1, means - se, , lwd = 2) #lower bar
	segments(csplot - .1, means + se, csplot + .1, means + se, , lwd = 2) #upper bar
	
	
	
	a.rep.mod4a = lme(acc_avg ~ groups * sense * active, random = list(~1|subind, ~1|city, ~1|runnum), data =
	all_sub_cond_med[all_sub_cond_med$city == 'austin' & (all_sub_cond_med$dup1 == 'TRUE' | all_sub_cond_med$dup2 == 'TRUE'),])
	summary(a.rep.mod4a)
	
	
#### END ####

