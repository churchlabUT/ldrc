# Author: Mary Abbe Roe, Oct. 2014
# This script reads in behavioral rt_all.txt files for Austin_thirds, specifically, going into MRI analysis

# LIBRARIES
 
  library(ggplot2)
  library(plyr)

  wd = getwd()

# FUNCTIONS   
 
  # function for extracting end of string
    substrRight = function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }

# READ IN DATA FOR RT_ALL.txt (can change, depending on what need to look at)

  group = c("ldrc_[0-9]_[0-9][0-9][0-9]", "ldrc_*second", "ldrc_*third", "ldrc2_*", "H_LD*_[0-9]_second", "H_LD*_[0-9]_third", "ldrc_c_[0-9][0-9][0-9]", "H_LD*_c_second", "H_LD*_c_third")
  chars = c(5,12,11,9,13,12,5,13,12)

  for (i in 1:length(group)){ 
    subdirs = Sys.glob(sprintf("/corral-repl/utexas/ldrc/%s" ,group[i]))
    rm("rt_all")
    print(group[i])

    # SUB
    for (sub in subdirs){
      behav_dirs = Sys.glob(sprintf("%s/behav/SC/SC_R*", sub))
      subnum = substrRight(sub, chars[i])

	# RUN
        for (run in behav_dirs){
          run_num = substrRight(run,1)
          RTfile = Sys.glob(sprintf("%s/rt_all.txt", run))

          if (length(RTfile)==0){
            warning(sprintf("cannot find rt_all.txt file for %s", run))
	    next
          }

          rt_loop = read.csv(RTfile, header=FALSE, sep="", na.strings="NaN")
          rt_loop$subind = rep(subnum, dim(rt_loop)[1])
          rt_loop$runnum = rep(run_num, dim(rt_loop)[1])

          # CREATE DAT_ALL IF DOESN'T EXIST
          if (exists("rt_all")==FALSE){
            rt_all = rt_loop 
          } 
          else{ 
            rt_all = rbind(rt_all, rt_loop)
          }
     
       } # END RUN LOOP
     }  # END SUB LOOP

    # ADD GROUP IDENTIFIER
      rt_all$group_num= i

      names(rt_all)[1] = "onset"
      names(rt_all)[2] = "pm"
      names(rt_all)[3] = "meanCentRT"


    # CREATE SEPARATE DATA FRAME FOR EACH GROUP
      assign(paste("rt_all",i,sep="_"),rt_all)


  } # END GROUP LOOP



 sub.mean = ddply(rt_all, .(subind, runnum), summarise, mean = mean(V3, na.rm = TRUE))


# MEAN CENTERED RT_ALL GRAPHS

  sub.run.meancent.1 = ggplot(rt_all_1, aes(x = subind, y = meanCentRT, color = runnum)) + geom_point() + ggtitle("Austin Firsts Mean Center RTs")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/A_1_rt_all_meanCentRT.png",wd),width=10,height=10)
  sub.run.onset.1 = ggplot(rt_all_1, aes(x = subind, y = onset, color = runnum)) + geom_point() + ggtitle("Austin Firsts Onsets")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/A_1_rt_all_onset.png",wd),width=10,height=10)

  sub.run.meancent.2 = ggplot(rt_all_2, aes(x = subind, y = meanCentRT, color = runnum)) + geom_point() + ggtitle("Austin Seconds Mean Center RTs")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/A_2_rt_all_meanCentRT.png",wd),width=10,height=10)
  sub.run.onset.2 = ggplot(rt_all_2, aes(x = subind, y = onset, color = runnum)) + geom_point() + ggtitle("Austin Seconds Onsets")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/A_2_rt_all_onset.png",wd),width=10,height=10)

  sub.run.meancent.3 = ggplot(rt_all_3, aes(x = subind, y = meanCentRT, color = runnum)) + geom_point() + ggtitle("Austin Thirds Mean Center RTs")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/A_3_rt_all_meanCentRT.png",wd),width=10,height=10)
  sub.run.onset.3 = ggplot(rt_all_3, aes(x = subind, y = onset, color = runnum)) + geom_point() + ggtitle("Austin Thirds Onsets")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/A_3_rt_all_onset.png",wd),width=10,height=10)

  sub.run.meancent.4 = ggplot(rt_all_4, aes(x = subind, y = meanCentRT, color = runnum)) + geom_point() + ggtitle("Austin ldrc2-Pre  Mean Center RTs")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/A_ldrc2_1_rt_all_meanCentRT.png",wd),width=10,height=10)
  sub.run.onset.4 = ggplot(rt_all_4, aes(x = subind, y = onset, color = runnum)) + geom_point() + ggtitle("Austin ldrc2-Pre Onsets")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/A_ldrc2_1_rt_all_onset.png",wd),width=10,height=10)

  sub.run.meancent.5 = ggplot(rt_all_5, aes(x = subind, y = meanCentRT, color = runnum)) + geom_point() + ggtitle("Houston Seconds Mean Center RTs")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/H_2_rt_all_meanCentRT.png",wd),width=10,height=10)
  sub.run.onset.5 = ggplot(rt_all_5, aes(x = subind, y = onset, color = runnum)) + geom_point() + ggtitle("Houston Seconds Onsets")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/H_2_rt_all_onset.png",wd),width=10,height=10)


  sub.run.meancent.6 = ggplot(rt_all_6, aes(x = subind, y = meanCentRT, color = runnum)) + geom_point() + ggtitle("Houston Thirds Mean Center RTs")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/H_3_rt_all_meanCentRT.png",wd),width=10,height=10)
  sub.run.onset.6 = ggplot(rt_all_6, aes(x = subind, y = onset, color = runnum)) + geom_point() + ggtitle("Houston Thirds Onsets")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/H_3_rt_all_onset.png",wd),width=10,height=10)


  sub.run.meancent.7 = ggplot(rt_all_7, aes(x = subind, y = meanCentRT, color = runnum)) + geom_point() + ggtitle("Austin Controls Mean Center RTs")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/A_c_rt_all_meanCentRT.png",wd),width=10,height=10)
  sub.run.onset.7 = ggplot(rt_all_7, aes(x = subind, y = onset, color = runnum)) + geom_point() + ggtitle("Austin Controls Onsets")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/A_c_rt_all_onset.png",wd),width=10,height=10)


  sub.run.meancent.8 = ggplot(rt_all_8, aes(x = subind, y = meanCentRT, color = runnum)) + geom_point() + ggtitle("Houston Second Controls Mean Center RTs")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/H_c_2_rt_all_meanCentRT.png",wd),width=10,height=10)
  sub.run.onset.8 = ggplot(rt_all_8, aes(x = subind, y = onset, color = runnum)) + geom_point() + ggtitle("Houston Second Controls Onsets")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/H_c_2_rt_all_onset.png",wd),width=10,height=10)


  sub.run.meancent.9 = ggplot(rt_all_9, aes(x = subind, y = meanCentRT, color = runnum)) + geom_point() + ggtitle("Houston Third Controls Mean Center RTs")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/H_c_3_rt_all_meanCentRT.png",wd),width=10,height=10)
  sub.run.onset.9 = ggplot(rt_all_9, aes(x = subind, y = onset, color = runnum)) + geom_point() + ggtitle("Houston Third Controls Onsets")
                       ggsave(filename=sprintf("%s/figures/Project_4/SC/onsets_check/H_c_3_rt_all_onset.png",wd),width=10,height=10)
