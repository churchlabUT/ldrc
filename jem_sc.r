# Authors: Leo Olmedo (first); Mary Abbe Roe & Joel Martinez (Dec. - Jan. 2013)
# This script reads in behavioral data for SC and calculates RT and ACC
# Graphs are located in SC_r_graphs.R



#### LIBRARIES, FUNCTIONS, READING IN DATA ####

# LIBRARIES
 
  library(ggplot2)
  library(plyr)
  library(nlme)

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


  group = c("ldrc_[0-9]_[0-9][0-9][0-9]", "ldrc_*second", "ldrc_*third", "ldrc2_*", "PHILIPS/H_LD*_[0-9]", "H_LD*_[0-9]_second", "H_LD*_[0-9]_third", "ldrc_c_[0-9][0-9][0-9]", "PHILIPS/H_LD*[0-9][0-9][0-9][0-9]", "H_LD*_c_second", "H_LD*_c_third")
  chars = c(5,12,11,9,6,13,12,5,4,13,12)

  # GROUP
  for (i in 1:length(group)){ 
    subdirs = Sys.glob(sprintf("/corral-repl/utexas/ldrc/%s" ,group[i]))
    rm("dat_all")
    print(group[i])
    # SUB
    for (sub in subdirs){
      behav_dirs = c(Sys.glob(sprintf("%s/behav/SC/SC_R*", sub))) #, Sys.glob(sprintf("%s/behav/SC/m_SC_R*", sub)))
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

          # CREATE DAT_ALL IF DOESN'T EXIST
          if (exists("dat_all")==FALSE){
            dat_all = dat_loop 
          } 
          else{ 
            dat_all = rbind(dat_all, dat_loop)
          }
     
       } # END RUN LOOP
     }  # END SUB LOOP



    # dat_all$correct: 1=yes, 0=no, 3=mismatch or RT out of range, 4=no response
    # dat_all$Cond: 1='active_s', 2='active_ns', 3='passive_s', 4='passive_ns'

      dat_all$cond_type = dat_all$Cond
      dat_all$cond_type[dat_all$Cond==1] = 'active_s'
      dat_all$cond_type[dat_all$Cond==2] = 'active_ns'
      dat_all$cond_type[dat_all$Cond==3] = 'passive_s'
      dat_all$cond_type[dat_all$Cond==4] = 'passive_ns'

    # creates vector of 0's and 1's from TrialNum, that does not include repeated correct responses 
    # (whether or not there was a response to each trial)
      indicator = c(dat_all$TrialNum,0)-c(0, dat_all$TrialNum)  					# ADDS 0 TO BEGINNING AND END OF TrialNum AND SUBTRACTS, SO IF REPEAT IT SUBTRACTS ITSELF AND BECOMES 0
      indicator[indicator!=0] = 1									# SETS EVERYTHING NOT = 0 TO 1, to get rid of -31's
      dat_all$first = indicator[1:(length(indicator)-1)]						# SUBTRACT EXTRA NUMBER ON END

    # ALL CORRECT AND INCORR TRIALS (not including repeated correct trials) 
      dat_all$correct_first = 0*dat_all$correct
      dat_all$correct_first[dat_all$correct==1 & dat_all$first==1] = 1

    # ADD GROUP IDENTIFIER
      dat_all$group_num= i

    # CREATE SEPARATE DATA FRAME FOR EACH GROUP
      assign(paste("dat_all",i,sep="_"),dat_all)

  } # END GROUP LOOP


#Figuring out sentence orders
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







  # DF OF ALL GROUPS
    dat = rbind(dat_all_1,dat_all_2, dat_all_3, dat_all_4, dat_all_5, dat_all_6, dat_all_7, dat_all_8, dat_all_9, dat_all_10, dat_all_11)

    
    #FILLING IN THE DATA FRAME
    dat$groups[dat$group_num == 1] = 'A_first'
    dat$groups[dat$group_num == 2] = 'A_Second'
    dat$groups[dat$group_num == 3] = 'A_third'
    dat$groups[dat$group_num == 4] = 'A_first2'
    dat$groups[dat$group_num == 5] = 'H_first'
    dat$groups[dat$group_num == 6] = 'H_second'
    dat$groups[dat$group_num == 7] = 'H_third'
    dat$groups[dat$group_num == 8] = 'A_control'
    dat$groups[dat$group_num == 9] = 'H_control_first'
    dat$groups[dat$group_num == 10] = 'H_control_second'
    dat$groups[dat$group_num == 11] = 'H_control_third'
    dat$city = 'empty'
    dat$city[grep('A_', dat$groups)] = 'Austin'
    dat$city[grep('H_', dat$groups)] = 'Houston'
    dat$active = 'empty'
    dat$active[grep('active', dat$cond_type)] = 'Active'
    dat$active[grep('passive', dat$cond_type)] = 'Passive'
    dat$sense = 'empty'
    dat$sense[grep('_ns', dat$cond_type)] = 'Nonsense'
    dat$sense[grep('_s', dat$cond_type)] = 'Sense'
    dat = dat[dat$first == 1,]    #Removes the repeated trials (multiple button presses)
    dat$subtype = 'empty'
    dat$subtype[grep('control', dat$groups)] ='c'
    dat$subtype[grep('control', dat$groups, invert = T)] ='i'
    dat$correct_first = dat$correct_first*100   #to remove the decimal
    dat$sub = 'empty'
    dat$sub[dat$groups=="A_first"]=substrLeft(dat$subind[dat$groups=="A_first"],5) 
    dat$sub[dat$groups=="A_second"]=substrLeft(dat$subind[dat$groups=="A_second"],5) 
    dat$sub[dat$groups=="A_third"]=substrLeft(dat$subind[dat$groups=="A_third"],5) 
    dat$sub[dat$groups=="H_second"]=substrLeft(dat$subind[dat$groups=="H_second"],6)
    dat$sub[dat$groups=="H_third"]=substrLeft(dat$subind[dat$groups=="H_third"],6)  
    dat$sub[dat$groups=="H_control_second"]=substrLeft(dat$subind[dat$groups=="H_control_second"],4)   
    dat$sub[dat$groups=="H_control_third"]=substrLeft(dat$subind[dat$groups=="H_control_third"],4)   
    
    
    library(coin)
    
    #Run Medians without sentence type
    run_med = ddply(dat, .(city, subtype, groups, subind, runnum), summarize, rt_med = median(RT[correct_first == 100], na.rm = T), acc_avg = mean(correct_first, na.rm = T))
    
    #Mean of medians for any combination you want
    sub_mean = ddply(run_med, .(groups, subind), summarize, N = length(subind), rt_mean = mean(rt_med), acc_mean = mean(acc_avg))
    
    group_run_mean = ddply(run_med, .(groups, runnum), summarize, N = length(subind), rt_mean = mean(rt_med, na.rm = T), acc_mean = mean(acc_avg))
    
    group_mean = ddply(group_run_mean, .(groups), summarize, N = length(groups), rt.mean = mean(rt_mean), acc.mean = mean(acc_mean))
    
    A12 = sub_mean[sub_mean$groups == 'A_first' | sub_mean$groups == 'A_Second',]
    #RT
    t.test(rt_mean ~ groups, data = A12)
    wilcox.test(rt_mean ~ groups, data = A12, exact =F, conf.int=T, distribution=approximate(B=10000)) 
    
    #ACC
    t.test(acc_mean ~ groups, data = A12)
    wilcox.test(acc_mean ~ groups, data = A12, exact =F, conf.int=T, distribution=approximate(B=10000))          
    
    AC1 = sub_mean[sub_mean$groups == 'A_first' | sub_mean$groups == 'A_control',]
    #RT
    t.test(rt_mean ~ groups, data = AC1)
    wilcox.test(rt_mean ~ groups, data = AC1, exact =F, conf.int=T, distribution=approximate(B=10000))   
    
    #ACC
    t.test(acc_mean ~ groups, data = AC1)
    wilcox.test(acc_mean ~ groups, data = AC1, exact =F, conf.int=T, distribution=approximate(B=10000))
    
    AC2 = sub_mean[sub_mean$groups == 'A_Second' | sub_mean$groups == 'A_control',]
    #RT
    t.test(rt_mean ~ groups, data = AC2)
    wilcox.test(rt_mean ~ groups, data = AC2, exact =F, conf.int=T, distribution=approximate(B=10000))   
    
    #ACC
    t.test(acc_mean ~ groups, data = AC2)
    wilcox.test(acc_mean ~ groups, data = AC2, exact =F, conf.int=T, distribution=approximate(B=10000)) 
 
    AC3 = sub_mean[sub_mean$groups == 'A_third' | sub_mean$groups == 'A_control',]
    #RT
    t.test(rt_mean ~ groups, data = AC3)
    wilcox.test(rt_mean ~ groups, data = AC3, exact =F, conf.int=T, distribution=approximate(B=10000))   
    
    #ACC
    t.test(acc_mean ~ groups, data = AC3)
    wilcox.test(acc_mean ~ groups, data = AC3, exact =F, conf.int=T, distribution=approximate(B=10000)) 
     
    A13 = sub_mean[sub_mean$groups == 'A_first' | sub_mean$groups == 'A_third',]
    #RT
    t.test(rt_mean ~ groups, data = A13)
    wilcox.test(rt_mean ~ groups, data = A13, exact =F, conf.int=T, distribution=approximate(B=10000)) 
       
    #ACC
    t.test(acc_mean ~ groups, data = A13)
    wilcox.test(acc_mean ~ groups, data = A13, exact =F, conf.int=T, distribution=approximate(B=10000))          
    
    A23 = sub_mean[sub_mean$groups == 'A_Second' | sub_mean$groups == 'A_third',]
    #RT
    t.test(rt_mean ~ groups, data = A23)
    wilcox.test(rt_mean ~ groups, data = A23, exact =F, conf.int=T, distribution=approximate(B=10000)) 
       
    #ACC
    t.test(acc_mean ~ groups, data = A23)
    wilcox.test(acc_mean ~ groups, data = A23, exact =F, conf.int=T, distribution=approximate(B=10000))          
    

    
    
    #Run medians with sentence type
    cond_med = ddply(dat, .(city, subtype, groups, subind, runnum, active, sense), summarize, N = length(groups), rt_med = median(RT[correct_first == 100], na.rm = T), acc_avg = mean(correct_first, na.rm = T))
    
    A_cond_med = cond_med[cond_med$city == "Austin" & !(cond_med$groups == "A_first2" | cond_med$groups == "A_third"),]
    
    A_cond_mean = ddply(A_cond_med, .(subind, groups, active, sense), summarize, N = length(groups), rt_mean = mean(rt_med, na.rm = T), acc_mean = mean(acc_avg, na.rm = T))

    # RT    
    mod.1 = lme(rt_mean ~ active + sense + groups, random=~1|subind, data = A_cond_mean, method="ML")
    summary(mod.1)	# main effects
    anova(mod.1)	

    mod.2 = lme(rt_mean ~ active + sense + groups*active + groups*sense, random=~1|subind, data = A_cond_mean, method="ML")
    summary(mod.2)
    anova(mod.2)	# tells you if have two way interactions

    mod.3 = lme(rt_mean ~ active*sense*groups, random=~1|subind, data = A_cond_mean, method="ML")
    summary(mod.3)
    anova(mod.3)	# tells you if you have 3-way interactions

    aov.rt = aov(rt_mean ~ (active*sense*groups) + Error(subind/(active*sense)) +(groups), A_cond_mean)
    summary(aov.rt)

   #ACC
    mod.4 = lme(acc_mean ~ active + sense + groups, random=~1|subind, data = A_cond_mean, method="ML")
    summary(mod.4)      # main effects
    anova(mod.4)	

    mod.5 = lme(acc_mean ~ active + sense + groups*active + groups*sense, random=~1|subind, data = A_cond_mean, method="ML")
    summary(mod.5)
    anova(mod.5)	# tells you if have two way interactions

    mod.6 = lme(acc_mean ~ active*sense*groups, random=~1|subind, data = A_cond_mean, method="ML")
    summary(mod.6)
    anova(mod.6)	# tells you if you have 3-way interactions

    aov.acc = aov(acc_mean ~ (active*sense*groups) + Error(subind/(active*sense)) +(groups), A_cond_mean)
    summary(aov.acc)
