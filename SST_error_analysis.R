# This script reads in SST behavioral data and analyzes error processing
# Author: Mary Abbe Roe June 2013


#### LIBRARIES, FUNCTIONS, READING IN DATA ####

# LIBRARIES

  library(gtools)
  library(ggplot2)
  library(plyr)
  library(miscTools)
  library(nlme)
  library(reshape)

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
    subdirs=Sys.glob(sprintf("/corral-repl/utexas/ldrc/%s" ,group[i]))
    rm("dat.all")
    rm("rle.all")
    print(group[i])

    # SUB
    for (sub in subdirs){
      behav_dirs=c(Sys.glob(sprintf("%s/behav/SST/SST_R*", sub))) 
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
           if (exists("dat.all")==FALSE){
             dat.all=dat_loop 
           }  else { 
             dat.all=rbind(dat.all, dat_loop)
           } 




         # CREATE DF WITH RLE INFO FOR EACH SUBS RUN (run length encoding, values = values in the order they appeared; length = number of each value)
           rle.l = as.data.frame(rle(dat.all$correct)$lengths)
           rle.v = as.data.frame(rle(dat.all$correct)$values)
           rle.loop = cbind(rle.l, rle.v)
           names(rle.loop) = c("lengths", "values")
           rle.loop$subind = subnum
           rle.loop$runnum = run_num
         
         # CREATE RLE_ALL DF IF DOESN'T ALREADY EXIST
           if (exists("rle.all")==FALSE){
             rle.all=rle.loop 
           }  else { 
             rle.all=rbind(rle.all, rle.loop)
           } 
    
    
       } # END RUN LOOP

    }  # END SUB LOOP


    # creates vector of 0's and 1's from TrialNum, that does not include repeated correct responses 
    # (whether or not there was a response to each trial)
      indicator=c(dat.all$TrialNum,0)-c(0, dat.all$TrialNum)						  # ADDS 0 TO BEGINNING AND END OF TrialNum AND SUBTRACTS, SO IF REPEAT IT SUBTRACTS ITSELF AND BECOMES 0
      indicator[indicator!=0]=1  										  # SETS EVERYTHING NOT = 0 TO 1, to get rid of -31's
      dat.all$first=indicator[1:(length(indicator)-1)]							  # SUBTRACT EXTRA NUMBER ON END

    # ADD GROUP IDENTIFIER
      dat.all$groupnum=i
      rle.all$groupnum=i


    # CREATE SEPARATE DATA FRAME FOR EACH GROUP
      assign(paste("dat.all",i,sep="."),dat.all)
      assign(paste("rle.all",i,sep="."), rle.all)

  } # END GROUP LOOP


    dat.all.1$group = "A_first"
    dat.all.2$group = "A_second"
    dat.all.3$group = "A_third"
    dat.all.4$group = "A2_first"
    dat.all.5$group = "A2_second"
    dat.all.6$group = "H_first"
    dat.all.7$group = "H_second"
    dat.all.8$group = "H_third"
    dat.all.9$group = "H2_first"
    dat.all.10$group = "H2_second"
    dat.all.11$group = "A_control"
    dat.all.12$group = "H_control_first"
    dat.all.13$group = "H_control_second"
    dat.all.14$group = "H_control_third"

    rle.all.1$group = "A_first"
    rle.all.2$group = "A_second"
    rle.all.3$group = "A_third"
    rle.all.4$group = "A2_first"
    rle.all.5$group = "A2_second"
    rle.all.6$group = "H_first"
    rle.all.7$group = "H_second"
    rle.all.8$group = "H_third"
    rle.all.9$group = "H2_first"
    rle.all.10$group = "H2_second"
    rle.all.11$group = "A_control"
    rle.all.12$group = "H_control_first"
    rle.all.13$group = "H_control_second"
    rle.all.14$group = "H_control_third"



# LARGE DFs OF ALL GROUPS
  dat = rbind(dat.all.1, dat.all.2, dat.all.3, dat.all.4, dat.all.5, dat.all.6, dat.all.7, dat.all.8, dat.all.9, dat.all.10, dat.all.11, dat.all.12, dat.all.13, dat.all.14)
  rle = rbind(rle.all.1, rle.all.2, rle.all.3, rle.all.4, rle.all.5, rle.all.6, rle.all.7, rle.all.8, rle.all.9, rle.all.10, rle.all.11, rle.all.12, rle.all.13, rle.all.14)

# LARGE DFs OF AUSTIN AND HOUSTON
  a.dat = dat[(dat$groupnum=="1" | dat$groupnum=="2" | dat$groupnum=="3" | dat$groupnum=="4" | dat$groupnum=="5" | dat$groupnum=="11"), ]
  a.dat$correct = as.character(a.dat$correct)
  a.dat$groupnum = as.character(a.dat$groupnum)
  a.dat$cond = as.character(a.dat$cond)
  a.dat$Stim = as.character(a.dat$Stim)

  write.csv(a.dat, file = "/corral-repl/utexas/ldrc/SCRIPTS/data_frames/SST/a_dat_err_all.csv", row.names = F)


  a.dat$group2 = a.dat$group
  a.dat$group2[grep('first', a.dat$group)] = "first"
  a.dat$group2[grep('second', a.dat$group)] = "second"
  a.dat$group2[grep('third', a.dat$group)] = "third"
  a.dat$group2[grep('control', a.dat$group)] = "control"

  a.dat$subind[a.dat$group=="A_second" | a.dat$group=="A_third"] = substrLeft(a.dat$subind[a.dat$group=="A_second" | a.dat$group=="A_third"],10)
  a.dat$subind[a.dat$group=="A2_second"] = substrLeft(a.dat$subind[a.dat$group=="A2_second"],11)
  a.dat$subind[a.dat$group=="A_first" | a.dat$group=="A_second" | a.dat$group=="A_third" | a.dat$group=="A2_first" | a.dat$group=="A2_second" | a.dat$group=="A_control"] = substrRight(a.dat$subind[a.dat$group=="A_first" | a.dat$group=="A_second" | a.dat$group=="A_third" | a.dat$group=="A2_first" | a.dat$group=="A2_second" | a.dat$group=="A_control"],5)

  a.dat$ID = substrLeft(a.dat$subind, 1)
  a.dat$ID[a.dat$group2=="control"] = "c"
  a.dat$ID2 = a.dat$ID
  a.dat$ID2[a.dat$ID == "0"] = "0"
  a.dat$ID2[a.dat$ID == "1" | a.dat$ID == "2"] = "1"


  a.rle = rbind(rle.all.1, rle.all.2, rle.all.3,rle.all.4,rle.all.5, rle.all.11)

  #h.dat = dat[(dat$groupnum=="6" | dat$groupnum=="7" | dat$groupnum=="8" | dat$groupnum=="9" | dat$groupnum=="10" | dat$groupnum=="12" | dat$groupnum=="13" | dat$groupnum=="14"), ]



#### RT and ACC ANALYSES ####

sessions = length(group)


# GO LOOP

for (k in 1:sessions){

	if (k==1){
	dat.all=dat.all.1
	group="A_first"

	} else {

	if (k==2){
	dat.all=dat.all.2
	group="A_second"

	} else {

	if (k==3){
	dat.all=dat.all.3
	group="A_third"

	} else {

	if (k==4){
	dat.all=dat.all.4
	group="A2_first"

	} else {

	if (k==5){
	dat.all=dat.all.5
	group="A2_second"

	} else {

	if (k==6){
	dat.all=dat.all.6
	group="H_first"

        } else {

	if (k==7){
	dat.all=dat.all.7
	group="H_second"

	} else {

	if (k==8){
	dat.all=dat.all.8
	group="H_third"

	} else {

	if (k==9){
	dat.all=dat.all.9
	group="H2_first"

	} else {

	if (k==10){
	dat.all=dat.all.10
	group="H2_second"

	} else {

	if (k==11){
	dat.all=dat.all.11
	group="A_control"

	} else {

	if (k==12){
	dat.all=dat.all.12
	group="H_control_first"

	} else {

	if (k==13){
	dat.all=dat.all.13
	group="H_control_second"
	
	} else {

	if (k==14){
	dat.all=dat.all.14
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
 
      go_run=ddply(dat.all, .(subind, runnum, group), summarise, N=length(subind), go_rt_med=median(RT[correct==1], na.rm=TRUE), go_rt_sd=sd(RT[correct==1], na.rm=TRUE), go_acc=length(correct[correct==1])/length(cond[cond==1|cond==0]), go_error=length(correct[correct==0])/length(cond[cond==1|cond==0]))
      assign(paste("go_run",k,sep="_"),go_run)

    # ACROSS ALL RUNS FOR EACH SUB (# lines are ACC cutoffs) - useful for SSRT calculation
   
      go_sub=ddply(go_run, .(subind, group), summarise, N=length(subind), go_rt_mean=mean(go_rt_med, na.rm=TRUE), go_rt_sd=sd(go_rt_med, na.rm=T), go_acc_mean=mean(go_acc, na.rm=TRUE), go_error_mean=mean(go_error, na.rm=TRUE))
      assign(paste("go_sub",k,sep="_"),go_sub)

} # END GO LOOP


  # COMBINE GO DATA FRAMES FOR AUSTIN AND HOUSTON
  all_subs = rbind(go_sub_1, go_sub_2, go_sub_3, go_sub_4, go_sub_5, go_sub_6, go_sub_7, go_sub_8, go_sub_9, go_sub_10, go_sub_11,go_sub_12, go_sub_13, go_sub_14)
 
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



----------------------------------

#### ERROR PROCESSING ANALYSES ####

# OVERALL DISTRIBUTION OF GO CORRECT

  # AUSTIN
    a.rle.gc = a.rle[a.rle$values==1,]

    # FREQ OF LENGTHS PER RUN PER SUB
      A.rle.1 =count(A.rle.gc, c("subind", "groupnum", "runnum", "lengths"))
      A.rle.sub = ddply(A.rle.1, .(subind, groupnum, lengths), summarise, N=length(subind), freq.sum = sum(freq, na.rm=TRUE))
      A.rle.group = ddply(A.rle.sub, .(groupnum, lengths), summarise, N=length(groupnum), freq.mean = mean(freq.sum, na.rm=TRUE), freq.se = sd(freq.sum, na.rm=TRUE)/sqrt(N))
      A.rle.group$group[A.rle.group$groupnum==1] = "A_first"
      A.rle.group$group[A.rle.group$groupnum==2] = "A_second"
      A.rle.group$group[A.rle.group$groupnum==3] = "A_third"
      A.rle.group$group[A.rle.group$groupnum==4] = "A_first2"
      A.rle.group$group[A.rle.group$groupnum==9] = "A_control"


    A_bar = ggplot(data = A.rle.group, aes(x = lengths, y = freq.mean, group = group, fill = group)) + geom_bar(position = "dodge", stat = "identity") + geom_errorbar(aes(y = freq.mean, ymin = freq.mean-freq.se, ymax = freq.mean+freq.se), position = position_dodge(.9), width=.4) + scale_x_discrete(limit = c("1", "2", "3", "4", "5", "6", "7","8", "9", "10", "11", "12", "13")) + xlab("Number of Go Corrects in a row") + ylab("Mean Frequency (s)") + ggtitle("Austin Mean Frequency of Go Corrects in a Row")










-----
# FIRST 2 GOs AFTER STOP CORRECT - AUSTIN

group = unique(as.character(a.dat$group))

for (g in 1:length(group)){
  rm("group.dat")
  rm("corr_all")
  group.dat = a.dat[a.dat$group==group[g],]
  group.dat$correct = as.character(group.dat$correct)

  subind = unique(as.character(group.dat$subind))

  #names = names(group.dat)

      for (sub in subind){
        sub.dat = group.dat[group.dat$subind==sub,]
        runnum = unique(sub.dat$runnum)

        for (run in runnum){
          run_dat = sub.dat[sub.dat$runnum==run,]
          run_dat_temp = run_dat[1,]

          if (length(run_dat)==0){
            warning(sprintf("cannot find df for %s", run))
            next
          }
 
          # GRAB FIRST 2 CORRECT GO'S(1) AFTER A CORRECT STOP(4)
            corr_0 = run_dat_temp
            corr_1 = run_dat_temp
            corr_2 = run_dat_temp

            for (k in 1:length(run_dat$correct)){
              #print(k)
              if(identical(c(run_dat$correct[k],run_dat$correct[k+1],run_dat$correct[k+2]), c("4","1","1"))){
 	        temp0 = run_dat[k,]
     	        temp1 = run_dat[k+1,]
	        temp2 = run_dat[k+2,]

	        corr_0 = smartbind(corr_0, temp0)
                corr_1 = smartbind(corr_1, temp1)
	        corr_2 = smartbind(corr_2, temp2)
	
 
             }
            }

            corr_all1 = rbind(corr_0, corr_1, corr_2)

            if (length(corr_all1$correct)>4){
              corr_0 = corr_0[-1,]
              corr_0$go = 0
              corr_1 = corr_1[-1,]
              corr_1$go = 1
              corr_2 = corr_2[-1,]
              corr_2$go = 2
              #corr_3 = corr_3[-1,]
              #corr_3$go = 3
              #corr_4 = corr_4[-1,]
              #corr_4$go = 4
              #corr_5 = corr_5[-1,]
              #corr_5$go = 5
              #corr_6 = corr_6[-1,]
              #corr_6$go = 6

              corr_loop = rbind(corr_0, corr_1, corr_2)

              if (exists("corr_all")==FALSE){
               corr_all=corr_loop 
              }  else { 
               corr_all=rbind(corr_all, corr_loop)
              }          
 
             
              }  else {
                warning(sprintf("stopCorr trial does not exist for %s run%s", sub, run))
                next
              }           
        
    
        } # END RUN LOOP
      } # END SUB LOOP


  # CREATE SEPARATE DATA FRAME FOR EACH GROUP
    assign(paste("corr_all",g,sep="_"),corr_all)       


} # END GROUP LOOP   
   


corr_all_groups = rbind(corr_all_1, corr_all_2, corr_all_3, corr_all_4, corr_all_5, corr_all_6)



# DFs FOR CALCULATING THE MEDIAN AND MEAN ACROSS SUBJECTS AND GROUPS
  corr_med = ddply(corr_all_groups, .(subind, group, group2, ID, ID2, go), summarise, N = length(subind), rt_med = median(RT))

  # AUSTIN
    A_corr_med = corr_med[grep("A", corr_med$group), ]
    A_corr_med$trial[A_corr_med$go == 0] = "stopCorr"
    A_corr_med$trial[A_corr_med$go == 1] = "goCorr1"
    A_corr_med$trial[A_corr_med$go == 2] = "goCorr2"

    A_corr_med$trial_num=A_corr_med$N
    A_corr_med_trials = ddply(A_corr_med, .(group2, go), summarise, N=length(group2), trial_tot = sum(trial_num))


   # MEAN W/OVERALL RT GROUP MEANS FOR AUSTIN SUBS - with 0s
     A_corr_mean = ddply(A_corr_med, .(group2, go, trial), summarise, N = length(go), rt_mean = mean(rt_med, na.rm=TRUE), se = sd(rt_med, na.rm=TRUE)/sqrt(N))

    # MEAN ACROSS REPEAT AUSTIN SUBS WITH 0s
      corr.sub.names = as.data.frame(table(A_corr_med$subind))
      corr.sub.names = as.data.frame(corr.sub.names[corr.sub.names$Freq>3, ])
      corr.sub.names$Var1 = as.character(corr.sub.names$Var1)

      A_corr_med[,"rep"] = "FALSE"
      A_corr_med$rep = A_corr_med$subind %in% corr.sub.names$Var1
      A_corr_med$rep[A_corr_med$group == "A_control"] = "TRUE"

      A_r_corr_mean = ddply(A_corr_med[A_corr_med$rep=="TRUE",], .(group, go), summarise, N = length(go), rt_mean = mean(rt_med, na.rm=TRUE), se = sd(rt_med, na.rm=TRUE)/sqrt(N))
      A_r_corr_mean$trial[A_r_corr_mean$go == 0] = "stopCorr"
      A_r_corr_mean$trial[A_r_corr_mean$go == 1] = "goCorr1"
      A_r_corr_mean$trial[A_r_corr_mean$go == 2] = "goCorr2"
      #A_r_corr_mean$trial[A_r_corr_mean$go == 3] = "goCorr3"
      #A_r_corr_mean$trial[A_r_corr_mean$go == 4] = "goCorr4"
      #A_r_corr_mean$trial[A_r_corr_mean$go == 5] = "goCorr5"

      A.go.sub.r1 = c()

      for (a in 1:length(A.go.sub$subind)) {
        for (b in 1:length(corr.sub.names$Var1)){
          if (corr.sub.names$Var1[b] == A.go.sub$subind[a]){
           A.go.sub.r1 = rbind(A.go.sub[a,], A.go.sub.r1)    
          }
      }
      }
    
      A.go.sub.r = rbind(A.go.sub[A.go.sub$groups=="A_control", ], A.go.sub.r1)
      A.go.group2 = ddply(A.go.sub.r, .(groups), summarise, N = length(groups), go_rt = mean(go_rt_mean, na.rm=TRUE), rt_se = sd(go_rt_mean)/sqrt(N), go_acc = mean(go_acc_mean, na.rm=TRUE), go_error = mean(go_error_mean, na.rm=TRUE))

      A_r_corr_mean$group_mean[A_r_corr_mean$group=="A_control"] = A.go.group2$go_rt[A.go.group2$groups=="A_control"]
      A_r_corr_mean$group_mean[A_r_corr_mean$group=="A_first"] = A.go.group2$go_rt[A.go.group2$groups=="A_first"]
      A_r_corr_mean$group_mean[A_r_corr_mean$group=="A_second"] = A.go.group2$go_rt[A.go.group2$groups=="A_second"]
      A_r_corr_mean$group_mean[A_r_corr_mean$group=="A_third"] = A.go.group2$go_rt[A.go.group2$groups=="A_third"]
 
  # MEAN ACROSS ALL SUBS  
    corr_mean = ddply(corr_med, .(group, go), summarise, N = length(go), rt_mean = mean(rt_med, na.rm=TRUE), se = sd(rt_med, na.rm=TRUE)/sqrt(N))
    corr_mean$trial[corr_mean$go == 0] = "stopCorr"
    corr_mean$trial[corr_mean$go == 1] = "goCorr1"
    corr_mean$trial[corr_mean$go == 2] = "goCorr2"
    #corr_mean$trial[corr_mean$go == 3] = "goCorr3"
    #corr_mean$trial[corr_mean$go == 4] = "goCorr4"
    #corr_mean$trial[corr_mean$go == 5] = "goCorr5"
  
  # MEAN ACROSS ALL HOUSTON SUBS
    H_corr_mean = corr_mean[grep("H_", corr_mean$group), ]



# IDA talk

  ggplot(data = A_corr_mean[A_corr_mean$go!=0 & A_corr_mean$group2!="third", ],aes(x = trial,y = rt_mean)) + geom_point(aes(size=1, color=group2)) + geom_line(aes(group = group2, color = group2), linetype = "dashed") + geom_errorbar(data = A_corr_mean[A_corr_mean$go!=0 & A_corr_mean$group2!="third", ], aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se, color = group2), width=.2) + scale_y_continuous(limits = c(.58, .70), breaks = c(.6, .65, .7)) + scale_color_manual(values=c("#999999","#339900", "#3366FF")) + xlab("Go Corrects") + ylab("Mean RT (s)")  + guides(size = FALSE) + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_blank(), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))  + guides(group = FALSE, fill=FALSE) 
  ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/a_stopcorr_gocorr_point.pdf",wd),width=5,height=5)


  ggplot(data = A_corr_mean[(A_corr_mean$trial!="stopCorr" & A_corr_mean$group2!="third"),], aes(x = group2, y = rt_mean, group = trial, fill = group2)) + geom_bar(position = "dodge", stat = "identity", color = "black") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + ylab("Mean Response Time (s)") + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + coord_cartesian(ylim=c(0.4,0.8)) + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.text.y = element_text(size = 15))  + guides(group = FALSE, color = F) 
  ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/a_stopcorr_gocorr_bar.pdf",wd),width=5,height=5)



   # GoCorr1 vs. GoCorr2 within group
     # FIRSTS
       sink("/corral-repl/utexas/ldrc/SCRIPTS/figures/Project_4/SST/Error/error_stats_stopco_goco.txt")
       go2.stop.s1.g1.g2 = t.test(A_corr_med$rt_med[A_corr_med$go == "1"  & A_corr_med$group2 == "first"], A_corr_med$rt_med[A_corr_med$go == "2" & A_corr_med$group2 == "first"], paired = TRUE)
       print("S1")
       print(go2.stop.s1.g1.g2)

     # SECONDS
       go2.stop.s2.g1.g2 = t.test(A_corr_med$rt_med[A_corr_med$go == "1"  & A_corr_med$group2 == "second"], A_corr_med$rt_med[A_corr_med$go == "2"  & A_corr_med$group2 == "second"], paired = TRUE) 
       print("S2")
       print(go2.stop.s2.g1.g2)

     # CONTROLS
       go2.stop.c.g1.g2 = t.test(A_corr_med$rt_med[A_corr_med$go == "1" & A_corr_med$group2 == "control"], A_corr_med$rt_med[A_corr_med$go == "2"  & A_corr_med$group2 == "control"], paired = TRUE)
       print("Controls")
       print(go2.stop.c.g1.g2)
       sink()

  # DIFF BETWEEN GOCORR1 & GOCORR2 WITHIN GROUP VS. GROUPS
     A_corr_med_diff1 = A_corr_med[(A_corr_med$group2!="third" & (A_corr_med$go == "1" | A_corr_med$go == "2")), ]
     A_corr_med_diff = ddply(A_corr_med_diff1, .(group2, subind), summarise, N=length(subind), med_diff = (rt_med[go=="2"])-(rt_med[go=="1"]))

     # FIRST VS. CONTROL
       sink("/corral-repl/utexas/ldrc/SCRIPTS/figures/Project_4/SST/Error/error_stats_stopco_change_btwngroup.txt")
       go2.stop.s1.c.change = t.test(A_corr_med_diff$med_diff[A_corr_med_diff$group2=="first"], A_corr_med_diff$med_diff[A_corr_med_diff$group2=="control"])
       print("S1 vs. Controls")
       print(go2.stop.s1.c.change)

     # SECOND VS. CONTROL
       go2.stop.s2.c.change = t.test(A_corr_med_diff$med_diff[A_corr_med_diff$group2=="second"], A_corr_med_diff$med_diff[A_corr_med_diff$group2=="control"])
       print("S2 vs. Controls")
       print(go2.stop.s2.c.change)

     # FIRST VS. SECOND
       go2.stop.s1.s2.change = t.test(A_corr_med_diff$med_diff[A_corr_med_diff$group2=="first"], A_corr_med_diff$med_diff[A_corr_med_diff$group2=="second"])
       print("S1 vs. S2")
       print(go2.stop.s1.s2.change)
       sink()







  # AUSTIN STATS

    # MAIN EFFECT OF GROUPS ON GOs
      rootdir = "/corral-repl/utexas/ldrc/SCRIPTS/Stats/Error/After_StopCorr/"

      # FIRST VS. CONTROL       
        sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopCorr/go2_Astop_maineffect_group.txt")
        go2.stop.s1.c = t.test(A_corr_med$rt_med[A_corr_med$go!="0" & A_corr_med$group2 == "A_first"], A_corr_med$rt_med[A_corr_med$go!="0" & A_corr_med$group2 == "A_control"])
        print("S1 vs. Controls")
        print(go2.stop.s1.c)

      # SECOND VS. CONTROL
        go2.stop.s2.c = t.test(A_corr_med$rt_med[A_corr_med$go!="0" & A_corr_med$group2 == "A_second"], A_corr_med$rt_med[A_corr_med$go!="0" & A_corr_med$group2 == "A_control"])
        print("S2 vs. Controls")
        print(go2.stop.s2.c)

      # FIRST VS. SECOND
        go2.stop.s1.s2 = t.test(A_corr_med$rt_med[A_corr_med$go!="0" & A_corr_med$group2 == "A_first"], A_corr_med$rt_med[A_corr_med$go!="0" & A_corr_med$group2 == "A_second"])
        print("S1 vs. S2")
        print(go2.stop.s1.s2)
        sink()

      #FIRST1 VS. FIRST2
       t.test(A_corr_med$rt_med[A_corr_med$go!="0" & A_corr_med$group == "A_first"], A_corr_med$rt_med[A_corr_med$go!="0" & A_corr_med$group == "A_first2"])



    # GOCORR1
      # first vs. control
        #goCorr1.1 = lme(rt_med ~ group, random = list(~1 | subind), data = A_corr_med[(A_corr_med$go == "1") & (A_corr_med$group == "A_first" | A_corr_med$group == "A_second" | A_corr_med$group == "A_control"), ], method = "ML")
        #summary(goCorr1.1)$tTable

        sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopCorr/go2_Astop_gcorr1_group.txt")
        go2.stop.s1.c.g1 = t.test(A_corr_med$rt_med[A_corr_med$go == "1"  & A_corr_med$group2 == "A_first"], A_corr_med$rt_med[A_corr_med$go == "1" & A_corr_med$group2 == "A_control"])
        print("S1 vs. Controls")
        print(go2.stop.s1.c.g1)

      # second vs. control
        go2.stop.s2.c.g1 = t.test(A_corr_med$rt_med[A_corr_med$go == "1"  & A_corr_med$group2 == "A_second"], A_corr_med$rt_med[A_corr_med$go == "1" & A_corr_med$group2 == "A_control"])
        print("S2 vs. Controls")
        print(go2.stop.s2.c.g1)
       
      # first  vs. second
        go2.stop.s1.s2.g1 =t.test(A_corr_med$rt_med[A_corr_med$go == "1"  & A_corr_med$group2 == "A_first"], A_corr_med$rt_med[A_corr_med$go == "1" & A_corr_med$group2 == "A_second"])
        print("S1 vs. S2")
        print(go2.stop.s1.s2.g1)
        sink()


      #FIRST1 VS. FIRST2
       t.test(A_corr_med$rt_med[A_corr_med$go=="1" & A_corr_med$group == "A_first"], A_corr_med$rt_med[A_corr_med$go=="1" & A_corr_med$group == "A_first2"])


    # GoCorr2
      # first vs. control
        sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopCorr/go2_Astop_gcorr2_group.txt")
        go2.stop.s1.c.g2 = t.test(A_corr_med$rt_med[A_corr_med$go == "2" & A_corr_med$group2 == "A_first"], A_corr_med$rt_med[A_corr_med$go == "2" & A_corr_med$group2 == "A_control"])
        print("S1 vs. Controls")
        print(go2.stop.s1.c.g2)

      # second vs. control
        go2.stop.s2.c.g2 = t.test(A_corr_med$rt_med[A_corr_med$go == "2" & A_corr_med$group2 == "A_second"], A_corr_med$rt_med[A_corr_med$go == "2"  & A_corr_med$group2 == "A_control"])
        print("S2 vs. Controls")
        print(go2.stop.s2.c.g2)

      # first  vs. second
        go2.stop.s1.s2.g2 = t.test(A_corr_med$rt_med[A_corr_med$go == "2" & A_corr_med$group2 == "A_first"], A_corr_med$rt_med[A_corr_med$go == "2" & A_corr_med$group2 == "A_second"])
        print("S1 vs. S2")
        print(go2.stop.s1.s2.g2)
        sink()

      #FIRST1 VS. FIRST2
       t.test(A_corr_med$rt_med[A_corr_med$go=="2" & A_corr_med$group == "A_first"], A_corr_med$rt_med[A_corr_med$go=="2" & A_corr_med$group == "A_first2"])


    # GoCorr3
      # first vs. control
        t.test(A_corr_med$rt_med[A_corr_med$go == "3" & A_corr_med$ID!="0" & A_corr_med$group == "A_first"], A_corr_med$rt_med[A_corr_med$go == "3" & A_corr_med$group == "A_control"])
      # second vs. control
        t.test(A_corr_med$rt_med[A_corr_med$go == "3" & A_corr_med$ID!="0" & A_corr_med$group == "A_second"], A_corr_med$rt_med[A_corr_med$go == "3" & A_corr_med$ID!="0" & A_corr_med$group == "A_control"])
      # first  vs. second
        t.test(A_corr_med$rt_med[A_corr_med$go == "3" & A_corr_med$ID!="0" & A_corr_med$group == "A_first"], A_corr_med$rt_med[A_corr_med$go == "3" & A_corr_med$ID!="0" & A_corr_med$group == "A_second"])

    # GoCorr4
      # first vs. control
        #t.test(A_corr_med$rt_med[A_corr_med$go == "4" & A_corr_med$ID!="0" & A_corr_med$group == "A_first"], A_corr_med$rt_med[A_corr_med$go == "4" & A_corr_med$group == "A_control"])
      # second vs. control
        #t.test(A_corr_med$rt_med[A_corr_med$go == "4" & A_corr_med$ID!="0" & A_corr_med$group == "A_second"], A_corr_med$rt_med[A_corr_med$go == "4"  & A_corr_med$group == "A_control"])
      # first  vs. second
        #t.test(A_corr_med$rt_med[A_corr_med$go == "4" & A_corr_med$ID!="0" & A_corr_med$group == "A_first"], A_corr_med$rt_med[A_corr_med$go == "4" & A_corr_med$ID!="0" & A_corr_med$group == "A_second"])

    # GOCORR5
      # first vs. control
        #goCorr1.1 = lme(rt_med ~ group, random = list(~1 | subind), data = A_corr_med[(A_corr_med$go == "5") & (A_corr_med$group == "A_first" | A_corr_med$group == "A_second" | A_corr_med$group == "A_control"), ], method = "ML")
        #summary(goCorr1.1)$tTable

        #t.test(A_corr_med$rt_med[A_corr_med$go == "5" & A_corr_med$ID!="0" & A_corr_med$group == "A_first"], A_corr_med$rt_med[A_corr_med$go == "5" & A_corr_med$ID!="0" & A_corr_med$group == "A_control"])

      # second vs. control
        #t.test(A_corr_med$rt_med[A_corr_med$go == "5" & A_corr_med$ID!="0" & A_corr_med$group == "A_second"], A_corr_med$rt_med[A_corr_med$go == "5" & A_corr_med$group == "A_control"])

      # first  vs. second
        #goCorr1.2 = lme(rt_med ~ group, random = list(~1 | subind), data = A_corr_med[(A_corr_med$go == "5") & (A_corr_med$group == "A_first" | A_corr_med$group == "A_second"), ], method = "ML")
        #summary(goCorr1.2)$tTable

        #t.test(A_corr_med$rt_med[A_corr_med$go == "5" & A_corr_med$ID!="0" & A_corr_med$group == "A_first"], A_corr_med$rt_med[A_corr_med$go == "5" & A_corr_med$ID!="0" & A_corr_med$group == "A_second"])


    # GoCorr1 vs. GoCorr2 within group
     # FIRSTS
       sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopCorr/go2_Astop_gcorr1_gcorr2_withingroup.txt")
       go2.stop.s1.g1.g2 = t.test(A_corr_med$rt_med[A_corr_med$go == "1"  & A_corr_med$group2 == "A_first"], A_corr_med$rt_med[A_corr_med$go == "2" & A_corr_med$group2 == "A_first"], paired = TRUE)
       print("S1")
       print(go2.stop.s1.g1.g2)

     # SECONDS
       go2.stop.s2.g1.g2 = t.test(A_corr_med$rt_med[A_corr_med$go == "1"  & A_corr_med$group2 == "A_second"], A_corr_med$rt_med[A_corr_med$go == "2"  & A_corr_med$group2 == "A_second"], paired = TRUE) 
       print("S2")
       print(go2.stop.s2.g1.g2)

     # CONTROLS
       go2.stop.c.g1.g2 = t.test(A_corr_med$rt_med[A_corr_med$go == "1" & A_corr_med$group2 == "A_control"], A_corr_med$rt_med[A_corr_med$go == "2"  & A_corr_med$group2 == "A_control"], paired = TRUE)
       print("Controls")
       print(go2.stop.c.g1.g2)
       sink()

     # FIRSTS1
       t.test(A_corr_med$rt_med[A_corr_med$go == "1"  & A_corr_med$group == "A_first"], A_corr_med$rt_med[A_corr_med$go == "2" & A_corr_med$group == "A_first"], paired = TRUE)
     # FIRSTS2
       t.test(A_corr_med$rt_med[A_corr_med$go == "1"  & A_corr_med$group == "A_first2"], A_corr_med$rt_med[A_corr_med$go == "2" & A_corr_med$group == "A_first2"], paired = TRUE)
 

    # GoCorr2 vs. GoCorr3 within group
     # FIRSTS
       t.test(A_corr_med$rt_med[A_corr_med$go == "2" & A_corr_med$ID!="0" & A_corr_med$group == "A_first"], A_corr_med$rt_med[A_corr_med$go == "3" & A_corr_med$ID!="0" & A_corr_med$group == "A_first"], paired = TRUE)
     # SECONDS
       t.test(A_corr_med$rt_med[A_corr_med$go == "2" & A_corr_med$ID!="0" & A_corr_med$group == "A_second"], A_corr_med$rt_med[A_corr_med$go == "3" & A_corr_med$ID!="0" & A_corr_med$group == "A_second"], paired = TRUE)
     # CONTROLS
       t.test(A_corr_med$rt_med[A_corr_med$go == "2" & A_corr_med$ID!="0" & A_corr_med$group == "A_control"], A_corr_med$rt_med[A_corr_med$go == "3" & A_corr_med$ID!="0" & A_corr_med$group == "A_control"], paired = TRUE)


    # GoCorr3 vs. GoCorr4 within group
     # FIRSTS
       #t.test(A_corr_med$rt_med[A_corr_med$go == "3"  & A_corr_med$ID!="0" & A_corr_med$group == "A_first"], A_corr_med$rt_med[A_corr_med$go == "4"  & A_corr_med$ID!="0" & A_corr_med$group == "A_first"], paired = TRUE)
     # SECONDS
       #t.test(A_corr_med$rt_med[A_corr_med$go == "3"  & A_corr_med$ID!="0" & A_corr_med$group == "A_second"], A_corr_med$rt_med[A_corr_med$go == "4"  & A_corr_med$ID!="0" & A_corr_med$group == "A_second"], paired = TRUE)
     # CONTROLS
       #t.test(A_corr_med$rt_med[A_corr_med$go == "3"  & A_corr_med$ID!="0" & A_corr_med$group == "A_control"], A_corr_med$rt_med[A_corr_med$go == "4"  & A_corr_med$ID!="0" & A_corr_med$group == "A_control"], paired = TRUE)


    # GoCorr4 vs. GoCorr5 within group
     # FIRSTS
       #t.test(A_corr_med$rt_med[A_corr_med$go == "4" & A_corr_med$group == "A_first"], A_corr_med$rt_med[A_corr_med$go == "5" & A_corr_med$group == "A_first"], paired = TRUE)
     # SECONDS
       #t.test(A_corr_med$rt_med[A_corr_med$go == "4" & A_corr_med$group == "A_second"], A_corr_med$rt_med[A_corr_med$go == "5" & A_corr_med$group == "A_second"], paired = TRUE)
     # CONTROLS
       #t.test(A_corr_med$rt_med[A_corr_med$go == "4" & A_corr_med$group == "A_control"], A_corr_med$rt_med[A_corr_med$go == "5" & A_corr_med$group == "A_control"], paired = TRUE)


   # GOCORR BEFORE STOPCORR (go = 4) VS. AFTER STOPCORR (go = 1)
     # FIRSTS
       $t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "4" & A_corr_b_med$group == "A_first"], A_corr_med$rt_med[A_corr_med$go == "1" & A_corr_med$group == "A_first"])
     # SECONDS
       $t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "4" & A_corr_b_med$group == "A_second"], A_corr_med$rt_med[A_corr_med$go == "1" & A_corr_med$group == "A_second"])
     # CONTROLS
       $t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "4" & A_corr_b_med$group == "A_control"], A_corr_med$rt_med[A_corr_med$go == "1" & A_corr_med$group == "A_control"])


   # DIFF BETWEEN GOCORR1 & GOCORR2 WITHIN GROUP VS. GROUPS
     A_corr_med_diff1 = A_corr_med[(A_corr_med$group2!="A_third" & (A_corr_med$go == "1" | A_corr_med$go == "2")), ]
     A_corr_med_diff = ddply(A_corr_med_diff1, .(group2, subind), summarise, N=length(subind), med_diff = (rt_med[go=="1"])-(rt_med[go=="2"]))

     # FIRST VS. CONTROL
       sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopCorr/go2_Astop_change_btwngroup.txt")
       go2.stop.s1.c.change = t.test(A_corr_med_diff$med_diff[A_corr_med_diff$group2=="A_first"], A_corr_med_diff$med_diff[A_corr_med_diff$group2=="A_control"])
       print("S1 vs. Controls")
       print(go2.stop.s1.c.change)

     # SECOND VS. CONTROL
       go2.stop.s2.c.change = t.test(A_corr_med_diff$med_diff[A_corr_med_diff$group2=="A_second"], A_corr_med_diff$med_diff[A_corr_med_diff$group2=="A_control"])
       print("S2 vs. Controls")
       print(go2.stop.s2.c.change)

     # FIRST VS. SECOND
       go2.stop.s1.s2.change = t.test(A_corr_med_diff$med_diff[A_corr_med_diff$group2=="A_first"], A_corr_med_diff$med_diff[A_corr_med_diff$group2=="A_second"])
       print("S1 vs. S2")
       print(go2.stop.s1.s2.change)
       sink()

     # FIRST1 VS. FIRST2
       t.test(A_corr_med_diff$med_diff[A_corr_med_diff$group=="A_first"], A_corr_med_diff$med_diff[A_corr_med_diff$group=="A_first2"])

    # GoCorr1 vs.average GoCorr RT within group
     # FIRSTS
       sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopCorr/go2_Astop_g1_v_gmean_withingroup.txt")
       go2_stop_g1_v_mean_s1 = t.test(A_corr_med$rt_med[A_corr_med$go == "1" & A_corr_med$group2 == "A_first"], A.go.sub$go_rt_mean[A.go.sub$groups2=="A_first"], paired = TRUE)
       print("Firsts")
       print(go2_stop_g1_v_mean_s1)
 
     # SECONDS
       go2_stop_g1_v_mean_s2 = t.test(A_corr_med$rt_med[A_corr_med$go == "1" & A_corr_med$group2 == "A_second"], A.go.sub$go_rt_mean[A.go.sub$groups2=="A_second"], paired = TRUE)
       print("Seconds")
       print(go2_stop_g1_v_mean_s2)
 
     # CONTROLS
       go2_stop_g1_v_mean_c = t.test(A_corr_med$rt_med[A_corr_med$go == "1" & A_corr_med$group2 == "A_control"], A.go.sub$go_rt_mean[A.go.sub$groups2=="A_control"], paired = TRUE)     
       print("Controls")
       print(go2_stop_g1_v_mean_c)
       sink()


    # GoCorr2 vs.average GoCorr RT within group
     # FIRSTS
       sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopCorr/go2_Astop_g2_v_gmean_withingroup.txt")
       go2_stop_g2_v_mean_s1=t.test(A_corr_med$rt_med[A_corr_med$go == "2" & A_corr_med$group2 == "A_first"], A.go.sub$go_rt_mean[A.go.sub$groups2=="A_first"], paired = TRUE)
       print("Firsts")
       print(go2_stop_g2_v_mean_s1)
 
     # SECONDS
       go2_stop_g2_v_mean_s2=t.test(A_corr_med$rt_med[A_corr_med$go == "2" & A_corr_med$group2 == "A_second"], A.go.sub$go_rt_mean[A.go.sub$groups2=="A_second"], paired = TRUE)
       print("Seconds")
       print(go2_stop_g2_v_mean_s2)
 
     # CONTROLS
       go2_stop_g2_v_mean_c = t.test(A_corr_med$rt_med[A_corr_med$go == "2" & A_corr_med$group2 == "A_control"], A.go.sub$go_rt_mean[A.go.sub$groups2=="A_control"], paired = TRUE)       
       print("Controls")
       print(go2_stop_g2_v_mean_c)
       sink()




  # FIGURES 
    # ALL
      All_line = ggplot(data = corr_mean[corr_mean$go!=1, ],aes(x = go,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group)) + xlab("Go Correct After Stop Correct") + ylab("Mean RT (s)") + ggtitle("Austin and Houston GoCorr After StopCorr")
               #ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_box.png",wd),width=10,height=10)

    # AUSTIN 
      A_sc_line = ggplot(data = A_corr_mean[A_corr_mean$go!=0, ],aes(x = trial,y = rt_mean)) + geom_point(aes(size=1)) + geom_line(aes(group = group, color = group), linetype = "dashed") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se, colour = group), width=.2) + geom_hline(aes(yintercept=group_mean, colour = group), position = "identity")+ xlab("Go Correct After Stop Correct") + ylab("Mean RT (s)") + ggtitle("Austin GoCorr After StopCorr") + guides(size = FALSE) +
                  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), 
                           axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
               ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopCorr_line.png",wd),width=10,height=10)

      A_sc_line2 = ggplot(data = A_corr_mean[A_corr_mean$go!=0 & A_corr_mean$group!="A_third", ],aes(x = trial,y = rt_mean)) + geom_point(aes(size=1)) + geom_line(aes(group = group, color = group), linetype = "dashed") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se, colour = group), width=.2) + geom_hline(aes(yintercept=group_mean, colour = group), position = "identity")+ xlab("Go Correct After Stop Correct") + ylab("Mean RT (s)") + ggtitle("Austin GoCorr After StopCorr") + guides(size = FALSE) +
                   theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), 
                           axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
               ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopCorr_line_1+2.png",wd),width=10,height=10)


      A_sc_bar_avg = ggplot(data = A.go.group,aes(x = group,y = go_rt_me, fill = group, group = group)) + geom_bar(position = "dodge", stat = "identity") + geom_errorbar(aes(y = go_rt_me, ymin = go_rt_me-go_rt_se, ymax = go_rt_me+go_rt_se), position = position_dodge(.9), width=.2) + xlab("Group") + ylab("Mean RT (s)") + ggtitle("Austin GoCorr Mean RT")


      A_sc_bar_SRCD = ggplot(data = A_corr_mean[(A_corr_mean$trial!="stopCorr" & A_corr_mean$group2!="A_third"),], aes(x = group2, y = rt_mean, group = trial, fill = group2)) + geom_bar(position = "dodge", stat = "identity",colour="black") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + ylab("Mean Response Time (s)") + coord_cartesian(ylim=c(0.4,0.8)) + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))  + guides(group = FALSE, fill=FALSE) 
               ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopCorr_2go_bar_SRCD_nox.pdf",wd),width=5,height=5)


      A_sc_bar_SRCD_split5 = ggplot(data = A_corr_mean[(A_corr_mean$trial!="stopCorr" & A_corr_mean$group!="A_third"),], aes(x = group, y = rt_mean, group = trial, fill = group)) + geom_bar(position = "dodge", stat = "identity",colour="black") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + xlab("Group") + ylab("Mean Response Time (s)") + coord_cartesian(ylim=c(0.4,0.8)) + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))  + guides(group = FALSE, fill=FALSE) 
               ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopCorr_2go_bar_SRCD_split5.pdf",wd),width=5,height=5)


      A_sc_box_SRCD = ggplot(data = A_corr_med[(A_corr_med$trial!="stopCorr" & A_corr_med$group2!="A_third"),], aes(x = trial, y = rt_med)) + geom_boxplot(aes(fill = group2)) + xlab("Go Correct Trials") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + guides(group = FALSE, fill=FALSE) 
               ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopCorr_2go_box_SRCD.pdf",wd),width=5,height=5)


      A_sc_box_SRCD2 = ggplot(data = A_corr_med[(A_corr_med$trial!="stopCorr" & A_corr_med$group2!="A_third"),], aes(x = trial, y = rt_med)) + geom_boxplot(aes(x=group2, fill = group2)) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + guides(group = FALSE, fill=FALSE) 
               ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopCorr_2go_box_SRCD.pdf",wd),width=5,height=5)


+ geom_point(data= A_corr_med[((A_corr_med$ID =="0" | A_corr_med$ID=="1" | A_corr_med$ID == "c") & A_corr_med$trial!="stopCorr" & A_corr_med$group2!="A_third"),], aes(color=ID, size=2, shape=ID),size=4.5)+ scale_colour_manual(values=c("black","black", "black")) + scale_shape_manual(values=c(17,2,1))+ geom_line(data= A_corr_med[((A_corr_med$ID =="0" | A_corr_med$ID=="1") & A_corr_med$group2!="A_third"),], aes(group = subind, color = ID, linetype = ID))



      A_sc_bar2 = ggplot(data = A_corr_mean[(A_corr_mean$trial!="stopCorr" & A_corr_mean$group2!="A_third"),], aes(x = group2, y = rt_mean, group = trial, fill = group2)) + geom_bar(position = "dodge", stat = "identity",colour="black") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + xlab("Group") + ylab("Mean Response Time (s)") + coord_cartesian(ylim=c(0.4,0.8)) + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))  + guides(group = FALSE, fill=FALSE) 
               ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopCorr_bar_5go.pdf",wd),width=5,height=5)


      # AUSTIN REPEAT SUBS
        A_sc_line_r = ggplot(data = A_r_corr_mean[A_r_corr_mean$go!=0, ],aes(x = trial,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group), linetype = "dashed") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se, colour = group), width=.2) + geom_hline(aes(yintercept=group_mean, colour = group), position = "identity")+ xlab("Go Correct After Stop Correct") + ylab("Mean RT (s)") + ggtitle("Austin Repeat GoCorr After StopCorr")
                    ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopCorr_repeat_line.png",wd),width=10,height=10)

        A_sc_bar_r = ggplot(data = A_r_corr_mean[A_r_corr_mean$go!=0, ], aes(x = group, y = rt_mean, group = trial, fill = trial)) + geom_bar(position = "dodge", stat = "identity") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + xlab("Go Correct After Stop Correct") + ylab("Mean RT (s)") + ggtitle("Austin Repeat Go Correct After Stop Correct") + guides(group = FALSE)
               ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopCorr_repeat_bar.png",wd),width=10,height=10)
          


    # HOUSTON
      H_line = ggplot(data = H_corr_mean[H_corr_mean$go!=0, ],aes(x = go,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group))+ xlab("GoCorr After StopCorr") + ylab("Mean RT (s)") + ggtitle("Houston GoCorr After StopCorr")

      H_bar = ggplot(data = H_corr_mean[H_corr_mean$go!=0, ], aes(x = go, y = rt_mean, group = group, fill = group)) + geom_bar(position = "dodge", stat = "identity") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + xlab("Go Correct After Stop Correct") + ylab("Mean RT (s)") + ggtitle("Houston Go Correct After Stop Correct")



--------------------------------
# 2 GOs BEFORE STOP CORRECT


group=c("ldrc_[0-9]_[0-9][0-9][0-9]", "ldrc_*second", "ldrc_*third", "ldrc2_*", "ldrc_c_[0-9][0-9][0-9]")
sessions = length(group)

for (j in 1:sessions){

	if (j==1){
	err_dat=dat_all_1

	} else {

	if (j==2){
	err_dat=dat_all_2

	} else {

	if (j==3){
	err_dat=dat_all_3

	} else {

	if (j==4){
	err_dat=dat_all_4

	} else {

	if (j==5){
	err_dat=dat_all_9

	}
	}
	}
        }
        }
        
        
               


    # GO THROUGH ONE SUBJECT, ONE RUN AT A TIME
      rm("corr_b_all")
      err_dat$correct = as.character(err_dat$correct)
      subind = unique(err_dat$subind)

      for (sub in subind){
        sub_dat = err_dat[err_dat$subind==sub,]
        runnum = unique(sub_dat$runnum)

        for (run in runnum){
          run_dat = sub_dat[sub_dat$runnum==run,]
          run_dat_temp = run_dat[1,]

          if (length(run_dat)==0){
            warning(sprintf("cannot find df for %s", run))
            next
          }
 
          # GRAB FIRST 2 CORRECT GO'S(1) BEFORE A CORRECT STOP(4)
            corr_b_0 = run_dat_temp
            corr_b_1 = run_dat_temp
            corr_b_2 = run_dat_temp
            #corr_b_3 = run_dat_temp
            #corr_b_4 = run_dat_temp
            #corr_b_5 = run_dat_temp
            #corr_b_6 = run_dat_temp

            for (k in 1:length(run_dat$correct)){
              #print(k)
              if(identical(c(run_dat$correct[k],run_dat$correct[k+1],run_dat$correct[k+2]), c("1","1","4"))){
 	        tempb0 = run_dat[k,]
     	        tempb1 = run_dat[k+1,]
	        tempb2 = run_dat[k+2,]
	        #tempb3 = run_dat[k+3,]
     	        #tempb4 = run_dat[k+4,]
     	        #tempb5 = run_dat[k+5,]
     	        #tempb6 = run_dat[k+6,]

	        corr_b_0 = smartbind(corr_b_0, tempb0)
                corr_b_1 = smartbind(corr_b_1, tempb1)
	        corr_b_2 = smartbind(corr_b_2, tempb2)
	        #corr_b_3 = smartbind(corr_b_3, tempb3)	
                #corr_b_4 = smartbind(corr_b_4, tempb4)	
                #corr_b_5 = smartbind(corr_b_5, tempb5)	
                #corr_b_6 = smartbind(corr_b_6, tempb6)	
 
             }
            }


            # error column key: 0 = first go before stop, 1 = second go before stop, 2 = correct stop

            corr_b_all1 = rbind(corr_b_0, corr_b_1, corr_b_2)

            if (length(corr_b_all1$correct)>4){
              corr_b_0 = corr_b_0[-1,]
              corr_b_0$go = 0
              corr_b_1 = corr_b_1[-1,]
              corr_b_1$go = 1
              corr_b_2 = corr_b_2[-1,]
              corr_b_2$go = 2
              #corr_b_3 = corr_b_3[-1,]
              #corr_b_3$go = 3
              ##corr_b_4 = corr_b_4[-1,]
              #corr_b_4$go = 4
              #corr_b_5 = corr_b_5[-1,]
              #corr_b_5$go = 5
              #corr_b_6 = corr_b_6[-1,]
              #corr_b_6$go = 6

              corr_b_loop = rbind(corr_b_0, corr_b_1, corr_b_2)

              if (exists("corr_b_all")==FALSE){
               corr_b_all=corr_b_loop 
              }  else { 
               corr_b_all=rbind(corr_b_all, corr_b_loop)
              }          
 
             
              }  else {
                warning(sprintf("stopCorr trial does not exist for %s run%s", sub, run))
                next
              }           
        
    
        } # END RUN LOOP
      } # END SUB LOOP


       # CREATE SEPARATE DATA FRAME FOR EACH GROUP IF EXISTS
         if (length(corr_b_all1$correct)>3){
           assign(paste("corr_b_all",j,sep="_"),corr_b_all)       
         } else {
           warning(sprintf("2 goCorrs before a stopCorr trial do not exist for subjects in group %s", j))
           next
         }   

} # END GROUP LOOP   
   

corr_b_all_1$group = "A_first"
corr_b_all_2$group = "A_second"
corr_b_all_3$group = "A_third"
corr_b_all_4$group = "A_first2"
corr_b_all_5$group = "A_control"

#corr_b_all_6$group = "H_third"
#corr_b_all_7$group = "A_control"
#corr_b_all_8$group = "H_control_first"
#corr_b_all_9$group = "H_control_second"
#corr_b_all_10$group = "H_control_third"

corr_b_all2 = rbind(corr_b_all_1, corr_b_all_2, corr_b_all_3, corr_b_all_4, corr_b_all_5)



# DFs FOR CALCULATING THE MEDIAN AND MEAN ACROSS SUBJECTS AND GROUPS
  corr_b_med = ddply(corr_b_all2, .(subind, group, go), summarise, N = length(subind), rt_med = median(RT))

    A_corr_b_med = corr_b_med[grep("A_", corr_b_med$group), ]
    A_corr_b_med$ID = "c"
    A_corr_b_med$ID[A_corr_b_med$group=="A_first" | A_corr_b_med$group=="A_first2" | A_corr_b_med$group=="A_second" | A_corr_b_med$group=="A_third"] = substrLeft(A_corr_b_med$subind[A_corr_b_med$group=="A_first" | A_corr_b_med$group=="A_first2" | A_corr_b_med$group=="A_second" | A_corr_b_med$group=="A_third"], 1)
    A_corr_b_med$group2 = A_corr_b_med$group
    A_corr_b_med$group2[A_corr_b_med$group2 == "A_first2"] = "A_first"
   
    A_corr_b_subs = unique(A_corr_b_med$subind[A_corr_b_med$group=="A_first" | A_corr_b_med$group=="A_first2" | A_corr_b_med$group=="A_second" | A_corr_b_med$group=="A_control"])

    A_corr_b_med$subind[A_corr_b_med$group=="A_second"] = substrLeft(A_corr_b_med$subind[A_corr_b_med$group=="A_second"],5)
    A_corr_b_med$subind[A_corr_b_med$group=="A_third"] = substrLeft(A_corr_b_med$subind[A_corr_b_med$group=="A_third"],5)
 
    A_corr_b_med$trial_num=A_corr_b_med$N
    A_corr_b_med_trials = ddply(A_corr_b_med, .(group2, go), summarise, N=length(group2), trial_tot = sum(trial_num))

 
  # MEAN ACROSS REPEAT SUBS
    corr.sub.names2 = as.data.frame(table(A_corr_b_med$subind))
    corr.sub.names2 = as.data.frame(corr.sub.names2[corr.sub.names2$Freq>6, ])
    corr.sub.names2$Var1 = as.character(corr.sub.names2$Var1)

    A_corr_b_med[,"rep"] = "FALSE"
    A_corr_b_med$rep = A_corr_b_med$subind %in% corr.sub.names2$Var1
    A_corr_b_med$rep[A_corr_b_med$group == "A_control"] = "TRUE"

    A_r_corr_b_mean = ddply(A_corr_b_med[A_corr_b_med$rep=="TRUE",], .(group, go), summarise, N = length(go), rt_mean = mean(rt_med, na.rm=TRUE), se = sd(rt_med, na.rm=TRUE)/sqrt(N))
    A_r_corr_b_mean$trial[A_r_corr_b_mean$go == 0] = "goCorr1"
    A_r_corr_b_mean$trial[A_r_corr_b_mean$go == 1] = "goCorr2"
    A_r_corr_b_mean$trial[A_r_corr_b_mean$go == 2] = "goCorr3"
    A_r_corr_b_mean$trial[A_r_corr_b_mean$go == 3] = "goCorr4"
    A_r_corr_b_mean$trial[A_r_corr_b_mean$go == 4] = "goCorr5"
    A_r_corr_b_mean$trial[A_r_corr_b_mean$go == 5] = "stopCorr"


    A.go.sub.r6 = c()

    for (a in 1:length(A.go.sub$subind)) {

      for (b in 1:length(corr.sub.names2$Var1)){
        if (corr.sub.names2$Var1[b] == A.go.sub$subind[a]){
         A.go.sub.r6 = rbind(A.go.sub[a,], A.go.sub.r6)    
        }

    }
    }
    
    A.go.sub.r7 = rbind(A.go.sub[A.go.sub$groups=="A_control", ], A.go.sub.r6)

    A.go.group5 = ddply(A.go.sub.r7, .(groups), summarise, N = length(groups), go_rt = mean(go_rt_mean, na.rm=TRUE), rt_se = sd(go_rt_mean)/sqrt(N), go_acc = mean(go_acc_mean, na.rm=TRUE), go_error = mean(go_error_mean, na.rm=TRUE))

    A_r_corr_b_mean$group_mean[A_r_corr_b_mean$group=="A_control"] = A.go.group5$go_rt[A.go.group5$groups=="A_control"]
    A_r_corr_b_mean$group_mean[A_r_corr_b_mean$group=="A_first"] = A.go.group5$go_rt[A.go.group5$groups=="A_first"]
    A_r_corr_b_mean$group_mean[A_r_corr_b_mean$group=="A_second"] = A.go.group5$go_rt[A.go.group5$groups=="A_second"]
    A_r_corr_b_mean$group_mean[A_r_corr_b_mean$group=="A_third"] = A.go.group5$go_rt[A.go.group5$groups=="A_third"]
 
  # MEAN ACROSS ALL SUBS  
    corr_b_mean = ddply(corr_b_med, .(group, go), summarise, N = length(go), rt_mean = mean(rt_med, na.rm=TRUE), se = sd(rt_med, na.rm=TRUE)/sqrt(N))
    corr_b_mean$trial[corr_b_mean$go == 0] = "goCorr1"
    corr_b_mean$trial[corr_b_mean$go == 1] = "goCorr2"
    corr_b_mean$trial[corr_b_mean$go == 2] = "stopCorr"

  
   # ADD OVERALL RT GROUP MEANS FOR  ALL AUSTIN SUBS
     # with firsts separated
       A_corr_b_mean1 = ddply(A_corr_b_med, .(group, go), summarise, N = length(go), rt_mean = mean(rt_med, na.rm=TRUE), se = sd(rt_med, na.rm=TRUE)/sqrt(N))
     # with firsts combined
       A_corr_b_mean2 = ddply(A_corr_b_med, .(group2, go), summarise, N = length(go), rt_mean = mean(rt_med, na.rm=TRUE), se = sd(rt_med, na.rm=TRUE)/sqrt(N))

       A_corr_b_mean2$trial[A_corr_b_mean2$go == 0] = "goCorr1"
       A_corr_b_mean2$trial[A_corr_b_mean2$go == 1] = "goCorr2"
       A_corr_b_mean2$trial[A_corr_b_mean2$go == 2] = "stopCorr"
       A_corr_b_mean2$group_mean[A_corr_b_mean2$group2=="A_control"] = A.go.group2$go_rt[A.go.group2$groups2=="A_control"]
       A_corr_b_mean2$group_mean[A_corr_b_mean2$group2=="A_first"] = A.go.group2$go_rt[A.go.group2$groups2=="A_first"]
       A_corr_b_mean2$group_mean[A_corr_b_mean2$group2=="A_second"] = A.go.group2$go_rt[A.go.group2$groups2=="A_second"]
       A_corr_b_mean2$group_mean[A_corr_b_mean2$group2=="A_third"] = A.go.group2$go_rt[A.go.group2$groups2=="A_third"]

  # MEAN ACROSS ALL HOUSTON SUBS
    H_corr_b_mean = corr_b_mean[grep("H_", corr_b_mean$group), ]




  # AUSTIN STATS

    # GOCORR1
      # first vs. control (go = 0)
        sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/Before_StopCorr/go2_b_Astop_gcorr1_group.txt")
        go2.b.stop.s1.c.g1 = t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "0" & A_corr_b_med$group2 == "A_first"], A_corr_b_med$rt_med[A_corr_b_med$go == "0" & A_corr_b_med$group2 == "A_control"])
        print("S1 vs. Controls")
        print(go2.b.stop.s1.c.g1)

      # second vs. control (go = 0)
        go2.b.stop.s2.c.g1 = t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "0" & A_corr_b_med$group2 == "A_second"], A_corr_b_med$rt_med[A_corr_b_med$go == "0" & A_corr_b_med$group2 == "A_control"])
        print("S2 vs. Controls")
        print(go2.b.stop.s2.c.g1)

      # first  vs. second (go = 0)
        go2.b.stop.s1.s2.g1 = t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "0" & A_corr_b_med$group2 == "A_first"], A_corr_b_med$rt_med[A_corr_b_med$go == "0" & A_corr_b_med$group2 == "A_second"])
        print("S1 vs. S2")
        print(go2.b.stop.s1.s2.g1)
        sink()

    # GOCORR2
      # first vs. control (go = 1)
        sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/Before_StopCorr/go2_b_Astop_gcorr2_group.txt")
        go2.b.stop.s1.c.g2 = t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "1" & A_corr_b_med$group2 == "A_first"], A_corr_b_med$rt_med[A_corr_b_med$go == "1" & A_corr_b_med$group2 == "A_control"])
        print("S1 vs. Controls")
        print(go2.b.stop.s1.c.g2)

      # second vs. control (go = 1)
        go2.b.stop.s2.c.g2 = t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "1" & A_corr_b_med$group2 == "A_second"], A_corr_b_med$rt_med[A_corr_b_med$go == "1" & A_corr_b_med$group2 == "A_control"])
        print("S2 vs. Controls")
        print(go2.b.stop.s2.c.g2)

      # first  vs. second (go = 1)
        go2.b.stop.s1.s2.g2 = t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "1" & A_corr_b_med$group2 == "A_first"], A_corr_b_med$rt_med[A_corr_b_med$go == "1" & A_corr_b_med$group2 == "A_second"])
        print("S1 vs. S2")
        print(go2.b.stop.s1.s2.g2)
        sink()

    # GOCORR3
      # first vs. control (go = 2)
        t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "2" & A_corr_b_med$group == "A_first"], A_corr_b_med$rt_med[A_corr_b_med$go == "2" & A_corr_b_med$group == "A_control"])
      # second vs. control (go = 2)
        t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "2" & A_corr_b_med$group == "A_second"], A_corr_b_med$rt_med[A_corr_b_med$go == "2" & A_corr_b_med$group == "A_control"])
      # first  vs. second (go = 2)
        t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "2" & A_corr_b_med$group == "A_first"], A_corr_b_med$rt_med[A_corr_b_med$go == "2" & A_corr_b_med$group == "A_second"])

    # GOCORR4
      # first vs. control (go = 3)
        t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "3" & A_corr_b_med$group == "A_first"], A_corr_b_med$rt_med[A_corr_b_med$go == "3" & A_corr_b_med$group == "A_control"])
      # second vs. control (go = 1)
        t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "3" & A_corr_b_med$group == "A_second"], A_corr_b_med$rt_med[A_corr_b_med$go == "3" & A_corr_b_med$group == "A_control"])
      # first  vs. second (go = 1)
        t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "3" & A_corr_b_med$group == "A_first"], A_corr_b_med$rt_med[A_corr_b_med$go == "3" & A_corr_b_med$group == "A_second"])

    # GOCORR5
      # first vs. control (go = 4)
        t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "4" & A_corr_b_med$group == "A_first"], A_corr_b_med$rt_med[A_corr_b_med$go == "4" & A_corr_b_med$group == "A_control"])
      # second vs. control (go = 1)
        t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "4" & A_corr_b_med$group == "A_second"], A_corr_b_med$rt_med[A_corr_b_med$go == "4" & A_corr_b_med$group == "A_control"])
      # first  vs. second (go = 1)
        t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "4" & A_corr_b_med$group == "A_first"], A_corr_b_med$rt_med[A_corr_b_med$go == "4" & A_corr_b_med$group == "A_second"])



    # GoCorr1 vs. GoCorr2 within group

     # FIRSTS
       sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/Before_StopCorr/go2_b_Astop_g1_g2_withingroup.txt")
       go2.b.stop.s1.g1.g2 = t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "0" & A_corr_b_med$group2 == "A_first"], A_corr_b_med$rt_med[A_corr_b_med$go == "1" & A_corr_b_med$group2 == "A_first"], paired = TRUE)
       print("S1")
       print(go2.b.stop.s1.g1.g2)

     # SECONDS
       go2.b.stop.s2.g1.g2 = t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "0" & A_corr_b_med$group2 == "A_second"], A_corr_b_med$rt_med[A_corr_b_med$go == "1" & A_corr_b_med$group2 == "A_second"], paired = TRUE)
       print("S2")
       print(go2.b.stop.s2.g1.g2)

     # CONTROLS
       go2.b.stop.c.g1.g2 = t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "0" & A_corr_b_med$group2 == "A_control"], A_corr_b_med$rt_med[A_corr_b_med$go == "1" & A_corr_b_med$group2 == "A_control"], paired = TRUE)
       print("Controls")
       print(go2.b.stop.c.g1.g2)
       sink()


    # GoCorr2 vs. GoCorr3 within group

     # FIRSTS
       t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "1" & A_corr_b_med$group == "A_first"], A_corr_b_med$rt_med[A_corr_b_med$go == "2" & A_corr_b_med$group == "A_first"], paired = TRUE)
     # SECONDS
       t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "1" & A_corr_b_med$group == "A_second"], A_corr_b_med$rt_med[A_corr_b_med$go == "2" & A_corr_b_med$group == "A_second"], paired = TRUE)
     # CONTROLS
       t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "1" & A_corr_b_med$group == "A_control"], A_corr_b_med$rt_med[A_corr_b_med$go == "2" & A_corr_b_med$group == "A_control"], paired = TRUE)


    # GoCorr3 vs. GoCorr4 within group

     # FIRSTS
       t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "2" & A_corr_b_med$group == "A_first"], A_corr_b_med$rt_med[A_corr_b_med$go == "3" & A_corr_b_med$group == "A_first"], paired = TRUE)
     # SECONDS
       t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "2" & A_corr_b_med$group == "A_second"], A_corr_b_med$rt_med[A_corr_b_med$go == "3" & A_corr_b_med$group == "A_second"], paired = TRUE)
     # CONTROLS
       t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "2" & A_corr_b_med$group == "A_control"], A_corr_b_med$rt_med[A_corr_b_med$go == "3" & A_corr_b_med$group == "A_control"], paired = TRUE)


    # GoCorr4 vs. GoCorr5 within group

     # FIRSTS
       t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "3" & A_corr_b_med$group == "A_first"], A_corr_b_med$rt_med[A_corr_b_med$go == "4" & A_corr_b_med$group == "A_first"], paired = TRUE)
     # SECONDS
       t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "3" & A_corr_b_med$group == "A_second"], A_corr_b_med$rt_med[A_corr_b_med$go == "4" & A_corr_b_med$group == "A_second"], paired = TRUE)
     # CONTROLS
       t.test(A_corr_b_med$rt_med[A_corr_b_med$go == "3" & A_corr_b_med$group == "A_control"], A_corr_b_med$rt_med[A_corr_b_med$go == "4" & A_corr_b_med$group == "A_control"], paired = TRUE)

   # DIFF BETWEEN GOCORR1 & GOCORR2 WITHIN GROUP VS. GROUPS
     A_corr_b_med_diff1 = A_corr_b_med[(A_corr_b_med$group2!="A_third" & (A_corr_b_med$go == "0" | A_corr_b_med$go == "1")), ]
     A_corr_b_med_diff = ddply(A_corr_b_med_diff1, .(group2, subind), summarise, N=length(subind), med_diff = (rt_med[go=="0"])-(rt_med[go=="1"]))

     # FIRST VS. CONTROL
       sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/Before_StopCorr/go2_b_Astop_change_btwngroup.txt")
       go2.b.stop.s1.c.change = t.test(A_corr_b_med_diff$med_diff[A_corr_b_med_diff$group2=="A_first"], A_corr_b_med_diff$med_diff[A_corr_b_med_diff$group2=="A_control"])
       print("S1 vs. Controls")
       print(go2.b.stop.s1.c.change)

     # SECOND VS. CONTROL
       go2.b.stop.s2.c.change = t.test(A_corr_b_med_diff$med_diff[A_corr_b_med_diff$group2=="A_second"], A_corr_b_med_diff$med_diff[A_corr_b_med_diff$group2=="A_control"])
       print("S2 vs. Controls")
       print(go2.b.stop.s2.c.change)

     # FIRST VS. SECOND
       go2.b.stop.s1.s2.change = t.test(A_corr_b_med_diff$med_diff[A_corr_b_med_diff$group2=="A_first"], A_corr_b_med_diff$med_diff[A_corr_b_med_diff$group2=="A_second"])
       print("S1 vs.S2")
       print(go2.b.stop.s1.s2.change)
       sink()

  # FIGURES 
    # ALL
      All_line = ggplot(data = corr_mean[corr_mean$go!=1, ],aes(x = go,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group)) + xlab("Go Correct After Stop Correct") + ylab("Mean RT (s)") + ggtitle("Austin and Houston GoCorr After StopCorr")
               #ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_box.png",wd),width=10,height=10)

    # AUSTIN 
      A_sc_b_line = ggplot(data = A_corr_b_mean[A_corr_b_mean$go!=5, ],aes(x = trial,y = rt_mean)) + geom_point(aes(size=1)) + geom_line(aes(group = group, color = group), linetype = "dashed") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se, colour = group), width=.2) + geom_hline(aes(yintercept=group_mean, colour = group), position = "identity")+ xlab("Go Correct Before Stop Correct") + ylab("Mean RT (s)") + ggtitle("Austin GoCorr Before StopCorr") + guides(size = FALSE)
               ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_b_stopCorr_line.png",wd),width=10,height=10)
 
      A_sc_b_line2 = ggplot(data = A_corr_b_mean[A_corr_b_mean$go!=5 & A_corr_b_mean$group!="A_third", ],aes(x = trial,y = rt_mean)) + geom_point(aes(size=1)) + geom_line(aes(group = group, color = group), linetype = "dashed") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se, colour = group), width=.2) + geom_hline(aes(yintercept=group_mean, colour = group), position = "identity")+ xlab("Go Correct Before Stop Correct") + ylab("Mean RT (s)") + ggtitle("Austin GoCorr Before StopCorr") + guides(size = FALSE) +
                     theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), 
                           axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
               ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_b_stopCorr_line_1+2.png",wd),width=10,height=10)
      
      A_sc_b_bar_avg = ggplot(data = A.go.group,aes(x = group,y = go_rt_me, fill = group, group = group)) + geom_bar(position = "dodge", stat = "identity") + geom_errorbar(aes(y = go_rt_me, ymin = go_rt_me-go_rt_se, ymax = go_rt_me+go_rt_se), position = position_dodge(.9), width=.2) + xlab("Group") + ylab("Mean RT (s)") + ggtitle("Austin GoCorr Mean RT")

      A_sc_b_bar_SRCD = ggplot(data = A_corr_b_mean2[(A_corr_b_mean2$group2!="A_third" & A_corr_b_mean2$trial!="stopCorr") , ], aes(x = group2, y = rt_mean, group = trial, fill = group2)) + geom_bar(position = "dodge", stat = "identity", colour="black") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + coord_cartesian(ylim=c(0.4,0.8)) + xlab("Group") + ylab("Mean Response Time (s)")  + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + guides(group = FALSE, fill=FALSE)
               ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_2go_b_stopCorr_bar_SRCD.pdf",wd),width=5,height=5)

      A_sc_b_bar2 = ggplot(data = A_corr_b_mean[A_corr_b_mean$go!=5 & A_corr_b_mean$group!="A_third", ], aes(x = group, y = rt_mean, group = trial, fill = trial)) + geom_bar(position = "dodge", stat = "identity") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + xlab("Go Correct Before Stop Correct") + ylab("Mean RT (s)") + ggtitle("Austin Go Correct Before Stop Correct") + guides(group = FALSE)+ scale_y_continuous(expand = c(0,0)) +
                  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), 
                           axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
               ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_b_stopCorr_bar_1+2.png",wd),width=10,height=10)


      # AUSTIN REPEAT SUBS
        A_sc_b_line_r = ggplot(data = A_r_corr_b_mean[A_r_corr_b_mean$go!=5, ],aes(x = trial,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group), linetype = "dashed") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se, colour = group), width=.2) + geom_hline(aes(yintercept=group_mean, colour = group), position = "identity")+ xlab("Go Correct Before Stop Correct") + ylab("Mean RT (s)") + ggtitle("Austin Repeat GoCorr Before StopCorr")
                    ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_b_stopCorr_repeat_line.png",wd),width=10,height=10)

        A_sc_b_bar_r = ggplot(data = A_r_corr_b_mean[A_r_corr_b_mean$go!=5, ], aes(x = group, y = rt_mean, group = trial, fill = trial)) + geom_bar(position = "dodge", stat = "identity") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + xlab("Go Correct Before Stop Correct") + ylab("Mean RT (s)") + ggtitle("Austin Repeat Go Correct Before Stop Correct") + guides(group = FALSE)
               ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_b_stopCorr_repeat_bar.png",wd),width=10,height=10)
          


    # HOUSTON
      H_line_b = ggplot(data = H_corr_mean[H_corr_mean$go!=0, ],aes(x = go,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group))+ xlab("GoCorr After StopCorr") + ylab("Mean RT (s)") + ggtitle("Houston GoCorr After StopCorr")

      H_bar_b = ggplot(data = H_corr_mean[H_corr_mean$go!=0, ], aes(x = go, y = rt_mean, group = group, fill = group)) + geom_bar(position = "dodge", stat = "identity") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + xlab("Go Correct After Stop Correct") + ylab("Mean RT (s)") + ggtitle("Houston Go Correct After Stop Correct")




--------------------------------

# FIRST 2 GOs AFTER STOP FAIL



for (g in 1:length(group)){
  rm("group.dat")
  rm("err_all")
  group.dat = a.dat[a.dat$group==group[g],]
  group.dat$correct = as.character(group.dat$correct)
  group.dat$error = group.dat$correct
  group.dat$error[group.dat$correct=="2"] = "3"    

  subind = unique(as.character(group.dat$subind))
         
      for (sub in subind){
        sub_dat = group.dat[group.dat$subind==sub,]
        runnum = unique(sub_dat$runnum)

        for (run in runnum){
          run_dat = sub_dat[sub_dat$runnum==run,]
          err_dat_temp = run_dat[1,]

          if (length(run_dat)==0){
            warning(sprintf("cannot find df for %s", run))
            next
          }


          # GRAB FIRST 2 CORRECT GO'S(1) AFTER A STOP FAIL (2,3 = 3 in error column)
            err_0 = err_dat_temp
            err_1 = err_dat_temp
            err_2 = err_dat_temp
 
            for (r in 1:length(run_dat$error)){
              if(identical(c(run_dat$error[r],run_dat$error[r+1],run_dat$error[r+2]), c("3","1","1"))){
 	        eTemp0 = run_dat[r,]
     	        eTemp1 = run_dat[r+1,]
	        eTemp2 = run_dat[r+2,]

	        err_0 = smartbind(err_0, eTemp0)
                err_1 = smartbind(err_1, eTemp1)
	        err_2 = smartbind(err_2, eTemp2)	
 
             }
            }


            err_all1 = rbind(err_0, err_1, err_2)

            if (length(err_all1$error)>4){
              err_0 = err_0[-1,]
              err_0$go = 0
              err_1 = err_1[-1,]
              err_1$go = 1
              err_2 = err_2[-1,]
              err_2$go = 2

              err_loop = rbind(err_0, err_1, err_2)

              if (exists("err_all")==FALSE){
               err_all=err_loop 
              }  else { 
               err_all=rbind(err_all, err_loop)
              }          
 
             
              }  else {
                warning(sprintf("stopFail trial does not exist for %s run%s", sub, run))
                next
              }           

 

        } # END RUN LOOP
      } # END SUB LOOP


  # CREATE SEPARATE DATA FRAME FOR EACH GROUP
    assign(paste("err_all",g,sep="_"),err_all)    


} # END GROUP LOOP   



err_all2 = rbind(err_all_1, err_all_2, err_all_3, err_all_4, err_all_5, err_all_6) 



# DFs FOR FIGURES
  err_med = ddply(err_all2, .(subind, group, group2, ID, ID2, go), summarise, N = length(subind), rt_med = median(RT))

  A_err_med = err_med[grep("A", err_med$group), ]
  A_err_med$trial_num=A_err_med$N
  A_err_med_trials = ddply(A_err_med, .(group2, go), summarise, N=length(group2), trial_tot = sum(trial_num))
  
  # AUSTIN REPEAT SUBS
    err.sub.names = as.data.frame(table(A_err_med$subind))
    err.sub.names = err.sub.names[err.sub.names$Freq>4, ]
    err.sub.names$Var1 = as.character(err.sub.names$Var1)

    A_err_med[,"rep"] = "FALSE"
    A_err_med$rep = A_err_med$subind %in% err.sub.names$Var1
    A_err_med$rep[A_err_med$group == "A_control"] = "TRUE"

    A_r_err_mean = ddply(A_err_med[A_err_med$rep=="TRUE",], .(group, go), summarise, N = length(go), rt_mean = mean(rt_med, na.rm=TRUE), se = sd(rt_med, na.rm=TRUE)/sqrt(N))
    A_r_err_mean$trial[A_r_err_mean$go == 0] = "stopFail"
    A_r_err_mean$trial[A_r_err_mean$go == 1] = "goCorr1"
    A_r_err_mean$trial[A_r_err_mean$go == 2] = "goCorr2"
    A_r_err_mean$trial[A_r_err_mean$go == 3] = "goCorr3"
    A_r_err_mean$trial[A_r_err_mean$go == 4] = "goCorr4"
    A_r_err_mean$trial[A_r_err_mean$go == 5] = "goCorr5"

    A_r_err_mean$trial = relevel(as.factor(A_r_err_mean$trial), ref = "stopFail")


    A.go.sub.r2 = c()

    for (c in 1:length(A.go.sub$subind)) {

      for (d in 1:length(err.sub.names$Var1)){
        if (err.sub.names$Var1[d] == A.go.sub$subind[c]){
         A.go.sub.r2 = rbind(A.go.sub[c,], A.go.sub.r2)    
        }

    }
    }
    
    A.go.sub.r3 = rbind(A.go.sub.r2, A.go.sub[A.go.sub$groups=="A_control", ])

    A.go.group3 = ddply(A.go.sub.r3, .(groups), summarise, N = length(groups), go_rt = mean(go_rt_mean, na.rm=TRUE), rt_se = sd(go_rt_mean)/sqrt(N), go_acc = mean(go_acc_mean, na.rm=TRUE), go_error = mean(go_error_mean, na.rm=TRUE))

    A_r_err_mean$group_mean[A_r_err_mean$group=="A_control"] = A.go.group3$go_rt[A.go.group3$groups=="A_control"]
    A_r_err_mean$group_mean[A_r_err_mean$group=="A_first"] = A.go.group3$go_rt[A.go.group3$groups=="A_first"]
    A_r_err_mean$group_mean[A_r_err_mean$group=="A_second"] = A.go.group3$go_rt[A.go.group3$groups=="A_second"]
    A_r_err_mean$group_mean[A_r_err_mean$group=="A_third"] = A.go.group3$go_rt[A.go.group3$groups=="A_third"]
 

  # ALL AUSTIN SUBS
  err_mean = ddply(err_med, .(group, go), summarise, N = length(go), rt_mean = mean(rt_med, na.rm=TRUE), se = sd(rt_med, na.rm=TRUE)/sqrt(N))
  err_mean$trial[err_mean$go == 0] = "stopFail"
  err_mean$trial[err_mean$go == 1] = "goCorr1"
  err_mean$trial[err_mean$go == 2] = "goCorr2"
  #err_mean$trial[err_mean$go == 3] = "goCorr3"
  err_mean$group2 = err_mean$group
  err_mean$group2[err_mean$group2=="A_first2"] = "A_first"

  #Austin first combined
  A_err_mean = ddply(A_err_med, .(group2, go), summarise, N = length(go), rt_mean = mean(rt_med, na.rm=TRUE), se = sd(rt_med, na.rm=TRUE)/sqrt(N))

  A_err_mean$trial[A_err_mean$go == 0] = "stopFail"
  A_err_mean$trial[A_err_mean$go == 1] = "goCorr1"
  A_err_mean$trial[A_err_mean$go == 2] = "goCorr2"
  A_err_mean$trial = relevel(as.factor(A_err_mean$trial), ref = "stopFail")




# IDA talk


  ggplot(data = A_err_mean[A_err_mean$group2!="third", ],aes(x = trial,y = rt_mean)) + geom_point(aes(size=1, color=group2)) + geom_line(aes(group = group2, color = group2), linetype = "dashed") + geom_errorbar(data = A_err_mean[A_err_mean$group2!="third", ], aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se, color = group2), width=.2) + scale_y_continuous(limits = c(.55, .75), breaks = c(.55, .6, .65, .7, .75)) + scale_color_manual(values=c("#999999","#339900", "#3366FF")) + ylab("Mean RT (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_blank(), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))  + guides(group = FALSE, fill=FALSE, size = F) 
  ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/a_stopfail_gocorr_point.pdf",wd),width=5,height=5)


  ggplot(data = A_err_mean[A_err_mean$group2!="third",], aes(x = group2, y = rt_mean, group = trial, fill = group2)) + geom_bar(position = "dodge", stat = "identity", color = "black") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + ylab("Mean Response Time (s)") + scale_fill_manual(values=c("#999999","#339900", "#3366FF")) + coord_cartesian(ylim=c(0.4,0.8)) + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15))  + guides(group = FALSE, color = F) 
  ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/a_stopfail_gocorr_bar.pdf",wd),width=5,height=5)



   # StopFail vs. GoCorr1 within group
     # FIRSTS
       sink("/corral-repl/utexas/ldrc/SCRIPTS/figures/Project_4/SST/Error/error_stats_stopfail_goco1.txt")
       go2.fail.s1.f.g1 = t.test(A_err_med$rt_med[A_err_med$go == "0" & A_err_med$group2 == "first"], A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "first"], paired = TRUE)
       print("S1")
       print(go2.fail.s1.f.g1)
  
     # SECONDS
       go2.fail.s2.f.g1 = t.test(A_err_med$rt_med[A_err_med$go == "0" & A_err_med$group2 == "second"], A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "second"], paired = TRUE)
       print("S2")
       print(go2.fail.s2.f.g1)
  
     # CONTROLS
       go2.fail.c.f.g1 = t.test(A_err_med$rt_med[A_err_med$go == "0" & A_err_med$group2 == "control"], A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "control"], paired = TRUE)
       print("Controls")
       print(go2.fail.c.f.g1)
       sink()

   # GoCorr1 vs. GoCorr2 within group
     # FIRSTS
       sink("/corral-repl/utexas/ldrc/SCRIPTS/figures/Project_4/SST/Error/error_stats_stopfail_goco1v2.txt")
       go2.fail.s1.g1.g2 = t.test(A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "first"], A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group2 == "first"], paired = TRUE)
       print("S1")
       print(go2.fail.s1.g1.g2)
  
     # SECONDS
       go2.fail.s2.g1.g2 = t.test(A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "second"], A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group2 == "second"], paired = TRUE)
       print("S2")
       print(go2.fail.s2.g1.g2)
  
     # CONTROLS
       go2.fail.c.g1.g2 = t.test(A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "control"], A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group2 == "control"], paired = TRUE)
       print("Controls")
       print(go2.fail.c.g1.g2)
       sink()


   # DIFF BETWEEN GOCORR1 & GOCORR2 WITHIN GROUP VS. GROUPS
     A_err_med_diff1 = A_err_med[(A_err_med$group2!="third" & (A_err_med$go == "1" | A_err_med$go == "2")), ]
     A_err_med_diff = ddply(A_err_med_diff1, .(group2, subind), summarise, N=length(subind), med_diff = (rt_med[go=="2"])-(rt_med[go=="1"]))

     # FIRST VS. CONTROL
       sink("/corral-repl/utexas/ldrc/SCRIPTS/figures/Project_4/SST/Error/error_stats_stopfail_change_btwngroup.txt")
       go2.fail.s1.c.change = t.test(A_err_med_diff$med_diff[A_err_med_diff$group2=="first"], A_err_med_diff$med_diff[A_err_med_diff$group2=="control"])
       print("S1 vs. Controls")
       print(go2.fail.s1.c.change)
  
     # SECOND VS. CONTROL
       go2.fail.s2.c.change = t.test(A_err_med_diff$med_diff[A_err_med_diff$group2=="second"], A_err_med_diff$med_diff[A_err_med_diff$group2=="control"])
       print("S2 vs. Controls")
       print(go2.fail.s2.c.change)
  
     # FIRST VS. SECOND
       go2.fail.s1.s2.change = t.test(A_err_med_diff$med_diff[A_err_med_diff$group2=="first"], A_err_med_diff$med_diff[A_err_med_diff$group2=="second"])
       print("S1 vs. S2")
       print(go2.fail.s1.s2.change)
       sink()  















  # AUSTIN STATS
    # MAIN EFFECT OF GROUPS ON GOs
      # FIRST VS. CONTROL
        sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopFail/go2_Afail_main_group.txt")
        go2.fail.main.s1.c = t.test(A_err_med$rt_med[A_err_med$go!="0" & A_err_med$group2 == "A_first"], A_err_med$rt_med[A_err_med$go!="0" & A_err_med$group2 == "A_control"])
        print("S1 vs.Controls")
        print(go2.fail.main.s1.c)
  
      # SECOND VS. CONTROL
        go2.fail.main.s2.c = t.test(A_err_med$rt_med[A_err_med$go!="0" & A_err_med$group2 == "A_second"], A_err_med$rt_med[A_err_med$go!="0" & A_err_med$group2 == "A_control"])
        print("S2 vs.Controls")
        print(go2.fail.main.s2.c)
  
      # FIRST VS. SECOND
        go2.fail.main.s1.s2 = t.test(A_err_med$rt_med[A_err_med$go!="0" & A_err_med$group2 == "A_first"], A_err_med$rt_med[A_err_med$go!="0" & A_err_med$group2 == "A_second"])
        print("S1 vs. S2")
        print(go2.fail.main.s1.s2)
        sink() 
  

    # STOPFAIL
      # FIRST VS. CONTROLS
        sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopFail/go2_Afail_stopfail.txt")
        go2.fail.s1.c = t.test(A_err_med$rt_med[A_err_med$go == "0" & A_err_med$group2 == "A_first"], A_err_med$rt_med[A_err_med$go == "0" & A_err_med$group2 == "A_control"])
        print("S1 vs.Controls")
        print(go2.fail.s1.c)
  
      # SECOND VS. CONTROLS
        go2.fail.s2.c = t.test(A_err_med$rt_med[A_err_med$go == "0" & A_err_med$group2 == "A_second"], A_err_med$rt_med[A_err_med$go == "0" & A_err_med$group2 == "A_control"])
        print("S2 vs.Controls")
        print(go2.fail.s2.c)
  
      # FIRST VS. SECOND
        go2.fail.s1.s2 = t.test(A_err_med$rt_med[A_err_med$go == "0" & A_err_med$group2 == "A_first"], A_err_med$rt_med[A_err_med$go == "0" & A_err_med$group2 == "A_second"])
        print("S1 vs.S2")
        print(go2.fail.s1.s2)
        sink()

   # GOCORR1
      # FIRST VS. CONTROLS
       sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopFail/go2_Afail_gocorr1.txt")
       go2.fail.s1.c.g1 = t.test(A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_first"], A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_control"])
        print("S1 vs.Controls")
        print(go2.fail.s1.c.g1)
  
      # SECOND VS. CONTROLS
        go2.fail.s2.c.g1 = t.test(A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_second"], A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_control"])
       print("S2 vs.Controls")
        print(go2.fail.s2.c.g1)
  
      # FIRST VS. SECOND
        go2.fail.s1.s2.g1 = t.test(A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_first"], A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_second"])
        print("S1 vs.S2")
        print(go2.fail.s1.s2.g1)
        sink()


   # GOCORR2
      # FIRST VS. CONTROLS
        sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopFail/go2_Afail_gocorr2.txt")
        go2.fail.s1.c.g2 = t.test(A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group2 == "A_first"], A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group2 == "A_control"])
        print("S1 vs.Controls")
        print(go2.fail.s1.c.g2)
  
      # SECOND VS. CONTROLS
        go2.fail.s2.c.g2 = t.test(A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group2 == "A_second"], A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group2 == "A_control"])
        print("S2 vs.Controls")
        print(go2.fail.s2.c.g2)
  
      # FIRST VS. SECOND
        go2.fail.s1.s2.g2 = t.test(A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group2 == "A_first"], A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group2 == "A_second"])
        print("S1 vs.S2")
        print(go2.fail.s1.s2.g2)
        sink()


   # GOCORR3
      # FIRST VS. CONTROLS
        t.test(A_err_med$rt_med[A_err_med$go == "3" & A_err_med$group == "A_first"], A_err_med$rt_med[A_err_med$go == "3" & A_err_med$group == "A_control"])
      # SECOND VS. CONTROLS
        t.test(A_err_med$rt_med[A_err_med$go == "3" & A_err_med$group == "A_second"], A_err_med$rt_med[A_err_med$go == "3" & A_err_med$group == "A_control"])
      # FIRST VS. SECOND
        t.test(A_err_med$rt_med[A_err_med$go == "3" & A_err_med$group == "A_first"], A_err_med$rt_med[A_err_med$go == "3" & A_err_med$group == "A_second"])


   # StopFail vs. GoCorr1 within group
     # FIRSTS
       sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopFail/go2_Afail_fail_v_gocorr1_withingroup.txt")
       go2.fail.s1.f.g1 = t.test(A_err_med$rt_med[A_err_med$go == "0" & A_err_med$group2 == "A_first"], A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_first"], paired = TRUE)
       print("S1")
       print(go2.fail.s1.f.g1)
  
     # SECONDS
       go2.fail.s2.f.g1 = t.test(A_err_med$rt_med[A_err_med$go == "0" & A_err_med$group2 == "A_second"], A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_second"], paired = TRUE)
       print("S2")
       print(go2.fail.s2.f.g1)
  
     # CONTROLS
       go2.fail.c.f.g1 = t.test(A_err_med$rt_med[A_err_med$go == "0" & A_err_med$group2 == "A_control"], A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_control"], paired = TRUE)
       print("Controls")
       print(go2.fail.c.f.g1)
       sink()

   # GoCorr1 vs. GoCorr2 within group
     # FIRSTS
       sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopFail/go2_Afail_g1_v_g2_withingroup.txt")
       go2.fail.s1.g1.g2 = t.test(A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_first"], A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group2 == "A_first"], paired = TRUE)
       print("S1")
       print(go2.fail.s1.g1.g2)
  
     # SECONDS
       go2.fail.s2.g1.g2 = t.test(A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_second"], A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group2 == "A_second"], paired = TRUE)
       print("S2")
       print(go2.fail.s2.g1.g2)
  
     # CONTROLS
       go2.fail.c.g1.g2 = t.test(A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_control"], A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group2 == "A_control"], paired = TRUE)
       print("Controls")
       print(go2.fail.c.g1.g2)
       sink()

   # GoCorr2 vs. GoCorr3 within group
     # FIRSTS
       t.test(A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group == "A_first"], A_err_med$rt_med[A_err_med$go == "3" & A_err_med$group == "A_first"], paired = TRUE)
     # SECONDS
       t.test(A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group == "A_second"], A_err_med$rt_med[A_err_med$go == "3" & A_err_med$group == "A_second"], paired = TRUE)
     # CONTROLS
       t.test(A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group == "A_control"], A_err_med$rt_med[A_err_med$go == "3" & A_err_med$group == "A_control"], paired = TRUE)


   # GOCORR BEFORE STOPFAIL (go = 2) VS. AFTER STOPFAIL (go = 1)
     # FIRSTS
       t.test(A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group == "A_first"], A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group == "A_first"])
     # SECONDS
       t.test(A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group == "A_second"], A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group == "A_second"])
     # CONTROLS
       t.test(A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group == "A_control"], A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group == "A_control"])


   # DIFF BETWEEN GOCORR1 & GOCORR2 WITHIN GROUP VS. GROUPS
     A_err_med_diff1 = A_err_med[(A_err_med$group2!="A_third" & (A_err_med$go == "1" | A_err_med$go == "2")), ]
     A_err_med_diff = ddply(A_err_med_diff1, .(group2, subind), summarise, N=length(subind), med_diff = (rt_med[go=="1"])-(rt_med[go=="2"]))

     # FIRST VS. CONTROL
       sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopFail/go2_Afail_change_btwngroup.txt")
       go2.fail.s1.c.change = t.test(A_err_med_diff$med_diff[A_err_med_diff$group2=="A_first"], A_err_med_diff$med_diff[A_err_med_diff$group2=="A_control"])
       print("S1 vs. Controls")
       print(go2.fail.s1.c.change)
  
     # SECOND VS. CONTROL
       go2.fail.s2.c.change = t.test(A_err_med_diff$med_diff[A_err_med_diff$group2=="A_second"], A_err_med_diff$med_diff[A_err_med_diff$group2=="A_control"])
       print("S2 vs. Controls")
       print(go2.fail.s2.c.change)
  
     # FIRST VS. SECOND
       go2.fail.s1.s2.change = t.test(A_err_med_diff$med_diff[A_err_med_diff$group2=="A_first"], A_err_med_diff$med_diff[A_err_med_diff$group2=="A_second"])
       print("S1 vs. S2")
       print(go2.fail.s1.s2.change)
       sink()  

    # GoCorr1 vs.average GoCorr RT within group
     # FIRSTS
       sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopFail/go2_Afail_g1_v_gmean_withingroup.txt")
       go2_fail_g1_v_mean_s1 = t.test(A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_first"], A.go.sub$go_rt_mean[A.go.sub$groups2=="A_first"], paired = TRUE)
       print("Firsts")
       print(go2_fail_g1_v_mean_s1)
 
     # SECONDS
       go2_fail_g1_v_mean_s2 = t.test(A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_second"], A.go.sub$go_rt_mean[A.go.sub$groups2=="A_second"], paired = TRUE)
       print("Seconds")
       print(go2_fail_g1_v_mean_s2)
 
     # CONTROLS
       go2_fail_g1_v_mean_c = t.test(A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_control"], A.go.sub$go_rt_mean[A.go.sub$groups2=="A_control"], paired = TRUE)     
       print("Controls")
       print(go2_fail_g1_v_mean_c)
       sink()


    # GoCorr2 vs.average GoCorr RT within group
     # FIRSTS
       sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/After_StopFail/go2_Afail_g2_v_gmean_withingroup.txt")
       go2_fail_g2_v_mean_s1=t.test(A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group2 == "A_first"], A.go.sub$go_rt_mean[A.go.sub$groups2=="A_first"], paired = TRUE)
       print("Firsts")
       print(go2_fail_g2_v_mean_s1)
 
     # SECONDS
       go2_fail_g2_v_mean_s2=t.test(A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group2 == "A_second"], A.go.sub$go_rt_mean[A.go.sub$groups2=="A_second"], paired = TRUE)
       print("Seconds")
       print(go2_fail_g2_v_mean_s2)
 
     # CONTROLS
       go2_fail_g2_v_mean_c = t.test(A_err_med$rt_med[A_err_med$go == "2" & A_err_med$group2 == "A_control"], A.go.sub$go_rt_mean[A.go.sub$groups2=="A_control"], paired = TRUE)       
       print("Controls")
       print(go2_fail_g2_v_mean_c)
       sink()



# FIGURES 
    # ALL
      All_line = ggplot(data = err_mean[err_mean$go!=0, ],aes(x = go,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group)) + xlab("Go Correct After Stop Fail") + ylab("Mean RT (s)") + ggtitle("Austin and Houston GoCorr After StopFail")
               #ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_box.png",wd),width=10,height=10)

    # AUSTIN 
      #A_line = ggplot(data = A_err_mean[A_err_mean$go!=0, ],aes(x = go,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group)) + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.1) + scale_x_discrete(limit=c("1","2","3")) + xlab("Go Correct After Stop Fail") + ylab("Mean RT (s)") + ggtitle("Austin GoCorr After StopFail")
               #ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopFail_RT_line.png",wd),width=10,height=10)

      A_sf_line = ggplot(data = A_err_mean, aes(x = trial,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group), linetype = "dashed") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se, colour = group), width=.2) + geom_hline(aes(yintercept=group_mean, colour = group), position = "identity") + scale_x_discrete(limits=c("stopFail", "goCorr1", "goCorr2", "goCorr3")) + xlab("Go Correct After Stop Fail") + ylab("Mean RT (s)") + ggtitle("Austin GoCorr After StopFail")
                ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopFail_line.png",wd),width=10,height=10)

      A_sf_line2 = ggplot(data = A_err_mean[A_err_mean$group!="A_third",], aes(x = trial,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group), linetype = "dashed") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se, colour = group), width=.2) + geom_hline(aes(yintercept=group_mean, colour = group), position = "identity") + scale_x_discrete(limits=c("stopFail", "goCorr1", "goCorr2", "goCorr3")) + xlab("Go Correct After Stop Fail") + ylab("Mean RT (s)") + ggtitle("Austin GoCorr After StopFail") +
                   theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), 
                           axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
                ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopFail_line_1+2.png",wd),width=10,height=10)
         

      #A_sf_bar_avg = ggplot(data = A.go.group,aes(x = group,y = go_rt_me, fill = group, group = group)) + geom_bar(position = "dodge", stat = "identity") + geom_errorbar(aes(y = go_rt_me, ymin = go_rt_me-go_rt_se, ymax = go_rt_me+go_rt_se), position = position_dodge(.9), width=.2) + xlab("Group") + ylab("Mean RT (s)") + ggtitle("Austin GoCorr Mean RT")

      A_sf_bar = ggplot(data = A_err_mean, aes(x = group, y = rt_mean, group = trial, fill = trial)) + geom_bar(position = "dodge", stat = "identity") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + xlab("Go Correct After Stop Fail") + ylab("Mean RT (s)") + ggtitle("Austin Go Correct After Stop Fail") + guides(group = FALSE)
              ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopFail_bar.png",wd),width=10,height=10)

      A_sf_bar_SRCD = ggplot(data = A_err_mean[A_err_mean$group2!="A_third",], aes(x = group2, y = rt_mean, group = trial, fill = group2)) + geom_bar(position = "dodge", stat = "identity", colour="black") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + coord_cartesian(ylim=c(0.4,0.8))  + ylab("Mean Response Time (s)")  + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + guides(group = FALSE, fill=FALSE)
              ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopFail_bar_SRCD_nox.pdf",wd),width=5,height=5)



      # REPEAT SUBS
        A_sf_line_r = ggplot(data = A_r_err_mean, aes(x = trial,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group), linetype = "dashed") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se, colour = group), width=.2) + geom_hline(aes(yintercept=group_mean, colour = group), position = "identity") + scale_x_discrete(limits=c("stopFail", "goCorr1", "goCorr2", "goCorr3")) + xlab("Go Correct After Stop Fail") + ylab("Mean RT (s)") + ggtitle("Austin Repeat GoCorr After StopFail")
                      ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopFail_repeat_line.png",wd),width=10,height=10)

        A_sf_bar_r = ggplot(data = A_r_err_mean, aes(x = group, y = rt_mean, group = trial, fill = trial)) + geom_bar(position = "dodge", stat = "identity") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + xlab("Go Correct After Stop Fail") + ylab("Mean RT (s)") + ggtitle("Austin Repeat Go Correct After Stop Fail") + guides(group = FALSE)
                     ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopFail_repeat_bar.png",wd),width=10,height=10)


---------


# FIRST 2 GOs BEFORE STOP FAIL

sessions = length(group)

for (j in 1:sessions){

	if (j==1){
	err_dat=dat_all_1

	} else {

	if (j==2){
	err_dat=dat_all_2

	} else {

	if (j==3){
	err_dat=dat_all_3

	} else {

	if (j==4){
	err_dat=dat_all_4

	} else {

	if (j==5){
	err_dat=dat_all_9

	}
	}
	}
	}
	}
        
             

    # GO THROUGH ONE SUBJECT, ONE RUN AT A TIME
      rm("err_b_all")
      err_dat$correct = as.character(err_dat$correct)
      err_dat$error = err_dat$correct
      err_dat$error[err_dat$correct=="2"] = "3"      
      subind = unique(err_dat$subind)

      for (sub in subind){
        sub_dat = err_dat[err_dat$subind==sub,]
        runnum = unique(sub_dat$runnum)

        for (run in runnum){
          run_dat = sub_dat[sub_dat$runnum==run,]
          err_b_dat_temp = run_dat[1,]

          if (length(run_dat)==0){
            warning(sprintf("cannot find df for %s", run))
            next
          }


          # GRAB FIRST 5 CORRECT GO'S(1) AFTER A STOP FAIL (2,3 = 3 in error column)
            err_b_0 = err_b_dat_temp
            err_b_1 = err_b_dat_temp
            err_b_2 = err_b_dat_temp
            #err_b_3 = err_b_dat_temp
            #err_b_4 = err_b_dat_temp
            #err_b_5 = err_b_dat_temp
            #err_b_6 = err_b_dat_temp

            for (r in 1:length(run_dat$error)){
              #print(k)
              if(identical(c(run_dat$error[r],run_dat$error[r+1],run_dat$error[r+2]), c("1","1","3"))){
 	        ebTemp0 = run_dat[r,]
     	        ebTemp1 = run_dat[r+1,]
	        ebTemp2 = run_dat[r+2,]
	        #ebTemp3 = run_dat[r+3,]
     	        #ebTemp4 = run_dat[r+4,]
     	        #ebTemp5 = run_dat[r+5,]
     	        #ebTemp6 = run_dat[r+6,]

	        err_b_0 = smartbind(err_b_0, ebTemp0)
                err_b_1 = smartbind(err_b_1, ebTemp1)
	        err_b_2 = smartbind(err_b_2, ebTemp2)
	        #err_b_3 = smartbind(err_b_3, ebTemp3)	
                #err_b_4 = smartbind(err_b_4, ebTemp4)	
                #err_b_5 = smartbind(err_b_5, ebTemp5)	
                #err_b_6 = smartbind(err_b_6, ebTemp6)	
 
             }
            }


            err_b_all1 = rbind(err_b_0, err_b_1, err_b_2)

            if (length(err_b_all1$error)>4){
              err_b_0 = err_b_0[-1,]
              err_b_0$go = 0
              err_b_1 = err_b_1[-1,]
              err_b_1$go = 1
              err_b_2 = err_b_2[-1,]
              err_b_2$go = 2
              #err_b_3 = err_b_3[-1,]
              #err_b_3$go = 3
              #err_b_4 = err_b_4[-1,]
              #err_b_4$go = 4
              #err_b_5 = err_b_5[-1,]
              #err_b_5$go = 5
              #err_b_6 = err_b_6[-1,]
              #err_b_6$go = 6

              err_b_loop = rbind(err_b_0, err_b_1, err_b_2)

              if (exists("err_b_all")==FALSE){
               err_b_all=err_b_loop 
              }  else { 
               err_b_all=rbind(err_b_all, err_b_loop)
              }          
 
             
              }  else {
                warning(sprintf("stopFail trial does not exist for %s run%s", sub, run))
                next
              }           

 

        } # END RUN LOOP
      } # END SUB LOOP


  # CREATE SEPARATE DATA FRAME FOR EACH GROUP
    assign(paste("err_b_all",j,sep="_"),err_b_all)    


} # END GROUP LOOP   



err_b_all_1$group = "A_first"
err_b_all_2$group = "A_second"
err_b_all_3$group = "A_third"
err_b_all_4$group = "A_first2"
err_b_all_5$group = "A_control"

#err_b_all_6$group = "H_third"
#err_b_all_7$group = "A_control"
#err_b_all_8$group = "H_control_first"
#err_b_all_9$group = "H_control_second"
#err_b_all_10$group = "H_control_third"

err_b_all2 = rbind(err_b_all_1, err_b_all_2, err_b_all_3, err_b_all_4, err_b_all_5) #err_b_all_6, err_b_all_7, err_b_all_8, err_b_all_9, err_b_all_10)



# DFs FOR CALCULATING THE MEDIAN AND MEAN ACROSS SUBJECTS AND GROUPS
  err_b_med = ddply(err_b_all2, .(subind, group, go), summarise, N = length(subind), rt_med = median(RT))
  A_err_b_med = err_b_med[grep("A_", err_b_med$group), ]
  A_err_b_subs = unique(A_err_b_med$subind[A_err_b_med$group=="A_first" | A_err_b_med$group=="A_second" | A_err_b_med$group=="A_control"])

  A_err_b_med$group2 = A_err_b_med$group
  A_err_b_med$group2[A_err_b_med$group2=="A_first2"] = "A_first"
  A_err_b_med$subind[A_err_b_med$group=="A_second"] = substrLeft(A_err_b_med$subind[A_err_b_med$group=="A_second"],5)
  A_err_b_med$subind[A_err_b_med$group=="A_third"] = substrLeft(A_err_b_med$subind[A_err_b_med$group=="A_third"],5)

   A_err_b_med$trial_num=A_err_b_med$N
   A_err_b_med_trials = ddply(A_err_b_med, .(group2, go), summarise, N=length(group2), trial_tot = sum(trial_num))


  # AUSTIN REPEAT SUBS
    err.sub.names4 = as.data.frame(table(A_err_b_med$subind))
    err.sub.names4 = err.sub.names4[err.sub.names4$Freq>4, ]

    A_err_b_med[,"rep"] = "FALSE"
    A_err_b_med$rep = A_err_b_med$subind %in% err.sub.names4$Var1
    A_err_b_med$rep[A_err_b_med$group == "A_control"] = "TRUE"

    A_r_err_b_mean = ddply(A_err_b_med[A_err_b_med$rep=="TRUE",], .(group, go), summarise, N = length(go), rt_mean = mean(rt_med, na.rm=TRUE), se = sd(rt_med, na.rm=TRUE)/sqrt(N))
    A_r_err_b_mean$trial[A_r_err_b_mean$go == 0] = "stopFail"
    A_r_err_b_mean$trial[A_r_err_b_mean$go == 1] = "goCorr1"
    A_r_err_b_mean$trial[A_r_err_b_mean$go == 2] = "goCorr2"
    A_r_err_b_mean$trial[A_r_err_b_mean$go == 3] = "goCorr3"
    A_r_err_b_mean$trial[A_r_err_b_mean$go == 4] = "goCorr4"
    A_r_err_b_mean$trial[A_r_err_b_mean$go == 5] = "goCorr5"

    A.go.sub.r4 = c()

    for (c in 1:length(A.go.sub$subind)) {

      for (d in 1:length(err.sub.names4$Var1)){
        if (err.sub.names4$Var1[d] == A.go.sub$subind[c]){
         A.go.sub.r4 = rbind(A.go.sub[c,], A.go.sub.r4)    
        }

    }
    }
    
    A.go.sub.r5 = rbind(A.go.sub.r4, A.go.sub[A.go.sub$groups=="A_control", ])

    A.go.group4 = ddply(A.go.sub.r5, .(groups), summarise, N = length(groups), go_rt = mean(go_rt_mean, na.rm=TRUE), rt_se = sd(go_rt_mean)/sqrt(N), go_acc = mean(go_acc_mean, na.rm=TRUE), go_error = mean(go_error_mean, na.rm=TRUE))

    A_r_err_b_mean$group_mean[A_r_err_b_mean$group=="A_control"] = A.go.group4$go_rt[A.go.group4$groups=="A_control"]
    A_r_err_b_mean$group_mean[A_r_err_b_mean$group=="A_first"] = A.go.group4$go_rt[A.go.group4$groups=="A_first"]
    A_r_err_b_mean$group_mean[A_r_err_b_mean$group=="A_second"] = A.go.group4$go_rt[A.go.group4$groups=="A_second"]
    A_r_err_b_mean$group_mean[A_r_err_b_mean$group=="A_third"] = A.go.group4$go_rt[A.go.group4$groups=="A_third"]

  # ALL AUSTIN SUBS
  err_b_mean = ddply(err_b_med, .(group, go), summarise, N = length(go), rt_mean = mean(rt_med, na.rm=TRUE), se = sd(rt_med, na.rm=TRUE)/sqrt(N))
  err_b_mean$trial[err_b_mean$go == 0] = "goCorr1"
  err_b_mean$trial[err_b_mean$go == 1] = "goCorr2"
  err_b_mean$trial[err_b_mean$go == 2] = "goCorr3"
  err_b_mean$trial[err_b_mean$go == 3] = "stopFail"

 # Austin with firsts combined
  A_err_b_mean = ddply(A_err_b_med, .(group2, go), summarise, N = length(go), rt_mean = mean(rt_med, na.rm=TRUE), se = sd(rt_med, na.rm=TRUE)/sqrt(N))

  A_err_b_mean$trial[A_err_b_mean$go == 0] = "goCorr1"
  A_err_b_mean$trial[A_err_b_mean$go == 1] = "goCorr2"
  A_err_b_mean$trial[A_err_b_mean$go == 2] = "stopFail"
  A_err_b_mean$group_mean[A_err_b_mean$group2=="A_control"] = A.go.group2$go_rt[A.go.group2$groups2=="A_control"]
  A_err_b_mean$group_mean[A_err_b_mean$group2=="A_first"] = A.go.group2$go_rt[A.go.group2$groups2=="A_first"]
  A_err_b_mean$group_mean[A_err_b_mean$group2=="A_second"] = A.go.group2$go_rt[A.go.group2$groups2=="A_second"]
  A_err_b_mean$group_mean[A_err_b_mean$group2=="A_third"] = A.go.group2$go_rt[A.go.group2$groups2=="A_third"]


  # AUSTIN STATS

    # GOCORR1
      # FIRST VS. CONTROLS
        sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/Before_StopFail/go2_b_fail_g1_group.txt")
        go2_bfail_s1_c_g1 = t.test(A_err_b_med$rt_med[A_err_b_med$go == "0" & A_err_b_med$group2 == "A_first"], A_err_b_med$rt_med[A_err_b_med$go == "0" & A_err_b_med$group2 == "A_control"])
        print("S1 vs. Controls")
        print(go2_bfail_s1_c_g1)

      # SECOND VS. CONTROLS
        go2_bfail_s2_c_g1 = t.test(A_err_b_med$rt_med[A_err_b_med$go == "0" & A_err_b_med$group2 == "A_second"], A_err_b_med$rt_med[A_err_b_med$go == "0" & A_err_b_med$group2 == "A_control"])
        print("S2 vs. Controls")
        print(go2_bfail_s2_c_g1)

      # FIRST VS. SECOND
        go2_bfail_s1_s2_g1 = t.test(A_err_b_med$rt_med[A_err_b_med$go == "0" & A_err_b_med$group2 == "A_first"], A_err_b_med$rt_med[A_err_b_med$go == "0" & A_err_b_med$group2 == "A_second"])
        print("S1 vs. S2")
        print(go2_bfail_s1_s2_g1)
        sink()

   # GOCORR2
      # FIRST VS. CONTROLS
        sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/Before_StopFail/go2_b_fail_g2_group.txt")
        go2_bfail_s1_c_g2 = t.test(A_err_b_med$rt_med[A_err_b_med$go == "1" & A_err_b_med$group2 == "A_first"], A_err_b_med$rt_med[A_err_b_med$go == "1" & A_err_b_med$group2 == "A_control"])
        print("S1 vs. Controls")
        print(go2_bfail_s1_c_g2)

      # SECOND VS. CONTROLS
        go2_bfail_s2_c_g2 = t.test(A_err_b_med$rt_med[A_err_b_med$go == "1" & A_err_b_med$group2 == "A_second"], A_err_b_med$rt_med[A_err_b_med$go == "1" & A_err_b_med$group2 == "A_control"])
        print("S2 vs. Controls")
        print(go2_bfail_s2_c_g2)

      # FIRST VS. SECOND
        go2_bfail_s1_s2_g2 = t.test(A_err_b_med$rt_med[A_err_b_med$go == "1" & A_err_b_med$group2 == "A_first"], A_err_b_med$rt_med[A_err_b_med$go == "1" & A_err_b_med$group2 == "A_second"])
        print("S1 vs. S2")
        print(go2_bfail_s1_s2_g2)
        sink()

   # GOCORR3
      # FIRST VS. CONTROLS
        t.test(A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group == "A_first"], A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group == "A_control"])
      # SECOND VS. CONTROLS
        t.test(A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group == "A_second"], A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group == "A_control"])
      # FIRST VS. SECOND
        t.test(A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group == "A_first"], A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group == "A_second"])

   # STOPFAIL
      # FIRST VS. CONTROLS
        sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/Before_StopFail/go2_b_fail_f_group.txt")
        go2_bfail_s1_c_f = t.test(A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group2 == "A_first"], A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group2 == "A_control"])
        print("S1 vs. Controls")
        print(go2_bfail_s1_c_f)

      # SECOND VS. CONTROLS
        go2_bfail_s2_c_f = t.test(A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group2 == "A_second"], A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group2 == "A_control"])
        print("S2 vs. Controls")
        print(go2_bfail_s2_c_f)

      # FIRST VS. SECOND
        go2_bfail_s1_s2_f = t.test(A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group2 == "A_first"], A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group2 == "A_second"])
        print("S1 vs. S2")
        print(go2_bfail_s1_s2_f)
        sink()


   # GoCorr1 vs. GoCorr2 within group
     # FIRSTS
       sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/Before_StopFail/go2_b_fail_g1_v_g2.txt")
       go2_bfail_s1_g1_g2 = t.test(A_err_b_med$rt_med[A_err_b_med$go == "0" & A_err_b_med$group2 == "A_first"], A_err_b_med$rt_med[A_err_b_med$go == "1" & A_err_b_med$group2 == "A_first"], paired = TRUE)
        print("S1")
        print(go2_bfail_s1_g1_g2)

     # SECONDS
       go2_bfail_s2_g1_g2 = t.test(A_err_b_med$rt_med[A_err_b_med$go == "0" & A_err_b_med$group2 == "A_second"], A_err_b_med$rt_med[A_err_b_med$go == "1" & A_err_b_med$group2 == "A_second"], paired = TRUE)
        print("S2")
        print(go2_bfail_s2_g1_g2)

     # CONTROLS
        go2_bfail_c_g1_g2 = t.test(A_err_b_med$rt_med[A_err_b_med$go == "0" & A_err_b_med$group2 == "A_control"], A_err_b_med$rt_med[A_err_b_med$go == "1" & A_err_b_med$group2 == "A_control"], paired = TRUE)
        print("Controls")
        print(go2_bfail_c_g1_g2)
        sink()


   # GoCorr2 vs. GoCorr3 within group
     # FIRSTS
       t.test(A_err_b_med$rt_med[A_err_b_med$go == "1" & A_err_b_med$group == "A_first"], A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group == "A_first"], paired = TRUE)
     # SECONDS
       t.test(A_err_b_med$rt_med[A_err_b_med$go == "1" & A_err_b_med$group == "A_second"], A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group == "A_second"], paired = TRUE)
     # CONTROLS
       t.test(A_err_b_med$rt_med[A_err_b_med$go == "1" & A_err_b_med$group == "A_control"], A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group == "A_control"], paired = TRUE)

   # GoCorr1 vs. StopFail within group
     # FIRSTS
       sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/Before_StopFail/go2_b_fail_f_v_g1.txt")
        go2_bfail_s1_f_g1 = t.test(A_err_b_med$rt_med[A_err_b_med$go == "1" & A_err_b_med$group2 == "A_first"], A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group2 == "A_first"], paired = TRUE)
        print("S1")
        print(go2_bfail_s1_f_g1)

     # SECONDS
       go2_bfail_s2_f_g1 = t.test(A_err_b_med$rt_med[A_err_b_med$go == "1" & A_err_b_med$group2 == "A_second"], A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group2 == "A_second"], paired = TRUE)
        print("S2")
        print(go2_bfail_s2_f_g1)

     # CONTROLS
       go2_bfail_c_f_g1 = t.test(A_err_b_med$rt_med[A_err_b_med$go == "1" & A_err_b_med$group2 == "A_control"], A_err_b_med$rt_med[A_err_b_med$go == "2" & A_err_b_med$group2 == "A_control"], paired = TRUE)
        print("Controls")
        print(go2_bfail_c_f_g1)
        sink()


   # DIFF BETWEEN GOCORR1 & GOCORR2 WITHIN GROUP VS. GROUPS
     A_err_b_med_diff1 = A_err_b_med[(A_err_b_med$group2!="A_third" & (A_err_b_med$go == "0" | A_err_med$go == "1")), ]
     A_err_b_med_diff = ddply(A_err_b_med_diff1, .(group2, subind), summarise, N=length(subind), med_diff = (rt_med[go=="0"])-(rt_med[go=="1"]))

     # FIRST VS. CONTROL
       sink("/corral-repl/utexas/ldrc/SCRIPTS/Stats/SST/Error/Before_StopFail/go2_b_fail_g1_g2_change_btwngroup.txt")
        go2_bfail_s1_c_g1g2 = t.test(A_err_b_med_diff$med_diff[A_err_b_med_diff$group2=="A_first"], A_err_b_med_diff$med_diff[A_err_b_med_diff$group2=="A_control"])
        print("S1 vs. Controls")
        print(go2_bfail_s1_c_g1g2)

     # SECOND VS. CONTROL
       go2_bfail_s2_c_g1g2 = t.test(A_err_b_med_diff$med_diff[A_err_b_med_diff$group2=="A_second"], A_err_b_med_diff$med_diff[A_err_b_med_diff$group2=="A_control"])
        print("S2 vs. Controls")
        print(go2_bfail_s2_c_g1g2)

     # FIRST VS. SECOND
       go2_bfail_s1_s2_g1g2 = t.test(A_err_b_med_diff$med_diff[A_err_b_med_diff$group2=="A_first"], A_err_b_med_diff$med_diff[A_err_b_med_diff$group2=="A_second"])
        print("S1 vs. S2")
        print(go2_bfail_s1_s2_g1g2)
        sink()



  # FIGURES 
    # ALL
      #All_line = ggplot(data = err_b_mean[err_b_mean$go!=0, ],aes(x = go,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group)) + xlab("Go Correct After Stop Fail") + ylab("Mean RT (s)") + ggtitle("Austin and Houston GoCorr After StopFail")
               #ggsave(filename=sprintf("%s/figures/Project_4/SST/RT/A_goRT_box.png",wd),width=10,height=10)

    # AUSTIN 
      #A_line = ggplot(data = A_err_mean[A_err_mean$go!=0, ],aes(x = go,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group)) + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.1) + scale_x_discrete(limit=c("1","2","3")) + xlab("Go Correct After Stop Fail") + ylab("Mean RT (s)") + ggtitle("Austin GoCorr After StopFail")
               #ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopFail_RT_line.png",wd),width=10,height=10)

      A_line_b = ggplot(data = A_err_b_mean, aes(x = trial,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group), linetype = "dashed") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se, colour = group), width=.2) + geom_hline(aes(yintercept=group_mean, colour = group), position = "identity") + scale_x_discrete(limits=c("goCorr1", "goCorr2", "goCorr3", "stopFail")) + xlab("Go Correct Before Stop Fail") + ylab("Mean RT (s)") + ggtitle("Austin GoCorr Before StopFail")
                ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_b_stopFail_line.png",wd),width=10,height=10)

      A_line_b2 = ggplot(data = A_err_b_mean[A_err_b_mean$group!="A_third",], aes(x = trial,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group), linetype = "dashed") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se, colour = group), width=.2) + geom_hline(aes(yintercept=group_mean, colour = group), position = "identity") + scale_x_discrete(limits=c("goCorr1", "goCorr2", "goCorr3", "stopFail")) + xlab("Go Correct Before Stop Fail") + ylab("Mean RT (s)") + ggtitle("Austin GoCorr Before StopFail") +
                  theme(legend.text=element_text(size=14), legend.title = element_text(size = 14), axis.text.y = element_text(size = 12, color = 'black'), axis.text.x = element_text(size = 12, angle=0, vjust=.7, color = 'black'), 
                           axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
                ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_b_stopFail_line_1+2.png",wd),width=10,height=10)
          

      #A_bar_avg = ggplot(data = A.go.group,aes(x = group,y = go_rt_me, fill = group, group = group)) + geom_bar(position = "dodge", stat = "identity") + geom_errorbar(aes(y = go_rt_me, ymin = go_rt_me-go_rt_se, ymax = go_rt_me+go_rt_se), position = position_dodge(.9), width=.2) + xlab("Group") + ylab("Mean RT (s)") + ggtitle("Austin GoCorr Mean RT")

      A_bar_b = ggplot(data = A_err_b_mean, aes(x = group, y = rt_mean, group = trial, fill = trial)) + geom_bar(position = "dodge", stat = "identity") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + xlab("Go Correct Before Stop Fail") + ylab("Mean RT (s)") + ggtitle("Austin Go Correct Before Stop Fail") + guides(group = FALSE)
              ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_b_stopFail_bar.png",wd),width=10,height=10)

      A_bar_b_SRCD = ggplot(data = A_err_b_mean[A_err_b_mean$group2!="A_third",], aes(x = group2, y = rt_mean, group = trial, fill = group2)) + geom_bar(position = "dodge", stat = "identity", colour="black") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + coord_cartesian(ylim=c(0.4,0.8)) + xlab("Group") + ylab("Mean Response Time (s)") + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + guides(group = FALSE, fill=FALSE)
              ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_b_stopFail_bar_SRCD.pdf",wd),width=5,height=5)


      # AUSTIN REPEAT

        A_line_b_r = ggplot(data = A_r_err_b_mean, aes(x = trial,y = rt_mean)) + geom_point() + geom_line(aes(group = group, color = group), linetype = "dashed") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se, colour = group), width=.2) + geom_hline(aes(yintercept=group_mean, colour = group), position = "identity") + scale_x_discrete(limits=c("goCorr1", "goCorr2", "goCorr3", "stopFail")) + xlab("Go Correct Before Stop Fail") + ylab("Mean RT (s)") + ggtitle("Austin Repeat GoCorr Before StopFail")
                   ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_b_stopFail_repeat_line.png",wd),width=10,height=10)

        A_bar_b_r = ggplot(data = A_r_err_b_mean, aes(x = group, y = rt_mean, group = trial, fill = trial)) + geom_bar(position = "dodge", stat = "identity") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + xlab("Go Correct Before Stop Fail") + ylab("Mean RT (s)") + ggtitle("Austin Repeat Go Correct Before Stop Fail") + guides(group = FALSE)
                  ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_b_stopFail_repeat_bar.png",wd),width=10,height=10)










# GOCORR1 AFTER STOPCORR VS. AFTER STOPFAIL

   # Means within group
     # FIRSTS
       t.test(A_corr_med$rt_med[A_corr_med$go == "1" & A_corr_med$group2 == "A_first"], A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_first"], paired = TRUE)
     # SECONDS
       t.test(A_corr_med$rt_med[A_corr_med$go == "1" & A_corr_med$group2 == "A_second"], A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_second"], paired = TRUE)
     # CONTROLS
       t.test(A_corr_med$rt_med[A_corr_med$go == "1" & A_corr_med$group2 == "A_control"], A_err_med$rt_med[A_err_med$go == "1" & A_err_med$group2 == "A_control"], paired = TRUE)

   # change between two gocorrs within group
     A_corr_diff1 = A_corr_med[A_corr_med$trial!="stopCorr",]
     A_err_diff1 = A_err_med[A_err_med$trial!="stopFail",]
     A_corr_diff1$err_rt_med = A_err_diff1$rt_med[A_err_diff1$subind==A_corr_diff1$subind]

     A_corr_diff = ddply(A_corr_diff1, .(group2, subind), summarise, N=length(subind), med_diff = (rt_med[go=="1"])-(err_rt_med[go=="1"]))
 
     # FIRST VS. CONTROL
       t.test(A_corr_diff$med_diff[A_corr_diff$group2=="A_first"], A_corr_diff$med_diff[A_corr_diff$group2=="A_control"])

     # SECOND VS. CONTROL
       t.test(A_corr_diff$med_diff[A_corr_diff$group2=="A_second"], A_corr_diff$med_diff[A_corr_diff$group2=="A_control"])

     # FIRST VS. SECOND
       t.test(A_corr_diff$med_diff[A_corr_diff$group2=="A_first"], A_corr_diff$med_diff[A_corr_diff$group2=="A_second"])


 # GRAPH


      A_corr_diff_mean = ddply(A_corr_diff1, .(group2, go), summarise, N = length(go), corr_rt_mean = mean(rt_med, na.rm=TRUE), corr_se = sd(rt_med, na.rm=TRUE)/sqrt(N), err_rt_mean = mean(err_rt_med, na.rm=TRUE), err_se = sd(err_rt_med, na.rm=TRUE)/sqrt(N))

      A_corr_bar_SRCD = ggplot(data = A_corr_diff_mean[A_corr_diff_mean$group2!="A_third",], aes(x = group2, y = rt_mean, group = trial, fill = group2)) + geom_bar(position = "dodge", stat = "identity", colour="black") + geom_errorbar(aes(y = rt_mean, ymin = rt_mean-se, ymax = rt_mean+se), position = position_dodge(.9), width=.2) + coord_cartesian(ylim=c(0.4,0.8)) + xlab("Group") + ylab("Mean Response Time (s)")  + theme_classic() + theme(axis.title.y = element_text(size = rel(2.0), vjust=0.4), axis.title.x = element_text(size = rel(2.0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_text(size = 15)) + guides(group = FALSE, fill=FALSE)
              ggsave(filename=sprintf("%s/figures/Project_4/SST/Error/A_stopFail_bar_SRCD.pdf",wd),width=5,height=5)
