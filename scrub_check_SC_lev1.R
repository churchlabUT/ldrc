# Grabs each out_confound.txt file from scrubbing output in BOLD folders
# Mary Abbe Roe Sept. 2014


# LIBRARIES
  library(plyr)

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

# READ IN DATA

  group = c("ldrc_[0-9]_[0-9][0-9][0-9]", "ldrc_*second", "ldrc_*third", "ldrc2_*_[0-9][0-9][0-9]", "H_LD*_[0-9]_second", "H_LD*_[0-9]_third", "LDFHO*","ldrc_c_[0-9][0-9][0-9]", "H_LD*_c_second", "H_LD*_c_third")
  chars = c(5,12,11,13,12,5,13,12)

  # GROUP
  for (i in 1:length(group)){ 
    sub.dirs = Sys.glob(sprintf("/corral-repl/utexas/ldrc/%s" ,group[i]))
    rm("scrub.all")
    rm("scrub.all.9")
    rm("out.scrub.all")
    print(group[i])

    # SUB
    for (sub in sub.dirs){
      bold.dirs = c(Sys.glob(sprintf("%s/BOLD/*SC*", sub)))
      subnum = substrRight(sub, chars[i])

	# RUN
        for (run in bold.dirs){
          run.num = substrRight(run,1)
          scrub.file = Sys.glob(sprintf("%s/scrub_files/fd_series.txt", run))

          if (length(scrub.file)==0){
            warning(sprintf("out_confound file for %s does not exist", run))
	    next
          }

          scrub.loop = read.table(scrub.file, sep="\t", na.strings="NaN")
          colnames(scrub.loop) = c("FD")
          scrub.loop$subind = rep(subnum, dim(scrub.loop)[1])
          scrub.loop$runnum = rep(run.num, dim(scrub.loop)[1])

          scrub.loop.9 = scrub.loop[scrub.loop$FD>=.9,]

          outliers = length(scrub.loop.9$FD)
          out.scrub = data.frame(outliers)
          out.scrub$subind = subnum
          out.scrub$runnum = run.num
          out.scrub = out.scrub[,c(2,3,1)]
   
          # CREATE SCRUB.ALL IF DOESN'T EXIST
          if (exists("scrub.all")==FALSE){
            scrub.all = scrub.loop 
          } 
          else{ 
            scrub.all = rbind(scrub.all, scrub.loop)
          }  

          # CREATE SCRUB.ALL.9 IF DOESN'T EXIST
          if (exists("scrub.all.9")==FALSE){
            scrub.all.9 = scrub.loop.9 
          } 
          else{ 
            scrub.all.9 = rbind(scrub.all.9, scrub.loop.9)
          }  

          # CREATE OUT.SCRUB.ALL IF DOESN'T EXIST
          if (exists("out.scrub.all")==FALSE){
            out.scrub.all = out.scrub 
          } 
          else{ 
            out.scrub.all = rbind(out.scrub.all, out.scrub)
          }  

     
       } # END RUN LOOP
     }  # END SUB LOOP

    # ADD GROUP IDENTIFIER
      scrub.all$group= i
      scrub.all.9$group = i
      out.scrub.all$group = i

    # CREATE SEPARATE DATA FRAME FOR EACH GROUP
      assign(paste("scrub.all",i,sep="_"),scrub.all)
      assign(paste("scrub.all.9",i,sep="_"),scrub.all.9)
      assign(paste("out.scrub.all",i,sep="_"),out.scrub.all)



  } # END GROUP LOOP


  out.scrub.all_1 = out.scrub.all_1[order(out.scrub.all_1$subind,out.scrub.all_1$runnum),]
  out.scrub.all_2 = out.scrub.all_2[order(out.scrub.all_2$subind,out.scrub.all_2$runnum),]
  out.scrub.all_3 = out.scrub.all_3[order(out.scrub.all_3$subind,out.scrub.all_3$runnum),]
  out.scrub.all_4 = out.scrub.all_4[order(out.scrub.all_4$subind,out.scrub.all_4$runnum),]
  out.scrub.all_5 = out.scrub.all_5[order(out.scrub.all_5$subind,out.scrub.all_5$runnum),]
  out.scrub.all_6 = out.scrub.all_6[order(out.scrub.all_6$subind,out.scrub.all_6$runnum),]
  out.scrub.all_7 = out.scrub.all_7[order(out.scrub.all_7$subind,out.scrub.all_7$runnum),]
  out.scrub.all_8 = out.scrub.all_8[order(out.scrub.all_8$subind,out.scrub.all_8$runnum),]



# SAVE ALL DATA FRAMES TO CSVs

    write.csv(out.scrub.all_1[,1:3], file=sprintf("%s/lev1_QC/SC/A_out_scrub_1.csv", wd), na="NA", row.names=FALSE)
    write.csv(out.scrub.all_2[,1:3], file=sprintf("%s/lev1_QC/SC/A_out_scrub_2.csv", wd), na="NA", row.names=FALSE)
    write.csv(out.scrub.all_3[,1:3], file=sprintf("%s/lev1_QC/SC/A_out_scrub_3.csv", wd), na="NA", row.names=FALSE)
    write.csv(out.scrub.all_4[,1:3], file=sprintf("%s/lev1_QC/SC/H_out_scrub_2.csv", wd), na="NA", row.names=FALSE)
    write.csv(out.scrub.all_5[,1:3], file=sprintf("%s/lev1_QC/SC/H_out_scrub_3.csv", wd), na="NA", row.names=FALSE)
    write.csv(out.scrub.all_6[,1:3], file=sprintf("%s/lev1_QC/SC/A_out_scrub_c.csv", wd), na="NA", row.names=FALSE)
    write.csv(out.scrub.all_7[,1:3], file=sprintf("%s/lev1_QC/SC/H_out_scrub_c_2.csv", wd), na="NA", row.names=FALSE)
    write.csv(out.scrub.all_8[,1:3], file=sprintf("%s/lev1_QC/SC/H_out_scrub_c_3.csv", wd), na="NA", row.names=FALSE)
