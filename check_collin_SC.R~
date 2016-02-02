# check the collinearity of SC using VIF (variance inflation factor)
# Jeanette & Mary Abbe, April 2014

# library with VIF function
  library(HH)

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
  group = c("ldrc_[0-9]_[0-9][0-9][0-9]", "ldrc_*second", "ldrc_*third", "ldrc2_[0-9]_[0-9][0-9][0-9]", "ldrc2_*second", "ldrc3_[0-2]_[0-9][0-9][0-9]*", "ldrc_c_[0-9][0-9][0-9]", "ldrc3_c_[0-9][0-9][0-9]*", "H_LD*_[0-9]_second", "H_LD*_[0-9]_third","H_LD*_c_second","H_LD*_c_third","LDFHO*_[0-2]", "LDFHO*_second", "LDFHO2*_1_3")
  chars = c(5,12,11,11,17,11,5,5,13,17,13,12,7,17,14)

  for (i in 1:length(group)){ 
    subdirs = Sys.glob(sprintf("/corral-repl/utexas/ldrc/%s" ,group[i]))
    rm("dat.all")
    
    for (sub in subdirs){
      model.dirs = Sys.glob(sprintf("%s/model/SC/Run?.feat", sub))
      subnum = substrRight(sub, chars[i])

        for (run in model.dirs){
          des.file = Sys.glob(sprintf("%s/design.mat", run))
          runnum1 = substrRight(run, 9)
          runnum = substrLeft(runnum1, 4)
          print(des.file)

          if (length(des.file)==0){
            warning(sprintf("cannot find design.mat file for %s", run))
	    next
          }

          # read in design matrix
            des.mat = as.matrix(read.table(des.file, skip = 5))
            dim.desmat = dim(des.mat)

          # create fake data and save VIF in loop
            y.fake = c(1:dim(des.mat)[1])
            mod = lm(y.fake ~ des.mat )
            dat = vif(mod)
            dat.loop = as.data.frame(as.table(dat))

          # add subnum and runnum
            dat.loop = cbind(dat.loop, subnum, runnum)
            colnames(dat.loop) = c("vif", "value", "subnum", "runnum")
            dat.loop = dat.loop[c(3,4,1,2)]
            dat.loop = dat.loop[c(1:14),]

          if (exists("dat.all")==FALSE){
            dat.all = dat.loop 
          } else{ 
            dat.all = rbind(dat.all, dat.loop)
          }   

        } # END RUN

    } # END SUB

    assign(paste("dat.all",i,sep="_"),dat.all)

  } # END GROUP



# set directory to save dfs to
  setwd('/corral-repl/utexas/ldrc/SCRIPTS/lev2_QC/SC')

# create one large df
  all.subs = rbind(dat.all_1, dat.all_2, dat.all_3, dat.all_4, dat.all_5, dat.all_6, dat.all_7, dat.all_8, dat.all_9, dat.all_10, dat.all_11, dat.all_12, dat.all_13, dat.all_14, dat.all_15)
  write.csv(x=all.subs, file='all_desmat_8_15.csv')
  vif.subs = all.subs[(all.subs$value > 10 & is.na(all.subs$value)==FALSE),]
  write.csv(x=vif.subs, file='desmat>10_8_15.csv')

# df of V15 (leftover RT -  important for model that includes leftover rt)
#  v15.subs = all.subs[all.subs$vif=="des.matV15", ]

# dfs with missing VIF values; junk includes omissions and fast responses; incor includes mismatch and incorrect
  all.empty = all.subs[all.subs$value=="NaN", ]
  write.csv(x=all.empty, file='empty_ev_subs_8_15.csv')
  incor.empty = all.subs[all.subs$value=="NaN" & (all.subs$vif=="des.matV9" | all.subs$vif=="des.matV10"), ]
  write.csv(x=incor.empty, file='empty_incor_ev_subs_8_15.csv')
  junk.empty = all.subs[all.subs$value=="NaN" & (all.subs$vif=="des.matV11" | all.subs$vif=="des.matV12"), ]
  write.csv(x=junk.empty, file='empty_junk_ev_subs_8_15s.csv')

  rt.all.empty = all.subs[all.subs$value=="NaN" & (all.subs$vif=="des.matV13" | all.subs$vif=="des.matV14"), ]








------------------------------------------------
# Jeanette's notes on how to create script above

# use Sys.glob() to help create loop; create matrix where numrows=(#subs)(#runs) and numcols=28
  des.mat = as.matrix(read.table('design.mat', skip = 5))

# create fake data
  y.fake = c(1:dim(des.mat)[1])

  mod = lm(y.fake ~ des.mat )

  vif(mod)


  mod_rt_left = lm(desmat[,15]~desmat[,-c(15)]-1)
  summary(mod_rt_left)

  mod_rt_left = lm(desmat[,15]~desmat[,c(1:8)]-1)
  summary(mod_rt_left)
