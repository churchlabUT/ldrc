# Rename thresholded and unthresholded zstat files to feed into Caret
# Mary Abbe Roe, September/October 2014

# LIBRARIES
  library(gtools)

# FUNCTIONS
	
  # RENAME THRESH ZSTATS
    runThresh = function(rootdir, group, task){
      gfeats = Sys.glob(sprintf("%s/*.gfeat", rootdir))
      print(gfeats)
      num.gfeats = length(gfeats)

      if (num.gfeats == 0) {
         print(sprintf("%s %s is empty", group, task))
      } 

      for (stat in 1:num.gfeats){
        split = strsplit(gfeats[stat], "/")
        split2 = strsplit(split[[1]][8], "[.]")
        contrast = split2[[1]][1]

        t.zstat1 = Sys.glob(sprintf("%s/cope1.feat/thresh_zstat1.nii.gz", gfeats[stat]))
        t.zstat2 = Sys.glob(sprintf("%s/cope1.feat/thresh_zstat2.nii.gz", gfeats[stat]))
 
        new.t.zstat1 = sprintf("%s/cope1.feat/%s_%s_%s_%s_thresh_zstat1.nii.gz", gfeats[stat], group, subs, task, contrast)
        new.t.zstat2 = sprintf("%s/cope1.feat/%s_%s_%s_%s_thresh_zstat2.nii.gz", gfeats[stat], group, subs, task, contrast)

        # set zstat directory
        zstat.dir = sprintf("/corral-repl/utexas/ldrc/GROUP/zstats/thresh/%s", task)

        # copy and rename into same directory
        stat1.cop1 = file.copy(t.zstat1,new.t.zstat1, copy.mode = TRUE)
        stat2.cop1 = file.copy(t.zstat2,new.t.zstat2, copy.mode = TRUE)
 
          if (stat1.cop1 == TRUE){
            print(sprintf("%s %s %s %s zstat1 was copied into the %s directory", group, subs, task, contrast, rootdir))
          } else if (file.exists(new.t.zstat1) == TRUE) {
              print(sprintf("%s %s %s %s zstat1 exists in the %s directory", group, subs, task, contrast, rootdir))
          } else {
              print(sprintf("%s %s %s %s zstat1 does not exist in the %s directory", group, subs, task, contrast, rootdir))
          }

          if (stat2.cop1 == TRUE){
            print(sprintf("%s %s %s %s zstat2 was copied into the %s directory", group, subs, task, contrast, rootdir))
          } else if (file.exists(new.t.zstat2) == TRUE) {
              print(sprintf("%s %s %s %s zstat2 exists in the %s directory", group, subs, task, contrast, rootdir))
          } else {
              print(sprintf("%s %s %s %s zstat2 does not exist in the %s directory", group, subs, task, contrast, rootdir))
          }

        # copy into zstats directory
        stat1.cop2 = file.copy(new.t.zstat1, zstat.dir, copy.mode = TRUE) 
        stat2.cop2 = file.copy(new.t.zstat2, zstat.dir, copy.mode = TRUE)

      }
    }



  # RENAME UNTHRESH ZSTATS
    runUnthresh = function(rootdir, group, task){
      gfeats = Sys.glob(sprintf("%s/*.gfeat", rootdir))
      print(gfeats)
      num.gfeats = length(gfeats)

      if (num.gfeats == 0) {
         print(sprintf("%s %s is empty", group, task))
      } 

      for (stat in 1:num.gfeats){
        split = strsplit(gfeats[stat], "/")
        split2 = strsplit(split[[1]][8], "[.]")
        contrast = split2[[1]][1]

        u.zstat1 = Sys.glob(sprintf("%s/cope1.feat/stats/zstat1.nii.gz", gfeats[stat]))
        u.zstat2 = Sys.glob(sprintf("%s/cope1.feat/stats/zstat2.nii.gz", gfeats[stat]))
 
        new.u.zstat1 = sprintf("%s/cope1.feat/stats/%s_%s_%s_%s_zstat1.nii.gz", gfeats[stat], group, subs, task, contrast)
        new.u.zstat2 = sprintf("%s/cope1.feat/stats/%s_%s_%s_%s_zstat2.nii.gz", gfeats[stat], group, subs, task, contrast)

        # set zstat directory
        zstat.dir = sprintf("/corral-repl/utexas/ldrc/GROUP/zstats/unthresh/%s", task)

        # copy and rename into same directory
        stat1.cop1 = file.copy(u.zstat1,new.u.zstat1, copy.mode = TRUE)
        stat2.cop1 = file.copy(u.zstat2,new.u.zstat2, copy.mode = TRUE)
 
          if (stat1.cop1 == TRUE){
            print(sprintf("%s %s %s %s zstat1 was copied into the %s directory", group, subs, task, contrast, rootdir))
          } else if (file.exists(new.u.zstat1) == TRUE) {
              print(sprintf("%s %s %s %s zstat1 exists in the %s directory", group, subs, task, contrast, rootdir))
          } else {
              print(sprintf("%s %s %s %s zstat1 does not exist in the %s directory", group, subs, task, contrast, rootdir))
          }

          if (stat2.cop1 == TRUE){
            print(sprintf("%s %s %s %s zstat2 was copied into the %s directory", group, subs, task, contrast, rootdir))
          } else if (file.exists(new.u.zstat2) == TRUE) {
              print(sprintf("%s %s %s %s zstat2 exists in the %s directory", group, subs, task, contrast, rootdir))
          } else {
              print(sprintf("%s %s %s %s zstat2 does not exist in the %s directory", group, subs, task, contrast, rootdir))
          }

        # copy into zstats directory
        stat1.cop2 = file.copy(new.u.zstat1, zstat.dir, copy.mode = TRUE) 
        stat2.cop2 = file.copy(new.u.zstat2, zstat.dir, copy.mode = TRUE)

      }
    }





##########

# Controls Austin
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/Controls/SC"
    group = "Controls"
    task = "SC"
    subs = "n21"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
 
  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/Controls/SST"
    group = "Controls"
    task = "SST"
    subs = "n20"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

# Controls Austin & Houston
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/Controls_ah/SC"
    group = "Controls_ah"
    task = "SC"
    subs = "n21_n6"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
 
  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/Controls_ah/SST"
    group = "Controls_ah"
    task = "SST"
    subs = "n20_n6"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S1 cohort1
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_c1/SC"
    group = "S1_c1"
    task = "SC"
    subs = "n17"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_c1/SST"
    group = "S1_c1"
    task = "SST"
    subs = "n16"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S1 cohort2
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_c2/SC"
    group = "S1_c2"
    task = "SC"
    subs = "n18"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_c2/SST"
    group = "S1_c2"
    task = "SST"
    subs = "n18"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S1 cohort2 Houston
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_h/SC"
    group = "S1_h"
    task = "SC"
    subs = "n7"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_h/SST"
    group = "S1_h"
    task = "SST"
    subs = "n10"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S1 cohort1, cohort2
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_c1_c2/SC"
    group = "S1"
    task = "SC"
    subs = "n17_n18"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_c1_c2/SST"
    group = "S1"
    task = "SST"
    subs = "n16_n18"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S1 low cohort1, cohort2
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1low/SC"
    group = "S1low"
    task = "SC"
    subs = "n19"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1low/SST"
    group = "S1"
    task = "SST"
    subs = "n16_n18"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

# S1 low vs. high
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1low_v_high/SC"
    group = "S1low_v_high"
    task = "SC"
    subs = "n19n16"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S1 high
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1high/SC"
    group = "S1high"
    task = "SC"
    subs = "n16"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)





# S1 cohort1 vs. S1 cohort2
  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1c1_v_S1c2/SST"
    group = "S1c1_v_S1c2"
    task = "SST"
    subs = "n16_n18"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S1 cohort1, cohort2 Austin and Houston
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_ah/SC"
    group = "S1_ah"
    task = "SC"
    subs = "n17n18_n7"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_ah/SST"
    group = "S1_ah"
    task = "SST"
    subs = "n16n18_n10"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

 
# S2 (both Austin cohorts)
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S2/SC"
    group = "S2"
    task = "SC"
    subs = "n16_n13"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S2/SST"
    group = "S2"
    task = "SST"
    subs = "n15_n12"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
 
# S2 Austin and Houston
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S2_ah/SC"
    group = "S2_ah"
    task = "SC"
    subs = "n16n13_n4n8"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S2_ah/SST"
    group = "S2_ah"
    task = "SST"
    subs = "n15n12_n4n6"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
 

# S3 Austin
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S3/SC"
    group = "S3"
    task = "SC"
    subs = "n12"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S3/SST"
    group = "S3"
    task = "SST"
    subs = "n11"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
 
# S3 Austin + Houston
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S3_ah/SC"
    group = "S3_ah"
    task = "SC"
    subs = "n12n9"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S3_ah/SST"
    group = "S3_ah"
    task = "SST"
    subs = "n11n7"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
 


# C,S1,S2
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/C_S1_S2/SC"
    group = "C_S1_S2"
    task = "SC"
    subs = "n25_n15_n16"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/C_S1_S2/SST"
    group = "C_S1_S2"
    task = "SST"
    subs = "n23_n13_n16"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
 
# C,S1,S2 Austin cohort 1,2 ALL
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/C_S1_S2_a/SC"
    group = "C_S1_S2_a"
    task = "SC"
    subs = "n85"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/C_S1_S2_a/SST"
    group = "C_S1_S2_a"
    task = "SST"
    subs = "n81"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
 

# C,S1,S2 
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_v_S2_v_C/SC"
    group = "S1_v_S2_v_C"
    task = "SC"
    subs = "n35_n29_21"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/C_S1_S2_a/SST"
    group = "C_S1_S2_a"
    task = "SST"
    subs = "n81"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
 



# C,S1,S2 Austin and Houston cohort 1,2 ALL
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/all_ah/SC"
    group = "all_ah"
    task = "SC"
    subs = "n85_n25"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/all_ah/SST"
    group = "all_ah"
    task = "SST"
    subs = "n81_n26"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

# Houston cohort 1,2 ALL
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_all/SC"
    group = "H_all"
    task = "SC"
    subs = "n25"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_all/SST"
    group = "H_all"
    task = "SST"
    subs = "n26"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S1 vs. C
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_C_a/SC"
    group = "S1_v_C"
    task = "SC"
    subs = "n17n18_n21"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_C_a/SST"
    group = "S1_v_C"
    task = "SST"
    subs = "n16n18_n20"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

# S1low vs. C
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1low_v_C/SC"
    group = "S1low_v_C"
    task = "SC"
    subs = "n19_n21"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1low_v_C/SST"
    group = "S1low_v_C"
    task = "SST"
    subs = "n16n18_n20"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S1high vs. C
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1high_v_C/SC"
    group = "S1high_v_C"
    task = "SC"
    subs = "n16_n21"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1high_v_C/SST"
    group = "S1low_v_C"
    task = "SST"
    subs = "n16n18_n20"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S1 vs. C Austin + Houston
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_v_C_ah/SC"
    group = "S1_v_C_ah"
    task = "SC"
    subs = "n17n18n7_n21n6"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_v_C_ah/SST"
    group = "S1_v_C_ah"
    task = "SST"
    subs = "n16n18n10_n20n6"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S2 vs. C
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S2_C_a/SC"
    group = "S2_v_C"
    task = "SC"
    subs = "n16n13_n21"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S2_C_a/SST"
    group = "S2_v_C"
    task = "SST"
    subs = "n15n12_n20"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
 
# S2 vs. C Austin + Houston
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S2_v_C_ah/SC"
    group = "S2_v_C_ah"
    task = "SC"
    subs = "n16n13n4n8_n21n6"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S2_v_C_ah/SST"
    group = "S2_v_C_ah"
    task = "SST"
    subs = "n15n12n4n6_n20n6"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
 


# S3 vs. C
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S3_C_7_14_14/SC"
    group = "S3_C"
    task = "SC"
    subs = "n11_n25"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S3_C_7_14_14/SST"
    group = "S3_C"
    task = "SST"
    subs = "n10_n23"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
 
# S1 vs. S2
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_v_S2/SC"
    group = "S1_v_S2"
    task = "SC"
    subs = "n17n18_n16n13"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_v_S2/SST"
    group = "S1_v_S2"
    task = "SST"
    subs = "n16n18_n15n12"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S1 vs. S2 austin repeat subs
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_v_S2_a_rm/SC"
    group = "S1_v_S2_a_rm"
    task = "SC"
    subs = "n9_n11"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_v_S2_a_rm/SST"
    group = "S1_v_S2_a_rm"
    task = "SST"
    subs = "n8_n10"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)




# S1 vs. S2 Austin + Houston
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_v_S2_ah/SC"
    group = "S1_v_S2_ah"
    task = "SC"
    subs = "n17n18n7_n16n13n4n8"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_v_S2_ah/SST"
    group = "S1_v_S2_ah"
    task = "SST"
    subs = "n16n18n10_n15n12n4n6"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S1 vs. S3
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_S3/SC"
    group = "S1_S3"
    task = "SC"
    subs = "n15_n11"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_S3/SST"
    group = "S1_S3"
    task = "SST"
    subs = "n13_n10"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
 

# S2 vs. S3
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S2_S3/SC"
    group = "S2_S3"
    task = "SC"
    subs = "n16_n11"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S2_S3/SST"
    group = "S2_S3"
    task = "SST"
    subs = "n16_n10"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
 

# P1 vs. P2
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/P1_P2_4_28_14/SC"
    group = "P1_P2"
    task = "SC"
    subs = "n9"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/P1_P2_4_28_14/SST"
    group = "P1_P2"
    task = "SST"
    subs = "n8"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# P1 vs. P3
    rootdir = "/corral-repl/utexas/ldrc/GROUP/P1_P3_7_14_14/SC"
    group = "P1_P3"
    task = "SC"
    subs = "n6"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/P1_P3_7_14_14/SST"
    group = "P1_P3"
    task = "SST"
    subs = "n5"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# P2 vs. P3
    rootdir = "/corral-repl/utexas/ldrc/GROUP/P2_P3_7_14_14/SC"
    group = "P2_P3"
    task = "SC"
    subs = "n9"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/P2_P3_7_14_14/SST"
    group = "P2_P3"
    task = "SST"
    subs = "n9"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
