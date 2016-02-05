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





########## AUSTIN ##########

# Controls
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/Controls/SC"
    group = "Controls"
    task = "SC"
    subs = "n31"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
 
  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/Controls/SST"
    group = "Controls"
    task = "SST"
    subs = "n30"

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


# S1 cohort3
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_c3/SC"
    group = "S1_c3"
    task = "SC"
    subs = "n18"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_c3/SST"
    group = "S1_c3"
    task = "SST"
    subs = "n18"

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


# S1 cohort1, cohort2, cohort3
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_all/SC"
    group = "S1"
    task = "SC"
    subs = "n17_n18_n18"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_all/SST"
    group = "S1"
    task = "SST"
    subs = "n16_n18_n21"

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
    subs = "n113"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/C_S1_S2_a/SST"
    group = "C_S1_S2_a"
    task = "SST"
    subs = "n112"

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
 

# S1 vs. C
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_C_a/SC"
    group = "S1_v_C"
    task = "SC"
    subs = "n17n18n18_n21n10"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_C_a/SST"
    group = "S1_v_C"
    task = "SST"
    subs = "n16n18n21_n20n10"

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


# S2 vs. C
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S2_C_a/SC"
    group = "S2_v_C"
    task = "SC"
    subs = "n16n13_n21n10"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S2_C_a/SST"
    group = "S2_v_C"
    task = "SST"
    subs = "n15n12_n20n10"

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
    subs = "n17n18n18_n16n13"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/S1_v_S2/SST"
    group = "S1_v_S2"
    task = "SST"
    subs = "n16n18n21_n15n12"

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
 

# S2 vs.S3
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
 


########## HOUSTON ##########



# Controls
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_Controls/SC"
    group = "h_Controls"
    task = "SC"
    subs = "n6"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_Controls/SST"
    group = "h_Controls"
    task = "SST"
    subs = "n5"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S1
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_S1/SC"
    group = "h_S1"
    task = "SC"
    subs = "n10"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_S1/SST"
    group = "h_S1"
    task = "SST"
    subs = "n15"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S2
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_S2/SC"
    group = "h_S2"
    task = "SC"
    subs = "n13"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_S2/SST"
    group = "h_S2"
    task = "SST"
    subs = "n11"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

# S3
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_S3/SC"
    group = "h_S3"
    task = "SC"
    subs = "n9"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_S3/SST"
    group = "h_S3"
    task = "SST"
    subs = "n7"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# ALL
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_all/SC"
    group = "h_S1S2C"
    task = "SC"
    subs = "n29"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_all/SST"
    group = "h_S1S2C"
    task = "SST"
    subs = "n31"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S1 v C
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_S1_v_C/SC"
    group = "h_S1_v_C"
    task = "SC"
    subs = "n10n6"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_S1_v_C/SST"
    group = "h_S1_v_C"
    task = "SST"
    subs = "n15n5"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S2 v C
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_S2_v_C/SC"
    group = "h_S2_v_C"
    task = "SC"
    subs = "n13n6"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_S2_v_C/SST"
    group = "h_S2_v_C"
    task = "SST"
    subs = "n11n5"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)


# S1 v S2
  # SC
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_S1_v_S2/SC"
    group = "h_S1_v_S2"
    task = "SC"
    subs = "n10_n13"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)

  # SST
    rootdir = "/corral-repl/utexas/ldrc/GROUP/H_S1_v_S2/SST"
    group = "h_S1_v_S2"
    task = "SST"
    subs = "n15_n11"

    runThresh(rootdir, group, task)
    runUnthresh(rootdir, group, task)
