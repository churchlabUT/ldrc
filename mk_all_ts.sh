#fslmaths $FSLDIR/data/standard/MNI152_T1_2mm_brain_mask.nii.gz   -roi 69 1 77 1 43 1 0 1 point -odt float
#fslmaths point -kernel sphere 8 -fmean -bin roi_69_77_43
#fslview zstat* ../../bg_image.nii.gz
#fslmeants -i /corral-repl/utexas/ldrc/GROUP/S1_S2_5_23_13/SST/stop_correct_-_go_correct.gfeat/cope1.feat/filtered_func_data -m roi_20_72_36 >> stop_correct_-_go_correct_roi_20_72_36.txt


##### Find coordinates in FSL

You must use voxel coordinates to create ROIs

You can find voxel coordinates by entering the MNI coordinates of your ROI into FSL




#### To make ROIs (in terminal you must be inside of an rois directory in GROUP to make these: /ldrc/GROUP/Controls/rois/; run this section in the terminal window)

MNI_coord_x=-48
MNI_coord_y=-68
MNI_coord_z=-08


voxel_coord_x=69
voxel_coord_y=29
voxel_coord_z=32



fslmaths $FSLDIR/data/standard/MNI152_T1_2mm_brain_mask.nii.gz   -roi ${voxel_coord_x} 1 ${voxel_coord_y} 1 ${voxel_coord_z} 1 0 1 point -odt float

fslmaths point -kernel sphere 8 -fmean -bin roi_${MNI_coord_x}_${MNI_coord_y}_${MNI_coord_z}






#### To apply ROIs on different SST contrasts


SST_rois=(roi_+-0_+15_+45 roi_+08_-16_-06 roi_+29_+57_+18 roi_-29_+57_+11 roi_+32_-60_+41 roi_-32_-57_+46 roi_-36_+18_+02 roi_+36_-23_+57 roi_-36_-23_+57 roi_+38_+21_-01 roi_+41_-65_-11 roi_-41_-65_-11 roi_-42_+07_+36 roi_-44_+27_+33 roi_+44_+07_+36 roi_-45_-54_-11 roi_-45_-71_-01 roi_+45_-71_-01 roi_+45_-54_-11 roi_+46_+28_+31 roi_+48_+16_+18 roi_-51_-62_+33 roi_-52_-26_-04 roi_-52_-42_+06 roi_-53_-04_+36 roi_+53_-04_+36 roi_-53_+27_+16 roi_-53_-50_+39 roi_-54_-41_+26 roi_+54_-26_-04 roi_+54_-32_+04 roi_+54_-44_+43 roi_+57_-43_+27)
SST_contrasts=(stop_corr_v_stop_fail go_corr_v_baseline go_corr_v_stop_fail rt_all_v_baseline stop_corr_v_baseline stop_corr_v_go_corr stop_fail_v_baseline go_incorr_v_baseline go_corr_v_go_incorr)


#SST_rois=(roi_+34_-50_+46 roi_-08_-58_+52 roi_+44_+16_+42 roi_+46_-46_+48 roi_+06_+26_+30 roi_+06_+34_+12 roi_+46_+42_+08)
#SST_contrasts=(stop_corr_v_stop_fail stop_fail_v_baseline)

basedir=H_S2_v_C


for contr in ${SST_contrasts[@]}

do
mkdir /corral-repl/utexas/ldrc/GROUP/$basedir/SST/${contr}.gfeat/rois
rm /corral-repl/utexas/ldrc/GROUP/$basedir/SST/${contr}.gfeat/rois/*txt

  for roi in ${SST_rois[@]}

    do

    fslmaths ${roi} -mul /corral-repl/utexas/ldrc/GROUP/$basedir/SST/${contr}.gfeat/cope1.feat/mask temp_${roi}

    fslmaths /corral-repl/utexas/ldrc/GROUP/$basedir/SST/${contr}.gfeat/cope1.feat/filtered_func_data -mul 20.88 -div /corral-repl/utexas/ldrc/GROUP/$basedir/SST/${contr}.gfeat/cope1.feat/mean_func -mul /corral-repl/utexas/ldrc/GROUP/$basedir/SST/${contr}.gfeat/cope1.feat/mask /corral-repl/utexas/ldrc/GROUP/$basedir/SST/${contr}.gfeat/cope1.feat/filtered_func_data_perch

    fslmeants -i /corral-repl/utexas/ldrc/GROUP/$basedir/SST/${contr}.gfeat/cope1.feat/filtered_func_data_perch -m temp_${roi} >> /corral-repl/utexas/ldrc/GROUP/$basedir/SST/${contr}.gfeat/rois/${contr}_${roi}.txt

    done

done
 

#### To apply ROIs on different SC contrasts

basedir=H_all

SC_rois=(roi_+-0_+15_+45 roi_+08_-16_-06 roi_+29_+57_+18 roi_-29_+57_+11 roi_+32_-60_+41 roi_-32_-57_+46 roi_-36_+18_+02 roi_+36_-23_+57 roi_-36_-23_+57 roi_+38_+21_-01 roi_+41_-65_-11 roi_-41_-65_-11 roi_-42_+07_+36 roi_-44_+27_+33 roi_+44_+07_+36 roi_-45_-54_-11 roi_-45_-71_-01 roi_+45_-71_-01 roi_+45_-54_-11 roi_+46_+28_+31 roi_+48_+16_+18 roi_-51_-62_+33 roi_-52_-26_-04 roi_-52_-42_+06 roi_-53_-04_+36 roi_+53_-04_+36 roi_-53_+27_+16 roi_-53_-50_+39 roi_-54_-41_+26 roi_+54_-26_-04 roi_+54_-32_+04 roi_+54_-44_+43 roi_+57_-43_+27)
SC_contrasts=(active_v_passive corr_v_baseline sensible_v_non_sensible active_ns_v_baseline active_s_v_baseline passive_ns_v_baseline passive_s_v_baseline rt_all_v_baseline incorr_v_baseline corr_v_incorr)

rm corr_v_baseline*.txt
rm active_v_passive*.txt
rm sensible_v_non_sensible*.txt

for contr in ${SC_contrasts[@]}


do
mkdir /corral-repl/utexas/ldrc/GROUP/$basedir/SC/${contr}.gfeat/rois
rm /corral-repl/utexas/ldrc/GROUP/$basedir/SC/${contr}.gfeat/rois/*txt
  

  for roi in ${SC_rois[@]}
  
    do 

    fslmaths ${roi} -mul /corral-repl/utexas/ldrc/GROUP/$basedir/SC/${contr}.gfeat/cope1.feat/mask temp_${roi}
    
    fslmaths /corral-repl/utexas/ldrc/GROUP/$basedir/SC/${contr}.gfeat/cope1.feat/filtered_func_data -mul 109 -div /corral-repl/utexas/ldrc/GROUP/$basedir/SC/${contr}.gfeat/cope1.feat/mean_func -mul /corral-repl/utexas/ldrc/GROUP/$basedir/SC/${contr}.gfeat/cope1.feat/mask /corral-repl/utexas/ldrc/GROUP/$basedir/SC/${contr}.gfeat/cope1.feat/filtered_func_data_perch

    fslmeants -i /corral-repl/utexas/ldrc/GROUP/$basedir/SC/${contr}.gfeat/cope1.feat/filtered_func_data_perch -m temp_${roi} >>/corral-repl/utexas/ldrc/GROUP/$basedir/SC/${contr}.gfeat/rois/${contr}_${roi}.txt

    done

done
