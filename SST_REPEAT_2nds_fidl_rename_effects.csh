#!/bin/csh
unlimit

set BIN = /corral-repl/utexas/ldrc/fidl/fidl_2.64/bin
set BINLINUX = /corral-repl/utexas/ldrc/fidl/fidl_2.64/bin_linux
set BINLINUX64 = /corral-repl/utexas/ldrc/fidl/fidl_2.64/bin_linux64
if(`uname` == Linux) then
    set dog = `uname -a`
    set cat = `expr $#dog - 1`
    if($dog[$cat] == x86_64) then
        set BIN = $BINLINUX64
    else
        set BIN = $BINLINUX
    endif
endif

nice +19 $BIN/fidl_rename_effects -glm_files ldrc_0_026_second/WU_preprocess/ldrc_0_026_second/ldrc_0_026_second_1SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc_1_004_second/WU_preprocess/ldrc_1_004_second/ldrc_1_004_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc_1_015_second/WU_preprocess/ldrc_1_015_second/ldrc_1_015_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc_1_020_second/WU_preprocess/ldrc_1_020_second/ldrc_1_020_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc_1_028_second/WU_preprocess/ldrc_1_028_second/ldrc_1_028_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc_2_008_second/WU_preprocess/ldrc_2_008_second/ldrc_2_008_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc_2_010_second/WU_preprocess/ldrc_2_010_second/ldrc_2_010_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc2_0_122_second/WU_preprocess/ldrc2_0_122_second/ldrc2_0_122_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc2_0_123_second/WU_preprocess/ldrc2_0_123_second/ldrc2_0_123_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc2_0_124_second/WU_preprocess/ldrc2_0_124_second/ldrc2_0_124_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc2_0_137_second/WU_preprocess/ldrc2_0_137_second/ldrc2_0_137_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc2_0_140_second/WU_preprocess/ldrc2_0_140_second/ldrc2_0_140_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc2_0_143_second/WU_preprocess/ldrc2_0_143_second/ldrc2_0_143_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc2_0_145_second/WU_preprocess/ldrc2_0_145_second/ldrc2_0_145_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc_2_031_second/WU_preprocess/ldrc_2_031_second/ldrc_2_031_second_1SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc2_1_129_second/WU_preprocess/ldrc2_1_129_second/ldrc2_1_129_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc2_1_133_second/WU_preprocess/ldrc2_1_133_second/ldrc2_1_133_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
nice +19 $BIN/fidl_rename_effects -glm_files ldrc2_1_141_second/WU_preprocess/ldrc2_1_141_second/ldrc2_1_141_second_2SST_REPEAT_SCRUBfd09_RTreg_wholeTR_7TR_092415.glm -contrasts 1 2 3 4 5 6 7 -effect_labels Incorrect_go_2 Correct_go_2 Failed_Stop_2 Correct_Stop_2 RT_2 Trend Baseline
