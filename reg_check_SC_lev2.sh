# Grabs each level 2 example_fun2standard1.png for SC to check registration and puts into html file; 4 groups: Austin Controls, Austin First, Austin Second, Houston Second
# Mary Abbe Roe April 2014 (adapted from Jeanette's fMRI class script)

outputdir='/corral-repl/utexas/ldrc/SCRIPTS/lev2_QC/SC'
#delete if there is an old version
rm  $outputdir/reg_check_c_results.html
rm  $outputdir/reg_check_1_results.html
rm  $outputdir/reg_check_2_results.html
rm  $outputdir/reg_check_3_results.html
rm  $outputdir/reg_check_coh2_1_results.html
rm  $outputdir/reg_check_coh2_2_results.html
rm  $outputdir/reg_check_coh3_1_results.html
rm  $outputdir/reg_check_H_2_results.html
rm  $outputdir/reg_check_H_3_results.html
rm  $outputdir/reg_check_H2_1_results.html
rm  $outputdir/reg_check_H2_2_results.html
rm  $outputdir/reg_check_H3_results.html

# Austin Controls
for subnum in ldrc_c_032 ldrc_c_037 ldrc_c_039 ldrc_c_043 ldrc_c_044 ldrc_c_045 ldrc_c_046 ldrc_c_047 ldrc_c_058 ldrc_c_059 ldrc_c_061 ldrc_c_062 ldrc_c_078 ldrc_c_079 ldrc_c_081 ldrc_c_082 ldrc_c_084 ldrc_c_085 ldrc_c_086 ldrc_c_101 ldrc_c_103 ldrc3_c_181 ldrc3_c_185 ldrc3_c_187 ldrc3_c_187_2 ldrc3_c_188 ldrc3_c_190 ldrc3_c_192 ldrc3_c_194 ldrc3_c_198 ldrc3_c_199 ldrc3_c_203 ldrc3_c_221
do

for i in Run1.feat Run2.feat Run3.feat 

do

dirpth=/corral-repl/utexas/ldrc/$subnum/model/SC/${i}/

echo '<p>============================================
<p> Registration for analysis in '$dirpth'

<p>FMRI to Highres<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2highres.png WIDTH=100%>
<p>Highres to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/highres2standard.png WIDTH=100%>
<p>FMRI to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2standard.png WIDTH=100%>
</BODY></HTML>' >> $outputdir/reg_check_c_results.html


done
done




# Austin Intervention First
for subnum in ldrc_0_009 ldrc_0_013 ldrc_0_018 ldrc_0_025 ldrc_0_054 ldrc_0_057 ldrc_1_001 ldrc_1_002 ldrc_1_004 ldrc_1_007 ldrc_1_015 ldrc_1_020 ldrc_1_028 ldrc_2_008 ldrc_2_010 ldrc_2_011 ldrc_2_027 ldrc_2_031
do

for i in Run1.feat Run2.feat Run3.feat 

do

dirpth=/corral-repl/utexas/ldrc/$subnum/model/SC/${i}/

echo '<p>============================================
<p> Registration for analysis in '$dirpth'

<p>FMRI to Highres<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2highres.png WIDTH=100%>
<p>Highres to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/highres2standard.png WIDTH=100%>
<p>FMRI to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2standard.png WIDTH=100%>
</BODY></HTML>' >> $outputdir/reg_check_1_results.html


done
done




# Austin Intervention Second
for subnum in ldrc_0_013_second ldrc_0_026_second ldrc_0_040_second ldrc_0_049_second ldrc_0_055_second ldrc_1_001_second ldrc_1_004_second ldrc_1_015_second ldrc_1_020_second ldrc_1_028_second ldrc_1_036_second ldrc_2_005_second ldrc_2_008_second ldrc_2_010_second ldrc_2_031_second ldrc_2_034_second ldrc_2_048_second
do

for i in Run1.feat Run2.feat Run3.feat 

do

dirpth=/corral-repl/utexas/ldrc/$subnum/model/SC/${i}/

echo '<p>============================================
<p> Registration for analysis in '$dirpth'

<p>FMRI to Highres<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2highres.png WIDTH=100%>
<p>Highres to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/highres2standard.png WIDTH=100%>
<p>FMRI to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2standard.png WIDTH=100%>
</BODY></HTML>' >> $outputdir/reg_check_2_results.html


done
done


# Austin Intervention Third
for subnum in ldrc_0_013_third ldrc_0_026_third ldrc_0_68_third ldrc_1_001_third ldrc_1_004_third ldrc_1_007_third ldrc_1_036_third ldrc_2_003_third ldrc_2_005_third ldrc_2_008_third ldrc_2_010_third ldrc_2_034_third
do

for i in Run1.feat Run2.feat Run3.feat 

do

dirpth=/corral-repl/utexas/ldrc/$subnum/model/SC/${i}/

echo '<p>============================================
<p> Registration for analysis in '$dirpth'

<p>FMRI to Highres<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2highres.png WIDTH=100%>
<p>Highres to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/highres2standard.png WIDTH=100%>
<p>FMRI to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2standard.png WIDTH=100%>
</BODY></HTML>' >> $outputdir/reg_check_3_results.html


done
done



# Austin Intervention Cohort 2, Firsts
for subnum in ldrc2_0_104 ldrc2_0_122 ldrc2_0_123 ldrc2_0_124 ldrc2_0_128 ldrc2_0_135 ldrc2_0_137 ldrc2_0_140 ldrc2_0_143 ldrc2_0_145 ldrc2_0_151 ldrc2_0_172 ldrc2_0_175 ldrc2_1_129 ldrc2_1_133 ldrc2_1_134 ldrc2_1_139 ldrc2_1_141 ldrc2_1_147 ldrc2_1_152 ldrc2_1_157 ldrc2_1_173
do

for i in Run1.feat Run2.feat Run3.feat 

do

dirpth=/corral-repl/utexas/ldrc/$subnum/model/SC/${i}/

echo '<p>============================================
<p> Registration for analysis in '$dirpth'

<p>FMRI to Highres<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2highres.png WIDTH=100%>
<p>Highres to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/highres2standard.png WIDTH=100%>
<p>FMRI to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2standard.png WIDTH=100%>
</BODY></HTML>' >> $outputdir/reg_check_coh2_1_results.html


done
done


# Austin Intervention Cohort 2, Seconds
for subnum in ldrc2_0_122_second ldrc2_0_123_second ldrc2_0_124_second ldrc2_0_137_second ldrc2_0_140_second ldrc2_0_143_second ldrc2_0_145_second ldrc2_0_151_second ldrc2_1_115_second ldrc2_1_125_second ldrc2_1_129_second ldrc2_1_133_second ldrc2_1_134_second ldrc2_1_139_second ldrc2_1_147_second ldrc2_1_154_second ldrc2_1_173_second
do

for i in Run1.feat Run2.feat Run3.feat 

do

dirpth=/corral-repl/utexas/ldrc/$subnum/model/SC/${i}/

echo '<p>============================================
<p> Registration for analysis in '$dirpth'

<p>FMRI to Highres<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2highres.png WIDTH=100%>
<p>Highres to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/highres2standard.png WIDTH=100%>
<p>FMRI to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2standard.png WIDTH=100%>
</BODY></HTML>' >> $outputdir/reg_check_coh2_2_results.html


done
done

# Austin Intervention Cohort 3, Firsts
for subnum in ldrc3_0_197 ldrc3_0_202 ldrc3_0_216 ldrc3_0_217 ldrc3_0_219 ldrc3_0_222 ldrc3_0_234 ldrc3_0_235 ldrc3_0_236 ldrc3_0_237 ldrc3_0_248 ldrc3_1_208 ldrc3_1_213 ldrc3_1_214 ldrc3_1_224 ldrc3_1_225 ldrc3_1_226 ldrc3_1_227 ldrc3_1_228 ldrc3_1_229 ldrc3_1_241 ldrc3_1_242 ldrc3_1_243 ldrc3_1_246
do

for i in Run1.feat Run2.feat Run3.feat 

do

dirpth=/corral-repl/utexas/ldrc/$subnum/model/SC/${i}/

echo '<p>============================================
<p> Registration for analysis in '$dirpth'

<p>FMRI to Highres<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2highres.png WIDTH=100%>
<p>Highres to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/highres2standard.png WIDTH=100%>
<p>FMRI to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2standard.png WIDTH=100%>
</BODY></HTML>' >> $outputdir/reg_check_coh3_1_results.html


done
done


# Houston Second
for subnum in H_LDFHO1794_1_second H_LDFHO2569_2_second H_LDFHO1986_1_second H_LDFHO2436_2_second H_LDFHO2748_1_second H_LDIMG8960_c_second H_LDIMG8961_c_second H_LDIMG8963_c_second H_LDIMG8967_c_second H_LDIMG8972_c_second H_LDIMG8974_c_second
do

for i in Run1.feat Run2.feat Run3.feat 

do

dirpth=/corral-repl/utexas/ldrc/$subnum/model/SC/${i}/

echo '<p>============================================
<p> Registration for analysis in '$dirpth'

<p>FMRI to Highres<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2highres.png WIDTH=100%>
<p>Highres to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/highres2standard.png WIDTH=100%>
<p>FMRI to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2standard.png WIDTH=100%>
</BODY></HTML>' >> $outputdir/reg_check_H_2_results.html


done
done


# Houston Third
for subnum in H_LDFHO1496_2_third H_LDFHO1589_2_third H_LDFHO1690_2_third H_LDFHO1697_1_third H_LDFHO2167_2_third H_LDFHO2416_1_third H_LDFHO2479_1_third H_LDFHO2569_2_third H_LDFHO2798_1_third H_LDIMG8960_c_third H_LDIMG8961_c_third H_LDIMG8963_c_third
do

for i in Run1.feat Run2.feat Run3.feat 

do

dirpth=/corral-repl/utexas/ldrc/$subnum/model/SC/${i}/

echo '<p>============================================
<p> Registration for analysis in '$dirpth'

<p>FMRI to Highres<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2highres.png WIDTH=100%>
<p>Highres to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/highres2standard.png WIDTH=100%>
<p>FMRI to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2standard.png WIDTH=100%>
</BODY></HTML>' >> $outputdir/reg_check_H_3_results.html


done
done

# Houston COHORT 2 firsts
for subnum in LDFHO1794_1 LDFHO5782_1 LDFHO6598_0 LDFHO9087_1 LDFHO10297_1 LDFHO10238_1 LDFHO10328_1 LDFHO10493_1
do

for i in Run1.feat Run2.feat Run3.feat 

do

dirpth=/corral-repl/utexas/ldrc/$subnum/model/SC/${i}/

echo '<p>============================================
<p> Registration for analysis in '$dirpth'

<p>FMRI to Highres<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2highres.png WIDTH=100%>
<p>Highres to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/highres2standard.png WIDTH=100%>
<p>FMRI to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2standard.png WIDTH=100%>
</BODY></HTML>' >> $outputdir/reg_check_H2_1_results.html


done
done

# Houston Cohort 2 Seconds
for subnum in LDFHO1794_1_second LDFHO5801_1_second LDFHO9087_1_second LDFHO10238_1_second LDFHO10286_1_second LDFHO10328_1_second LDFHO 10378_1_second LDFHO10493_1_second LDFHO10743_1_second LDFHO10942_1_second LDFHO12307_1_second
do

for i in Run1.feat Run2.feat Run3.feat 

do

dirpth=/corral-repl/utexas/ldrc/$subnum/model/SC/${i}/

echo '<p>============================================
<p> Registration for analysis in '$dirpth'

<p>FMRI to Highres<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2highres.png WIDTH=100%>
<p>Highres to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/highres2standard.png WIDTH=100%>
<p>FMRI to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2standard.png WIDTH=100%>
</BODY></HTML>' >> $outputdir/reg_check_H2_2_results.html


done
done

# Houston COHORT 3 firsts
for subnum in LDFHO23801_1_3 LDFHO23901_1_3 LDFHO24617_1_3 LDFHO24761_1_3 LDFHO24768_1_3 LDFHO24839_1_3 LDFHO24861_1_3
do

for i in Run1.feat Run2.feat Run3.feat 

do

dirpth=/corral-repl/utexas/ldrc/$subnum/model/SC/${i}/

echo '<p>============================================
<p> Registration for analysis in '$dirpth'

<p>FMRI to Highres<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2highres.png WIDTH=100%>
<p>Highres to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/highres2standard.png WIDTH=100%>
<p>FMRI to standard space<br><IMG BORDER=0 SRC='$dirpth'reg/example_func2standard.png WIDTH=100%>
</BODY></HTML>' >> $outputdir/reg_check_H3_results.html


done
done
