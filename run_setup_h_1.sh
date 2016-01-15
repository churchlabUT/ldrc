SUBCODE='LDFHO24839_F2015_V1'
STUDYNAME='ldrc'
cd /corral-repl/utexas/${STUDYNAME}
SETUP_SUBJECT='/corral-repl/utexas/ldrc/setup_subject.py'
#$SETUP_SUBJECT --xnat-project church --getdata --keepdata -b /corral-repl/utexas/ --studyname $STUDYNAME -s $SUBCODE
echo "$SETUP_SUBJECT -o --dcm2nii --skip --dtiqa --keepdata -b /corral-repl/utexas/ --studyname $STUDYNAME -s $SUBCODE">setup_$SUBCODE.sh

launch -s setup_$SUBCODE.sh -r 01:00:00 -n setup_$SUBCODE -j Analysis_Lonestar -m churchlab@austin.utexas.edu

