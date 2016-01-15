SUBCODE='LDFHO24768_F2015_V1'
STUDYNAME='ldrc'
cd /corral-repl/utexas/${STUDYNAME}
SETUP_SUBJECT='/corral-repl/utexas/ldrc/setup_subject.py'
#$SETUP_SUBJECT --xnat-project church --getdata --keepdata -b /corral-repl/utexas/ --studyname $STUDYNAME -s $SUBCODE
#echo "$SETUP_SUBJECT -o --skip --motcorr --betfunc --qa --fsrecon --fs-subdir /corral-repl/utexas/ldrc/FREESURFER/ --keepdata -b /corral-repl/utexas/ --studyname $STUDYNAME -s $SUBCODE">setup_$SUBCODE.sh
echo "$SETUP_SUBJECT -o --skip --fsrecon --fs-subdir /corral-repl/utexas/ldrc/FREESURFER/ --keepdata -b /corral-repl/utexas/ --studyname $STUDYNAME -s $SUBCODE">setup_$SUBCODE.sh


launch -s setup_$SUBCODE.sh -r 04:00:00 -n setup_$SUBCODE -j Analysis_Lonestar -m churchlab@austin.utexas.edu
