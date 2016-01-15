#!/bin/sh 

rm /corral-repl/utexas/ldrc/SCRIPTS/SC_buttonsubs.txt
rm /corral-repl/utexas/ldrc/SCRIPTS/SST_buttonsubs.txt

#SC

cat "/corral-repl/utexas/ldrc/SCRIPTS/SC_buttonfix_subs.txt" | grep -v "^#" | while read run
do
	echo python /corral-repl/utexas/ldrc/SCRIPTS/SC_event_maker_buttonfix.py ${run}/*subdata.pkl ${run} >> SC_buttonsubs.txt
done

#SST


cat "/corral-repl/utexas/ldrc/SCRIPTS/SST_buttonfix_subs.txt" | grep -v "^#" | while read run
do
	echo python /corral-repl/utexas/ldrc/SCRIPTS/SC_event_maker_buttonfix.py ${run}/*subdata.pkl ${run} >> SST_buttonsubs.txt
done
