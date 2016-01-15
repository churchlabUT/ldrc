#!/bin/sh

x="ldrc_1_015 ldrc_1_020"
for x
do
	cd $x/WU_preprocess
	convertDicoms preprocess.prm
	cd ../..
done
