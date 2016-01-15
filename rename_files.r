#Joel Martinez April 2014, looks through the WU folders and renames the old names to the new ones.
#I had to separate the subjects with second in their name from the ones without it because of the way I search for their names. The first loop is
#for subjects without second in their name. The second loop is for subjects with second in their name. If you need to rerun a particular subject,
#if they don't have second in their name, run filenames1 and look for the row number next to their name. then type i = [row #] in the R console.
#Then you can run everything inside the subject loop. If the subject has a second in their name, then do the same but with filenames2 in the
#second loop.


library(plyr)

dir = '/corral-repl/utexas/ldrc/'
filenames = c(Sys.glob(sprintf('%sldrc_*', dir)))

#function
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


#FIRST LOOP
filenames1 = filenames[grep('second', filenames, invert = T)]



#SUBJECT LOOP
for (i in 1: length(filenames1)){
	
	
	
	print(filenames1[i])
	
	#OLD SUBNUM
	oldnum = sprintf('ldrc_%s', substrRight(filenames1[i], 3)) #The old subnum
	
	#NEW SUBNUM
	newsplit = unlist(strsplit(filenames1[i], '/'))
	newnum = newsplit[5] #the new num
	
	#FINDS THE WU FOLDERS
	dirs = list.dirs(filenames1[i], full.names = T)[grep('WU_preprocess', list.dirs(filenames1[i], full.names = T))]
	
	#GOES THROUGH EACH FOLDER, FINDS THE FILES WITH THE OLD NAME AND SWITCHES IT TO THE NEW NAME
	for (a in 1:length(dirs)){
		files = list.files(dirs[a], pattern = oldnum, full.names = T)
		sapply(files, FUN = function(eachpath){
			file.rename(from = eachpath, to = sub(pattern = oldnum, replacement = newnum, eachpath))
		})
	
	} #END RENAME LOOP



} #END SUBJECT LOOP




#SECOND LOOP
filenames2 = filenames[grep('second', filenames)]


#SUBJECT LOOP
for (i in 1: length(filenames2)){
	
	
	
	print(filenames2[i])
	#OLD SUBNUM
	oldnum = sprintf('ldrc_%s', substrRight(filenames2[i], 10))
	
	#NEW SUBNUM
	newsplit = unlist(strsplit(filenames2[i], '/'))
	newnum = newsplit[5] #the new num
	
	#FINDS THE WU FOLDERS
	dirs = list.dirs(filenames2[i], full.names = T)[grep('WU_preprocess', list.dirs(filenames2[i], full.names = T))]
	
	
	#GOES THROUGH EACH FOLDER, FINDS THE FILES WITH THE OLD NAME AND SWITCHES IT TO THE NEW NAME
	for (a in 1:length(dirs)){
		files = list.files(dirs[a], pattern = oldnum, full.names = T)
		sapply(files, FUN = function(eachpath){
			file.rename(from = eachpath, to = sub(pattern = oldnum, replacement = newnum, eachpath))
		})
	
	} #END RENAME LOOP



} #END SUBJECT LOOP
