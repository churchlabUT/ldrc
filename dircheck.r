#This script is useful if you need an easy way to see what's in everyone's directory.
#To run this script type 'Rscript dircheck.r ______ __'  into the terminal,
#The first blank after the name has five options: runs, check, fs, all, allfiles, subfiles. Each are explained below
#The second blank is the subject index you get from 'Rscript dircheck.r check'. The number to the left of the subject you want to look at
#will be the number you stick in the second blank. Only works for the 'subfiles' option. Ex. 'Rscript dircheck.r subfiles 20'
#Whichever option you choose: allfiles, subfiles, all, fs - the output will be inside a .txt file of the same name. Ex. all.txt

#sets up the filenames for the subjects in main folder-both austin and houston
dir = '/corral-repl/utexas/ldrc/'
filenames = c(Sys.glob(sprintf('%sldrc_*', dir)), Sys.glob(sprintf('%sH_*', dir)))

#Houston data
#dir ='/corral-repl/utexas/ldrc/PHILIPS/'
#filenames = Sys.glob(sprintf('%sH_*', dir))


#takes in arguments
args = commandArgs(trailingOnly = TRUE)

#sets the argument classes
type = as.character(args[1])
sub = as.numeric(args[2])

#check = Gives you an index of the subject you're trying to look at. Put after subfiles.
if (type == 'check'){
  print(filenames)
}

#creates files for the loop to write into
all = file('all.txt', 'w')
fs = file('fs.txt', 'w')
subfiles = file('subfiles.txt', 'w')
allfiles = file('allfiles.txt', 'w')
runs = file('runs.txt', 'w')

#First blank
#fs = shows subjects that have Freesurfer files in the wrong place
#all = shows all the folders every subject has in their main folder
#allfiles - shows all the files in the subdirectories for all the subjects. Lots of info, takes a
#while
#subfiles = shows all the subfiles within the sub directories of the subject you want to look at. Look for subject's index number in the
#check option.
#run = lists all the runs for each participant

for (i in 1:length(filenames)){
     if (type == 'fs'){
        dirs = list.files(path = filenames[i], all.files = F)
        if ('bem' %in% dirs){
	   sink(fs, append = T)
           print(filenames[i])
           print(dirs)
	   sink()
        }
     } else if (type == 'all'){
         dirs = list.files(path = filenames[i], all.files = F)
	   sink(all, append = T)
	   print(filenames[i])
	   print(dirs)
	   sink()
       } else if (type == 'allfiles'){
             dirs = list.files(path = filenames[i], all.files = F, full.names = T, include.dirs = T)
	     sink(allfiles)
	     for (f in 1: length(dirs)){
	        print(dirs[f])
	        print(sprintf('                        %s',list.files(dirs[f], recursive = F)))
	     }
	     sink()
         } else if (type == 'runs'){
		dirs = list.files(path = filenames[i], all.files = F, full.names = T, include.dirs = T)
		sink(runs, append = T)
		print(list.dirs(dirs[grep('BOLD', dirs)]))
		sink()
	  } 
}

if (type == 'subfiles'){
             dirs = list.files(path = filenames[sub], all.files = F, full.names = T, include.dirs = T)
	     sink(subfiles)
	     for (f in 1: length(dirs)){
	        print(dirs[f])
	        print(sprintf('                        %s',list.files(dirs[f])))
	     }
	     sink()
           }





close(all)
close(fs)
close(subfiles)
close(allfiles)
close(runs)
