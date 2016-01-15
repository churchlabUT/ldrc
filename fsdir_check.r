dir = '/corral-repl/utexas/ldrc/FREESURFER/'

filenames = Sys.glob(sprintf('%sldrc*', dir))

for (i in 1:length(filenames)){
     dirs = list.files(path = filenames[i], all.files = T)
     print(filenames[i])
     print(dirs)
}

