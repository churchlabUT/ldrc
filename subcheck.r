dir = '/corral-repl/utexas/ldrc/'
subnames = c(Sys.glob(sprintf('%sldrc_*', dir)), Sys.glob(sprintf('%sH_*', dir)))

dir2 = '/corral-repl/utexas/ldrc/FREESURFER/'
fsnames = c(Sys.glob(sprintf('%sldrc_*', dir2)), Sys.glob(sprintf('%sH_*', dir2)))

sink('subs.txt')
print('Subjects in the main folder')
print(subnames)
print('---------------------------------------')
print('Subjects in the FREESURFER folder')
print(fsnames)
sink()



