#JEM Jan 2014
#For a cheat sheet, use the fidl_master.sh script
#Fidl file maker, uses the output Events files from the mk_SC_onsets.py and mk_SST_onsets.py

#To make the fidl_subs.txt file so you can edit who is run type: 'Rscript fidl_maker.r setsubs {the name of the group you want to run}'
#Example, To make the fidl_subs.txt file for first austin you would type: 'Rscript fidl_maker.r 1 A_first'
#Groups: Austin second = 'A_second', all = 'All', houston = 'Houston', third = 'A_third, Controls = 'A_controls', Intervention2 = 'A2_first'

#To make the actual fidl files type: 'Rscript fidl_maker.r {SC or SST} {Output folder}'
#Example, To make the SC fidl_files for the subs in the fidl_subs.txt AND you want the output in the intervention_second folder you would type: 'Rscript fidl_maker.r SC Intervention_second'
#Example, To make the SST fidl files for subs in the fidl_subs.txt AND output to intervention_second folder you would type: 'Rscript fidl_maker.r SST Intervention_second'
#Ouput folders: controls = 'Controls', Intervention first = 'Intervention', third = 'Intervention_third', houston = 'Houston', Intervention 2 = 'Intervention2'

#New function. Add a r at the end of the call 'Rscript fidl_maker.r SC r' to remove the runs with bad motion. Put a k to keep the runs
#with bad motion. 


args = commandArgs(trailingOnly = TRUE)

if (args[1] == 'setsubs'){

	dir = '/corral-repl/utexas/ldrc/'


	if (args[2] == 'All'){
		filenames = c(Sys.glob(sprintf('%sldrc_*', dir)), Sys.glob(sprintf('%sldrc2_*', dir)), Sys.glob(sprintf('%sH_*', dir)), Sys.glob(sprintf('%sPHILIPS/H_*', dir)))
	} else if (args[2] == 'A_first'){
		filenames = Sys.glob(sprintf('%sldrc_*', dir))
		filenames = filenames[grep('second', filenames, invert = T)] #'invert=T' means don't include these
		filenames = filenames[grep('third', filenames, invert = T)]
		filenames = filenames[grep('_c_', filenames, invert = T)]
	} else if (args[2] == 'A_controls'){
		filenames = Sys.glob(sprintf('%sldrc_c_*', dir))
	} else if (args[2] == 'A_second'){
		filenames = Sys.glob(sprintf('%sldrc_*second', dir))
	} else if (args[2] == 'A_third'){
		filenames = Sys.glob(sprintf('%sldrc_*third', dir))
	} else if (args[2] == 'Houston'){
		filenames = Sys.glob(sprintf(c('%sH_*', '%sLD*'), dir))
	} else if (args[2] == 'A2_first'){
		filenames = Sys.glob(sprintf('%sldrc2_*', dir))
	} 
	write(filenames, sprintf('%sSCRIPTS/fidl_subs.txt', dir))

} else if (args[1] == 'SC'){
	
	dir = '/corral-repl/utexas/ldrc/'	
	filenames = scan(sprintf('%sSCRIPTS/fidl_subs.txt', dir), what = character())
	filenames = filenames[grep('test', filenames, invert=T)] #remove MAR's mock data
	filenames = filenames[grep('.gz', filenames, invert=T)] #remove fake subjects
	subtype = args[2]

	for (i in 1: length(filenames)){
		print(filenames[i])
		name.split = unlist(strsplit(filenames[i], split = '/'))
		sub = name.split[length(name.split)]
		
		
		#Find behavior folder
		behav.dir = list.dirs(filenames[i])[grepl("(?=.*behav)(?=.*SC)", list.dirs(filenames[i]), perl = TRUE)]
		behav.dir = behav.dir[grep('NEW', behav.dir, invert = T)]
		behav.dir = behav.dir[grep('x_', behav.dir, invert = T)]
		behav.dir = behav.dir[grep('bad_', behav.dir, invert = T)]
		if (args[3] == 'r'){
			behav.dir = behav.dir[grep('m_', behav.dir, invert = T)] #Takes out runs with bad motion. 
		}
		#find behavioral files
		behav.files = list.files(behav.dir, include.dirs = F, full.names = T, all.files = F)
		
		#Find number of frames, use dvars file
		qa.dir = list.dirs(filenames[i])[grepl("(?=.*QA)(?=.*SC)", list.dirs(filenames[i]), perl = TRUE)]
		qa.dir = qa.dir[grep('NEW', qa.dir, invert = T, ignore.case = T)]
		qa.dir = qa.dir[grep('x_', qa.dir, invert = T, ignore.case = T)]
		qa.dir = qa.dir[grep('bad', qa.dir, invert = T, ignore.case = T)]
		if (args[3] == 'r'){
			qa.dir = qa.dir[grep('m_', qa.dir, invert = T)] #Takes out runs with bad motion. 
		}
		dvars.file = list.files(qa.dir, include.dirs = F, full.names = T, all.files = F)[grep('dvars.txt', list.files(qa.dir, include.dirs = F, full.names = T, all.files = F))]
		
		
		events = behav.files[grep('Events', behav.files)]
		runs = length(events)
		if (runs == 0){
			print(sprintf('No runs for %s', sub))
			next
		}
	
		output = c()
		frames = c()
		trialnum = c() #the number of trials. becomes a vector like: 33 33 33, which means 33 trials for 3 runs.
		tr = 2 #seconds
		for (r in 1:runs){
			
			dat = read.table(events[r],  header = T, na.strings = ' ', sep = '\t',  fill = T)
			dvars = read.table(dvars.file[r], header = T, na.strings = ' ', sep = '\t',  fill = T)
			
			output = rbind(output, dat)
			frames = c(frames, dim(dvars)[1])
			trialnum = c(trialnum, dim(dat)[1])
			output[is.na(output)] = ''
		}
		
		frames = frames + 1 #dvars starts counting at 0, the true number of frames in each run.
	        
                # makes the frame numbers continous, if more than one run, and changes it to account for time across run
		if (length(trialnum) > 1){
			for (a in (trialnum[1]+1):(trialnum[1] + trialnum[2])){
			
				output$X2[a] = output$X2[a] + frames[1]*tr
			}
		}		
		
		if (length(trialnum) > 2){
			for (a in (trialnum[1] + trialnum[2] + 1):sum(trialnum)){
			
				output$X2[a] = output$X2[a] + (frames[1]*tr + frames[2]*tr)
			}
		}
		colnames(output)[1] = 2
		
		#Check for sequential timing - Gather offending row numbers
		remove = c()
		for (row in 2:dim(output)[1]){
			if (output[row, 1] > output[row-1, 1]){
				next
			} else {
				warning(sprintf('*** Timings are not in sequential order. Error: Sub - %s, line - %d***', sub, row))
				write(sprintf('*** Timings are not in sequential order. Error: SC: Sub - %s, line - %d***', sub, row), sprintf('%sSCRIPTS/fidl_error.txt', dir), append=T)
				remove = c(remove, row-1)
				#Check and see if any more are out of sync by going backwards and comparing to the row that originally flagged
				for (r in (row-2):(row-5)){
					if (output[row,1] < output[r,1]){
						remove = c(remove, r)
					}
				}
			}
		}
		
		#Remove the rows
		if (!is.null(remove)){
			warning(sprintf('**removing line - %d***', remove))
			output = output[-remove,]
			warning('***No more timing issues***')
		}
		
		
		if (args[3] == 'r'){
			#With motion censoring, use if removing the runs with bad motion
			write.table(output, sprintf('%sWU_analysis/EVENT_FILES/NoMovement/%s/%s_SC_%druns.fidl.txt', dir, subtype, sub, runs), sep = '\t', row.names = F, quote = F)
		} else if (args[3] == 'k'){
			#Without motion censoring, use if keeping the bad motion runs
			write.table(output, sprintf('%sWU_analysis/EVENT_FILES/%s/%s_SC_%druns.fidl.txt', dir, subtype, sub, runs), sep = '\t', row.names = F, quote = F)
		}
		
	
	}
	
	print('done!')
	
} else if (args[1] == 'SST'){

	dir = '/corral-repl/utexas/ldrc/'	
	filenames = scan(sprintf('%sSCRIPTS/fidl_subs.txt', dir), what = character())
	subtype = args[2]

	for (i in 1: length(filenames)){
		print(filenames[i])
		name.split = unlist(strsplit(filenames[i], split = '/'))
		sub = name.split[length(name.split)]
		
		
		#Find behavior folder
		behav.dir = list.dirs(filenames[i])[grepl("(?=.*behav)(?=.*SST)", list.dirs(filenames[i]), perl = TRUE)]
		behav.dir = behav.dir[grep('NEW', behav.dir, invert = T)]
		behav.dir = behav.dir[grep('x_', behav.dir, invert = T)] #Takes out runs with bad behaviors
		behav.dir = behav.dir[grep('bad_', behav.dir, invert = T)]
		if (args[3] == 'r'){
			behav.dir = behav.dir[grep('m_', behav.dir, invert = T)] #Takes out runs with bad motion. 
		}
		#find behavioral files
		behav.files = list.files(behav.dir, include.dirs = F, full.names = T, all.files = F)
	
		#Find number of frames
		qa.dir = list.dirs(filenames[i])[grepl("(?=.*QA)(?=.*SST)", list.dirs(filenames[i]), perl = TRUE)]
		qa.dir = qa.dir[grep('NEW', qa.dir, invert = T, ignore.case = T)]
		qa.dir = qa.dir[grep('x_', qa.dir, invert = T, ignore.case = T)]
		qa.dir = qa.dir[grep('bad', qa.dir, invert = T, ignore.case = T)]
		if (args[3] == 'r'){
			qa.dir = qa.dir[grep('m_', qa.dir, invert = T)] #Takes out runs with bad motion. 
		}
		dvars.file = list.files(qa.dir, include.dirs = F, full.names = T, all.files = F)[grep('dvars.txt', list.files(qa.dir, include.dirs = F, full.names = T, all.files = F))]
		
		
		events = behav.files[grep('Events', behav.files)]
		runs = length(events)
		if (runs == 0){
			print(sprintf('No runs for %s', sub))
			next
		}
		
		output = c()
		frames = c()
		trialnum = c()
		tr = 2 #seconds
		for (r in 1:runs){
			
			dat = read.table(events[r],  header = T, na.strings = ' ', sep = '\t',  fill = T)
			dvars = read.table(dvars.file[r], header = T, na.strings = ' ', sep = '\t',  fill = T)
			
			output = rbind(output, dat)
			output[is.na(output)] = ''
			frames = c(frames, dim(dvars)[1])
			trialnum = c(trialnum, dim(dat)[1])
		}
		
		frames = frames + 1 #dvars undershoots the frame size by 1
	
		if (length(trialnum) > 1){
			for (a in (trialnum[1]+1):sum(trialnum)){
			
				output$X2[a] = output$X2[a] + frames[1]*tr
			}
		}		

		colnames(output)[1] = 2
		
		#Check for sequential timing - Gather offending row numbers
		remove = c()
		for (row in 2:dim(output)[1]){
			if (output[row, 1] > output[row-1, 1]){
				next
			} else {
				warning(sprintf('*** Timings are not in sequential order. Error: Sub - %s, line - %d***', sub, row))
				write(sprintf('*** Timings are not in sequential order. Error: SST: Sub - %s, line - %d***', sub, row), sprintf('%sSCRIPTS/fidl_error.txt', dir), append=T)
				remove = c(remove, row-1)
				#Check and see if any more are out of sync by going backwards and comparing to the row that originally flagged
				for (r in (row-2):(row-5)){
					if (output[row,1] < output[r,1]){
						remove = c(remove, r)
					}
				}
			}
		}
		
		#Remove the rows
		if (!is.null(remove)){
			warning(sprintf('**removing line - %d***', remove))
			output = output[-remove,]
			warning('***No more timing issues***')
		}
		
		
		if (args[3] == 'r'){
			#With motion censoring, use if removing runs with bad motion
			write.table(output, sprintf('%sWU_analysis/EVENT_FILES/NoMovement/%s/%s_SST_%druns.fidl.txt', dir, subtype, sub, runs), sep = '\t', row.names = F, quote = F)
		} else if (args[3] == 'k'){
			#Without motion censoring, use if keeping runs with bad motion
			write.table(output, sprintf('%sWU_analysis/EVENT_FILES/%s/%s_SST_%druns.fidl.txt', dir, subtype, sub, runs), sep = '\t', row.names = F, quote = F)
		}
	
	}
	
	print('done!')
}
