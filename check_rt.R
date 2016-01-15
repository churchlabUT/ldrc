#read in onset information an look at RT within subject.  This code can also be adapted to look at multiple subjects

#This grabs data file (input)
args=commandArgs(trailingOnly=TRUE)

filepth=args[1]
outdir=dirname(filepth)
dat=read.table(filepth, header=TRUE, sep='\t', na.string='NaN')

#create data with no reps per trial
len_dat=length(dat$TrialNum)
dat_rep=c(0, dat$TrialNum[2:len_dat]==dat$TrialNum[1:(len_dat-1)])

dat_norep=dat[dat_rep==0,]


#We can get a mean RT for each trial
figoutfile_name=sprintf('%s/summary_plots.png', outdir)
png(filename=figoutfile_name, width=1000,height=1000, res=150)

par(mfrow=c(3,1))
minrt=min(dat$RT, na.rm=TRUE)
maxrt_bit=max(dat$RT, na.rm=TRUE)+1.4
boxplot(RT~TrialNum, data=dat, col=dat$Cond, border=dat$Cond, ylab="RT", xlab="trial num", main="RT dist per trial", ylim=c(minrt,maxrt_bit))
legend(0, maxrt_bit, c("cond 1", "cond 2", "cond 3", "cond 4"), col=c(1:4), pch=rep(15,4), horiz=TRUE)


boxplot(RT~Cond, data=dat_norep, border=c(1:4), col=c(1:4), ylab="RT", xlab="Condition", main="RT dist per trial type")
 

corname=dat_norep$correct
corname[dat_norep$correct==0]='incorrect'
corname[dat_norep$correct==1]='correct'
corname[dat_norep$correct==3]='bad resp/RT'
corname[dat_norep$correct==4]='no response'

tab_vec=c(table(corname, dat_norep$Cond))
resp_names=names(table(corname))
nresp=length(resp_names)

bp=barplot(tab_vec, col=rep(1:4, each=nresp),axisnames=FALSE, names.arg=rep(resp_names, 4), ylab="number of trials",main="Number of trials for each response type" )
text(bp, par("usr")[3], labels=rep(resp_names,4), srt=45, adj=c(1,1,1,1), xpd=TRUE)
dev.off()
