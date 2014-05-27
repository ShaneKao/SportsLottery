AL.Batting=read.table("AL.Batting.txt",sep=",",header=TRUE,stringsAsFactors=FALSE)
AL.Pitching=read.table("AL.Pitching.txt",sep=",",header=TRUE,stringsAsFactors=FALSE)
AL.Fielding=read.table("AL.Fielding.txt",sep=",",header=TRUE,stringsAsFactors=FALSE)
NL.Batting=read.table("NL.Batting.txt",sep=",",header=TRUE,stringsAsFactors=FALSE)
NL.Pitching=read.table("NL.Pitching.txt",sep=",",header=TRUE,stringsAsFactors=FALSE)
NL.Fielding=read.table("NL.Fielding.txt",sep=",",header=TRUE,stringsAsFactors=FALSE)
AL.Batting.2014=read.table("AL.Batting.2014.txt",sep=",",header=TRUE,stringsAsFactors=FALSE)
AL.Pitching.2014=read.table("AL.Pitching.2014.txt",sep=",",header=TRUE,stringsAsFactors=FALSE)
AL.Fielding.2014=read.table("AL.Fielding.2014.txt",sep=",",header=TRUE,stringsAsFactors=FALSE)
NL.Batting.2014=read.table("NL.Batting.2014.txt",sep=",",header=TRUE,stringsAsFactors=FALSE)
NL.Pitching.2014=read.table("NL.Pitching.2014.txt",sep=",",header=TRUE,stringsAsFactors=FALSE)
NL.Fielding.2014=read.table("NL.Fielding.2014.txt",sep=",",header=TRUE,stringsAsFactors=FALSE)

AL=cbind(AL.Batting,AL.Pitching[,-1],AL.Fielding[,-1])
NL=cbind(NL.Batting,NL.Pitching[,-1],NL.Fielding[,-1])
AL.year=rep(c("09","10","11","12","13"),c(14,14,14,14,15))
NL.year=rep(c("09","10","11","12","13"),c(16,16,16,16,15))
AL$Tm=paste(AL.year,AL$Tm)
NL$Tm=paste(NL.year,NL$Tm)

AL.2014=cbind(AL.Batting.2014,AL.Pitching.2014[,-1],AL.Fielding.2014[,-1])
NL.2014=cbind(NL.Batting.2014,NL.Pitching.2014[,-1],NL.Fielding.2014[,-1])
AL.2014$Tm=paste(rep("14",15),AL.2014$Tm)
NL.2014$Tm=paste(rep("14",15),NL.2014$Tm)
AL.TOTAL=rbind(AL,AL.2014)
NL.TOTAL=rbind(NL,NL.2014)
##############################################################
AL.model=kmeans(AL.TOTAL[,-1],10,nstart=length(AL.TOTAL$Tm))
AL.cluster.team=data.frame(AL.TOTAL$Tm,AL.model$cluster)
NL.model=kmeans(NL.TOTAL[,-1],10,nstart=length(NL.TOTAL$Tm))
NL.cluster.team=data.frame(NL.TOTAL$Tm,10+NL.model$cluster)
##################################################################
library(XML)
library(RCurl)
webpage<-getURL("http://www.baseball-reference.com/teams/TOR/2014-schedule-scores.shtml")
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
tablelines <- xpathSApply(pagetree, "//tr[@class='']", xmlValue) 
strsplit(tablelines[2]," ",fixed=FALSE, useBytes = TRUE)
strsplit("a b c", " ")
sub("\",replacement=" ",tablelines[1])  
nchar(tablelines[1])