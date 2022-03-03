######################################################
###### Matching article ID/SS ID for behav + SS ######
######################################################
#setwd("~/Desktop/Desktop/Cleaned PREDICT data")
setwd('G:/My Drive/Graduate School/Research/Projects/SocialityConservationAnalyses/Data')
library("stringr")

#read in SS data
SS_001_500old 		<- read.csv("SS_001_500.csv")
SS_501_1000old 		<- read.csv("SS_501_1000.csv")
SS_001_500verified 	<- read.csv("SS_001_500_verified.csv")
SS_501_1000verified 	<- read.csv("SS_501_1000_verified.csv")
SS_1001_1500	 	<- read.csv("SS_1001_1500.csv")
SS_1501_2000	 	<- read.csv("SS_1501_2000.csv")
SS_2001_2500	 	<- read.csv("SS_2001_2500.csv")
SS_2501_3000	 	<- read.csv("SS_2501_3000.csv")
SS_3001_3500	 	<- read.csv("SS_3001_3500.csv")
SS_3501_4000	 	<- read.csv("SS_3501_4000.csv")

#reading in behavior data
behavior_001_500 			<- read.csv("behavior_001_500.csv")
behavior_501_1000 		<- read.csv("behavior_501_1000.csv")
behavior_1001_1500	 	<- read.csv("behavior_1001_1500.csv")
behavior_1501_2000	 	<- read.csv("behavior_1501_2000.csv")
behavior_2001_2500	 	<- read.csv("behavior_2001_2500.csv")
behavior_2501_3000	 	<- read.csv("behavior_2501_3000.csv")
behavior_3001_3500	 	<- read.csv("behavior_3001_3500.csv")
behavior_3501_4000	 	<- read.csv("behavior_3501_4000.csv")

#force col names
colnames(SS_3501_4000) 		<- colnames(SS_3001_3500) <- colnames(SS_2501_3000) <- colnames(SS_2001_2500) <- colnames(SS_1501_2000) <- colnames(SS_1001_1500) <- colnames(SS_501_1000old) <- colnames(SS_001_500old)
colnames(behavior_3501_4000)	<- colnames(behavior_3001_3500) <- colnames(behavior_2501_3000) <- colnames(behavior_2001_2500) <- colnames(behavior_1501_2000) <- colnames(behavior_1001_1500) <- colnames(behavior_501_1000) <- colnames(behavior_001_500)
colnames(SS_001_500verified)	<- colnames(SS_501_1000verified)

#bind together
oldformat_studysite	<- rbind.data.frame(SS_001_500old, SS_501_1000old, SS_1001_1500, SS_1501_2000, SS_2001_2500, SS_2501_3000, SS_3001_3500, SS_3501_4000)
all_behavior 		<- rbind.data.frame(behavior_001_500, behavior_501_1000, behavior_1001_1500, behavior_1501_2000, behavior_2001_2500, behavior_2501_3000, 									behavior_3001_3500, behavior_3501_4000)
newformat_studysite	<- rbind.data.frame(SS_001_500verified, SS_501_1000verified)

#create list of verified article IDs
verified_article_ids <- unique(newformat_studysite$Article.ID)
oldformat_studysite_notverified <- oldformat_studysite[!oldformat_studysite$Article.ID%in%verified_article_ids,]

#create a list of the oldformat nonverified IDs for SS
nonverified_article_ids <- unique(oldformat_studysite_notverified$Article.ID)

#combine nonverified article IDs with verified article ids
SS_article_IDs <- c(nonverified_article_ids, verified_article_ids)

behavior_article_ids <- unique(all_behavior$Article.ID)
max(behavior_article_ids, na.rm=TRUE)

all_possible_article_ids <- 1:3990

#missing article IDs based on SS data
SS_article_IDs[!(SS_article_IDs%in%behavior_article_ids)]

####################################################################
### Identify ID/SSID combos that are only present in SS or Behav ###
####################################################################
studysiteunique 	<- oldformat_studysite[!duplicated(oldformat_studysite[,c("Article.ID","Study.Site.ID")]),]
studysiteIDfromSS <- as.character(factor(factor(studysiteunique$Article.ID):factor(studysiteunique$Study.Site.ID)))

behaviorunique 		<- all_behavior[!duplicated(all_behavior[,c("Article.ID","Study.Site.ID")]),]
studysiteIDfrombehavior <- as.character(factor(factor(behaviorunique$Article.ID):factor(behaviorunique$Study.Site.ID)))

inSSnotBehavior <- studysiteIDfromSS[!studysiteIDfromSS %in% studysiteIDfrombehavior]
write.csv(inSSnotBehavior,"inSSnotBehavior.csv", row.names=F)

inBehaviornotSS <- studysiteIDfrombehavior[!studysiteIDfrombehavior %in% studysiteIDfromSS]
write.csv(inBehaviornotSS, "inBehaviornotSS.csv", row.names=F)
