######################################################
###### Matching article ID/SS ID for behav + SS ######
######################################################
#setwd("~/Desktop/Desktop/Cleaned PREDICT data")
setwd('G:/My Drive/Graduate School/Research/Projects/SocialityConservationAnalyses/Data')
library("stringr")

#read in SS data
SS_001_500 			<- read.csv("Article Coding Database (IDs_ 001 - 500) - Study site data.csv")
SS_501_1000 		<- read.csv("Article Coding Database (IDs_ 501 - 1000) - Study site data.csv")
SS_1001_1500	 	<- read.csv("Article Coding Database (IDs_ 1001 - 1500) - Study site data.csv")
SS_1501_2000	 	<- read.csv("Article Coding Database (IDs_ 1501 - 2000) - Study site data.csv")
SS_2001_2500	 	<- read.csv("Article Coding Database (IDs_ 2001 - 2500) - Study site data.csv")
SS_2501_3000	 	<- read.csv("Article Coding Database (IDs_ 2501 - 3000) - Study site data.csv")
SS_3001_3500	 	<- read.csv("Article Coding Database (IDs_ 3001 - 3500) - Study site data.csv")
SS_3501_4000	 	<- read.csv("Article Coding Database (IDs_ 3501 - 4000) - Study site data.csv")

#reading in behavior data
behavior_001_500 			<- read.csv("Article Coding Database (IDs_ 001 - 500) - Behavior data.csv")
behavior_501_1000 		<- read.csv("Article Coding Database (IDs_ 501 - 1000) - Behavior data.csv")
behavior_1001_1500	 	<- read.csv("Article Coding Database (IDs_ 1001 - 1500) - Behavior data.csv")
behavior_1501_2000	 	<- read.csv("Article Coding Database (IDs_ 1501 - 2000) - Behavior data.csv")
behavior_2001_2500	 	<- read.csv("Article Coding Database (IDs_ 2001 - 2500) - Behavior data.csv")
behavior_2501_3000	 	<- read.csv("Article Coding Database (IDs_ 2501 - 3000) - Behavior data.csv")
behavior_3001_3500	 	<- read.csv("Article Coding Database (IDs_ 3001 - 3500) - Behavior data.csv")
behavior_3501_4000	 	<- read.csv("Article Coding Database (IDs_ 3501 - 4000) - Behavior data.csv")

#force col names
colnames(SS_3501_4000) 		<- colnames(SS_3001_3500) <- colnames(SS_2501_3000) <- colnames(SS_2001_2500) <- colnames(SS_1501_2000) <- colnames(SS_1001_1500) <- colnames(SS_501_1000) <- colnames(SS_001_500)
colnames(behavior_3501_4000)	<- colnames(behavior_3001_3500) <- colnames(behavior_2501_3000) <- colnames(behavior_2001_2500) <- colnames(behavior_1501_2000) <- colnames(behavior_1001_1500) <- colnames(behavior_501_1000) <- colnames(behavior_001_500)

#bind together
all_studysite		<- rbind.data.frame(SS_001_500, SS_501_1000, SS_1001_1500, SS_1501_2000, SS_2001_2500, SS_2501_3000, SS_3001_3500, SS_3501_4000)
all_behavior 		<- rbind.data.frame(behavior_001_500, behavior_501_1000, behavior_1001_1500, behavior_1501_2000, behavior_2001_2500, behavior_2501_3000, behavior_3001_3500, behavior_3501_4000)

behavior_article_ids	<- unique(all_behavior$Article.ID)
ss_article_ids		<- unique(all_studysite$Article.ID)
all_possible_article_ids <- 1:3990

all_possible_article_ids[!all_possible_article_ids%in%behavior_article_ids]
#missing article IDs based on SS data
ss_article_ids[!(ss_article_ids%in%behavior_article_ids)]

####################################################################
### Identify ID/SSID combos that are only present in SS or Behav ###
####################################################################
studysiteunique 	<- all_studysite[!duplicated(all_studysite[,c("Coder.Initials", "Article.ID","Study.Site.ID")]),]
studysiteIDfromSS <- as.character(factor(factor(studysiteunique$Coder.Initials):factor(studysiteunique$Article.ID):factor(studysiteunique$Study.Site.ID)))

behaviorunique 		<- all_behavior[!duplicated(all_behavior[,c("Coder.Initials", "Article.ID","Study.Site.ID")]),]
studysiteIDfrombehavior <- as.character(factor(factor(behaviorunique$Coder.Initials):factor(behaviorunique$Article.ID):factor(behaviorunique$Study.Site.ID)))
behaviorunique[is.na(studysiteIDfrombehavior),]

inSSnotBehavior <- studysiteIDfromSS[!studysiteIDfromSS %in% studysiteIDfrombehavior]
write.csv(inSSnotBehavior,"inSSnotBehavior.csv", row.names=F)

inBehaviornotSS <- studysiteIDfrombehavior[!studysiteIDfrombehavior %in% studysiteIDfromSS]
write.csv(inBehaviornotSS, "inBehaviornotSS.csv", row.names=F)
