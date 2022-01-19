setwd("G:/My Drive/Graduate School/Research/Projects/SocialityConservationAnalyses/Data")
library("stringr")

#read in SS data

SS_001_500old 		<- read.csv("SS_001_500.csv")
SS_501_1000old 		<- read.csv("SS_501_1000.csv")
SS_001_500verified 	<- read.csv("SS_001_500_verified.csv")
SS_501_1000verified <- read.csv("SS_501_1000_verified.csv")
SS_1001_1500	 	<- read.csv("SS_1001_1500.csv")
SS_1501_2000	 	<- read.csv("SS_1501_2000.csv")
SS_2001_2500	 	<- read.csv("SS_2001_2500.csv")
SS_2501_3000	 	<- read.csv("SS_2501_3000.csv")
SS_3001_3500	 	<- read.csv("SS_3001_3500.csv")
SS_3501_4000	 	<- read.csv("SS_3501_4000.csv")

#force col names

colnames(SS_3501_4000) <- colnames(SS_3001_3500) <- colnames(SS_2501_3000) <- colnames(SS_2001_2500) <- colnames(SS_1501_2000) <- colnames(SS_1001_1500) <- colnames(SS_501_1000old) <- colnames(SS_001_500old)

#bind together
oldformat_studysite	<- rbind.data.frame(SS_001_500old, SS_501_1000old, SS_1001_1500, SS_1501_2000, SS_2001_2500, SS_2501_3000, SS_3001_3500, SS_3501_4000)

#force col names for verified data
colnames(SS_001_500verified) <- colnames(SS_501_1000verified)

#bind together verified data
newformat_studysite <- rbind.data.frame(SS_001_500verified, SS_501_1000verified)

#create list of verified article IDs
verified_article_ids <- unique(newformat_studysite$Article.ID)

oldformat_studysite_notverified <- oldformat_studysite[!oldformat_studysite$Article.ID%in%verified_article_ids,]


#split study sites by small sites and large sites
install.packages("stringr")
library("stringr")

study_sites_split <- data.frame(str_split_fixed(as.character(oldformat_studysite_notverified$Location.of.study),",",n=2))
colnames(study_sites_split) <- c("small_site", "large_site")

#takes info from small study site column and places into large study site column if large site is empty
for(i in 1:dim(study_sites_split)[1]){
if(study_sites_split[i,]$large_site==""){
	study_sites_split[i,]$large_site <- study_sites_split[i,]$small_site
	study_sites_split[i,]$small_site <- ""
}}
head(study_sites_split, 50)

newformat_small_and_large <- newformat_studysite[,c(19,27)]

colnames(study_sites_split) <- colnames(newformat_small_and_large)
SS_combined <- rbind.data.frame(study_sites_split, newformat_small_and_large)

colnames(SS_combined)

unique_small_sites <- sort(unique(SS_combined$Location.of.small.study.site))
unique_large_sites <- sort(unique(SS_combined$Location.of.large.study.site))

#checking a specific case
oldformat_studysite_notverified[oldformat_studysite_notverified$Location.of.study=="Yasuní National Park",]

newformat_studysite[newformat_studysite$Location.of.large.study.site=="Yasuní National Park",]

write.csv(newformat_studysite, "newformat_studysite.csv")
write.csv(oldformat_studysite_notverified, "oldformat_studysite.csv")
write.csv(unique_small_sites, "unique_small_sites.csv", row.names=FALSE)
write.csv(unique_large_sites, "unique_large_sites.csv", row.names=FALSE)

#reading in behavior data

behavior_001_500 		<- read.csv("behavior_001_500.csv")
behavior_501_1000 		<- read.csv("behavior_501_1000.csv")
behavior_1001_1500	 	<- read.csv("behavior_1001_1500.csv")
behavior_1501_2000	 	<- read.csv("behavior_1501_2000.csv")
behavior_2001_2500	 	<- read.csv("behavior_2001_2500.csv")
behavior_2501_3000	 	<- read.csv("behavior_2501_3000.csv")
behavior_3001_3500	 	<- read.csv("behavior_3001_3500.csv")
behavior_3501_4000	 	<- read.csv("behavior_3501_4000.csv")

#force col names behavior
colnames(behavior_3501_4000) <- colnames(behavior_3001_3500) <- colnames(behavior_2501_3000) <- colnames(behavior_2001_2500) <- colnames(behavior_1501_2000) <- colnames(behavior_1001_1500) <- colnames(behavior_501_1000) <- colnames(behavior_001_500)

#bind together
all_behavior <- 	rbind.data.frame(behavior_001_500, behavior_501_1000, behavior_1001_1500, 						behavior_1501_2000, behavior_2001_2500, behavior_2501_3000, 									behavior_3001_3500, behavior_3501_4000)

#create a list of the oldformat nonverified IDs for SS
nonverified_article_ids <- unique(oldformat_studysite_notverified$Article.ID)

#combine nonverified article IDs with verified article ids
SS_article_IDs <- c(nonverified_article_ids, verified_article_ids)

behavior_article_ids <- unique( all_behavior$Article.ID)
max(behavior_article_ids, na.rm=TRUE)

all_possible_article_ids <- 1:3982

#missing article IDs based on SS data
SS_article_IDs[!(SS_article_IDs%in%behavior_article_ids)]
