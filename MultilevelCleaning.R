setwd("C:/Users/janac/Documents/Records - School/Internships/PREdiCTers/Behavior Data")
#Read in all of the behavioral data & combine together 
behavior001_500	<- read.csv('Article Coding Database (IDs_ 001 - 500) - Behavior data.csv', stringsAsFactors = FALSE) 
behavior501_1000	<- read.csv('Article Coding Database (IDs_ 501 - 1000) - Behavior data.csv', stringsAsFactors = FALSE) 
behavior1001_1500	<- read.csv('Article Coding Database (IDs_ 1001 - 1500) - Behavior data.csv', stringsAsFactors = FALSE) 
behavior1501_2000	<- read.csv('Article Coding Database (IDs_ 1501 - 2000) - Behavior data.csv', stringsAsFactors = FALSE) 
behavior2001_2500	<- read.csv('Article Coding Database (IDs_ 2001 - 2500) - Behavior data.csv', stringsAsFactors = FALSE) 
behavior2501_3000	<- read.csv('Article Coding Database (IDs_ 2501 - 3000) - Behavior data.csv', stringsAsFactors = FALSE) 
behavior3001_3500	<- read.csv('Article Coding Database (IDs_ 3001 - 3500) - Behavior data.csv', stringsAsFactors = FALSE) 
behavior3501_4000	<- read.csv('Article Coding Database (IDs_ 3501 - 4000) - Behavior data.csv', stringsAsFactors = FALSE) 
 
colnames(behavior3501_4000) <- colnames(behavior3001_3500) <- colnames(behavior2501_3000) <- colnames(behavior1501_2000) <- colnames(behavior501_1000) <- colnames(behavior2001_2500) <- colnames(behavior1001_1500) <- colnames(behavior001_500) 
 
behaviorAll	<- rbind(behavior001_500, behavior501_1000, behavior1001_1500, behavior1501_2000, 
                     behavior2001_2500, behavior2501_3000, behavior3001_3500, behavior3501_4000) 

dim(behaviorAll)

head(behaviorAll)

table(behaviorAll$species)

behaviorAll[behaviorAll$Genus=="",]

behaviorAll$sciName	<- ifelse(behaviorAll$subspecies == '', 
		paste(behaviorAll$Genus, behaviorAll$species, sep = '_'),
		paste(behaviorAll$Genus, behaviorAll$species, behaviorAll$subspecies, sep = '_'))

subset<-behaviorAll[behaviorAll$sciName=="Ateles_paniscus",]

dim(subset)

table(subset$Type.of.data)

subset[subset$Type.of.data=="Not a complicated species",c(1,3,4,6,7,8,92)]