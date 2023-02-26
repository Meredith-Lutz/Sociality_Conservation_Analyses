########################################
########################################
##### Heat Map Construction by Arianna #####
########################################
########################################



##**Data required
##*1)min, max, mean, median of females and males **completed** --> split with general **General**
##*2) social organization --> simple **completed**
##*3) dispersal: a) female b) secondary female c) male  d) secondary male **completed** --> split with general **General = NEED HELP WITH GENERAL IFELSE FUNCTION**
##*4) allomaternal care --> only coded for "yes" **completed**
##*5) social learnign --> only coded for "yes" **completed**
##*6) intergroup encounter studies --> only coded for "yes" **completed**
##*7) activity budget:  a) % social b) % grooming c) % feeding d) rate of aggresstion  e) submission --> split with general (anything that isn't blank) (confirm) **General = NEED HELP WITH GENERAL IFELSE FUNCTION**
##*8) feeding data:  a) specific data (plant reproductive parts, fungi, insect, foliage)  b) general (anything that isn't blank) **General = NEED HELP WITH GENERAL IFELSE FUNCTION**
##*9) home range size 

#**Starting point*
  
setwd("C:/Users/arian/OneDrive/Desktop/PREdiCT")

library(ape)

behavior001_500	<- read.csv('Article Coding Database (IDs_ 001 - 500) - Behavior data.csv', stringsAsFactors = FALSE)
behavior501_1000	<- read.csv('Article Coding Database (IDs_ 501 - 1000) - Behavior data.csv', stringsAsFactors = FALSE)
behavior1001_1500	<- read.csv('Article Coding Database (IDs_ 1001 - 1500) - Behavior data.csv', stringsAsFactors = FALSE)
behavior1501_2000	<- read.csv('Article Coding Database (IDs_ 1501 - 2000) - Behavior data.csv', stringsAsFactors = FALSE)
behavior2001_2500	<- read.csv('Article Coding Database (IDs_ 2001 - 2500) - Behavior data.csv', stringsAsFactors = FALSE)
behavior2501_3000	<- read.csv('Article Coding Database (IDs_ 2501 - 3000) - Behavior data.csv', stringsAsFactors = FALSE)
behavior3001_3500	<- read.csv('Article Coding Database (IDs_ 3001 - 3500) - Behavior data.csv', stringsAsFactors = FALSE)
behavior3501_4000	<- read.csv('Article Coding Database (IDs_ 3501 - 4000) - Behavior data.csv', stringsAsFactors = FALSE)

lumper		<- read.csv('Taxonomy Conversion - Lumper Taxonomy Conversion.csv', stringsAsFactors = FALSE)
IUCN_tax		<- read.csv('Taxonomy Conversion - IUCN Taxonomy Conversion.csv', stringsAsFactors = FALSE)
TenKTrees		<- read.csv('Taxonomy Conversion - 10K Trees Taxonomy Conversion.csv', stringsAsFactors = FALSE)

colnames(behavior3501_4000) <- colnames(behavior3001_3500) <- colnames(behavior2501_3000) <- colnames(behavior1501_2000) <- colnames(behavior501_1000) <- colnames(behavior2001_2500) <- colnames(behavior1001_1500) <- colnames(behavior001_500)

behaviorAll	<- rbind(behavior001_500, behavior501_1000, behavior1001_1500, behavior1501_2000,
                     behavior2001_2500, behavior2501_3000, behavior3001_3500, behavior3501_4000) #21329 lines

behaviorAll$sciName	<- ifelse(behaviorAll$species == '' & behaviorAll$subspecies == '', behaviorAll$Genus,
                              ifelse(behaviorAll$subspecies == '', paste(behaviorAll$Genus, '_', behaviorAll$species, sep = ''),
                                     paste(behaviorAll$Genus, '_', behaviorAll$species, '_', behaviorAll$subspecies, sep = '')))

behaviorAll			<- merge(behaviorAll, lumper, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
behaviorAll			<- merge(behaviorAll, IUCN_tax, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
behaviorAll			<- merge(behaviorAll, TenKTrees, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 


##**summarizing by species*
social_organization <- aggregate(behaviorAll$Social.organization, by= list(behaviorAll$sciName), FUN = length)
#group 1 = species 
#x = # of line we have for that species that are not blank

#for combined variable, create a new variable if/else, 
#if the first isn't blank then, if it is blank then move forward 
#until you find a blank, if all of them call the variable 0 (1 = has values) -> general column for 
#specific data per column use aggregate () as done above
#watch out for numerical vs categorical variables  

#**Allomaternal care*
allomaternal_care <- aggregate(behaviorAll$Allomaternal.care, by= list(behaviorAll$sciName), FUN = length)

#**social learning, only yes*
#df[df$var1 == 'value', ]
behaviorAll <- behaviorAll[behaviorAll$Social.learning == 'Yes',] #selecting only for "yes"
social_learning <- aggregate(behaviorAll$Social.learning, by= list(behaviorAll$sciName), FUN = length)
#need to change behaviorAll back to the original, otherwise I have less variables 

#**Intergroup encounter study*
behaviorAll <- behaviorAll[behaviorAll$Intergroup.encounter.study. == 'Yes',] #selecting only for "yes"
intergroup_encounter_study <- aggregate(behaviorAll$Intergroup.encounter.study., by= list(behaviorAll$sciName), FUN = length)


#**mean/median/min/max_adult_males* 
behaviorAll$Aggregatemale <- ifelse(is.na(behaviorAll$Mean.adult.males) == TRUE & is.na(behaviorAll$Median.adult.males) == TRUE & is.na(behaviorAll$Min...adult.males) == TRUE & is.na(behaviorAll$Max...adult.males) ==TRUE, 0,1)
#0 are missing 
#1 has at least 1 of the variables
                                   
table(behaviorAll$Aggregatemale) 
aggregare_male <- aggregate(behaviorAll$Aggregatemale, by= list(behaviorAll$sciName), FUN = sum)


#For general is.na == TRUE for all male, females, individual (12 totals:mean, max, median, min)
#For individual follow behaviorAll$Aggregatemale for females and individuals --> 4 is.na


#**mean/median/min/max_adult_females*
behaviorAll$Aggregatefemale <- ifelse(is.na(behaviorAll$Mean.adult.females) == TRUE & is.na(behaviorAll$Median.adult.females) == TRUE & is.na(behaviorAll$Min...adult.females) == TRUE & is.na(behaviorAll$Max...adult.females) ==TRUE, 0,1)
table(behaviorAll$Aggregatefemale)
aggregare_female <- aggregate(behaviorAll$Aggregatefemale, by= list(behaviorAll$sciName), FUN = sum)


#**mean/median/min/max_individuals*
behaviorAll$Aggregateindividual <- ifelse(is.na(behaviorAll$Mean.individuals) == TRUE & is.na(behaviorAll$Median.individuals) == TRUE & is.na(behaviorAll$Min...of.individuals) == TRUE & is.na(behaviorAll$Max...of.individuals) ==TRUE, 0,1)
table(behaviorAll$Aggregateindividual)
aggregare_individual <- aggregate(behaviorAll$Aggregateindividual, by= list(behaviorAll$sciName), FUN = sum)


#**general mean/median/min/max*

behaviorAll$Aggregategeneral <- ifelse(is.na(behaviorAll$Mean.adult.males) == TRUE & is.na(behaviorAll$Median.adult.males) == TRUE & is.na(behaviorAll$Min...adult.males) == TRUE & is.na(behaviorAll$Max...adult.males) ==TRUE & is.na(behaviorAll$Mean.adult.females) == TRUE & is.na(behaviorAll$Median.adult.females) == TRUE & is.na(behaviorAll$Min...adult.females) == TRUE & is.na(behaviorAll$Max...adult.females) ==TRUE &is.na(behaviorAll$Mean.individuals) == TRUE & is.na(behaviorAll$Median.individuals) == TRUE & is.na(behaviorAll$Min...of.individuals) == TRUE & is.na(behaviorAll$Max...of.individuals) ==TRUE, 0,))



behaviorAll$Aggregate_general <- ifelse(is.na(behaviorAll$Mean.individuals) == TRUE & is.na(behaviorAll$Mean.adult.females)== TRUE &is.na(behaviorAll$Mean.adult.males)== TRUE & is.na(behaviorAll$Median.individuals)== TRUE & is.na(behaviorAll$Median.adult.males)== TRUE & is.na(behaviorAll$Median.adult.females)== TRUE & is.na(behaviorAll$Min...of.individuals)== TRUE & is.na(behaviorAll$Min...adult.males)== TRUE & is.na(behaviorAll$Min...adult.females)== TRUE & is.na(behaviorAll$Max...of.individuals)== TRUE & is.na(behaviorAll$Max...adult.males)== TRUE & is.na(behaviorAll$Max...adult.females)== TRUE, 0,1)
View(behaviorAll$Aggregate_general)
table(behaviorAll$Aggregate_general)

aggregate_general <- aggregate(behaviorAll$Aggregate_general, by= list(behaviorAll$sciName), FUN = sum)
                                       
#**Dispersal*
##*dispersal: a) female b) secondary female c) male  d) secondary male 
behaviorAll$Aggregatefem_dispersal <- ifelse(is.na(behaviorAll$Female.dispersal) == TRUE, 1,0)
aggregate_fem_dispersal <- aggregate(behaviorAll$Aggregatefem_dispersal, by= list(behaviorAll$sciName), FUN = sum)  

behaviorAll$Aggregatefemsec_dispersal <- ifelse(is.na(behaviorAll$Female.secondary.dispersal) == TRUE, 1,0)
aggregate_femsec_dispersal <- aggregate(behaviorAll$Aggregatefemsec_dispersal, by= list(behaviorAll$sciName), FUN = sum)  

behaviorAll$Aggregatemale_dispersal <- ifelse(is.na(behaviorAll$Male.dispersal) == TRUE, 1,0)
aggregate_male_dispersal <- aggregate(behaviorAll$Aggregatemale_dispersal, by= list(behaviorAll$sciName), FUN = sum)  

behaviorAll$Aggregatemalesec_dispersal <- ifelse(is.na(behaviorAll$Male.secondary.dispersal) == TRUE, 1,0)
aggregate_malesec_dispersal <- aggregate(behaviorAll$Aggregatemalesec_dispersal, by= list(behaviorAll$sciName), FUN = sum)  


behaviorAll$Aggregate_dispersal <- ifelse(is.na(behaviorAll$Female.dispersal) == TRUE & is.na(behaviorAll$Female.secondary.dispersal) == TRUE & is.na(behaviorAll$Male.dispersal) == TRUE & is.na(behaviorAll$Male.secondary.dispersal) == TRUE, 0, 1)
aggregate_dispersalgeneral <- aggregate(behaviorAll$Aggregate_dispersal, by= list(behaviorAll$sciName), FUN = sum)


#**HOW TO MERGE SEASONAL DATA WITH SPECIES ID COLUMN** -> need to merge sciname colum

#**mergin g csv**
library("dplyr")                                   
library("plyr")                                     
library("readr")



library(dplyr)
library(readr)

merge(x, y, # Data frames or objects to be coerced
      by = intersect(names(x), names(y)), # Columns used for merging
      by.x = by, by.y = by, # Columns used for merging
      all = FALSE, # If TRUE, all.x = TRUE and all.y = TRUE
      all.x = all, all.y = all, # If TRUE, adds rows for each row in x (y) that not match a row in y (x).
      sort = TRUE, # Whether to sort the output by the 'by' columns
      suffixes = c(".x",".y"), # Suffixes for creating unique column names
      no.dups = TRUE, # Whether to avoid duplicated column names appending more suffixes or not
      incomparables = NULL, # How to deal with values that can not be matched
      ...) # Additional arguments

##*Activity budget
activity001_500	<- read.csv('Article Coding Database (IDs_ 001 - 500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity501_1000	<- read.csv('Article Coding Database (IDs_ 501 - 1000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity1001_1500	<- read.csv('Article Coding Database (IDs_ 1001 - 1500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity1501_2000	<- read.csv('Article Coding Database (IDs_ 1501 - 2000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity2001_2500	<- read.csv('Article Coding Database (IDs_ 2001 - 2500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity2501_3000	<- read.csv('Article Coding Database (IDs_ 2501 - 3000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity3001_3500	<- read.csv('Article Coding Database (IDs_ 3001 - 3500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity3501_4000	<- read.csv('Article Coding Database (IDs_ 3501 - 4000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
#*ran, it works 

lumper		<- read.csv('Taxonomy Conversion - Lumper Taxonomy Conversion.csv', stringsAsFactors = FALSE)
IUCN_tax		<- read.csv('Taxonomy Conversion - IUCN Taxonomy Conversion.csv', stringsAsFactors = FALSE)
TenKTrees		<- read.csv('Taxonomy Conversion - 10K Trees Taxonomy Conversion.csv', stringsAsFactors = FALSE)

colnames(activity3501_4000) <- colnames(activity3001_3500) <- colnames(activity2501_3000) <- colnames(activity1501_2000) <- colnames(activity501_1000) <- colnames(activity2001_2500) <- colnames(activity1001_1500) <- colnames(activity001_500)
#it works

activityAll	<- rbind(activity001_500, activity501_1000, activity1001_1500, activity1501_2000,
                     activity2001_2500, activity2501_3000, activity3001_3500, activity3501_4000) 



mergeddata <- merge(behaviorAll, activityAll,
                    by = intersect(names(behaviorAll), names(activityAll)),
                    by.behaviorAll = c("Article.ID", "Study.Site.ID", "Date.of.Coding", "Type.of.Data", "Names.of.group"), by.activityAll = c("X.1", "X.2", "X.3", "X.4", "X.6"), 
                    all.behaviorAll = TRUE)

#WORKS

mergeddata_1 <- merge(behaviorAll, activityAll,
                    by = intersect(names(behaviorAll), names(activityAll)),
                    by.behaviorAll = c("Article.ID", "Study.Site.ID", "Date.of.Coding", "Type.of.Data", "Names.of.group"), by.activityAll = c("X.1", "X.2", "X.3", "X.4", "X.6"), 
                    all.behaviorAll = TRUE, all.activityAll = TRUE)
View(mergeddata_1)


mergeddata_1$SciName	<- ifelse(mergeddata_1$species == '' & mergeddata_1$subspecies == '', mergeddata_1$Genus,
                              ifelse(mergeddata_1$subspecies == '', paste(mergeddata_1$Genus, '_', mergeddata_1$species, sep = ''),
                                     paste(mergeddata_1$Genus, '_', mergeddata_1$species, '_', mergeddata_1$subspecies, sep = '')))


mergeddata_1			<- merge(mergeddata_1, lumper, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
mergeddata_1			<- merge(mergeddata_1, IUCN_tax, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
mergeddata_1			<- merge(mergeddata_1, TenKTrees, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
#warning message


##*activity budget:  a) % social b) % grooming c) % feeding d) rate of aggresstion  e) submission
##*a) % social
##*% time social in table as "X..time.social"

#maybe need to do ifelse to gather data with names?

activityAll$Aggregate_all <- ifelse(is.na(activityAll$X.10) == TRUE & is.na(activityAll$X.12)== TRUE &is.na(activityAll$X.14)== TRUE & is.na(activityAll$X.16)== TRUE & is.na(activityAll$Activity.Budget.Data)== TRUE, 0,1)

mergeddata_1$Aggregate_all <- ifelse(is.na(mergeddata_1$X..time.feeding) == TRUE & is.na(mergeddata_1$X..time.grooming) == TRUE & is.na(mergeddata_1$X..time.social) == TRUE & is.na(mergeddata_1$Rate.of.aggression) == TRUE & is.na(mergeddata_1$Rate.of.submission) == TRUE, 0 ,1)

mergeddata_1$X..time.social <- ifelse(is.na(mergeddata_1$X..time.social) == TRUE, 0, 1)

mergeddata_1_social <- aggregate(mergeddata_1$X..time.social, list(mergeddata_1$sciName), FUN = length) #different results using FUN = sum
#* unsure which FUN to use 
#* prior to carry out ifelse option -- #error: Error in aggregate.data.frame(as.data.frame(x), ...) : arguments must have same length
#* now error seems resolved 
#* unsure if this is the right step

table(mergeddata_1_social)

#percent_feeding
mergeddata_1$X..time.feeding <- ifelse(is.na(mergeddata_1$X..time.feeding) == TRUE, 0, 1)

mergeddata_1_feeding <- aggregate(mergeddata_1$X..time.feeding, list(mergeddata_1$sciName), FUN = length)

#percent_grooming

mergeddata_1$X..time.grooming <- ifelse(is.na(mergeddata_1$X..time.grooming) == TRUE, 0, 1)

mergeddata_1_grooming <- aggregate(mergeddata_1$X..time.grooming, list(mergeddata_1$sciName), FUN = length)

#rate_aggression

mergeddata_1$Rate.of.aggression <- ifelse(is.na(mergeddata_1$Rate.of.aggression) == TRUE, 0, 1)

mergeddata_1_aggression <- aggregate(mergeddata_1$Rate.of.aggression, list(mergeddata_1$sciName), FUN = length)

#rate_submission

mergeddata_1$Rate.of.submission <- ifelse(is.na(mergeddata_1$Rate.of.submission) == TRUE, 0, 1)

mergeddata_1_submission <- aggregate(mergeddata_1$Rate.of.submission, list(mergeddata_1$sciName), FUN = length)

##**Feeding data 
##*feeding data:  a) specific data (plant reproductive parts, fungi, insect, foliage)

setwd("C:/Users/arian/OneDrive/Desktop/PREdiCT")

library(ape)


feeding001_500 <- read.csv("Article Coding Database (IDs_ 001 - 500) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding501_1000	<- read.csv("Article Coding Database (IDs_ 501 - 1000) - Seasonal feeding data.csv", stringsAsFactors = FALSE)
feeding1001_1500 <- read.csv("Article Coding Database (IDs_ 1001 - 1500) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding1501_2000 <- read.csv("Article Coding Database (IDs_ 1501 - 2000) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding2001_2500 <- read.csv("Article Coding Database (IDs_ 2001 - 2500) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding2501_3000 <- read.csv("Article Coding Database (IDs_ 2501 - 3000) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding3001_3500 <- read.csv("Article Coding Database (IDs_ 3001 - 3500) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding3501_4000 <- read.csv("Article Coding Database (IDs_ 3501 - 4000) - Seasonal feeding data.csv",stringsAsFactors = FALSE)

lumper		<- read.csv('Taxonomy Conversion - Lumper Taxonomy Conversion.csv', stringsAsFactors = FALSE)
IUCN_tax		<- read.csv('Taxonomy Conversion - IUCN Taxonomy Conversion.csv', stringsAsFactors = FALSE)
TenKTrees		<- read.csv('Taxonomy Conversion - 10K Trees Taxonomy Conversion.csv', stringsAsFactors = FALSE)

colnames(feeding3501_4000) <- colnames(feeding3001_3500) <- colnames(feeding2501_3000) <- colnames(feeding1501_2000) <- colnames(feeding501_1000) <- colnames(feeding2001_2500) <- colnames(feeding1001_1500) <- colnames(feeding001_500)

feedingAll	<- rbind(feeding001_500, feeding501_1000, feeding1001_1500, feeding1501_2000,
                    feeding2001_2500, feeding2501_3000, feeding3001_3500, feeding3501_4000) 


mergeddata_feeding <- merge(behaviorAll, feedingAll,
                      by = intersect(names(behaviorAll), names(feedingAll)),
                      by.behaviorAll = c("Article.ID", "Study.Site.ID", "Date.of.Coding", "Type.of.Data", "Names.of.group"), by.activityAll = c("X.9", "X.11", "X.3", "X.13", "X.14"), 
                      all.behaviorAll = TRUE, all.activityAll = TRUE)


mergeddata_feeding$SciName	<- ifelse(mergeddata_feeding$species == '' & mergeddata_feeding$subspecies == '', mergeddata_feeding$Genus,
                               ifelse(mergeddata_feeding$subspecies == '', paste(mergeddata_feeding$Genus, '_', mergeddata_feeding$species, sep = ''),
                                      paste(mergeddata_feeding$Genus, '_', mergeddata_feeding$species, '_', mergeddata_feeding$subspecies, sep = '')))


mergeddata_feeding <- merge(mergeddata_feeding, lumper, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
mergeddata_feeding <- merge(mergeddata_feeding, IUCN_tax, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
mergeddata_feeding <- merge(mergeddata_feeding, TenKTrees, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 

#% plant reproductive parts

#%folivory


#%insects

#% fungi

##*Home range size
##**downloaded seasonal ranging data*

homerange001_500	<- read.csv('Article Coding Database (IDs_ 001 - 500) - Seasonal ranging data.csv', stringsAsFactors = FALSE)
homerange501_1000	<- read.csv('Article Coding Database (IDs_ 501 - 1000) - Seasonal ranging data.csv', stringsAsFactors = FALSE)
homerange1001_1500	<- read.csv('Article Coding Database (IDs_ 1001 - 1500) - Seasonal ranging data.csv', stringsAsFactors = FALSE)
homerange1501_2000	<- read.csv('Article Coding Database (IDs_ 1501 - 2000) - Seasonal ranging data.csv', stringsAsFactors = FALSE)
homerange2001_2500	<- read.csv('Article Coding Database (IDs_ 2001 - 2500) - Seasonal ranging data.csv', stringsAsFactors = FALSE)
homerange2501_3000	<- read.csv('Article Coding Database (IDs_ 2501 - 3000) - Seasonal ranging data.csv', stringsAsFactors = FALSE)
homerange3001_3500	<- read.csv('Article Coding Database (IDs_ 3001 - 3500) - Seasonal ranging data.csv', stringsAsFactors = FALSE)
homerange3501_4000	<- read.csv('Article Coding Database (IDs_ 3501 - 4000) - Seasonal ranging data.csv', stringsAsFactors = FALSE)





    