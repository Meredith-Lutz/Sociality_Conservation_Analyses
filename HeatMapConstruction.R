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

#**Allomaternal care, only yes*
###############################################################################################################
###### want to include yes and no here (not not mentioned)so should be similar to the social learning one #####
############################################################################################################### **corrected 2/28**
behaviorAll_ac <- behaviorAll[behaviorAll$Allomaternal.care == 'Yes',]

allomaternal_care <- aggregate(behaviorAll_ac$Allomaternal.care, by= list(behaviorAll_ac$sciName), FUN = length)

#**social learning, only yes*
#df[df$var1 == 'value', ]
###############################################################################################################
###### because you keep saving over the dataset when you subset it ############################################
###### as we get further in your code you are using the subset instead of the whole thing #####################
###### I would rename it in the next line and then use that new dataset in your aggregate #####################
############################################################################################################### *corrected 2/28**
behaviorAll_sl <- behaviorAll[behaviorAll$Social.learning == 'Yes',] #selecting only for "yes"
social_learning <- aggregate(behaviorAll_sl$Social.learning, by= list(behaviorAll_sl$sciName), FUN = length)


#**Intergroup encounter study*
behaviorAll_igs <- behaviorAll[behaviorAll$Intergroup.encounter.study. == 'Yes',] #selecting only for "yes"
intergroup_encounter_study <- aggregate(behaviorAll_igs$Intergroup.encounter.study., by= list(behaviorAll_igs$sciName), FUN = length)


#**mean/median/min/max_adult_males* 
behaviorAll$Aggregatemale <- ifelse(is.na(behaviorAll$Mean.adult.males) == TRUE & is.na(behaviorAll$Median.adult.males) == TRUE & is.na(behaviorAll$Min...adult.males) == TRUE & is.na(behaviorAll$Max...adult.males) ==TRUE, 0,1)
#0 are missing 
#1 has at least 1 of the variables
                                   
table(behaviorAll$Aggregatemale) 
aggregate_male <- aggregate(behaviorAll$Aggregatemale, by= list(behaviorAll$sciName), FUN = sum)


#For general is.na == TRUE for all male, females, individual (12 totals:mean, max, median, min)
#For individual follow behaviorAll$Aggregatemale for females and individuals --> 4 is.na


#**mean/median/min/max_adult_females*
behaviorAll$Aggregatefemale <- ifelse(is.na(behaviorAll$Mean.adult.females) == TRUE & is.na(behaviorAll$Median.adult.females) == TRUE & is.na(behaviorAll$Min...adult.females) == TRUE & is.na(behaviorAll$Max...adult.females) ==TRUE, 0,1)
table(behaviorAll$Aggregatefemale)
aggregate_female <- aggregate(behaviorAll$Aggregatefemale, by= list(behaviorAll$sciName), FUN = sum)


#**mean/median/min/max_individuals*
behaviorAll$Aggregateindividual <- ifelse(is.na(behaviorAll$Mean.individuals) == TRUE & is.na(behaviorAll$Median.individuals) == TRUE & is.na(behaviorAll$Min...of.individuals) == TRUE & is.na(behaviorAll$Max...of.individuals) ==TRUE, 0,1)
table(behaviorAll$Aggregateindividual)
aggregate_individual <- aggregate(behaviorAll$Aggregateindividual, by= list(behaviorAll$sciName), FUN = sum)


#**general mean/median/min/max*
##############################################################################
###### I fixed the error here, but make sure that the output makes sense #####
##############################################################################
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


#**merging csv**
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
##*redownloaded 
activity001_500	<- read.csv('Article Coding Database (IDs_ 001 - 500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity501_1000	<- read.csv('Article Coding Database (IDs_ 501 - 1000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity1001_1500	<- read.csv('Article Coding Database (IDs_ 1001 - 1500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity1501_2000	<- read.csv('Article Coding Database (IDs_ 1501 - 2000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity2001_2500	<- read.csv('Article Coding Database (IDs_ 2001 - 2500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity2501_3000	<- read.csv('Article Coding Database (IDs_ 2501 - 3000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity3001_3500	<- read.csv('Article Coding Database (IDs_ 3001 - 3500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity3501_4000	<- read.csv('Article Coding Database (IDs_ 3501 - 4000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
#*ran, it works 

colnames(activity3501_4000) <- colnames(activity3001_3500) <- colnames(activity2501_3000) <- colnames(activity1501_2000) <- colnames(activity501_1000) <- colnames(activity2001_2500) <- colnames(activity1001_1500) <- colnames(activity001_500)
#it works

activityAll	<- rbind(activity001_500, activity501_1000, activity1001_1500, activity1501_2000,
                     activity2001_2500, activity2501_3000, activity3001_3500, activity3501_4000) 

View(activityAll)
###################################################################################################
##### Here I switched the first and second data sets ##############################################
#####(since we just want to add data onto each seasonal line instead of the other way around) #####
###################################################################################################

mergedActivity <- merge(activityAll, behaviorAll,
				by.x = c("Article.ID", "Study.Site.ID", "Article.Initials", "Type.of.data", "Names.of.group"), 
				by.y = c("Article.ID", "Study.Site.ID", "Coder.Initials", "Type.of.data", "Names.of.group"),
				all.x = TRUE)
###** 

#####################################################################################################################################
#### There's a lot of places that didn't merge correctly, which we will have to deal with once the group name verifiers are done ####
#####################################################################################################################################
mergedActivity$sciName	<- ifelse(mergedActivity$species == '' & mergedActivity$subspecies == '', mergedActivity$Genus,
                              ifelse(mergedActivity$subspecies == '', paste(mergedActivity$Genus, '_', mergedActivity$species, sep = ''),
                                     paste(mergedActivity$Genus, '_', mergedActivity$species, '_', mergedActivity$subspecies, sep = '')))


mergedActivity			<- merge(mergedActivity, lumper, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
mergedActivity			<- merge(mergedActivity, IUCN_tax, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
mergedActivity			<- merge(mergedActivity, TenKTrees, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
#warning message

View(mergedActivity)

##*activity budget:  a) % social b) % grooming c) % feeding d) rate of aggresstion  e) submission
##*a) % social
##*% time social in table as "X..time.social" --> 2/28 works
mergedActivity$X..time.social <- ifelse(is.na(mergedActivity$X..time.social.x) == TRUE & is.na(mergedActivity$X..time.social.y) == TRUE, 0,1)

mergedActivity_social <- aggregate(mergedActivity$X..time.social, list(mergedActivity$sciName), FUN = sum)

#percent_feeding --> 2/28 works
mergedActivity$X..time.feeding <- ifelse(is.na(mergedActivity$X..time.feeding.x) == TRUE & is.na(mergedActivity$X..time.feeding.y) == TRUE, 0,1)

mergedActivity_feeding <- aggregate(mergedActivity$X..time.feeding, list(mergedActivity$sciName), FUN = sum)

#percent_grooming --> 2/28 works
mergedActivity$X..time.grooming <- ifelse(is.na(mergedActivity$X..time.grooming.x) == TRUE & is.na(mergedActivity$X..time.grooming.y) == TRUE, 0,1)

mergedActivity_grooming <- aggregate(mergedActivity$X..time.grooming, list(mergedActivity$sciName), FUN = sum)


#rate_aggression --> 2/28 works
mergedActivity$Rate.of.aggression <- ifelse(is.na(mergedActivity$Rate.of.aggression.x) == TRUE & is.na(mergedActivity$Rate.of.aggression.y) == TRUE, 0,1)

mergedActivity_aggression <- aggregate(mergedActivity$Rate.of.aggression, list(mergedActivity$sciName), FUN = sum)

#rate_submission --> 2/28 works
mergedActivity$Rate.of.submission <- ifelse(is.na(mergedActivity$Rate.of.submission.x) == TRUE & is.na(mergedActivity$Rate.of.submission.y) == TRUE, 0,1)

mergedActivity_submission <- aggregate(mergedActivity$Rate.of.submission, list(mergedActivity$sciName), FUN = sum)

############################################################################################################
############################################################################################################
2/28 #**notes on FUN**#
#* by using FUN = length, results vary in number 1 to >50 approx
#* by using FUN = sum, results still present numbers between 1-50 but many 0s --> should I move forward with selecting
#* only those species with actual numbers and disregard the species with 0s -- code similar to select "yes" in 
#* previous examples
#############################################################################################################
#############################################################################################################



##**Feeding data 
##*feeding data:  a) specific data (plant reproductive parts, fungi, insect, foliage)

feeding001_500 <- read.csv("Article Coding Database (IDs_ 001 - 500) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding501_1000	<- read.csv("Article Coding Database (IDs_ 501 - 1000) - Seasonal feeding data.csv", stringsAsFactors = FALSE)
feeding1001_1500 <- read.csv("Article Coding Database (IDs_ 1001 - 1500) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding1501_2000 <- read.csv("Article Coding Database (IDs_ 1501 - 2000) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding2001_2500 <- read.csv("Article Coding Database (IDs_ 2001 - 2500) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding2501_3000 <- read.csv("Article Coding Database (IDs_ 2501 - 3000) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding3001_3500 <- read.csv("Article Coding Database (IDs_ 3001 - 3500) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding3501_4000 <- read.csv("Article Coding Database (IDs_ 3501 - 4000) - Seasonal feeding data.csv",stringsAsFactors = FALSE)

colnames(feeding3501_4000) <- colnames(feeding3001_3500) <- colnames(feeding2501_3000) <- colnames(feeding1501_2000) <- colnames(feeding501_1000) <- colnames(feeding2001_2500) <- colnames(feeding1001_1500) <- colnames(feeding001_500)

feedingAll	<- rbind(feeding001_500, feeding501_1000, feeding1001_1500, feeding1501_2000,
                    feeding2001_2500, feeding2501_3000, feeding3001_3500, feeding3501_4000) 

mergedFeeding <- merge(feedingAll, behaviorAll,
                        by.x = c("Article.ID", "Study.Site.ID", "Article.Initials", "Type.of.data", "Names.of.group"), 
                        by.y = c("Article.ID", "Study.Site.ID", "Coder.Initials", "Type.of.data", "Names.of.group"),
                        all.x = TRUE)

mergedFeeding$sciName	<- ifelse(mergedFeeding$species == '' & mergedFeeding$subspecies == '', mergedFeeding$Genus,
                                 ifelse(mergedFeeding$subspecies == '', paste(mergedFeeding$Genus, '_', mergedFeeding$species, sep = ''),
                                        paste(mergedFeeding$Genus, '_', mergedFeeding$species, '_', mergedFeeding$subspecies, sep = '')))


mergedFeeding			<- merge(mergedFeeding, lumper, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
mergedFeeding			<- merge(mergedFeeding, IUCN_tax, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
mergedFeeding			<- merge(mergedFeeding, TenKTrees, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 

#% plant reproductive parts
mergedFeeding$X..plant.reproductive.parts <- ifelse(is.na(mergedFeeding$X..plant.reproductive.parts.x) == TRUE & is.na(mergedFeeding$X..plant.reproductive.parts.y) == TRUE, 0,1)

mergedFeeding_prp <- aggregate(mergedFeeding$X..plant.reproductive.parts, list(mergedFeeding$sciName), FUN = sum)

#%folivory
mergedFeeding$X..folivory <- ifelse(is.na(mergedFeeding$X..folivory.x) == TRUE & is.na(mergedFeeding$X..folivory.y) == TRUE, 0,1)

mergedFeeding_fol <- aggregate(mergedFeeding$X..folivory, list(mergedFeeding$sciName), FUN = sum)

#%insects
mergedFeeding$X..insects <- ifelse(is.na(mergedFeeding$X..insects.x) == TRUE & is.na(mergedFeeding$X..insects.y) == TRUE, 0,1)

mergedFeeding_insects <- aggregate(mergedFeeding$X..insects, list(mergedFeeding$sciName), FUN = sum)

#% fungi
mergedFeeding$X..fungus <- ifelse(is.na(mergedFeeding$X..fungus.x) == TRUE & is.na(mergedFeeding$X..fungus.y) == TRUE, 0,1)

mergedFeeding_fungus <- aggregate(mergedFeeding$X..fungus, list(mergedFeeding$sciName), FUN = sum)


##*Home range size
##**downloaded seasonal ranging data*

setwd("C:/Users/arian/OneDrive/Desktop/PREdiCT")

homerange001_500 <- read.csv("Article Coding Database (IDs_ 001 - 500) - Seasonal ranging data.csv", stringsAsFactors = FALSE)
homerange501_1000 <- read.csv("Article Coding Database (IDs_ 501 - 1000) - Seasonal ranging data.csv", stringsAsFactors = FALSE)
homerange1001_1500 <- read.csv("Article Coding Database (IDs_ 1001 - 1500) - Seasonal ranging data.csv", stringsAsFactors = FALSE)
homerange1501_2000 <- read.csv("Article Coding Database (IDs_ 1501 - 2000) - Seasonal ranging data.csv", stringsAsFactors = FALSE)
homerange2001_2500 <- read.csv("Article Coding Database (IDs_ 2001 - 2500) - Seasonal ranging data.csv", stringsAsFactors = FALSE)
homerange2501_3000 <- read.csv("Article Coding Database (IDs_ 2501 - 3000) - Seasonal ranging data.csv", stringsAsFactors = FALSE)
homerange3001_3500 <- read.csv("Article Coding Database (IDs_ 3001 - 3500) - Seasonal ranging data (1).csv", stringsAsFactors = FALSE)
homerange3501_4000 <- read.csv("Article Coding Database (IDs_ 3501 - 4000) - Seasonal ranging data.csv", stringsAsFactors = FALSE)

colnames(homerange3501_4000) <- colnames(homerange3001_3500) <- colnames(homerange2501_3000) <- colnames(homerange1501_2000) <- colnames(homerange501_1000) <- colnames(homerange2001_2500) <- colnames(homerange1001_1500) <- colnames(homerange001_500)

homerangeAll	<- rbind(homerange001_500, homerange501_1000, homerange1001_1500, homerange1501_2000,
                      homerange2001_2500, homerange2501_3000, homerange3001_3500, homerange3501_4000) 

mergedRange <- merge(homerangeAll, behaviorAll,
                       by.x = c("Article.ID", "Study.Site.ID", "Article.Initials", "Type.of.data", "Names.of.group"), 
                       by.y = c("Article.ID", "Study.Site.ID", "Coder.Initials", "Type.of.data", "Names.of.group"),
                       all.x = TRUE)

mergedRange$sciName	<- ifelse(mergedRange$species == '' & mergedRange$subspecies == '', mergedRange$Genus,
                                ifelse(mergedRange$subspecies == '', paste(mergedRange$Genus, '_', mergedRange$species, sep = ''),
                                       paste(mergedRange$Genus, '_', mergedRange$species, '_', mergedRange$subspecies, sep = '')))


mergedRange			<- merge(mergedRange, lumper, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
mergedRange			<- merge(mergedRange, IUCN_tax, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
mergedRange			<- merge(mergedRange, TenKTrees, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 

mergedRange$Home.range.size..ha.. <- ifelse(is.na(mergedRange$Home.range.size..ha..x) == TRUE & is.na(mergedRange$Home.range.overlap..y) == TRUE, 0,1)
mergedRange_size <- aggregate(mergedRange$Home.range.size..ha.., list(mergedRange$sciName), FUN = sum)

mergedRange$Home.range.overlap.. <- ifelse(is.na(mergedRange$Home.range.overlap..x) == TRUE & is.na(mergedRange$Home.range.overlap..y) == TRUE, 0,1)
mergedRange_overlap <- aggregate(mergedRange$Home.range.overlap.., list(mergedRange$sciName), FUN = sum)
#####**If overlap needed I may need to select for yes as follow*

mergedRange_overlap <- homerangeAll[homerangeAll$Home.range.overlap. == 'Yes',] #selecting only for "yes"
mergedRange_overlap <- aggregate(mergedRange$Home.range.overlap.., list(mergedRange$sciName), FUN = sum)


########################################################################################################################################
##### HEAT MAP ##### https://r-graph-gallery.com/215-the-heatmap-function.html

behavior_list <- list(allomaternal_care, social_learning, social_organization, intergroup_encounter_study, aggregate_malesec_dispersal, aggregate_male_dispersal, aggregate_male, aggregate_individual, aggregate_general, aggregate_femsec_dispersal, aggregate_female, aggregate_fem_dispersal, aggregate_dispersalgeneral)
activty_list <- list(mergedActivity_aggression, mergedActivity_feeding, mergedActivity_grooming, mergedActivity_social, mergedActivity_submission)
feeding_list <- list(mergedFeeding_fol, mergedFeeding_fungus, mergedFeeding_insects, mergedFeeding_prp)
homerange_list <- list(mergedRange_overlap, mergedRange_size)
###not a table -- possibly explains why other functions have errors  

datamerged <- merge(behavior_list, activty_list, feeding_list, homerange_list, by = "id") #error

datamerged_1 <- merge(behavior_list, activty_list, by = "id", all.x = TRUE) #error

dataframes_all <- cbind(behavior_list, activty_list, feeding_list, homerange_list) #error 

dataframes_all <- cbind(allomaternal_care, social_learning, social_organization, intergroup_encounter_study, aggregate_malesec_dispersal, aggregate_male_dispersal, aggregate_male, aggregate_individual, aggregate_general, aggregate_femsec_dispersal, aggregate_female, aggregate_fem_dispersal, aggregate_dispersalgeneral, mergedActivity_aggression, mergedActivity_feeding, mergedActivity_grooming, mergedActivity_social, mergedActivity_submission, mergedFeeding_fol, mergedFeeding_fungus, mergedFeeding_insects, mergedFeeding_prp, mergedRange_overlap, mergedRange_size)
#ERROR


all_df <- cbind(mergedActivity, mergedRange) ##different dfs, errors seems to be the same: differing #of rows
all_activity <- 
all_feeding <- 
all_homerange <-

#Takes a sequence of vector, matrix or data-frame arguments and combines them by columns
  bind_cols(df1, df2)


library(dplyr)
all_df <- bind_cols(allomaternal_care, social_learning, social_organization, intergroup_encounter_study, aggregate_malesec_dispersal, aggregate_male_dispersal, aggregate_male, aggregate_individual, aggregate_general, aggregate_femsec_dispersal, aggregate_female, aggregate_fem_dispersal, aggregate_dispersalgeneral, mergedActivity_aggression, mergedActivity_feeding, mergedActivity_grooming, mergedActivity_social, mergedActivity_submission, mergedFeeding_fol, mergedFeeding_fungus, mergedFeeding_insects, mergedFeeding_prp, mergedRange_overlap, mergedRange_size)
##getting somehwere --> Error in `bind_cols()`:
! Can't recycle `..1` (size 100) to match `..2` (size 25).
Run `rlang::last_error()` to see where the error occurred.' 

all_df <- bind_cols(list(behavior_list, activty_list))
dplyr::bind_cols(list(behavior_list, activty_list))
vctrs::vec_cbind(!!!dots, .name_repair = .name_repair)


mergedRange$sciName	<- ifelse(mergedRange$species == '' & mergedRange$subspecies == '', mergedRange$Genus,
                              ifelse(mergedRange$subspecies == '', paste(mergedRange$Genus, '_', mergedRange$species, sep = ''),
                                     paste(mergedRange$Genus, '_', mergedRange$species, '_', mergedRange$subspecies, sep = '')))


mergedRange			<- merge(mergedRange, lumper, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
mergedRange			<- merge(mergedRange, IUCN_tax, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
mergedRange			<- merge(mergedRange, TenKTrees, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 

