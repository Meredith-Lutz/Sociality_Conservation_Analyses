############################################
############################################
##### Heat Map Construction by Arianna #####
############################################
############################################

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

setwd("C:/Users/arian/OneDrive/Desktop/PREdiCT")

library(ape)

#######################
### Reading data in ###
#######################
behavior001_500	<- read.csv('Article Coding Database (IDs_ 001 - 500) - Behavior data.csv', stringsAsFactors = FALSE)
behavior501_1000	<- read.csv('Article Coding Database (IDs_ 501 - 1000) - Behavior data.csv', stringsAsFactors = FALSE)
behavior1001_1500	<- read.csv('Article Coding Database (IDs_ 1001 - 1500) - Behavior data.csv', stringsAsFactors = FALSE)
behavior1501_2000	<- read.csv('Article Coding Database (IDs_ 1501 - 2000) - Behavior data.csv', stringsAsFactors = FALSE)
behavior2001_2500	<- read.csv('Article Coding Database (IDs_ 2001 - 2500) - Behavior data.csv', stringsAsFactors = FALSE)
behavior2501_3000	<- read.csv('Article Coding Database (IDs_ 2501 - 3000) - Behavior data.csv', stringsAsFactors = FALSE)
behavior3001_3500	<- read.csv('Article Coding Database (IDs_ 3001 - 3500) - Behavior data.csv', stringsAsFactors = FALSE)
behavior3501_4000	<- read.csv('Article Coding Database (IDs_ 3501 - 4000) - Behavior data.csv', stringsAsFactors = FALSE)

activity001_500	<- read.csv('Article Coding Database (IDs_ 001 - 500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity501_1000	<- read.csv('Article Coding Database (IDs_ 501 - 1000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity1001_1500	<- read.csv('Article Coding Database (IDs_ 1001 - 1500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity1501_2000	<- read.csv('Article Coding Database (IDs_ 1501 - 2000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity2001_2500	<- read.csv('Article Coding Database (IDs_ 2001 - 2500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity2501_3000	<- read.csv('Article Coding Database (IDs_ 2501 - 3000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity3001_3500	<- read.csv('Article Coding Database (IDs_ 3001 - 3500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
activity3501_4000	<- read.csv('Article Coding Database (IDs_ 3501 - 4000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)

feeding001_500 	<- read.csv("Article Coding Database (IDs_ 001 - 500) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding501_1000	<- read.csv("Article Coding Database (IDs_ 501 - 1000) - Seasonal feeding data.csv", stringsAsFactors = FALSE)
feeding1001_1500	<- read.csv("Article Coding Database (IDs_ 1001 - 1500) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding1501_2000 	<- read.csv("Article Coding Database (IDs_ 1501 - 2000) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding2001_2500 	<- read.csv("Article Coding Database (IDs_ 2001 - 2500) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding2501_3000 	<- read.csv("Article Coding Database (IDs_ 2501 - 3000) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding3001_3500 	<- read.csv("Article Coding Database (IDs_ 3001 - 3500) - Seasonal feeding data.csv",stringsAsFactors = FALSE)
feeding3501_4000 	<- read.csv("Article Coding Database (IDs_ 3501 - 4000) - Seasonal feeding data.csv",stringsAsFactors = FALSE)

homerange001_500 		<- read.csv("Article Coding Database (IDs_ 001 - 500) - Seasonal ranging data.csv", stringsAsFactors = FALSE)
homerange501_1000 	<- read.csv("Article Coding Database (IDs_ 501 - 1000) - Seasonal ranging data.csv", stringsAsFactors = FALSE)
homerange1001_1500	<- read.csv("Article Coding Database (IDs_ 1001 - 1500) - Seasonal ranging data.csv", stringsAsFactors = FALSE)
homerange1501_2000	<- read.csv("Article Coding Database (IDs_ 1501 - 2000) - Seasonal ranging data.csv", stringsAsFactors = FALSE)
homerange2001_2500	<- read.csv("Article Coding Database (IDs_ 2001 - 2500) - Seasonal ranging data.csv", stringsAsFactors = FALSE)
homerange2501_3000	<- read.csv("Article Coding Database (IDs_ 2501 - 3000) - Seasonal ranging data.csv", stringsAsFactors = FALSE)
homerange3001_3500	<- read.csv("Article Coding Database (IDs_ 3001 - 3500) - Seasonal ranging data.csv", stringsAsFactors = FALSE)
homerange3501_4000	<- read.csv("Article Coding Database (IDs_ 3501 - 4000) - Seasonal ranging data.csv", stringsAsFactors = FALSE)

lumper		<- read.csv('Taxonomy Conversion - Lumper Taxonomy Conversion.csv', stringsAsFactors = FALSE)
IUCN_tax		<- read.csv('Taxonomy Conversion - IUCN Taxonomy Conversion.csv', stringsAsFactors = FALSE)
TenKTrees		<- read.csv('Taxonomy Conversion - 10K Trees Taxonomy Conversion.csv', stringsAsFactors = FALSE)

colnames(behavior3501_4000)	<- colnames(behavior3001_3500) <- colnames(behavior2501_3000) <- colnames(behavior1501_2000) <- colnames(behavior501_1000) <- colnames(behavior2001_2500) <- colnames(behavior1001_1500) <- colnames(behavior001_500)
colnames(activity3501_4000)	<- colnames(activity3001_3500) <- colnames(activity2501_3000) <- colnames(activity1501_2000) <- colnames(activity501_1000) <- colnames(activity2001_2500) <- colnames(activity1001_1500) <- colnames(activity001_500)
colnames(feeding3501_4000)	<- colnames(feeding3001_3500) <- colnames(feeding2501_3000) <- colnames(feeding1501_2000) <- colnames(feeding501_1000) <- colnames(feeding2001_2500) <- colnames(feeding1001_1500) <- colnames(feeding001_500)
colnames(homerange3501_4000)	<- colnames(homerange3001_3500) <- colnames(homerange2501_3000) <- colnames(homerange1501_2000) <- colnames(homerange501_1000) <- colnames(homerange2001_2500) <- colnames(homerange1001_1500) <- colnames(homerange001_500)

behaviorAll	<- rbind(behavior001_500, behavior501_1000, behavior1001_1500, behavior1501_2000,
                     behavior2001_2500, behavior2501_3000, behavior3001_3500, behavior3501_4000) #21329 lines

activityAll	<- rbind(activity001_500, activity501_1000, activity1001_1500, activity1501_2000,
                     activity2001_2500, activity2501_3000, activity3001_3500, activity3501_4000) 

feedingAll	<- rbind(feeding001_500, feeding501_1000, feeding1001_1500, feeding1501_2000,
                    feeding2001_2500, feeding2501_3000, feeding3001_3500, feeding3501_4000) 

homerangeAll	<- rbind(homerange001_500, homerange501_1000, homerange1001_1500, homerange1501_2000,
                      homerange2001_2500, homerange2501_3000, homerange3001_3500, homerange3501_4000) 

behaviorAll$sciName	<- ifelse(behaviorAll$species == '' & behaviorAll$subspecies == '', behaviorAll$Genus,
                              ifelse(behaviorAll$subspecies == '', paste(behaviorAll$Genus, '_', behaviorAll$species, sep = ''),
                                     paste(behaviorAll$Genus, '_', behaviorAll$species, '_', behaviorAll$subspecies, sep = '')))

behaviorAll			<- merge(behaviorAll, lumper, by.x = 'sciName', by.y = 'Mendeley_tag', all.x=TRUE) 
behaviorAll			<- merge(behaviorAll, IUCN_tax, by.x = 'sciName', by.y = 'Mendeley_tag', all.x=TRUE) 
behaviorAll			<- merge(behaviorAll, TenKTrees, by.x = 'sciName', by.y = 'Mendeley_tag', all.x=TRUE) 

mergedActivity <- merge(activityAll, behaviorAll,
				by.x = c("Article.ID", "Study.Site.ID", "Article.Initials", "Type.of.data", "Names.of.group"), 
				by.y = c("Article.ID", "Study.Site.ID", "Article.Initials", "Type.of.data", "Names.of.group"),
				all.x = TRUE)

mergedFeeding <- merge(feedingAll, behaviorAll,
                        by.x = c("Article.ID", "Study.Site.ID", "Article.Initials", "Type.of.data", "Names.of.group"), 
                        by.y = c("Article.ID", "Study.Site.ID", "Article.Initials", "Type.of.data", "Names.of.group"),
                        all.x = TRUE)

mergedRange <- merge(homerangeAll, behaviorAll,
                       by.x = c("Article.ID", "Study.Site.ID", "Article.Initials", "Type.of.data", "Names.of.group"), 
                       by.y = c("Article.ID", "Study.Site.ID", "Article.Initials", "Type.of.data", "Names.of.group"),
                       all.x = TRUE)


#####################################################################################################################################
#### There's a lot of places that didn't merge correctly, which we will have to deal with once the group name verifiers are done ####
#####################################################################################################################################

########################################
### Summarizing variables by species ###
########################################
#**summarizing social organization*
social_organization <- aggregate(behaviorAll$Social.organization, by= list(behaviorAll$sciName), FUN = length)
#group 1 = species 
#x = # of line we have for that species that are not blank

#**Allomaternal care, only yes*
behaviorAll_ac <- behaviorAll[behaviorAll$Allomaternal.care == 'Yes',]
allomaternal_care <- aggregate(behaviorAll_ac$Allomaternal.care, by= list(behaviorAll_ac$sciName), FUN = length)

#**social learning, only yes*
behaviorAll_sl <- behaviorAll[behaviorAll$Social.learning == 'Yes',] #selecting only for "yes"
social_learning <- aggregate(behaviorAll_sl$Social.learning, by= list(behaviorAll_sl$sciName), FUN = length)

#**Intergroup encounter study*
behaviorAll_igs <- behaviorAll[behaviorAll$Intergroup.encounter.study. == 'Yes',] #selecting only for "yes"
intergroup_encounter_study <- aggregate(behaviorAll_igs$Intergroup.encounter.study., by= list(behaviorAll_igs$sciName), FUN = length)

#**mean/median/min/max_adult_males* 
behaviorAll$Aggregatemale <- ifelse(is.na(behaviorAll$Mean.adult.males) == TRUE & is.na(behaviorAll$Median.adult.males) == TRUE & is.na(behaviorAll$Min...adult.males) == TRUE & is.na(behaviorAll$Max...adult.males) ==TRUE, 0,1)
#0 are missing 
#1 has at least 1 of the variables
aggregate_male <- aggregate(behaviorAll$Aggregatemale, by= list(behaviorAll$sciName), FUN = sum)

#For general is.na == TRUE for all male, females, individual (12 totals:mean, max, median, min)
#For individual follow behaviorAll$Aggregatemale for females and individuals --> 4 is.na

#**mean/median/min/max_adult_females*
behaviorAll$Aggregatefemale <- ifelse(is.na(behaviorAll$Mean.adult.females) == TRUE & is.na(behaviorAll$Median.adult.females) == TRUE & is.na(behaviorAll$Min...adult.females) == TRUE & is.na(behaviorAll$Max...adult.females) ==TRUE, 0,1)
aggregate_female <- aggregate(behaviorAll$Aggregatefemale, by= list(behaviorAll$sciName), FUN = sum)

#**mean/median/min/max_individuals*
behaviorAll$Aggregateindividual <- ifelse(is.na(behaviorAll$Mean.individuals) == TRUE & is.na(behaviorAll$Median.individuals) == TRUE & is.na(behaviorAll$Min...of.individuals) == TRUE & is.na(behaviorAll$Max...of.individuals) ==TRUE, 0,1)
aggregate_individual <- aggregate(behaviorAll$Aggregateindividual, by= list(behaviorAll$sciName), FUN = sum)

#**general mean/median/min/max*
behaviorAll$Aggregate_general <- ifelse(is.na(behaviorAll$Mean.individuals) == TRUE & is.na(behaviorAll$Mean.adult.females)== TRUE &is.na(behaviorAll$Mean.adult.males)== TRUE & is.na(behaviorAll$Median.individuals)== TRUE & is.na(behaviorAll$Median.adult.males)== TRUE & is.na(behaviorAll$Median.adult.females)== TRUE & is.na(behaviorAll$Min...of.individuals)== TRUE & is.na(behaviorAll$Min...adult.males)== TRUE & is.na(behaviorAll$Min...adult.females)== TRUE & is.na(behaviorAll$Max...of.individuals)== TRUE & is.na(behaviorAll$Max...adult.males)== TRUE & is.na(behaviorAll$Max...adult.females)== TRUE, 0,1)
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

#*Seasonal Activity budget
##*activity budget:  a) % social b) % grooming c) % feeding d) rate of aggresstion  e) submission
##*a) % social
##*% time social in table as "X..time.social" 
mergedActivity$X..time.social <- ifelse(is.na(mergedActivity$X..time.social.x) == TRUE & is.na(mergedActivity$X..time.social.y) == TRUE, 0,1)
mergedActivity_social <- aggregate(mergedActivity$X..time.social, list(mergedActivity$sciName), FUN = sum)

#percent_feeding 
mergedActivity$X..time.feeding <- ifelse(is.na(mergedActivity$X..time.feeding.x) == TRUE & is.na(mergedActivity$X..time.feeding.y) == TRUE, 0,1)
mergedActivity_feeding <- aggregate(mergedActivity$X..time.feeding, list(mergedActivity$sciName), FUN = sum)

#percent_grooming 
mergedActivity$X..time.grooming <- ifelse(is.na(mergedActivity$X..time.grooming.x) == TRUE & is.na(mergedActivity$X..time.grooming.y) == TRUE, 0,1)
mergedActivity_grooming <- aggregate(mergedActivity$X..time.grooming, list(mergedActivity$sciName), FUN = sum)

#rate_aggression 
mergedActivity$Rate.of.aggression <- ifelse(is.na(mergedActivity$Rate.of.aggression.x) == TRUE & is.na(mergedActivity$Rate.of.aggression.y) == TRUE, 0,1)
mergedActivity_aggression <- aggregate(mergedActivity$Rate.of.aggression, list(mergedActivity$sciName), FUN = sum)

#rate_submission 
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

##**Seasonal Feeding data 
##*feeding data:  a) specific data (plant reproductive parts, fungi, insect, foliage)
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

##*Seasonal Home range size
mergedRange$Home.range.size..ha.. <- ifelse(is.na(mergedRange$Home.range.size..ha..x) == TRUE & is.na(mergedRange$Home.range.overlap..y) == TRUE, 0,1)
mergedRange_size <- aggregate(mergedRange$Home.range.size..ha.., list(mergedRange$sciName), FUN = sum)

mergedRange$Home.range.overlap.. <- ifelse(is.na(mergedRange$Home.range.overlap..x) == TRUE & is.na(mergedRange$Home.range.overlap..y) == TRUE, 0,1)
mergedRange_overlap <- aggregate(mergedRange$Home.range.overlap.., list(mergedRange$sciName), FUN = sum)
mergedRange_overlap <- homerangeAll[homerangeAll$Home.range.overlap. == 'Yes',] #selecting only for "yes"
mergedRange_overlap <- aggregate(mergedRange$Home.range.overlap.., list(mergedRange$sciName), FUN = sum)

############################################
##### Merging summarized data together #####
############################################
mergedrange_all <- merge(mergedRange_overlap, mergedRange_size,
                     by.x = c("Group.1"), 
                     by.y = c("Group.1"),
                     all.x = TRUE, all.y = TRUE)

names(mergedrange_all) <- c("Lumper_Taxonomy", "Range Overlap", "Range Size")

---
mergedfeeding1	 <-merge(mergedFeeding_fungus, mergedFeeding_insects,
                            by.x = c("Group.1"), 
                            by.y = c("Group.1"),
                            all.x = TRUE, all.y = TRUE)
mergedfeeding2	 <-merge(mergedfeeding1, mergedFeeding_fol, mergedFeeding_prp,
                            by.x = c("Group.1"), 
                            by.y = c("Group.1"),
                            all.x = TRUE, all.y = TRUE)
mergedfeeding_all	 <-merge(mergedfeeding2, mergedFeeding_prp,
                            by.x = c("Group.1"), 
                            by.y = c("Group.1"),
                            all.x = TRUE, all.y = TRUE)

names(mergedfeeding_all) <- c("Lumper_Taxonomy", "Fungus", "Insects", " Folivory", "Plant reproductive parts")

###Name your columns after completing all of the merges

---
mergedactivity_all_1 <- merge(mergedActivity_aggression, mergedActivity_feeding, 
                            by.x = c("Group.1"), 
                            by.y = c("Group.1"),
                            all.x = TRUE, all.y = TRUE)

mergedactivity_all_2 <- merge(mergedActivity_grooming, mergedActivity_social, 
                              by.x = c("Group.1"), 
                              by.y = c("Group.1"),
                              all.x = TRUE, all.y = TRUE)

mergedactivity_all_3 <- merge(mergedactivity_all_1, mergedactivity_all_2, 
                              by.x = c("Group.1"), 
                              by.y = c("Group.1"),
                              all.x = TRUE, all.y = TRUE)

mergedactivity_all <- merge(mergedactivity_all_3, mergedActivity_submission, 
                            by.x = c("Group.1"), 
                            by.y = c("Group.1"),
                            all.x = TRUE, all.y = TRUE)

names(mergedactivity_all) <- c("Lumper_Taxonomy", "Rate of aggression", "%Feeding", "%Grooming", "%Social", "Rate of submission")


behavior_all <- merge(allomaternal_care, social_learning, social_organization, intergroup_encounter_study, 
                      by.x = c("Group.1"), 
                      by.y = c("Group.1"),
                      all.x = TRUE, all.y = TRUE)
View(behavior_all)
names(behavior_all) <- c("Lumper_Taxonomy") #only 3 colums, need to find names for 2 data columns
# columns do not match the # of variables merged in the function above

names(behavior_all) <- c("Lumper_Taxonomy")



aggregate_female_all <-merge(aggregate_female, aggregate_fem_dispersal, aggregate_femsec_dispersal, 
                      by.x = c("Group.1"), 
                      by.y = c("Group.1"),
                      all.x = TRUE, all.y = TRUE)
  
aggregate_male_all <- merge(aggregate_male, aggregate_male_dispersal, aggregate_malesec_dispersal, 
                            by.x = c("Group.1"), 
                            by.y = c("Group.1"),
                            all.x = TRUE, all.y = TRUE)

aggregate_general_all <- merge(aggregate_general, aggregate_dispersalgeneral,
                               by.x = c("Group.1"), 
                               by.y = c("Group.1"),
                               all.x = TRUE, all.y = TRUE)

aggregate_all <- merge(aggregate_female_all, aggregate_male_all, aggregate_general_all,
                       by.x = c("Group.1"), 
                       by.y = c("Group.1"),
                       all.x = TRUE, all.y = TRUE)

merged_aggregate_all <- merge(aggregate_individual,  aggregate_all, 
                                  by.x = c("Group.1"), 
                                  by.y = c("Group.1"),
                                  all.x = TRUE, all.y = TRUE)

View(merged_aggregate_all) ## 6 columns (1 being Lumper taxonomy hence:)
names(merged_aggregate_all) <- c("Lumper_Taxonomy", "Dispersal Female data", "Dispersal Male data", "Dispersal General data", "Dispersal fem, male, gen data", "Dispersal Individual data")

#mergedrange_all
#mergedfeeding_all
#mergedactivity_all
#behavior_all
#merged_aggregate_all



all_data_merged_1 <-  merge(mergedrange_all,  mergedfeeding_all,
                          by.x = c("Lumper_Taxonomy"), 
                          by.y = c("Lumper_Taxonomy"),
                          all.x = TRUE, all.y = TRUE)

all_data_merged_2 <- merge(mergedactivity_all,  behavior_all,
                           by.x = c("Lumper_Taxonomy"), 
                           by.y = c("Lumper_Taxonomy"),
                           all.x = TRUE, all.y = TRUE)

all_data_merged_3 <- merge(all_data_merged_1,  all_data_merged_2,
                           by.x = c("Lumper_Taxonomy"), 
                           by.y = c("Lumper_Taxonomy"),
                           all.x = TRUE, all.y = TRUE)

all_data_merged <- merge(all_data_merged_3,  merged_aggregate_all,
                         by.x = c("Lumper_Taxonomy"), 
                         by.y = c("Lumper_Taxonomy"),
                         all.x = TRUE, all.y = TRUE)


## Error in `[.data.frame`(x, rep.int(NA_integer_, nyy), nm.x, drop = FALSE) : 
## undefined columns selected
## changed from Group.1 ro Lumper_taxonomy, and still errors

#############################
##### Plotting heat map #####
#############################
# https://r-graph-gallery.com/215-the-heatmap-function.html
# The mtcars dataset:
data <- as.matrix(mtcars)

# Default Heatmap
heatmap(data)

matrix_data			<- as.matrix(all_data_merged[, -1])
rownames(matrix_data)	<- all_data_merged$Lumper_Taxonomy
heatmap(matrix_data)

heatmap(matrix_data, scale = "column") 

heatmap(matrix_data, col = terrain.colors(256)) #works