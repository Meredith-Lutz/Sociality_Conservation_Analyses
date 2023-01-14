########################################
########################################
##### Heat Map Construction by Arianna #####
########################################
########################################


setwd("C:/Users/arian/OneDrive/Desktop/PREdiCT")

install.packages("ape")
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


##summarizing by species 
social_organization <- aggregate(behaviorAll$Social.organization, by= list(behaviorAll$sciName), FUN = length)
#group 1 = species 
#x = # of line we have for that species that are not blank

#for combined variable, create a new variable if/else, 
#if the first isn't blank then, if it is blank then move forward 
#until you find a blank, if all of them call the variable 0 (1 = has values) -> general column for 
#specific data per column use aggregate () as done above
#watch out for numerical vs categorical variables  

allomaternal_care <- aggregate(behaviorAll$Allomaternal.care, by= list(behaviorAll$sciName), FUN = length)

#social learning, only yes 
#df[df$var1 == 'value', ]
behaviorAll <- behaviorAll[behaviorAll$Social.learning == 'Yes',] #selecting only for "yes"
social_learning <- aggregate(behaviorAll$Social.learning, by= list(behaviorAll$sciName), FUN = length)
#need to change behaviorAll back to the original, otherwise I have less variables 

#Intergroup encounter study
behaviorAll <- behaviorAll[behaviorAll$Intergroup.encounter.study. == 'Yes',] #selecting only for "yes"
intergroup_encounter_study <- aggregate(behaviorAll$Intergroup.encounter.study., by= list(behaviorAll$sciName), FUN = length)

#mean/median/min/max_individuals
mean_individuals <- aggregate(behaviorAll$Mean.individuals, by= list(behaviorAll$sciName), FUN = length)
median_individuals <- aggregate(behaviorAll$Median.individuals, by= list(behaviorAll$sciName), FUN = length)
min_individuals <- aggregate(behaviorAll$Min...of.individuals, by= list(behaviorAll$sciName), FUN = length)
max_individuals <- aggregate(behaviorAll$Max...of.individuals, by= list(behaviorAll$sciName), FUN = length)

#mean/median/min/max_adult_females
mean_adult_females <- aggregate(behaviorAll$Mean.adult.females, by= list(behaviorAll$sciName), FUN = length)
median_adult_females <- aggregate(behaviorAll$Median.adult.females, by= list(behaviorAll$sciName), FUN = length)
min_adult_females <- aggregate(behaviorAll$Min...adult.females, by= list(behaviorAll$sciName), FUN = length)
max_adult_females <- aggregate(behaviorAll$Max...adult.females, by= list(behaviorAll$sciName), FUN = length)

#mean/median/min/max_adult_males
mean_adult_males <- aggregate(behaviorAll$Mean.adult.males, by= list(behaviorAll$sciName), FUN = length)
median_adult_males <- aggregate(behaviorAll$Median.adult.males, by= list(behaviorAll$sciName), FUN = length)
min_adult_males <- aggregate(behaviorAll$Min...adult.males, by= list(behaviorAll$sciName), FUN = length)
max_adult_males <- aggregate(behaviorAll$Max...adult.males, by= list(behaviorAll$sciName), FUN = length)

#mean/median/min/max_general 
#**do I combine with rbind everything to make a general column, or is the general col "individuals"**

mean_general <- rbind(mean_individuals, mean_adult_females, mean_adult_males)
median_general <- rbind(median_individuals, median_adult_females, median_adult_males)
min_general <- rbind(min_individuals, min_adult_females, min_adult_males)
max_general <- rbind(max_individuals, max_adult_females, max_adult_females

#need to add column with general groups 
behaviorAll <- behaviorAll$mean_general
