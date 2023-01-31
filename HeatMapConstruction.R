########################################
########################################
##### Heat Map Construction by Arianna #####
########################################
########################################



##**Data required
##*1)min, max, mean, median of females and males **completed** --> split with general **General = NEED HELP WITH GENERAL IFELSE FUNCTION**
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
behaviorAll$Aggregatemale <- ifelse(is.na(behaviorAll$Mean.adult.males) == TRUE & is.na(behaviorAll$Median.adult.males) == TRUE & is.na(behaviorAll$Min...adult.males) == TRUE & is.na(behaviorAll$Max...adult.males) ==TRUE, 0,1)
#0 are missing 
#1 has at least 1 of the variables
                                   
table(behaviorAll$Aggregatemale) 

aggregare_male <- aggregate(behaviorAll$Aggregatemale, by= list(behaviorAll$sciName), FUN = sum)


#For general is.na == TRUE for all male, females, individual (12 totals:mean, max, median, min)
#For individual follow behaviorAll$Aggregatemale for females and individuals --> 4 is.na


#Dispersal -- simple portion (**need to confirm ifelse function with J/M) 
##*dispersal: a) female b) secondary female c) male  d) secondary male 

dispersal_female <- aggregate(behaviorAll$Female.dispersal, by= list(behaviorAll$sciName), FUN = length)     
dispersal_sec_fem <- aggregate(behaviorAll$Female.secondary.dispersal, by= list(behaviorAll$sciName), FUN = length)
dispersal_male <- aggregate(behaviorAll$Male.dispersal, by= list(behaviorAll$sciName), FUN = length)
dispersal_sec_male <- aggregate(behaviorAll$Male.secondary.dispersal, by= list(behaviorAll$sciName), FUN = length)
    
 
##*Activity budget
##*activity budget:  a) % social b) % grooming c) % feeding d) rate of aggresstion  e) submission
##**downloaded seasonal activity budget .csv* *check with J/M if code below is correct*

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

activityAll$sciName	<- ifelse(activityAll$species == '' & activityAll$subspecies == '', activityAll$Genus,
                              ifelse(activityAll$subspecies == '', paste(activityAll$Genus, '_', activityAll$species, sep = ''),
                                     paste(activityAll$Genus, '_', activityAll$species, '_', activityAll$subspecies, sep = '')))
##**does not work*
##need to merge seasonal activity budget with behavior to match scientific name 
## or I may no need this step if scientific name is already associated with article number in behaviorAl


behaviorAll			<- merge(behaviorAll, lumper, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
behaviorAll			<- merge(behaviorAll, IUCN_tax, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 
behaviorAll			<- merge(behaviorAll, TenKTrees, by.x = 'sciName', by.y = 'Mendeley.tag', all.x=TRUE) 



percent_social
percent_grooming
percent_feeding
rate_aggression
submission


##**Feeding data 
##*feeding data:  a) specific data (plant reproductive parts, fungi, insect, foliage)
#**dowloaded seasonal feeding data*

feeding001_500	<- read.csv('Article Coding Database (IDs_ 001 - 500) -Seasonal feeding data.csv', stringsAsFactors = FALSE)
feeding501_1000	<- read.csv('Article Coding Database (IDs_ 501 - 1000) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
feeding1001_1500	<- read.csv('Article Coding Database (IDs_ 1001 - 1500) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
feeding1501_2000	<- read.csv('Article Coding Database (IDs_ 1501 - 2000) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
feeding2001_2500	<- read.csv('Article Coding Database (IDs_ 2001 - 2500) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
feeding2501_3000	<- read.csv('Article Coding Database (IDs_ 2501 - 3000) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
feeding3001_3500	<- read.csv('Article Coding Database (IDs_ 3001 - 3500) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
feeding3501_4000	<- read.csv('Article Coding Database (IDs_ 3501 - 4000) - Seasonal feeding data.csv', stringsAsFactors = FALSE)

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





    
#General_male_mean 
#erorr keeps changing 
#"unexpected symbol "  " -- does not provide information where to find error
#tried to run the formula with
##*a) different column names
##*b) eliminating | at the end 
##*c) running throuhg any spelling mistake 
##*need to check repeated numbeers **this next**
behaviorAll$generalmean_male <- ifelse (behaviorAll$Mean.adult.males == 0 | behaviorAll$Mean.adult.males == 1 | behaviorAll$Mean.adult.males ==  2.32 | behaviorAll$Mean.adult.males == 4 | behaviorAll$Mean.adult.males == 9 |
                                          ifelse(behaviorAll$Mean.adult.males == 1.26 | behaviorAll$Mean.adult.males == 3.75 | behaviorAll$Mean.adult.males == 13 |
                                                   ifelse(behaviorAll$Mean.adult.males == 3.5 | behaviorAll$Mean.adult.males == 4.3| behaviorAll$Mean.adult.males == 3.2| behaviorAll$Mean.adult.males == 1.5 |
                                                            ifelse(behaviorAll$Mean.adult.males == 1.6 | behaviorAll$Mean.adult.males == 1.83 | behaviorAll$Mean.adult.males  == 6 |behaviorAll$Mean.adult.males == 5 |
                                                                     ifelse(behaviorAll$Mean.adult.males == 3.83 | behaviorAll$Mean.adult.males == 11 | behaviorAll$Mean.adult.males == 1.8 | behaviorAll$Mean.adult.males == 2.6|
                                                                              ifelse(behaviorAll$Mean.adult.males == 12 | behaviorAll$Mean.adult.males == 3.06 | behaviorAll$Mean.adult.males == 1.9 | behaviorAll$Mean.adult.males == 19 |
                                                                                       ifelse(behaviorAll$Mean.adult.males == 15 | behaviorAll$Mean.adult.males == 10 | behaviorAll$Mean.adult.males == 7 | behaviorAll$Mean.adult.males == 2.73 |
                                                                                                ifelse(behaviorAll$Mean.adult.males == 1.1 | behaviorAll$Mean.adult.males == 1.4 | behaviorAll$Mean.adult.males == 1.3 | behaviorAll$Mean.adult.males == 0.7 | behaviorAll$Mean.adult.males == 2.75 |  behaviorAll$Mean.adult.males == 2.44 |  behaviorAll$Mean.adult.males == 2.08 |
                                                                                                         ifelse(behaviorAll$Mean.adult.males == 1.6 | behaviorAll$Mean.adult.males == 2.4 | behaviorAll$Mean.adult.males == 2.5 | behaviorAll$Mean.adult.males == 2.05 | 
                                                                                                                  ifelse(behaviorAll$Mean.adult.males == 26 |
                                                                                                                           ifelse(behaviorAll$Mean.adult.males == 1.750 |  behaviorAll$Mean.adult.males == 2.3 |  behaviorAll$Mean.adult.males == 3.9 | behaviorAll$Mean.adult.males == 2.2 | behaviorAll$Mean.adult.males == 1.42 | behaviorAll$Mean.adult.males == 9 |  behaviorAll$Mean.adult.males == 20 |
                                                                                                                                    ifelse(behaviorAll$Mean.adult.males == 1.580 |  behaviorAll$Mean.adult.males == 8 |  behaviorAll$Mean.adult.males == 1.7 |  behaviorAll$Mean.adult.males == 18 |  behaviorAll$Mean.adult.males == 16 | behaviorAll$Mean.adult.males == 4.54 |  behaviorAll$Mean.adult.males == 27 | 
                                                                                                                                             ifelse(behaviorAll$Mean.adult.males == 1.375 |  behaviorAll$Mean.adult.males ==  0.17 |  behaviorAll$Mean.adult.males == 12 |  behaviorAll$Mean.adult.males == 33 | behaviorAll$Mean.adult.males == 3.3 | behaviorAll$Mean.adult.males == 11 |  behaviorAll$Mean.adult.males == 26.9 |
                                                                                                                                                      ifelse(behaviorAll$Mean.adult.males == 24.3 | behaviorAll$Mean.adult.males == 2.92 | behaviorAll$Mean.adult.males == 3.33 | behaviorAll$Mean.adult.males == 2.67 | behaviorAll$Mean.adult.males == 1.246875 | behaviorAll$Mean.adult.males == 1.717949 | behaviorAll$Mean.adult.males == 7.07 |
                                                                                                                                                               ifelse(behaviorAll$Mean.adult.males == 4.5 | behaviorAll$Mean.adult.males == 3.5 | behaviorAll$Mean.adult.males == 0.89 | behaviorAll$Mean.adult.males == 4.43 | behaviorAll$Mean.adult.males ==9.14 |behaviorAll$Mean.adult.males == 14 |  behaviorAll$Mean.adult.males == 1.291 |
                                                                                                                                                                        ifelse(behaviorAll$Mean.adult.males == 31.0 | behaviorAll$Mean.adult.males == 6 | behaviorAll$Mean.adult.males == 6.23 | behaviorAll$Mean.adult.males == 3.6 | behaviorAll$Mean.adult.males == 8.66 | behaviorAll$Mean.adult.males == 13 |  behaviorAll$Mean.adult.males == 21 |
                                                                                                                                                                                 ifelse(behaviorAll$Mean.adult.males == 3.2 | behaviorAll$Mean.adult.males == 6.67 | behaviorAll$Mean.adult.males == 2.20 | behaviorAll$Mean.adult.males == 5.1 | behaviorAll$Mean.adult.males == 4.714286 | behaviorAll$Mean.adult.males == 32 |  
                                                                                                                                                                                          ifelse(behaviorAll$Mean.adult.males == 23 |  behaviorAll$Mean.adult.males == 44 |  behaviorAll$Mean.adult.males == 47 |  behaviorAll$Mean.adult.males == 155 |  behaviorAll$Mean.adult.males == 31 |  behaviorAll$Mean.adult.males == 2.7 |  behaviorAll$Mean.adult.males == 17 | 
                                                                                                                                                                                                   ifelse(behaviorAll$Mean.adult.males == 42 |  behaviorAll$Mean.adult.males == 39 |  behaviorAll$Mean.adult.males == 55 |  behaviorAll$Mean.adult.males == 82 |  behaviorAll$Mean.adult.males == 189 |  behaviorAll$Mean.adult.males == 24 |  behaviorAll$Mean.adult.males == 7.67 |
                                                                                                                                                                                                            ifelse(behaviorAll$Mean.adult.males == 7.3 |  behaviorAll$Mean.adult.males == 22 |  behaviorAll$Mean.adult.males == 21 |  behaviorAll$Mean.adult.males == 6.8 |  behaviorAll$Mean.adult.males == 25 |  behaviorAll$Mean.adult.males == 4.54 |  behaviorAll$Mean.adult.males == 3.8 |  
                                                                                                                                                                                                                     ifelse(behaviorAll$Mean.adult.males == 17 | behaviorAll$Mean.adult.males == 32 | behaviorAll$Mean.adult.males == 2.94 |
                                                                                                                                                                                                                              ifelse(behaviorAll$Mean.adult.males == 1.36 |  behaviorAll$Mean.adult.males == 1.33 |  behaviorAll$Mean.adult.males ==  1.45 |
                                                                                                                                                                                                                                       ifelse(behaviorAll$Mean.adult.males ==  2.51 | behaviorAll$Mean.adult.males ==  1.5 | behaviorAll$Mean.adult.males == 2.89 |
                                                                                                                                                                                                                                                ifelse(behaviorAll$Mean.adult.males == 2.71 |  behaviorAll$Mean.adult.males == 2.5 |  behaviorAll$Mean.adult.males == 2.86 |  behaviorAll$Mean.adult.males == 3.29 |
                                                                                                                                                                                                                                                         ifelse(behaviorAll$Mean.adult.males == 2.88 |  behaviorAll$Mean.adult.males == 3.36| behaviorAll$Mean.adult.males == 2.86 |
                                                                                                                                                                                                                                                                  ifelse(behaviorAll$Mean.adult.males == 0.71 | behaviorAll$Mean.adult.males == 1.75 | behaviorAll$Mean.adult.males == 1.25 |
                                                                                                                                                                                                                                                                           ifelse(behaviorAll$Mean.adult.males == 2.24 | behaviorAll$Mean.adult.males == 2.71 | behaviorAll$Mean.adult.males == 2.36 |behaviorAll$Mean.adult.males ==  2.15 |   
                                                                                                                                                                                                                                                                                    ifelse(behaviorAll$Mean.adult.males == 6.1 |  behaviorAll$Mean.adult.males == 3.90 |  behaviorAll$Mean.adult.males ==  2.90 | behaviorAll$Mean.adult.males == 2.50 |
                                                                                                                                                                                                                                                                                             ifelse(behaviorAll$Mean.adult.males == 2.09 |  behaviorAll$Mean.adult.males ==  2.58 |  behaviorAll$Mean.adult.males ==  1.87 |
                                                                                                                                                                                                                                                                                                      ifelse(behaviorAll$Mean.adult.males == 2.37| behaviorAll$Mean.adult.males == 1.89 |  behaviorAll$Mean.adult.males == 3.46 |  behaviorAll$Mean.adult.males == 1.43 |
                                                                                                                                                                                                                                                                                                               ifelse(behaviorAll$Mean.adult.males == 2.54 | behaviorAll$Mean.adult.males == 1.33 |  behaviorAll$Mean.adult.males == 106 | behaviorAll$Mean.adult.males == 7.7 |
                                                                                                                                                                                                                                                                                                                        ifelse(behaviorAll$Mean.adult.males == 41 |  behaviorAll$Mean.adult.males == 96.8 |  behaviorAll$Mean.adult.males == 17 | behaviorAll$Mean.adult.males == 2.9 |
                                                                                                                                                                                                                                                                                                                                 ifelse(behaviorAll$Mean.adult.males == 3.5 | behaviorAll$Mean.adult.males == 2 | 
                                                                                                                                                                                                                                                                                                                                          ifelse(behaviorAll$Mean.adult.males == 5 | behaviorAll$Mean.adult.males == 4.1 |  behaviorAll$Mean.adult.males == 1.5 | behaviorAll$Mean.adult.males == 1.73 | behaviorAll$Mean.adult.males == 1.97 |
                                                                                                                                                                                                                                                                                                                                                   ifelse(behaviorAll$Mean.adult.males == 1.36 |  behaviorAll$Mean.adult.males == 1.17 | behaviorAll$Mean.adult.males == 2.33 |  behaviorAll$Mean.adult.males == 0.89 |
                                                                                                                                                                                                                                                                                                                                                            ifelse(behaviorAll$Mean.adult.males == 2.22 | behaviorAll$Mean.adult.males == 1.29 |  behaviorAll$Mean.adult.males ==  1.15 
                                                                                                                                                                                                                                                                                                                                                                     ifelse(behaviorAll$Mean.adult.males == 1.13 | behaviorAll$Mean.adult.males ==  1.20 | behaviorAll$Mean.adult.males == 1.25 | behaviorAll$Mean.adult.males == 1.57 | behaviorAll$Mean.adult.males == 1.50 
                                                                                                                                                                                                                                                                                                                                                                              ifelse(behaviorAll$Mean.adult.males == 1.14| behaviorAll$Mean.adult.males == 0.80
                                                                                                                                                                                                                                                                                                                                                                                     ))))))))))))))))))))))))))))))))))))))))))))))))
                                          
behaviorAll$Mean.adult.males <- ifelse (behaviorAll$Mean.adult.males == 0 | behaviorAll$Mean.adult.males == 1 | behaviorAll$Mean.adult.males ==  2.32 | behaviorAll$Mean.adult.males == 4 | behaviorAll$Mean.adult.males == 9 
                                          ifelse(behaviorAll$Mean.adult.males == 1.26 | behaviorAll$Mean.adult.males == 3.75 | behaviorAll$Mean.adult.males == 13 
                                                   ifelse(behaviorAll$Mean.adult.males == 3.5 | behaviorAll$Mean.adult.males == 4.3| behaviorAll$Mean.adult.males == 3.2| behaviorAll$Mean.adult.males == 1.5 
                                                            ifelse(behaviorAll$Mean.adult.males == 1.6 | behaviorAll$Mean.adult.males == 1.83 | behaviorAll$Mean.adult.males  == 6 |behaviorAll$Mean.adult.males == 5 
                                                                     ifelse(behaviorAll$Mean.adult.males == 3.83 | behaviorAll$Mean.adult.males == 11 | behaviorAll$Mean.adult.males == 1.8 | behaviorAll$Mean.adult.males == 2.6
                                                                              ifelse(behaviorAll$Mean.adult.males == 12 | behaviorAll$Mean.adult.males == 3.06 | behaviorAll$Mean.adult.males == 1.9 | behaviorAll$Mean.adult.males == 19 
                                                                                       ifelse(behaviorAll$Mean.adult.males == 15 | behaviorAll$Mean.adult.males == 10 | behaviorAll$Mean.adult.males == 7 | behaviorAll$Mean.adult.males == 2.73 
                                                                                                ifelse(behaviorAll$Mean.adult.males == 1.1 | behaviorAll$Mean.adult.males == 1.4 | behaviorAll$Mean.adult.males == 1.3 | behaviorAll$Mean.adult.males == 0.7 | behaviorAll$Mean.adult.males == 2.75 |  behaviorAll$Mean.adult.males == 2.44 |  behaviorAll$Mean.adult.males == 2.08 
                                                                                                         ifelse(behaviorAll$Mean.adult.males == 1.6 | behaviorAll$Mean.adult.males == 2.4 | behaviorAll$Mean.adult.males == 2.5 | behaviorAll$Mean.adult.males == 2.05 
                                                                                                                  ifelse(behaviorAll$Mean.adult.males == 1.83 |  behaviorAll$Mean.adult.males == |  behaviorAll$Mean.adult.males == 26 
                                                                                                                           ifelse(behaviorAll$Mean.adult.males == 1.750 |  behaviorAll$Mean.adult.males == 2.3 |  behaviorAll$Mean.adult.males == 3.9 | behaviorAll$Mean.adult.males == 2.2 | behaviorAll$Mean.adult.males == 1.42 | behaviorAll$Mean.adult.males == 9 |  behaviorAll$Mean.adult.males == 20 
                                                                                                                                    ifelse(behaviorAll$Mean.adult.males == 1.580 |  behaviorAll$Mean.adult.males == 8 |  behaviorAll$Mean.adult.males == 1.7 |  behaviorAll$Mean.adult.males == 18 |  behaviorAll$Mean.adult.males == 16 | behaviorAll$Mean.adult.males == 4.54 |  behaviorAll$Mean.adult.males == 27 
                                                                                                                                             ifelse(behaviorAll$Mean.adult.males == 1.375 |  behaviorAll$Mean.adult.males ==  0.17 |  behaviorAll$Mean.adult.males == 12 |  behaviorAll$Mean.adult.males == 33 | behaviorAll$Mean.adult.males == 3.3 | behaviorAll$Mean.adult.males == 11 |  behaviorAll$Mean.adult.males == 26.9
                                                                                                                                                      ifelse(behaviorAll$Mean.adult.males == 24.3 | behaviorAll$Mean.adult.males == 2.92 | behaviorAll$Mean.adult.males == 3.33 | behaviorAll$Mean.adult.males == 2.67 | behaviorAll$Mean.adult.males == 1.246875 | behaviorAll$Mean.adult.males == 1.717949 | behaviorAll$Mean.adult.males == 7.07
                                                                                                                                                               ifelse(behaviorAll$Mean.adult.males == 4.5 | behaviorAll$Mean.adult.males == 3.5 | behaviorAll$Mean.adult.males == 0.89 | behaviorAll$Mean.adult.males == 4.43 | behaviorAll$Mean.adult.males ==9.14 |behaviorAll$Mean.adult.males == 14 |  behaviorAll$Mean.adult.males == 1.291
                                                                                                                                                                        ifelse(behaviorAll$Mean.adult.males == 31.0 | behaviorAll$Mean.adult.males == 6 | behaviorAll$Mean.adult.males == 6.23 | behaviorAll$Mean.adult.males == 3.6 | behaviorAll$Mean.adult.males == 8.66 | behaviorAll$Mean.adult.males == 13 |  behaviorAll$Mean.adult.males == 21
                                                                                                                                                                                 ifelse(behaviorAll$Mean.adult.males == 3.2 | behaviorAll$Mean.adult.males == 6.67 | behaviorAll$Mean.adult.males == 2.20 | behaviorAll$Mean.adult.males == 5.1 | behaviorAll$Mean.adult.males == 4.714286 | behaviorAll$Mean.adult.males == 32
                                                                                                                                                                                          ifelse(behaviorAll$Mean.adult.males == 23 |  behaviorAll$Mean.adult.males == 44 |  behaviorAll$Mean.adult.males == 47 |  behaviorAll$Mean.adult.males == 155 |  behaviorAll$Mean.adult.males == 31 |  behaviorAll$Mean.adult.males == 2.7 |  behaviorAll$Mean.adult.males == 17 
                                                                                                                                                                                                   ifelse(behaviorAll$Mean.adult.males == 42 |  behaviorAll$Mean.adult.males == 39 |  behaviorAll$Mean.adult.males == 55 |  behaviorAll$Mean.adult.males == 82 |  behaviorAll$Mean.adult.males == 189 |  behaviorAll$Mean.adult.males == 24 |  behaviorAll$Mean.adult.males == 7.67 
                                                                                                                                                                                                            ifelse(behaviorAll$Mean.adult.males == 7.3 |  behaviorAll$Mean.adult.males == 22 |  behaviorAll$Mean.adult.males == 21 |  behaviorAll$Mean.adult.males == 6.8 |  behaviorAll$Mean.adult.males == 25 |  behaviorAll$Mean.adult.males == 4.54 |  behaviorAll$Mean.adult.males == 3.8   
                                                                                                                                                                                                                     ifelse(behaviorAll$Mean.adult.males == 17 | behaviorAll$Mean.adult.males == 32 | behaviorAll$Mean.adult.males == 2.94 
                                                                                                                                                                                                                              ifelse(behaviorAll$Mean.adult.males == 1.36 |  behaviorAll$Mean.adult.males == 1.33 |  behaviorAll$Mean.adult.males ==  1.45 
                                                                                                                                                                                                                                       ifelse(behaviorAll$Mean.adult.males ==  2.51 | behaviorAll$Mean.adult.males ==  1.5 | behaviorAll$Mean.adult.males == 2.89 
                                                                                                                                                                                                                                                ifelse(behaviorAll$Mean.adult.males == 2.71 |  behaviorAll$Mean.adult.males == 2.5 |  behaviorAll$Mean.adult.males == 2.86 |  behaviorAll$Mean.adult.males == 3.29 
                                                                                                                                                                                                                                                         ifelse(behaviorAll$Mean.adult.males == 2.88 |  behaviorAll$Mean.adult.males == 3.36| behaviorAll$Mean.adult.males == 2.86 
                                                                                                                                                                                                                                                                  ifelse(behaviorAll$Mean.adult.males == 0.71 | behaviorAll$Mean.adult.males == 1.75 | behaviorAll$Mean.adult.males == 1.25 
                                                                                                                                                                                                                                                                           ifelse(behaviorAll$Mean.adult.males == 2.24 | behaviorAll$Mean.adult.males == 2.71 | behaviorAll$Mean.adult.males == 2.36 |behaviorAll$Mean.adult.males ==  2.15
                                                                                                                                                                                                                                                                                    ifelse(behaviorAll$Mean.adult.males == 6.1 |  behaviorAll$Mean.adult.males == 3.90 |  behaviorAll$Mean.adult.males ==  2.90 | behaviorAll$Mean.adult.males == 2.50 
                                                                                                                                                                                                                                                                                             ifelse(behaviorAll$Mean.adult.males == 2.09 |  behaviorAll$Mean.adult.males ==  2.58 |  behaviorAll$Mean.adult.males ==  1.87 
                                                                                                                                                                                                                                                                                                      ifelse(behaviorAll$Mean.adult.males == 2.37| behaviorAll$Mean.adult.males == 1.89 |  behaviorAll$Mean.adult.males == 3.46 |  behaviorAll$Mean.adult.males == 1.43 
                                                                                                                                                                                                                                                                                                               ifelse(behaviorAll$Mean.adult.males == 2.54 | behaviorAll$Mean.adult.males == 1.33 |  behaviorAll$Mean.adult.males == 106 | behaviorAll$Mean.adult.males == 7.7 
                                                                                                                                                                                                                                                                                                                        ifelse(behaviorAll$Mean.adult.males == 41 |  behaviorAll$Mean.adult.males == 96.8 |  behaviorAll$Mean.adult.males == 17 | behaviorAll$Mean.adult.males == 2.9 
                                                                                                                                                                                                                                                                                                                                 ifelse(behaviorAll$Mean.adult.males == 3.5 | behaviorAll$Mean.adult.males == 2  
                                                                                                                                                                                                                                                                                                                                          ifelse(behaviorAll$Mean.adult.males == 5 | behaviorAll$Mean.adult.males == 4.1 |  behaviorAll$Mean.adult.males == 1.5 | behaviorAll$Mean.adult.males == 1.73 | behaviorAll$Mean.adult.males == 1.97 
                                                                                                                                                                                                                                                                                                                                                   ifelse(behaviorAll$Mean.adult.males == 1.36 |  behaviorAll$Mean.adult.males == 1.17 | behaviorAll$Mean.adult.males == 2.33 |  behaviorAll$Mean.adult.males == 0.89 
                                                                                                                                                                                                                                                                                                                                                            ifelse(behaviorAll$Mean.adult.males == 2.22 |  behaviorAll$Mean.adult.males == 1.29 |  behaviorAll$Mean.adult.males ==  1.15 
                                                                                                                                                                                                                                                                                                                                                                     ifelse(behaviorAll$Mean.adult.males == 1.13 |  behaviorAll$Mean.adult.males ==  1.20 | behaviorAll$Mean.adult.males == 1.25 | behaviorAll$Mean.adult.males == 1.57 | behaviorAll$Mean.adult.males == 1.50
                                                                                                                                                                                                                                                                                                                                                                              ifelse(behaviorAll$Mean.adult.males == 1.14|  behaviorAll$Mean.adult.males == 0.80))))))))))))))))))))))))))))))))))))))))))))))))





socialData$season <- ifelse(socialData$month ==  1 | socialData$month == 2 | socialData$month == 3 , 'mating',
                            ifelse(socialData$month == 7 | socialData$month == 8 | socialData$month == 9, 'birthing', 'other'))

socialData$monthNum <- ifelse(socialData$Month == 'Jan', '01',
                              ifelse(socialData$Month == 'Feb', '02',
                                     ifelse(socialData$Month == 'Mar', '03',
                                            ifelse(socialData$Month == 'Apr', '04',
                                                   ifelse(socialData$Month == 'May', '05',
                                                          ifelse(socialData$Month == 'Jun', '06',
                                                                 ifelse(socialData$Month == 'Jul', '07',
                                                                        ifelse(socialData$Month == 'Aug', '08',
                                                                               ifelse(socialData$Month == 'Sep', '09',
                                                                                      ifelse(socialData$Month == 'Oct', '10',
                                                                                             ifelse(socialData$Month == 'Nov', '11', '12')))))))))))

observerToKeep <- ifelse(observer1 == "Becca", "Becca",
                         ifelse(observer1 == "Meredith", "Meredith",
                                ifelse(observer1 == "Max", "Max",
                                       ifelse(observer1 == "Max and Becca", "Max and Becca",
                                              ifelse(observer1 == "Patrick", "Patrick",
                                                     ifelse(observer1 == "Andry", "Andry",
                                                            ifelse(observer1 == "Daniel", "Daniel",
                                                                   ifelse(observer1 == "Dessy", "Dessy",
                                                                          ifelse(observer1 == "Francis", "Francis",
                                                                                 ifelse(observer1 == "Laura", "Laura",
                                                                                        ifelse(observer1 == "Mampionona", "Mampionona",
                                                                                               ifelse(observer1 == "Elvis", "Elvis", "Felana"
                                                                                               ))))))))))))

socialData$season <- ifelse(socialData$month ==  1 | socialData$month == 2 | socialData$month == 3 , 'mating',
                            ifelse(socialData$month == 7 | socialData$month == 8 | socialData$month == 9, 'birthing', 'other'))



