################################################################
################################################################
##### Behavior Data (Actv Budget, Diet, Seasonal) Cleaning #####
################################################################
################################################################

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
 
 #copy the database and replace blanks with NAs
 behaviorAllNoBlanks <- behaviorAll
behaviorAllNoBlanks[behaviorAllNoBlanks == ""] <- NA

#Read in all of the seasonal activity budget data & combine together
seasonalactivity001_500	<- read.csv('Article Coding Database (IDs_ 001 - 500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
seasonalactivity501_1000	<- read.csv('Article Coding Database (IDs_ 501 - 1000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
seasonalactivity1001_1500	<- read.csv('Article Coding Database (IDs_ 1001 - 1500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
seasonalactivity1501_2000	<- read.csv('Article Coding Database (IDs_ 1501 - 2000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
seasonalactivity2001_2500	<- read.csv('Article Coding Database (IDs_ 2001 - 2500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
seasonalactivity2501_3000	<- read.csv('Article Coding Database (IDs_ 2501 - 3000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
seasonalactivity3001_3500	<- read.csv('Article Coding Database (IDs_ 3001 - 3500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
seasonalactivity3501_4000	<- read.csv('Article Coding Database (IDs_ 3501 - 4000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)

colnames(seasonalactivity3501_4000) <- colnames(seasonalactivity3001_3500) <- colnames(seasonalactivity2501_3000) <- colnames(seasonalactivity1501_2000) <- colnames(seasonalactivity501_1000) <- colnames(seasonalactivity2001_2500) <- colnames(seasonalactivity1001_1500) <- colnames(seasonalactivity001_500)

seasonalActivityAll	<- rbind(seasonalactivity001_500, seasonalactivity501_1000, seasonalactivity1001_1500, 			seasonalactivity1501_2000, seasonalactivity2001_2500, seasonalactivity2501_3000, seasonalactivity3001_3500, seasonalactivity3501_4000)

#Read in all of the seasonal feeding budget data & combine together
seasonalfeeding001_500	<- read.csv('Article Coding Database (IDs_ 001 - 500) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
seasonalfeeding501_1000	<- read.csv('Article Coding Database (IDs_ 501 - 1000) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
seasonalfeeding1001_1500	<- read.csv('Article Coding Database (IDs_ 1001 - 1500) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
seasonalfeeding1501_2000	<- read.csv('Article Coding Database (IDs_ 1501 - 2000) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
seasonalfeeding2001_2500	<- read.csv('Article Coding Database (IDs_ 2001 - 2500) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
seasonalfeeding2501_3000	<- read.csv('Article Coding Database (IDs_ 2501 - 3000) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
seasonalfeeding3001_3500	<- read.csv('Article Coding Database (IDs_ 3001 - 3500) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
seasonalfeeding3501_4000	<- read.csv('Article Coding Database (IDs_ 3501 - 4000) - Seasonal feeding data.csv', stringsAsFactors = FALSE)

colnames(seasonalfeeding3501_4000) <- colnames(seasonalfeeding3001_3500) <- colnames(seasonalfeeding2501_3000) <- colnames(seasonalfeeding1501_2000) <- colnames(seasonalfeeding501_1000) <- colnames(seasonalfeeding2001_2500) <- colnames(seasonalfeeding1001_1500) <- colnames(seasonalfeeding001_500)

seasonalFeedingAll	<- rbind(seasonalfeeding001_500, seasonalfeeding501_1000, seasonalfeeding1001_1500, 			seasonalfeeding1501_2000, seasonalfeeding2001_2500, seasonalfeeding2501_3000, seasonalfeeding3001_3500, seasonalfeeding3501_4000)

#########################
#########################
##### Data Cleaning #######
#########################
#########################


####################################
####### Behavior Sheet Feeding #########
####################################

head(behaviorAllNoBlanks)

#shows the column names of behavior All
colnames(behaviorAllNoBlanks)

#check the class of the variable ' % time feeding' and change class to numeric
class(behaviorAllNoBlanks $X..time.feeding)
behaviorAllNoBlanks $X..time.feeding <- as.numeric(behaviorAllNoBlanks $X..time.feeding)
class(behaviorAllNoBlanks $X..time.feeding)

#Call records in behavior sheet that have extreme values for %time feeding (less than 10%)
LowFeedingValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..time.feeding <= 10 & is.na(behaviorAllNoBlanks $X..time.feeding)==FALSE, 
			c('Article.ID', 'X..time.feeding')]

#this object gives you a table of article IDs and X..time.feeding that is <= 10 %. Check to make sure these are correct
#using the PDFs to verify. Check off on the "datatodoublecheck" sheet for documentation.
LowFeedingValues	
			
#Call records in behavior sheet that have extreme values for %time feeding (greater than 90%)
HighFeedingValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..time.feeding >= 90 & is.na(behaviorAllNoBlanks $X..time.feeding)==FALSE, 
			c('Article.ID', 'X..time.feeding')]		
HighFeedingValues
#looks like there are no rows, so maybe no errors to check?

####################################
####### Behavior Sheet Social ##########
####################################

class(behaviorAllNoBlanks $X..time.social)
#this is already a numeric

#Call records in behavior sheet that have extreme values for %time social (less than 10%)
LowSocialValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..time.social <= 10 & is.na(behaviorAllNoBlanks $X..time.social)==FALSE, 
			c('Article.ID', 'X..time.social')]

#this object gives you a table of article IDs and X..time.social that is <= 10 %. Check to make sure these are correct
#using the PDFs to verify. Check off on the "datatodoublecheck" sheet for documentation.
LowSocialValues	
#this might be more realistic for some to actually be in this low range, but better to double check?


#Call records in behavior sheet that have extreme values for %time social (greater than 90%)
HighSocialValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..time.social >= 90 & is.na(behaviorAllNoBlanks $X..time.social)==FALSE, 
			c('Article.ID', 'X..time.social')]		
HighSocialValues
#again, says 0 rows so maybe no errors to check?


####################################
####### Behavior Sheet Grooming #######
####################################

class(behaviorAllNoBlanks $X..time.grooming)
#this is already a numeric

#Call records in behavior sheet that have extreme values for %time grooming (less than 10%)
LowGroomingValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..time.grooming <= 10 & is.na(behaviorAllNoBlanks $X..time.grooming)==FALSE, 
			c('Article.ID', 'X..time.grooming')]

#this object gives you a table of article IDs and X..time.social that is <= 10 %. Check to make sure these are correct
#using the PDFs to verify. Check off on the "datatodoublecheck" sheet for documentation.
LowGroomingValues	
#this might be more realistic for some to actually be in this low range, but better to double check?


#Call records in behavior sheet that have extreme values for %time social (greater than 90%)
HighGroomingValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..time.grooming >= 90 & is.na(behaviorAllNoBlanks $X..time.grooming)==FALSE, 
			c('Article.ID', 'X..time.grooming')]		

HighGroomingValues
#again, says 0 rows so maybe no errors to check?

#############STILL WORKING ON THIS SECTION: 
#######check to see if there is a grooming %, is there a method present? #######

GroomingValuesWithMethod <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks $X..time.grooming)==FALSE,
			c('Article.ID', 'X..time.grooming', 'Data.method.grooming')]

GroomingValuesWithMethod$Data.method.grooming <- as.factor(GroomingValuesWithMethod$Data.method.grooming)

GroomingMethodBlanks <- GroomingValuesWithMethod[GroomingValuesWithMethod$Data.method.grooming == "<NA>", c('Article.ID', 'X..time.grooming', 'Data.method.grooming')]
GroomingMethodBlanks

colnames(GroomingValuesWithMethod)

GroomingValuesAndMethod <- behaviorAll[behaviorAll$Data.method.grooming =="NA" & is.na(behaviorAll$X..time.grooming)==FALSE,  c('Article.ID', 'X..time.grooming'), 'Data.method.grooming']		

class(behaviorAll$Data.method.grooming)
behaviorAll$Data.method.grooming <- as.factor(behaviorAll$Data.method.grooming)
class(behaviorAll$Data.method.grooming)
behaviorAll$Data.method.grooming

#########################################
####### Behavior Sheet Plant Repr. Parts #######
#########################################

#check for % plant reproductive parts extreme values
PlantReproductivePartsLowValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..plant.reproductive.parts <= 10 &is.na(behaviorAllNoBlanks $X..plant.reproductive.parts) == FALSE, 
            c('Article.ID', 'X..plant.reproductive.parts')]
            
PlantReproductivePartsLowValues
#there are a few to check through

PlantReproductivePartsHighValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..plant.reproductive.parts >= 90 &is.na(behaviorAllNoBlanks $X..plant.reproductive.parts) == FALSE, 
            c('Article.ID', 'X..plant.reproductive.parts')]
            
PlantReproductivePartsHighValues
#there are a few to check through


####################################
####### Behavior Sheet Folivory ########
####################################

#check for % folivory extreme values
FolivoryLowValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..folivory <= 10 &is.na(behaviorAllNoBlanks $X..folivory) == FALSE, 
            c('Article.ID', 'X..folivory')]
            
FolivoryLowValues
#there's a handful to check here

FolivoryHighValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..folivory >= 90 &is.na(behaviorAllNoBlanks $X..folivory) == FALSE, 
            c('Article.ID', 'X..folivory')]
            
FolivoryHighValues
#there are a handful here to check

####################################
####### Behavior Sheet Insects ##########
####################################

#check for % insect extreme values
InsectLowValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..insects <= 10 &is.na(behaviorAllNoBlanks $X..insects) == FALSE, 
            c('Article.ID', 'X..insects')]
            
InsectLowValues
#theres a handful to check here

InsectHighValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..insects >= 90 &is.na(behaviorAllNoBlanks $X..insects) == FALSE, 
            c('Article.ID', 'X..insects')]
            
InsectHighValues
#there is one to check here

####################################
####### Behavior Sheet Fungus #########
####################################

#check for % fungus extreme values
FungusLowValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..fungus <= 10 &is.na(behaviorAllNoBlanks $X..fungus) == FALSE, 
            c('Article.ID', 'X..fungus')]
            
FungusLowValues
#theres a handful to check here

FungusHighValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..fungus >= 90 &is.na(behaviorAllNoBlanks $X..fungus) == FALSE, 
            c('Article.ID', 'X..fungus')]
  
  FungusHighValues
#there are no rows here to check


### Here check to make sure that if insect.eater was marked "yes" that the % insects were not present 
class(behaviorAllNoBlanks$Insect.eater)
behaviorAllNoBlanks$Insect.eater <- as.factor(behaviorAllNoBlanks$Insect.eater)

behaviorAllNoBlanks$Insect.eater

#this gives you a list of all the article IDs where insect eaters were marked yes - but the % insects did not have a value
InsectEatersWPercentInsects <- behaviorAllNoBlanks[behaviorAllNoBlanks$Insect.eater == 'Yes' & is.na(behaviorAllNoBlanks$X..insects) == TRUE &is.na(behaviorAllNoBlanks$Insect.eater) == FALSE, 
            c('Article.ID', 'Insect.eater', 'X..insects')]
            
 InsectEatersWPercentInsects
 #theres quite a list here to check
 
 #this gives you a list of all the article IDs where % insects was not blank but the insect eater was not marked "yes"
 PerentInsectsWInsectEaters <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..insects) ==FALSE & behaviorAllNoBlanks]
 
 PerentInsectsWInsectEaters <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..insects) == FALSE & behaviorAllNoBlanks$Insect.eater == 'No', 
            c('Article.ID', 'Insect.eater', 'X..insects')]
            
 # total diets
 # first make sure all variables being summed are numeric
 behaviorAllNoBlanks$X..plant.reproductive.parts <- as.numeric(behaviorAllNoBlanks$X..plant.reproductive.parts)
 behaviorAllNoBlanks$X..folivory <- as.numeric(behaviorAllNoBlanks$X..folivory)
 behaviorAllNoBlanks$X..insects <- as.numeric(behaviorAllNoBlanks$X..insects)
 behaviorAllNoBlanks$X..fungus <- as.numeric(behaviorAllNoBlanks$X..fungus)

 
#this section is meant to sum all categories for diet to see if it goes over 100%. However, I dont think it is working because there are a lot of NA returns in this newly created variable
 behaviorAllNoBlanks$SumDiet <- behaviorAllNoBlanks$X..plant.reproductive.parts + behaviorAllNoBlanks$X..folivory + behaviorAllNoBlanks$X..insects + 
				behaviorAllNoBlanks$X..fungus

#this checks to see if any of the summed diets are over 100 percent. There are two entried returned, so the code seems to be working, but need to check validity of previous line of code. 
behaviorAllNoBlanks[behaviorAllNoBlanks $SumDiet > 100 & is.na(behaviorAllNoBlanks $SumDiet) == FALSE, c('Article.ID', 'SumDiet')] 
                                                                            
 # total activity budgets
 behaviorAllNoBlanks $SumActivity	<- behaviorAllNoBlanks $X..time.feeding + behaviorAllNoBlanks $X..time.social
 
 #this does not return any rows. code seems to be working?
behaviorAllNoBlanks[behaviorAllNoBlanks $SumActivity == 100 & is.na(behaviorAllNoBlanks $SumActivity) == FALSE, c('Article.ID', 'SumActivity')]
                                                                            
                                                                           

#SumDiet has no issues
#SumActivity has no issues
#PerFeed checked
#PerSocial checked
#PerGroom checked
#PerPlantRepro checked 
#PerFoliv checked 
#PerInsect checked
#PerFungus checked
#check that method is present when values are present (ex. grooming, feeding, diet, etc.)