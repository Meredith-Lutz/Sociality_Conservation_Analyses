################################################################
################################################################
##### Behavior Data (Actv Budget, Diet, Seasonal) Cleaning #####
################################################################
################################################################

#Meredith's working directory
setwd('C:/Users/mclutz/Box/GoogleDriveBackup/Graduate School/Research/Projects/SocialityConservationAnalyses/Data')

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
seasonalactivity001_500		<- read.csv('Article Coding Database (IDs_ 001 - 500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
seasonalactivity501_1000	<- read.csv('Article Coding Database (IDs_ 501 - 1000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
seasonalactivity1001_1500	<- read.csv('Article Coding Database (IDs_ 1001 - 1500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
seasonalactivity1501_2000	<- read.csv('Article Coding Database (IDs_ 1501 - 2000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
seasonalactivity2001_2500	<- read.csv('Article Coding Database (IDs_ 2001 - 2500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
seasonalactivity2501_3000	<- read.csv('Article Coding Database (IDs_ 2501 - 3000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
seasonalactivity3001_3500	<- read.csv('Article Coding Database (IDs_ 3001 - 3500) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)
seasonalactivity3501_4000	<- read.csv('Article Coding Database (IDs_ 3501 - 4000) - Seasonal activity budget data.csv', stringsAsFactors = FALSE)

colnames(seasonalactivity3501_4000) <- colnames(seasonalactivity3001_3500) <- colnames(seasonalactivity2501_3000) <- colnames(seasonalactivity1501_2000) <- colnames(seasonalactivity501_1000) <- colnames(seasonalactivity2001_2500) <- colnames(seasonalactivity1001_1500) <- colnames(seasonalactivity001_500)

seasonalActivityAll	<- rbind(seasonalactivity001_500, seasonalactivity501_1000, seasonalactivity1001_1500, seasonalactivity1501_2000, seasonalactivity2001_2500, seasonalactivity2501_3000, seasonalactivity3001_3500, seasonalactivity3501_4000)

#Read in all of the seasonal feeding budget data & combine together
seasonalfeeding001_500		<- read.csv('Article Coding Database (IDs_ 001 - 500) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
seasonalfeeding501_1000		<- read.csv('Article Coding Database (IDs_ 501 - 1000) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
seasonalfeeding1001_1500	<- read.csv('Article Coding Database (IDs_ 1001 - 1500) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
seasonalfeeding1501_2000	<- read.csv('Article Coding Database (IDs_ 1501 - 2000) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
seasonalfeeding2001_2500	<- read.csv('Article Coding Database (IDs_ 2001 - 2500) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
seasonalfeeding2501_3000	<- read.csv('Article Coding Database (IDs_ 2501 - 3000) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
seasonalfeeding3001_3500	<- read.csv('Article Coding Database (IDs_ 3001 - 3500) - Seasonal feeding data.csv', stringsAsFactors = FALSE)
seasonalfeeding3501_4000	<- read.csv('Article Coding Database (IDs_ 3501 - 4000) - Seasonal feeding data.csv', stringsAsFactors = FALSE)

colnames(seasonalfeeding3501_4000) <- colnames(seasonalfeeding3001_3500) <- colnames(seasonalfeeding2501_3000) <- colnames(seasonalfeeding1501_2000) <- colnames(seasonalfeeding501_1000) <- colnames(seasonalfeeding2001_2500) <- colnames(seasonalfeeding1001_1500) <- colnames(seasonalfeeding001_500)

seasonalFeedingAll	<- rbind(seasonalfeeding001_500, seasonalfeeding501_1000, seasonalfeeding1001_1500, seasonalfeeding1501_2000, seasonalfeeding2001_2500, seasonalfeeding2501_3000, seasonalfeeding3001_3500, seasonalfeeding3501_4000)

#########################
#########################
##### Data Cleaning #####
#########################
#########################

##########################################
##### Behavior Sheet Activity budget #####
##########################################

head(behaviorAllNoBlanks)
colnames(behaviorAllNoBlanks)

#check the class of the variable ' % time feeding' and change class to numeric
behaviorAllNoBlanks$X..time.feeding	<- as.numeric(behaviorAllNoBlanks$X..time.feeding)

#Call records in behavior sheet that have extreme values for %time feeding (less than 10%)
LowFeedingValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..time.feeding <= 10 & is.na(behaviorAllNoBlanks $X..time.feeding) == FALSE, 
			c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..time.feeding')]

#this object gives you a table of article IDs and X..time.feeding that is <= 10 %. Check to make sure these are correct
#using the PDFs to verify. Check off on the "datatodoublecheck" sheet for documentation.
LowFeedingValues	
			
#Call records in behavior sheet that have extreme values for %time feeding (greater than 90%)
HighFeedingValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..time.feeding >= 90 & is.na(behaviorAllNoBlanks $X..time.feeding) == FALSE, 
			c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..time.feeding')]	
	
HighFeedingValues
#looks like there are no rows, so maybe no errors to check?

#Call records in behavior sheet that have extreme values for %time social (less than 10%)
LowSocialValues <- behaviorAllNoBlanks[behaviorAllNoBlanks$X..time.social <= 10 & is.na(behaviorAllNoBlanks$X..time.social) == FALSE, 
			c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..time.social')]

#this object gives you a table of article IDs and X..time.social that is <= 10 %. Check to make sure these are correct
#using the PDFs to verify. Check off on the "datatodoublecheck" sheet for documentation.
LowSocialValues	
#this might be more realistic for some to actually be in this low range, but better to double check?

#Call records in behavior sheet that have extreme values for %time social (greater than 90%)
HighSocialValues <- behaviorAllNoBlanks[behaviorAllNoBlanks$X..time.social >= 90 & is.na(behaviorAllNoBlanks$X..time.social) == FALSE, 
			c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..time.social')]		
HighSocialValues
#again, says 0 rows so maybe no errors to check?

#Call records in behavior sheet that have extreme values for %time grooming (less than 10%)
LowGroomingValues	<- behaviorAllNoBlanks[behaviorAllNoBlanks$X..time.grooming <= 10 & is.na(behaviorAllNoBlanks $X..time.grooming) == FALSE, 
				c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..time.grooming')]

#this object gives you a table of article IDs and X..time.social that is <= 10 %. Check to make sure these are correct
#using the PDFs to verify. Check off on the "datatodoublecheck" sheet for documentation.
LowGroomingValues	
#this might be more realistic for some to actually be in this low range, but better to double check?

#Call records in behavior sheet that have extreme values for %time social (greater than 90%)
HighGroomingValues	<- behaviorAllNoBlanks[behaviorAllNoBlanks$X..time.grooming >= 90 & is.na(behaviorAllNoBlanks$X..time.grooming) == FALSE, 
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..time.grooming')]		

HighGroomingValues
#again, says 0 rows so maybe no errors to check?

##################################
##### Behavior Sheet Feeding #####
##################################

#check for % plant reproductive parts extreme values
PlantReproductivePartsLowValues 	<- behaviorAllNoBlanks[behaviorAllNoBlanks $X..plant.reproductive.parts <= 10 &is.na(behaviorAllNoBlanks $X..plant.reproductive.parts) == FALSE, 
           						 c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..plant.reproductive.parts')]
            
PlantReproductivePartsLowValues
#there are a few to check through

PlantReproductivePartsHighValues 	<- behaviorAllNoBlanks[behaviorAllNoBlanks $X..plant.reproductive.parts >= 90 &is.na(behaviorAllNoBlanks $X..plant.reproductive.parts) == FALSE, 
            					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..plant.reproductive.parts')]
            
PlantReproductivePartsHighValues
#there are a few to check through

#check for % folivory extreme values
FolivoryLowValues 	<- behaviorAllNoBlanks[behaviorAllNoBlanks $X..folivory <= 10 &is.na(behaviorAllNoBlanks $X..folivory) == FALSE, 
          				  c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..folivory')]
            
FolivoryLowValues
#there's a handful to check here

FolivoryHighValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..folivory >= 90 &is.na(behaviorAllNoBlanks $X..folivory) == FALSE, 
            c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..folivory')]
            
FolivoryHighValues
#there are a handful here to check

#check for % insect extreme values
InsectLowValues	<- behaviorAllNoBlanks[behaviorAllNoBlanks$X..insects <= 10 &is.na(behaviorAllNoBlanks$X..insects) == FALSE, 
            		c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..insects')]
            
InsectLowValues
#theres a handful to check here

InsectHighValues	 <- behaviorAllNoBlanks[behaviorAllNoBlanks$X..insects >= 90 &is.na(behaviorAllNoBlanks$X..insects) == FALSE, 
            		c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..insects')]
            
InsectHighValues
#there is one to check here

####Still need to do low/high fungus
#check for % fungus extreme values
FungusLowValues	<- behaviorAllNoBlanks[behaviorAllNoBlanks $X..fungus <= 10 &is.na(behaviorAllNoBlanks $X..fungus) == FALSE, 
            		c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..fungus')]
            
FungusLowValues
#theres a handful to check here

FungusHighValues	<- behaviorAllNoBlanks[behaviorAllNoBlanks $X..fungus >= 90 &is.na(behaviorAllNoBlanks $X..fungus) == FALSE, 
            		c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..fungus')]
  
FungusHighValues
#there are no rows here to check

######################################
##### Check insect/fungus eater ######
######################################
### Here check to make sure that if insect.eater was marked "yes" when the % insects was present 
class(behaviorAllNoBlanks$Insect.eater)
behaviorAllNoBlanks$Insect.eater <- as.factor(behaviorAllNoBlanks$Insect.eater)

#this gives you a list of all the article IDs where % insects had a value, but insect eater is marked no
InsectPercentNoEater <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..insects) == FALSE & behaviorAllNoBlanks$Insect.eater == 'No' & is.na(behaviorAllNoBlanks$Insect.eater) == FALSE, 
            c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Insect.eater', 'X..insects')]

InsectPercentNoEater 

#This gives you the times where % insects is filled out, but insect eater isn't marked
InsectPercentNAEater <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..insects) == FALSE & is.na(behaviorAllNoBlanks$Insect.eater) == TRUE, 
            c('Article.ID', 'Insect.eater', 'X..insects')]

InsectPercentNAEater

## Analagous code for fungus eater
class(behaviorAllNoBlanks$Fungus.eater)
behaviorAllNoBlanks$Fungus.eater <- as.factor(behaviorAllNoBlanks$Fungus.eater)

#this gives you a list of all the article IDs where % insects had a value, but insect eater is marked no
FungusPercentNoEater <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..fungus) == FALSE & behaviorAllNoBlanks$Fungus.eater == 'No' & is.na(behaviorAllNoBlanks$Fungus.eater) == FALSE, 
            c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Fungus.eater', 'X..fungus')]

FungusPercentNoEater 

#This gives you the times where % insects is filled out, but insect eater isn't marked
FungusPercentNAEater <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..fungus) == FALSE & is.na(behaviorAllNoBlanks$Fungus.eater) == TRUE, 
            c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Fungus.eater', 'X..fungus')]

FungusPercentNAEater

#########################
##### Checking sums #####
#########################
#Want to ensure that diets and activity budgets sum up to less than 100 and more than 1
            
### total diets
# first make sure all variables being summed are numeric
behaviorAllNoBlanks$X..plant.reproductive.parts		<- as.numeric(behaviorAllNoBlanks$X..plant.reproductive.parts)
behaviorAllNoBlanks$X..folivory				<- as.numeric(behaviorAllNoBlanks$X..folivory)
behaviorAllNoBlanks$X..insects				<- as.numeric(behaviorAllNoBlanks$X..insects)
behaviorAllNoBlanks$X..fungus					<- as.numeric(behaviorAllNoBlanks$X..fungus)
behaviorAllFeedingCols	<- behaviorAllNoBlanks[, c('X..plant.reproductive.parts', 'X..folivory', 'X..insects', 'X..fungus')]
 
#this section is meant to sum all categories for diet to see if it goes over 100%.
behaviorAllNoBlanks$SumDiet <- apply(behaviorAllFeedingCols, 1, sum, na.rm = TRUE)

#this checks to see if any of the summed diets are over 100 percent. 
behaviorAllNoBlanks[behaviorAllNoBlanks$SumDiet > 100 & is.na(behaviorAllNoBlanks$SumDiet) == FALSE, c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'SumDiet')]

#checks to see if the sum is less than 1 (as it would be if they were entered as propotion instead of %
behaviorAllNoBlanks[behaviorAllNoBlanks$SumDiet < 1 & behaviorAllNoBlanks$SumDiet > 0 & is.na(behaviorAllNoBlanks$SumDiet) == FALSE, c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'SumDiet')] 
                                                                           
# total activity budgets
behaviorAllActivityCols		<- behaviorAllNoBlanks[, c('X..time.feeding', 'X..time.social')]
behaviorAllNoBlanks$SumActv	<- apply(behaviorAllActivityCols, 1, sum, na.rm = TRUE)

#this does not return any rows. code seems to be working?
behaviorAllNoBlanks[behaviorAllNoBlanks$SumActv > 100 & is.na(behaviorAllNoBlanks$SumActv) == FALSE, c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'SumActv')]

#checks to see if the sum is less than 1 (as it would be if they were entered as propotion instead of %
behaviorAllNoBlanks[behaviorAllNoBlanks$SumActv < 1 & behaviorAllNoBlanks$SumActv > 0 & is.na(behaviorAllNoBlanks$SumActv) == FALSE, c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'SumActv')]              

############################
##### Checking methods #####
############################
### check to see if there is a grooming %, is there a method present?
GroomingValuesMissingMethod <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..time.grooming) == FALSE & is.na(behaviorAllNoBlanks$Data.method.grooming) == TRUE,
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..time.grooming', 'Data.method.grooming')]

SocialValuesMissingMethod <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..time.social) == FALSE & is.na(behaviorAllNoBlanks$Data.method.social) == TRUE,
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..time.social', 'Data.method.social')]

FeedingValuesMissingMethod <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..time.feeding) == FALSE & is.na(behaviorAllNoBlanks$Data.method.feeding) == TRUE,
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..time.feeding', 'Data.method.feeding')]

FolivoryValuesMissingMethod <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..folivory) == FALSE & is.na(behaviorAllNoBlanks$Diet.methodology) == TRUE,
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..folivory', 'Diet.methodology')]

PRPValuesMissingMethod <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..plant.reproductive.parts) == FALSE & is.na(behaviorAllNoBlanks$Diet.methodology) == TRUE,
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..plant.reproductive.parts', 'Diet.methodology')]

InsectsValuesMissingMethod <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..insects) == FALSE & is.na(behaviorAllNoBlanks$Diet.methodology) == TRUE,
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..insects', 'Diet.methodology')]

FungusValuesMissingMethod <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..fungus) == FALSE & is.na(behaviorAllNoBlanks$Diet.methodology) == TRUE,
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'X..fungus', 'Diet.methodology')]


##############################################################################################                               
##### Now need to do the same for the seasonal activity budget and seasonal feeding data #####
##############################################################################################

##replace blanks with NA's (see example above where you read in seasonal activity budget data
seasonalActivityAllNoBlanks <- seasonalActivityAll
seasonalActivityAllNoBlanks[seasonalActivityAllNoBlanks == ""] <- NA

seasonalFeedingAllNoBlanks <- seasonalFeedingAll
seasonalFeedingAllNoBlanks[seasonalFeedingAllNoBlanks == ""] <- NA

##copy in the section titled "Behavior sheet Activity budget" and replace the dataset name with the seasonal activity budget dataset

###################################################
##### Behavior Sheet Seasonal Activity budget #####
###################################################
head(seasonalActivityAllNoBlanks)
colnames(seasonalActivityAllNoBlanks)

#check the class of the variable ' % time feeding' and change class to numeric
seasonalActivityAllNoBlanks$X..time.feeding	<- as.numeric(seasonalActivityAllNoBlanks$X..time.feeding)

#Call records in behavior sheet that have extreme values for %time feeding (less than 10%)
LowFeedingValues <- seasonalActivityAllNoBlanks[seasonalActivityAllNoBlanks $X..time.feeding <= 10 & is.na(seasonalActivityAllNoBlanks $X..time.feeding) == FALSE, 
			c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..time.feeding')]

#this object gives you a table of article IDs and X..time.feeding that is <= 10 %. Check to make sure these are correct
#using the PDFs to verify. Check off on the "datatodoublecheck" sheet for documentation.
LowFeedingValues	
			
#Call records in behavior sheet that have extreme values for %time feeding (greater than 90%)
HighFeedingValues <- seasonalActivityAllNoBlanks[seasonalActivityAllNoBlanks $X..time.feeding >= 90 & is.na(seasonalActivityAllNoBlanks $X..time.feeding) == FALSE, 
			c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..time.feeding')]	
	
HighFeedingValues
#looks like there are no rows, so maybe no errors to check?

#Call records in behavior sheet that have extreme values for %time social (less than 10%)
LowSocialValues <- seasonalActivityAllNoBlanks[seasonalActivityAllNoBlanks$X..time.social <= 10 & is.na(seasonalActivityAllNoBlanks$X..time.social) == FALSE, 
			c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..time.social')]

#this object gives you a table of article IDs and X..time.social that is <= 10 %. Check to make sure these are correct
#using the PDFs to verify. Check off on the "datatodoublecheck" sheet for documentation.
LowSocialValues	
#this might be more realistic for some to actually be in this low range, but better to double check?

#Call records in behavior sheet that have extreme values for %time social (greater than 90%)
HighSocialValues <- seasonalActivityAllNoBlanks[seasonalActivityAllNoBlanks$X..time.social >= 90 & is.na(seasonalActivityAllNoBlanks$X..time.social) == FALSE, 
			c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..time.social')]		
HighSocialValues
#again, says 0 rows so maybe no errors to check?

#Call records in behavior sheet that have extreme values for %time grooming (less than 10%)
LowGroomingValues	<- seasonalActivityAllNoBlanks[seasonalActivityAllNoBlanks$X..time.grooming <= 10 & is.na(seasonalActivityAllNoBlanks $X..time.grooming) == FALSE, 
				c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..time.grooming')]

#this object gives you a table of article IDs and X..time.social that is <= 10 %. Check to make sure these are correct
#using the PDFs to verify. Check off on the "datatodoublecheck" sheet for documentation.
LowGroomingValues	
#this might be more realistic for some to actually be in this low range, but better to double check?

#Call records in behavior sheet that have extreme values for %time social (greater than 90%)
HighGroomingValues	<- seasonalActivityAllNoBlanks[seasonalActivityAllNoBlanks$X..time.grooming >= 90 & is.na(seasonalActivityAllNoBlanks$X..time.grooming) == FALSE, 
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..time.grooming')]		

HighGroomingValues
#again, says 0 rows so maybe no errors to check?

###########################################
##### Behavior Sheet Seasonal Feeding #####
###########################################
#check for % plant reproductive parts extreme values
PlantReproductivePartsLowValues 	<- seasonalFeedingAllNoBlanks[seasonalFeedingAllNoBlanks $X..plant.reproductive.parts <= 10 &is.na(seasonalFeedingAllNoBlanks $X..plant.reproductive.parts) == FALSE, 
           						 c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..plant.reproductive.parts')]
            
PlantReproductivePartsLowValues
#there are a few to check through

PlantReproductivePartsHighValues 	<- seasonalFeedingAllNoBlanks[seasonalFeedingAllNoBlanks $X..plant.reproductive.parts >= 90 &is.na(seasonalFeedingAllNoBlanks $X..plant.reproductive.parts) == FALSE, 
            					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..plant.reproductive.parts')]
            
PlantReproductivePartsHighValues
#there are a few to check through

#check for % folivory extreme values
FolivoryLowValues 	<- seasonalFeedingAllNoBlanks[seasonalFeedingAllNoBlanks $X..folivory <= 10 &is.na(seasonalFeedingAllNoBlanks $X..folivory) == FALSE, 
          				  c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..folivory')]
            
FolivoryLowValues
#there's a handful to check here

FolivoryHighValues <- seasonalFeedingAllNoBlanks[seasonalFeedingAllNoBlanks $X..folivory >= 90 &is.na(seasonalFeedingAllNoBlanks $X..folivory) == FALSE, 
            c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..folivory')]
            
FolivoryHighValues
#there are a handful here to check

#check for % insect extreme values
InsectLowValues	<- seasonalFeedingAllNoBlanks[seasonalFeedingAllNoBlanks$X..insects <= 10 &is.na(seasonalFeedingAllNoBlanks$X..insects) == FALSE, 
            		c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..insects')]
            
InsectLowValues
#theres a handful to check here

InsectHighValues	 <- seasonalFeedingAllNoBlanks[seasonalFeedingAllNoBlanks$X..insects >= 90 &is.na(seasonalFeedingAllNoBlanks$X..insects) == FALSE, 
            		c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..insects')]
            
InsectHighValues
#there is one to check here

####Still need to do low/high fungus
#check for % fungus extreme values
FungusLowValues	<- seasonalFeedingAllNoBlanks[seasonalFeedingAllNoBlanks $X..fungus <= 10 &is.na(seasonalFeedingAllNoBlanks $X..fungus) == FALSE, 
            		c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..fungus')]
            
FungusLowValues
#theres a handful to check here

FungusHighValues	<- seasonalFeedingAllNoBlanks[seasonalFeedingAllNoBlanks $X..fungus >= 90 &is.na(seasonalFeedingAllNoBlanks $X..fungus) == FALSE, 
            		c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..fungus')]
  
FungusHighValues
#there are no rows here to check

################################################
##### Check insect/fungus eater (seasonal)######
################################################
### Here check to make sure that if insect.eater was marked "yes" when the % insects was present 
class(seasonalFeedingAllNoBlanks$Insect.eater)
seasonalFeedingAllNoBlanks$Insect.eater <- as.factor(seasonalFeedingAllNoBlanks$Insect.eater)

#this gives you a list of all the article IDs where % insects had a value, but insect eater is marked no
InsectPercentNoEater <- seasonalFeedingAllNoBlanks[is.na(seasonalFeedingAllNoBlanks$X..insects) == FALSE & seasonalFeedingAllNoBlanks$Insect.eater == 'No' & is.na(seasonalFeedingAllNoBlanks$Insect.eater) == FALSE, 
            c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'Insect.eater', 'X..insects')]

InsectPercentNoEater 

#This gives you the times where % insects is filled out, but insect eater isn't marked
InsectPercentNAEater <- seasonalFeedingAllNoBlanks[is.na(seasonalFeedingAllNoBlanks$X..insects) == FALSE & is.na(seasonalFeedingAllNoBlanks$Insect.eater) == TRUE, 
            c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'Insect.eater', 'X..insects')]

InsectPercentNAEater

## Analagous code for fungus eater
class(seasonalFeedingAllNoBlanks$Fungus.eater)
seasonalFeedingAllNoBlanks$Fungus.eater <- as.factor(seasonalFeedingAllNoBlanks$Fungus.eater)

#this gives you a list of all the article IDs where % insects had a value, but insect eater is marked no
FungusPercentNoEater <- seasonalFeedingAllNoBlanks[is.na(seasonalFeedingAllNoBlanks$X..fungus) == FALSE & seasonalFeedingAllNoBlanks$Fungus.eater == 'No' & is.na(seasonalFeedingAllNoBlanks$Fungus.eater) == FALSE, 
            c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'Fungus.eater', 'X..fungus')]

FungusPercentNoEater 

#This gives you the times where % insects is filled out, but insect eater isn't marked
FungusPercentNAEater <- seasonalFeedingAllNoBlanks[is.na(seasonalFeedingAllNoBlanks$X..fungus) == FALSE & is.na(seasonalFeedingAllNoBlanks$Fungus.eater) == TRUE, 
            c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'Fungus.eater', 'X..fungus')]

FungusPercentNAEater

###################################
##### Checking sums (seasonal)#####
###################################
#Want to ensure that diets and activity budgets sum up to less than 100 and more than 1
            
### total diets
# first make sure all variables being summed are numeric
seasonalFeedingAllNoBlanks$X..plant.reproductive.parts		<- as.numeric(seasonalFeedingAllNoBlanks$X..plant.reproductive.parts)
seasonalFeedingAllNoBlanks$X..folivory				<- as.numeric(seasonalFeedingAllNoBlanks$X..folivory)
seasonalFeedingAllNoBlanks$X..insects				<- as.numeric(seasonalFeedingAllNoBlanks$X..insects)
seasonalFeedingAllNoBlanks$X..fungus					<- as.numeric(seasonalFeedingAllNoBlanks$X..fungus)
behaviorAllFeedingCols	<- seasonalFeedingAllNoBlanks[, c('X..plant.reproductive.parts', 'X..folivory', 'X..insects', 'X..fungus')]
 
#this section is meant to sum all categories for diet to see if it goes over 100%.
seasonalFeedingAllNoBlanks$SumDiet <- apply(behaviorAllFeedingCols, 1, sum, na.rm = TRUE)

#this checks to see if any of the summed diets are over 100 percent. 
seasonalFeedingAllNoBlanks[seasonalFeedingAllNoBlanks$SumDiet > 105 & is.na(seasonalFeedingAllNoBlanks$SumDiet) == FALSE, c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'SumDiet')]

#checks to see if the sum is less than 1 (as it would be if they were entered as propotion instead of %
seasonalFeedingAllNoBlanks[seasonalFeedingAllNoBlanks$SumDiet < 1 & seasonalFeedingAllNoBlanks$SumDiet > 0 & is.na(seasonalFeedingAllNoBlanks$SumDiet) == FALSE, c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'SumDiet')] 
                                                                           
# total activity budgets
behaviorAllActivityCols		<- seasonalActivityAllNoBlanks[, c('X..time.feeding', 'X..time.social')]
seasonalActivityAllNoBlanks$SumActv	<- apply(behaviorAllActivityCols, 1, sum, na.rm = TRUE)

#this does not return any rows. code seems to be working?
seasonalActivityAllNoBlanks[seasonalActivityAllNoBlanks$SumActv > 100 & is.na(seasonalActivityAllNoBlanks$SumActv) == FALSE, c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'SumActv')]

#checks to see if the sum is less than 1 (as it would be if they were entered as propotion instead of %
seasonalActivityAllNoBlanks[seasonalActivityAllNoBlanks$SumActv < 1 & seasonalActivityAllNoBlanks$SumActv > 0 & is.na(seasonalActivityAllNoBlanks$SumActv) == FALSE, c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'SumActv')]              

######################################
##### Checking methods (seasonal)#####
######################################
### check to see if there is a grooming %, is there a method present?
GroomingValuesMissingMethod <- seasonalActivityAllNoBlanks[is.na(seasonalActivityAllNoBlanks$X..time.grooming) == FALSE & is.na(seasonalActivityAllNoBlanks$Data.method.grooming) == TRUE,
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..time.grooming', 'Data.method.grooming')]

SocialValuesMissingMethod <- seasonalActivityAllNoBlanks[is.na(seasonalActivityAllNoBlanks$X..time.social) == FALSE & is.na(seasonalActivityAllNoBlanks$Data.method.social) == TRUE,
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..time.social', 'Data.method.social')]

FeedingValuesMissingMethod <- seasonalFeedingAllNoBlanks[is.na(seasonalFeedingAllNoBlanks$X..time.feeding) == FALSE & is.na(seasonalFeedingAllNoBlanks$Data.method.feeding) == TRUE,
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..time.feeding', 'Data.method.feeding')]

FolivoryValuesMissingMethod <- seasonalFeedingAllNoBlanks[is.na(seasonalFeedingAllNoBlanks$X..folivory) == FALSE & is.na(seasonalFeedingAllNoBlanks$Diet.methodology) == TRUE,
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..folivory', 'Diet.methodology')]

PRPValuesMissingMethod <- seasonalFeedingAllNoBlanks[is.na(seasonalFeedingAllNoBlanks$X..plant.reproductive.parts) == FALSE & is.na(seasonalFeedingAllNoBlanks$Diet.methodology) == TRUE,
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..plant.reproductive.parts', 'Diet.methodology')]

InsectsValuesMissingMethod <- seasonalFeedingAllNoBlanks[is.na(seasonalFeedingAllNoBlanks$X..insects) == FALSE & is.na(seasonalFeedingAllNoBlanks$Diet.methodology) == TRUE,
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..insects', 'Diet.methodology')]

FungusValuesMissingMethod <- seasonalFeedingAllNoBlanks[is.na(seasonalFeedingAllNoBlanks$X..fungus) == FALSE & is.na(seasonalFeedingAllNoBlanks$Diet.methodology) == TRUE,
					c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Phase', 'Phase.Year', 'X..fungus', 'Diet.methodology')]

##when you subset the data you'll want to add in the phase information for seasonal
##For example:
##LowFeedingValues <- behaviorAllNoBlanks[behaviorAllNoBlanks$X..time.feeding <= 10 & is.na(behaviorAllNoBlanks $X..time.feeding) == FALSE, 
##			c('Article.Initials', 'Article.ID', 'Study.Site.ID', ADD PHASE VARIABLES HERE, 'X..time.feeding')]