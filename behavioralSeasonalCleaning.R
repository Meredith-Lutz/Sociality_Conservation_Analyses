################################################################
################################################################
##### Behavior Data (Actv Budget, Diet, Seasonal) Cleaning #####
################################################################
################################################################

#Meredith's working directory
setwd('G:/My Drive/Graduate School/Research/Projects/SocialityConservationAnalyses/Data')

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

##################################
##### Behavior Sheet Feeding #####
##################################

head(behaviorAllNoBlanks)

#shows the column names of behaviorAll
colnames(behaviorAllNoBlanks)

#check the class of the variable ' % time feeding' and change class to numeric
class(behaviorAllNoBlanks$X..time.feeding)
behaviorAllNoBlanks$X..time.feeding	<- as.numeric(behaviorAllNoBlanks$X..time.feeding)
class(behaviorAllNoBlanks$X..time.feeding)

#Call records in behavior sheet that have extreme values for %time feeding (less than 10%)
LowFeedingValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..time.feeding <= 10 & is.na(behaviorAllNoBlanks $X..time.feeding) == FALSE, 
			c('Article.ID', 'X..time.feeding')]

#this object gives you a table of article IDs and X..time.feeding that is <= 10 %. Check to make sure these are correct
#using the PDFs to verify. Check off on the "datatodoublecheck" sheet for documentation.
LowFeedingValues	
			
#Call records in behavior sheet that have extreme values for %time feeding (greater than 90%)
HighFeedingValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..time.feeding >= 90 & is.na(behaviorAllNoBlanks $X..time.feeding) == FALSE, 
			c('Article.ID', 'X..time.feeding')]	
	
HighFeedingValues
#looks like there are no rows, so maybe no errors to check?

##################################
##### Behavior Sheet Social ######
##################################

class(behaviorAllNoBlanks$X..time.social)
#this is already a numeric

#Call records in behavior sheet that have extreme values for %time social (less than 10%)
LowSocialValues <- behaviorAllNoBlanks[behaviorAllNoBlanks$X..time.social <= 10 & is.na(behaviorAllNoBlanks$X..time.social) == FALSE, 
			c('Article.ID', 'X..time.social')]

#this object gives you a table of article IDs and X..time.social that is <= 10 %. Check to make sure these are correct
#using the PDFs to verify. Check off on the "datatodoublecheck" sheet for documentation.
LowSocialValues	
#this might be more realistic for some to actually be in this low range, but better to double check?

#Call records in behavior sheet that have extreme values for %time social (greater than 90%)
HighSocialValues <- behaviorAllNoBlanks[behaviorAllNoBlanks$X..time.social >= 90 & is.na(behaviorAllNoBlanks$X..time.social) == FALSE, 
			c('Article.ID', 'X..time.social')]		
HighSocialValues
#again, says 0 rows so maybe no errors to check?


###################################
##### Behavior Sheet Grooming #####
###################################

class(behaviorAllNoBlanks$X..time.grooming)
#this is already a numeric

#Call records in behavior sheet that have extreme values for %time grooming (less than 10%)
LowGroomingValues	<- behaviorAllNoBlanks[behaviorAllNoBlanks$X..time.grooming <= 10 & is.na(behaviorAllNoBlanks $X..time.grooming) == FALSE, 
				c('Article.ID', 'X..time.grooming')]

#this object gives you a table of article IDs and X..time.social that is <= 10 %. Check to make sure these are correct
#using the PDFs to verify. Check off on the "datatodoublecheck" sheet for documentation.
LowGroomingValues	
#this might be more realistic for some to actually be in this low range, but better to double check?

#Call records in behavior sheet that have extreme values for %time social (greater than 90%)
HighGroomingValues	<- behaviorAllNoBlanks[behaviorAllNoBlanks$X..time.grooming >= 90 & is.na(behaviorAllNoBlanks$X..time.grooming) == FALSE, 
					c('Article.ID', 'X..time.grooming')]		

HighGroomingValues
#again, says 0 rows so maybe no errors to check?

### check to see if there is a grooming %, is there a method present?
GroomingValuesWithMethod <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..time.grooming) == FALSE & is.na(behaviorAllNoBlanks$Data.method.grooming) == TRUE,
					c('Article.ID', 'X..time.grooming', 'Data.method.grooming')]

##Use this line to modify for % social, % feed, % plant repro parts, % folivory

############################################
##### Behavior Sheet Plant Repr. Parts #####
############################################

#check for % plant reproductive parts extreme values
PlantReproductivePartsLowValues 	<- behaviorAllNoBlanks[behaviorAllNoBlanks $X..plant.reproductive.parts <= 10 &is.na(behaviorAllNoBlanks $X..plant.reproductive.parts) == FALSE, 
           						 c('Article.ID', 'X..plant.reproductive.parts')]
            
PlantReproductivePartsLowValues
#there are a few to check through

PlantReproductivePartsHighValues 	<- behaviorAllNoBlanks[behaviorAllNoBlanks $X..plant.reproductive.parts >= 90 &is.na(behaviorAllNoBlanks $X..plant.reproductive.parts) == FALSE, 
            					c('Article.ID', 'X..plant.reproductive.parts')]
            
PlantReproductivePartsHighValues
#there are a few to check through

###################################
##### Behavior Sheet Folivory #####
###################################

#check for % folivory extreme values
FolivoryLowValues 	<- behaviorAllNoBlanks[behaviorAllNoBlanks $X..folivory <= 10 &is.na(behaviorAllNoBlanks $X..folivory) == FALSE, 
          				  c('Article.ID', 'X..folivory')]
            
FolivoryLowValues
#there's a handful to check here

FolivoryHighValues <- behaviorAllNoBlanks[behaviorAllNoBlanks $X..folivory >= 90 &is.na(behaviorAllNoBlanks $X..folivory) == FALSE, 
            c('Article.ID', 'X..folivory')]
            
FolivoryHighValues
#there are a handful here to check

###################################
###### Behavior Sheet Insects #####
###################################

#check for % insect extreme values
InsectLowValues	<- behaviorAllNoBlanks[behaviorAllNoBlanks$X..insects <= 10 &is.na(behaviorAllNoBlanks$X..insects) == FALSE, 
            		c('Article.ID', 'X..insects')]
            
InsectLowValues
#theres a handful to check here

InsectHighValues	 <- behaviorAllNoBlanks[behaviorAllNoBlanks$X..insects >= 90 &is.na(behaviorAllNoBlanks$X..insects) == FALSE, 
            		c('Article.ID', 'X..insects')]
            
InsectHighValues
#there is one to check here

### Here check to make sure that if insect.eater was marked "yes" when the % insects was present 
class(behaviorAllNoBlanks$Insect.eater)
behaviorAllNoBlanks$Insect.eater <- as.factor(behaviorAllNoBlanks$Insect.eater)

behaviorAllNoBlanks$Insect.eater

#this gives you a list of all the article IDs where % insects had a value, but insect eater is marked no
InsectPercentNoEater <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..insects) == FALSE & behaviorAllNoBlanks$Insect.eater != 'No' & is.na(behaviorAllNoBlanks$Insect.eater) == FALSE, 
            c('Article.ID', 'Insect.eater', 'X..insects')]

InsectPercentNoEater 

InsectPercentNAEater <- behaviorAllNoBlanks[is.na(behaviorAllNoBlanks$X..insects) == FALSE & is.na(behaviorAllNoBlanks$Insect.eater) == TRUE, 
            c('Article.ID', 'Insect.eater', 'X..insects')]

InsectPercentNAEater

##################################
###### Behavior Sheet Fungus #####
##################################

#check for % fungus extreme values
FungusLowValues	<- behaviorAllNoBlanks[behaviorAllNoBlanks $X..fungus <= 10 &is.na(behaviorAllNoBlanks $X..fungus) == FALSE, 
            		c('Article.ID', 'X..fungus')]
            
FungusLowValues
#theres a handful to check here

FungusHighValues	<- behaviorAllNoBlanks[behaviorAllNoBlanks $X..fungus >= 90 &is.na(behaviorAllNoBlanks $X..fungus) == FALSE, 
            		c('Article.ID', 'X..fungus')]
  
FungusHighValues
#there are no rows here to check


###Use the insect.eater code from above to modify to get code for the analogous fungus variables

#########################
##### Checking sums #####
#########################
            
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
behaviorAllNoBlanks[behaviorAllNoBlanks$SumDiet > 100 & is.na(behaviorAllNoBlanks$SumDiet) == FALSE, c('Article.ID', 'SumDiet')]

#checks to see if the sum is less than 1 (as it would be if they were entered as propotion instead of %
behaviorAllNoBlanks[behaviorAllNoBlanks$SumDiet < 1 & behaviorAllNoBlanks$SumDiet > 0 & is.na(behaviorAllNoBlanks$SumDiet) == FALSE, c('Article.ID', 'SumDiet')] 

                                                                           
# total activity budgets
behaviorAllActivityCols		<- behaviorAllNoBlanks[, c('X..time.feeding', 'X..time.social')]
behaviorAllNoBlanks$SumActv	<- apply(behaviorAllActivityCols, 1, sum, na.rm = TRUE)

#this does not return any rows. code seems to be working?
behaviorAllNoBlanks[behaviorAllNoBlanks$SumActv > 100 & is.na(behaviorAllNoBlanks$SumActv) == FALSE, c('Article.ID', 'SumActv')]

#checks to see if the sum is less than 1 (as it would be if they were entered as propotion instead of %
behaviorAllNoBlanks[behaviorAllNoBlanks$SumActv < 1 & behaviorAllNoBlanks$SumActv > 0 & is.na(behaviorAllNoBlanks$SumActv) == FALSE, c('Article.ID', 'SumActv')]                                                    