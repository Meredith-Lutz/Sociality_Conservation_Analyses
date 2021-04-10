###########################################################################
###########################################################################
##### Preliminary Analyses of Social Organization Variation for DDRIG #####
###########################################################################
###########################################################################

setwd('G:/My Drive/Graduate School/Research/Projects/SocialityConservationAnalyses/Data')

behavior001_500	<- read.csv('behavior_001_500.csv', stringsAsFactors = FALSE)
behavior501_1000	<- read.csv('behavior_501_1000.csv', stringsAsFactors = FALSE)
behavior1001_1500	<- read.csv('behavior_1001_1500.csv', stringsAsFactors = FALSE)
behavior1501_2000	<- read.csv('behavior_1501_2000.csv', stringsAsFactors = FALSE)
behavior2001_2500	<- read.csv('behavior_2001_2500.csv', stringsAsFactors = FALSE)
behavior2501_3000	<- read.csv('behavior_2501_3000.csv', stringsAsFactors = FALSE)
behavior3001_3500	<- read.csv('behavior_3001_3500.csv', stringsAsFactors = FALSE)

behaviorAll	<- rbind(behavior001_500, behavior501_1000, behavior1001_1500,
				behavior1501_2000, behavior2001_2500, behavior2501_3000,
				behavior3001_3500)

behaviorAll$sciName	<- ifelse(behaviorAll$subspecies == '', 
		paste(behaviorAll$Genus, behaviorAll$species, sep = '_'),
		paste(behaviorAll$Genus, behaviorAll$species, behaviorAll$subspecies, sep = '_'))

sort(behaviorAll[behaviorAll$sciName == 'Varecia_variegata_subcincta', 'Max...of.individuals'])
sort(unique(behaviorAll[behaviorAll$sciName == 'Varecia_variegata_subcincta' &
		 behaviorAll$Max...of.individuals > 40, 'Article.ID']))
