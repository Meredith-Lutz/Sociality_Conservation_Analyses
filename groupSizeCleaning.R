###########################################################################
###########################################################################
##### Preliminary Analyses of Social Organization Variation for DDRIG #####
###########################################################################
###########################################################################

setwd('G:/My Drive/Graduate School/Research/Projects/SocialityConservationAnalyses/Data')

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
behaviorAll	<- rbind(behavior001_500, behavior501_1000, behavior1001_1500,
				behavior1501_2000, behavior2001_2500, behavior2501_3000,
				behavior3001_3500, behavior3501_4000)

behaviorAll$sciName	<- ifelse(behaviorAll$subspecies == '', 
		paste(behaviorAll$Genus, behaviorAll$species, sep = '_'),
		paste(behaviorAll$Genus, behaviorAll$species, behaviorAll$subspecies, sep = '_'))

behaviorAll			<- merge(behaviorAll, lumper, by.x = 'sciName', by.y = 'Mendeley_tag', all.x=TRUE) 
behaviorAll			<- merge(behaviorAll, IUCN_tax, by.x = 'sciName', by.y = 'Mendeley_tag', all.x=TRUE) 
behaviorAll			<- merge(behaviorAll, TenKTrees, by.x = 'sciName', by.y = 'Mendeley_tag', all.x=TRUE) 

aggregate(behaviorAll$Mean.individuals, by = list(species = behaviorAll$IUCN_Name), FUN = max, na.rm = TRUE)
plot(as.factor(behaviorAll$IUCN_Name), behaviorAll$Mean.individuals, cex.axis = .65, pch = 16, xlim = c(1, 15), pt.cex = 0.5)

behaviorAll[behaviorAll$IUCN_Name == 'Varecia_variegata_subcincta' &
		behaviorAll$Mean.individuals == 40, c('Article.ID', 'Study.Site.ID',
		'X..of.groups.that.data.represents', 'Names.of.group', 'Mean.individuals']))
