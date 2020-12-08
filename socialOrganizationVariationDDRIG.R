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
taxonomy		<- read.csv('TaxonomicConversionTable.csv', stringsAsFactor = FALSE)

colnames(behavior2501_3000) <- colnames(behavior1501_2000) <- colnames(behavior501_1000) <- colnames(behavior2001_2500) <- colnames(behavior1001_1500) <- colnames(behavior001_500)

behaviorAll	<- rbind(behavior001_500, behavior501_1000, behavior1001_1500, behavior1501_2000,
				behavior2001_2500, behavior2501_3000)

behaviorAll$sciName	<- ifelse(behaviorAll$subspecies == '', paste(behaviorAll$Genus, '_', behaviorAll$species, sep = ''),
					paste(behaviorAll$Genus, '_', behaviorAll$species, '_', behaviorAll$subspecies, sep = ''))

noTaxonomyConversion	<- sort(unique(behaviorAll[!(behaviorAll$sciName %in% taxonomy$Mendeley_tag), 'sciName']))
write.csv(noTaxonomyConversion, 'needToAddToTaxonomyTable.csv')

behaviorAll			<- merge(behaviorAll, taxonomy, by.x = 'sciName', by.y = 'Mendeley_tag')