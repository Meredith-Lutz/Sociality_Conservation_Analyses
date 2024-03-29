###########################################################################
###########################################################################
##### Preliminary Analyses of Social Organization Variation for DDRIG #####
###########################################################################
###########################################################################

setwd('G:/My Drive/Graduate School/Research/Projects/SocialityConservationAnalyses/Data')

library(ape)

behavior001_500	<- read.csv('behavior_001_500.csv', stringsAsFactors = FALSE)
behavior501_1000	<- read.csv('behavior_501_1000.csv', stringsAsFactors = FALSE)
behavior1001_1500	<- read.csv('behavior_1001_1500.csv', stringsAsFactors = FALSE)
behavior1501_2000	<- read.csv('behavior_1501_2000.csv', stringsAsFactors = FALSE)
behavior2001_2500	<- read.csv('behavior_2001_2500.csv', stringsAsFactors = FALSE)
behavior2501_3000	<- read.csv('behavior_2501_3000.csv', stringsAsFactors = FALSE)
taxonomy		<- read.csv('TaxonomicConversionTable.csv', stringsAsFactors = FALSE)
iucn			<- read.csv('IUCNAssessments.csv', stringsAsFactors = FALSE)
iucn$catNum		<- ifelse(iucn$redlistCategory == 'Least_Concern', 0,
				ifelse(iucn$redlistCategory == 'Near_Threatened', 1,
				ifelse(iucn$redlistCategory == 'Vulnerable', 2,
				ifelse(iucn$redlistCategory == 'Endangered', 3,
				ifelse(iucn$redlistCategory == 'Critically_Endangered', 4,
				ifelse(iucn$redlistCategory == 'Extinct', 5, NA))))))

shannon.entropy <- function(p){
	p.norm	<- p[p > 0]/sum(p)
	-sum(log2(p.norm)*p.norm)
}

colnames(behavior2501_3000) <- colnames(behavior1501_2000) <- colnames(behavior501_1000) <- colnames(behavior2001_2500) <- colnames(behavior1001_1500) <- colnames(behavior001_500)

behaviorAll	<- rbind(behavior001_500, behavior501_1000, behavior1001_1500, behavior1501_2000,
				behavior2001_2500, behavior2501_3000) #17586 lines

behaviorAll$sciName	<- ifelse(behaviorAll$species == '' & behaviorAll$subspecies == '', behaviorAll$Genus,
					ifelse(behaviorAll$subspecies == '', paste(behaviorAll$Genus, '_', behaviorAll$species, sep = ''),
					paste(behaviorAll$Genus, '_', behaviorAll$species, '_', behaviorAll$subspecies, sep = '')))

noTaxonomyConversion	<- sort(unique(behaviorAll[!(behaviorAll$sciName %in% taxonomy$Mendeley_tag), 'sciName']))
write.csv(noTaxonomyConversion, 'needToAddToTaxonomyTable.csv')

behaviorAll			<- merge(behaviorAll, taxonomy, by.x = 'sciName', by.y = 'Mendeley_tag') #10710 with conversion

notDoneCorrectly		<- sort(unique(behaviorAll[!(behaviorAll$IUCN_name %in% iucn$scientificName), 'sciName']))
write.csv(notDoneCorrectly, 'wrongIUCN.csv')

behaviorAll			<- merge(behaviorAll, iucn, by.x = 'IUCN_name', by.y = 'scientificName') #11729

socialOrg			<- behaviorAll[behaviorAll$Social.organization != '',] #3190

groupSize			<- behaviorAll[behaviorAll$Max...of.individuals != '' & is.na(behaviorAll$Max...of.individuals) == FALSE,] #4378

socialOrgSummary			<- aggregate(socialOrg$Social.organization, by = list(socialOrg$Social.organization, socialOrg$IUCN_name), FUN = length)
colnames(socialOrgSummary)	<- c('socialOrganization', 'species', 'numGroups')
socialOrgEntropy			<- aggregate(socialOrgSummary$numGroups, by = list(socialOrgSummary$species), FUN = shannon.entropy)
colnames(socialOrgEntropy) 	<- c('species', 'entropy')
socialOrgNTypes			<- aggregate(socialOrgSummary$numGroups, by = list(socialOrgSummary$species), FUN = length)
colnames(socialOrgNTypes) 	<- c('species', 'nTypes')
socialOrgNGroups			<- aggregate(socialOrgSummary$numGroups, by = list(socialOrgSummary$species), FUN = sum)
colnames(socialOrgNGroups) 	<- c('species', 'nGroups')
socialOrgEntropy			<- cbind(socialOrgEntropy, socialOrgNTypes[,2], socialOrgNGroups[,2])
colnames(socialOrgEntropy) 	<- c('species', 'entropy', 'nTypes', 'nGroups')

groupSizeSummary			<- aggregate(groupSize$Max...of.individuals, by = list(groupSize$IUCN_name), FUN = mean)

socialOrgEntropyIUCN	<- merge(socialOrgEntropy, iucn, by.x = 'species', by.y = 'scientificName')

plot(jitter(socialOrgEntropyIUCN[socialOrgEntropyIUCN$nGroups >= 2,]$nTypes), jitter(socialOrgEntropyIUCN[socialOrgEntropyIUCN$nGroups >= 2,]$catNum), pch = 16)
model1	<- lm(socialOrgEntropyIUCN[socialOrgEntropyIUCN$nGroups >= 2,]$catNum ~ socialOrgEntropyIUCN[socialOrgEntropyIUCN$nGroups >= 2,]$nTypes)