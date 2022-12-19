################################################################
################################################################
##### Behavior Data (Actv Budget, Diet, Seasonal) Cleaning #####
################################################################
################################################################

#Need to add rest of behavior files
#And the seasonal files
behavior001_500		<- read.csv('behavior_001_500.csv', stringsAsFactors = FALSE)
behavior501_1000		<- read.csv('behavior_501_1000.csv', stringsAsFactors = FALSE)
behavior1001_1500		<- read.csv('behavior_1001_1500.csv', stringsAsFactors = FALSE)
behavior1501_2000		<- read.csv('behavior_1501_2000.csv', stringsAsFactors = FALSE)
behavior2001_2500		<- read.csv('behavior_2001_2500.csv', stringsAsFactors = FALSE)
behavior2501_3000		<- read.csv('behavior_2501_3000.csv', stringsAsFactors = FALSE)
LumperTaxonomy		<- read.csv('Taxonomy Conversion - Lumper Taxonomy Conversion.csv', stringsAsFactors = FALSE)
TenKTreesTaxonomy		<- read.csv('Taxonomy Conversion - 10K Trees Taxonomy Conversion.csv', stringsAsFactors = FALSE)

colnames(behavior2501_3000) <- colnames(behavior1501_2000) <- colnames(behavior501_1000) <- colnames(behavior2001_2500) <- colnames(behavior1001_1500) <- colnames(behavior001_500)

behaviorAll	<- rbind(behavior001_500, behavior501_1000, behavior1001_1500, behavior1501_2000,
				behavior2001_2500, behavior2501_3000) #17586 lines

behaviorAll$sciName	<- ifelse(behaviorAll$species == '' & behaviorAll$subspecies == '', behaviorAll$Genus,
					ifelse(behaviorAll$subspecies == '', paste(behaviorAll$Genus, '_', behaviorAll$species, sep = ''),
					paste(behaviorAll$Genus, '_', behaviorAll$species, '_', behaviorAll$subspecies, sep = '')))

behaviorAll			<- merge(behaviorAll, TenKTreesTaxonomy, by.x = 'sciName', by.y = 'Mendeley.tag',) #10710 with conversion

#########################
#########################
##### Data Cleaning #####
#########################
#########################
behaviorAll[behaviorAll$PerFoliv <= 10 &is.na(behaviorAll$PerFoliv) == FALSE, 
            c('Article.ID', 'sciName')]

behaviorAll[behaviorAll$InsectEat == 'Yes' &is.na(behaviorAll$InsectEat) == FALSE, 
            c('Article.ID', 'InsectEat', 'PerInsect')]

sort(table(behaviorAll$PerPlantRepro))

behaviorAll$SumDiet <- behaviorAll$PerPlantRepro + behaviorAll$PerFoliv + behaviorAll$PerInsect + 
				behaviorAll$PerFungus
behaviorAll[behaviorAll$SumDiet > 100 & is.na(behaviorAll$SumDiet) == FALSE, c('Article.ID', 'sciName', 
                                                                            'SumDiet')]

behaviorAll$SumActivity	<- behaviorAll$PerFeed + behaviorAll$PerSocial
behaviorAll[behaviorAll$SumActivity == 100 & is.na(behaviorAll$SumActivity) == FALSE, c('Article.ID', 'sciName', 'SumActivity')]
behaviorSummary	<- aggregate(behaviorAll$Max...of.individuals, by = list(behaviorAll$sciName), FUN = max, 
                             na.rm=TRUE)

#SumDiet has no issues
#MaxIndiv checked
#MinIndiv checked
#SumActivity has no issues
#PerFeed checked
#PerSocial checked
#PerGroom checked
#PerPlantRepro checked
#PerFoliv checked
#PerInsect checked
#PerFungus checked