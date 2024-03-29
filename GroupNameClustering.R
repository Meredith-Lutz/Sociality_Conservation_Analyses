###################################################
###################################################
##### Student Code for Clustering group names #####
###################################################
###################################################

#First set your working directory to the folder where you put all your files
setwd('G:/My Drive/Graduate School/Research/Projects/SocialityConservationAnalyses/Data')

#Read in cleaned list of study site names
studySite		<- read.csv('CleanedStudySiteNames.csv', stringsAsFactors = FALSE)

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

#Find all of the article ID/Study site IDs with the study site of interest
#Replace "Parc National des Volcans" in the 3rd line of this section with the study site you are looking up
studySite$articleIDssID		<- factor(factor(studySite$Article.ID):factor(studySite$Study.Site.ID))
behaviorAll$articleIDssID	<- factor(factor(behaviorAll$Article.ID):factor(behaviorAll$Study.Site.ID))
relevantArticleSSIDs		<- as.character(studySite[studySite$Location.of.large.site == "Baimaxueshan National Nature Reserve" & is.na(studySite$Location.of.large.site) == FALSE, 'articleIDssID'])

#Now pull all of the relevant behavior lines for those article/ss ID combos
relevantBehaviorLines		<- behaviorAll[behaviorAll$articleIDssID %in% relevantArticleSSIDs,]

#Now look at a table of all of the group names to begin fixing
table(relevantBehaviorLines$Names.of.group)

#Notice how there is "Beetsme's group" "Beetsme's Group" and "BEE" - all need to be combined, and similar problems throughout

#Use this line to find the article IDs for the group name you want to change
relevantBehaviorLines[relevantBehaviorLines$Names.of.group == '',c(1:11, 92)]

#########################
### Group name combos ###
#########################
lumper		<- read.csv('Taxonomy Conversion - Lumper Taxonomy Conversion.csv', stringsAsFactors = FALSE)

behaviorAll$sciName	<- ifelse(behaviorAll$species == '' & behaviorAll$subspecies == '', behaviorAll$Genus,
                              ifelse(behaviorAll$subspecies == '', paste(behaviorAll$Genus, '_', behaviorAll$species, sep = ''),
                                     paste(behaviorAll$Genus, '_', behaviorAll$species, '_', behaviorAll$subspecies, sep = '')))

behaviorAll			<- merge(behaviorAll, lumper, by.x = 'sciName', by.y = 'Mendeley_tag', all.x=TRUE) 
groupLines			<- behaviorAll[,c('Article.Initials', 'Article.ID', 'Study.Site.ID', 'Names.of.group', 'Lumper_Taxonomy', 'articleIDssID')]

studySite$smallLargeSite	<- factor(factor(studySite$Location.of.large.site):factor(studySite$Location.of.small.site))
smallLargeSites			<- unique(studySite$smallLargeSite)

behaviorAll				<- merge(behaviorAll, studySite[, c(29, 30, 26:28)], by.x = 'articleIDssID', by.y = 'articleIDssID', all.x = TRUE)
factor(factor(behaviorAll$Article.ID):factor(behaviorAll$Study.Site.ID))

namesBySpeciesSite	<- aggregate(behaviorAll$Names.of.group, by = list(country = behaviorAll$Country, site = behaviorAll$smallLargeSite, 
					smallSite = behaviorAll$Location.of.small.site, 
					largeSite = behaviorAll$Location.of.large.site, species = behaviorAll$Lumper_Taxonomy, names = behaviorAll$Names.of.group), FUN = length)

namesBySpeciesSite	<- namesBySpeciesSite[order(namesBySpeciesSite$country, namesBySpeciesSite$site, namesBySpeciesSite$species, namesBySpeciesSite$names),]
write.csv(namesBySpeciesSite, 'namesBySpeciesSite.csv', row.names = FALSE)

###########################
### Ignore this section ###
###########################
largeSiteCountry	<- aggregate(as.numeric(studySite[,22]), by = list(studySite$Country, as.character(studySite$Location.of.large.site)), FUN = sum)[,1:2]
largeSiteCountry	<- largeSiteCountry[order(largeSiteCountry$Group.1, largeSiteCountry$Group.2),]
write.csv(largeSiteCountry, "largeSiteCountry.csv", row.names = FALSE)

countries	<- sort(unique(studySite$Country))
studySite	<- studySite[order(studySite$Country, studySite$Location.of.large.site),]

for(i in countries){
	print(paste('Working on', i))
	largeSites	<- unique(studySite[studySite$Country == i, 'Location.of.large.site'])
	for(j in largeSites){
		smallSites	<- unique(studySite[studySite$Country == i & studySite$Location.of.large.site == j & is.na(studySite$Location.of.large.site) == FALSE, 'Location.of.small.site'])
		if(length(smallSites) >= 2){
			if('' %in% smallSites){
				print(paste('Revisit small sites for', j))
			}
		}
	}
}
