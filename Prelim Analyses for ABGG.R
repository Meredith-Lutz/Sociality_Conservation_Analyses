##########################################
##########################################
##### Preliminary Analyses for ABGG ######
##########################################
##########################################

setwd("~/Desktop/Cleaned PREDICT data")

library(ape)

behavior001_500	<- read.csv('behavior_001_500.csv', stringsAsFactors = FALSE)
behavior501_1000	<- read.csv('behavior_501_1000.csv', stringsAsFactors = FALSE)
behavior1001_1500	<- read.csv('behavior_1001_1500.csv', stringsAsFactors = FALSE)
behavior1501_2000	<- read.csv('behavior_1501_2000.csv', stringsAsFactors = FALSE)
behavior2001_2500	<- read.csv('behavior_2001_2500.csv', stringsAsFactors = FALSE)
behavior2501_3000	<- read.csv('behavior_2501_3000.csv', stringsAsFactors = FALSE)
behavior3001_3500	<- read.csv('behavior_3001_3500.csv', stringsAsFactors = FALSE)

SS001_500	<- read.csv('SS_001_500.csv', stringsAsFactors = FALSE)
SS501_1000	<- read.csv('SS_501_1000.csv', stringsAsFactors = FALSE)
SS1001_1500	<- read.csv('SS_1001_1500.csv', stringsAsFactors = FALSE)
SS1501_2000	<- read.csv('SS_1501_2000.csv', stringsAsFactors = FALSE)
SS2001_2500	<- read.csv('SS_2001_2500.csv', stringsAsFactors = FALSE)
SS2501_3000	<- read.csv('SS_2501_3000.csv', stringsAsFactors = FALSE)
SS3001_3500	<- read.csv('SS_3001_3500.csv', stringsAsFactors = FALSE)

lumper		<- read.csv('Lumper_Taxonomy.csv', stringsAsFactors = FALSE)
IUCN_tax		<- read.csv('IUCN_Taxonomy.csv', stringsAsFactors = FALSE)
TenKTrees		<- read.csv('10KTrees_Taxonomy.csv', stringsAsFactors = FALSE)
Tree		<- read.nexus('consensusTree_10kTrees_Primates_Version3.nex')


iucn			<- read.csv('assessments.csv', stringsAsFactors = FALSE)
iucn$catNum		<- ifelse(iucn$redlistCategory == 'Least Concern', 0,
                       ifelse(iucn$redlistCategory == 'Near Threatened', 1,
                              ifelse(iucn$redlistCategory == 'Vulnerable', 2,
                                     ifelse(iucn$redlistCategory == 'Endangered', 3,
                                            ifelse(iucn$redlistCategory == 'Critically Endangered', 4,
                                                   ifelse(iucn$redlistCategory == 'Extinct', 5, NA))))))



shannon.entropy <- function(p){
  p.norm	<- p[p > 0]/sum(p)
  -sum(log2(p.norm)*p.norm)
}

colnames(behavior3001_3500) <- colnames(behavior2501_3000) <- colnames(behavior1501_2000) <- colnames(behavior501_1000) <- colnames(behavior2001_2500) <- colnames(behavior1001_1500) <- colnames(behavior001_500)

behaviorAll	<- rbind(behavior001_500, behavior501_1000, behavior1001_1500, behavior1501_2000,
                     behavior2001_2500, behavior2501_3000, behavior3001_3500) #21329 lines

behaviorAll$sciName	<- ifelse(behaviorAll$species == '' & behaviorAll$subspecies == '', behaviorAll$Genus,
                              ifelse(behaviorAll$subspecies == '', paste(behaviorAll$Genus, '_', behaviorAll$species, sep = ''),
                                     paste(behaviorAll$Genus, '_', behaviorAll$species, '_', behaviorAll$subspecies, sep = '')))

behaviorAll			<- merge(behaviorAll, lumper, by.x = 'sciName', by.y = 'Mendeley_tag', all.x=TRUE) 
behaviorAll			<- merge(behaviorAll, IUCN_tax, by.x = 'sciName', by.y = 'Mendeley_tag', all.x=TRUE) 
behaviorAll			<- merge(behaviorAll, TenKTrees, by.x = 'sciName', by.y = 'Mendeley_tag', all.x=TRUE) 

behaviorAll			<- merge(behaviorAll, iucn, by.x = 'IUCN_Name', by.y = 'scientificName', all.x=TRUE) #21329
length(unique(behaviorAll$sciName)) #890 species
length(unique(behaviorAll$Lumper_Taxonomy)) #183 species

lengthUnique <- function(dat){
  return(length(unique(dat)))
}

##################################################
### How are papers distributed across species? ###
##################################################

NpapersPerSpecies <- aggregate(behaviorAll$Article.ID, by=list(behaviorAll$Lumper_Taxonomy), FUN=lengthUnique)

NpapersPerSpecies	<- NpapersPerSpecies[NpapersPerSpecies$Group.1 != "",]

NpapersPerSpecies	<- NpapersPerSpecies[order(NpapersPerSpecies$x, decreasing=TRUE), ] 
head(NpapersPerSpecies)
par(mar=c(10,4.1,2,2.1))
barplot(NpapersPerSpecies[1:25,]$x, names.arg=NpapersPerSpecies[1:25,]$Group.1, las=2, ylab="Number of Papers", col="midnight blue")

#############################################################################
### How much variation is there within a species for social organization? ###
#############################################################################

socialOrg			<- behaviorAll[behaviorAll$Social.organization != '',] #4843

socialOrgSummary			<- aggregate(socialOrg$Social.organization, by = list(socialOrg$Social.organization, socialOrg$Lumper_Taxonomy), FUN = length)
colnames(socialOrgSummary)	<- c('socialOrganization', 'species', 'numGroups')

socialOrgEntropy			<- aggregate(socialOrgSummary$numGroups, by = list(socialOrgSummary$species), FUN = shannon.entropy)
colnames(socialOrgEntropy) 	<- c('species', 'entropy')
socialOrgNTypes			<- aggregate(socialOrgSummary$numGroups, by = list(socialOrgSummary$species), FUN = length)
colnames(socialOrgNTypes) 	<- c('species', 'nTypes')
socialOrgNGroups			<- aggregate(socialOrgSummary$numGroups, by = list(socialOrgSummary$species), FUN = sum)
colnames(socialOrgNGroups) 	<- c('species', 'nGroups')
socialOrgEntropy			<- cbind(socialOrgEntropy, socialOrgNTypes[,2], socialOrgNGroups[,2])
colnames(socialOrgEntropy) 	<- c('species', 'entropy', 'nTypes', 'nGroups')

socialOrgEntropyIUCN	<- merge(socialOrgEntropy, iucn, by.x = 'species', by.y = 'scientificName')

plot(jitter(socialOrgEntropyIUCN[socialOrgEntropyIUCN$nGroups >= 2,]$nTypes),(socialOrgEntropyIUCN[socialOrgEntropyIUCN$nGroups >= 2,]$catNum), pch = 16, xlab="Number of Social Organization Types", ylab="IUCN status")
model1	<- lm(socialOrgEntropyIUCN[socialOrgEntropyIUCN$nGroups >= 2,]$catNum ~ socialOrgEntropyIUCN[socialOrgEntropyIUCN$nGroups >= 2,]$nTypes)
summary(model1)

plot((socialOrgEntropyIUCN[socialOrgEntropyIUCN$nGroups >= 2,]$entropy),(socialOrgEntropyIUCN[socialOrgEntropyIUCN$nGroups >= 2,]$catNum), pch = 16, xlab="Entropy", ylab="IUCN status")
model2	<- lm(socialOrgEntropyIUCN[socialOrgEntropyIUCN$nGroups >= 2,]$catNum ~ socialOrgEntropyIUCN[socialOrgEntropyIUCN$nGroups >= 2,]$entropy)
summary(model2)

install.packages("MASS") 
library("MASS")
socialOrgEntropyIUCN$combinedcat	<- ifelse(socialOrgEntropyIUCN$catNum==0,0,
										ifelse(socialOrgEntropyIUCN$catNum==1,0,
										ifelse(socialOrgEntropyIUCN$catNum==2,1,2)))
table(socialOrgEntropyIUCN$catNum,socialOrgEntropyIUCN$combinedcat)

model3	<- polr(factor(combinedcat) ~ entropy, data=socialOrgEntropyIUCN)
summary(model3)

model4	<- polr(factor(combinedcat) ~ nTypes, data=socialOrgEntropyIUCN)
summary(model4)


####################################################################
### How much variation is there within a species for group size? ###
####################################################################

behaviorAll$Max...of.individuals	<-as.numeric(behaviorAll$Max...of.individuals)
summary(behaviorAll$Max...of.individuals)

NotLargestLevel	<- behaviorAll[behaviorAll$Type.of.data %in% c("Fission-fusion: Party/subgroup", "Multi-level: Smallest level", "Not a complicated species") & behaviorAll$Max...of.individuals<2000,]

groupSize			<- NotLargestLevel[NotLargestLevel$Max...of.individuals != '' & is.na(NotLargestLevel$Max...of.individuals) == FALSE,] #6156

cv	<- function(dat){
				return(sd(dat,na.rm=TRUE)/mean(dat, na.rm=TRUE))
}

groupSizeSummary			<- aggregate(groupSize$Max...of.individuals, by = list(groupSize$IUCN_Name), FUN = cv)

colnames(groupSizeSummary)	<-c("species", "cvGroupSize")
groupSizeIUCN	<- merge(groupSizeSummary, iucn, by.x = 'species', by.y = 'scientificName')
groupSizeIUCN

plot(as.numeric(groupSizeIUCN$cvGroupSize), jitter(groupSizeIUCN$catNum), pch=16, xlab="Coefficient of Variation in Group Size", ylab="IUCN status")

groupSizeSummary[groupSizeSummary$cvGroupSize>1, ]


#############################
### Phylogenetic analyses ###
#############################

class(Tree)

groupSizeSummary	<- merge(groupSizeSummary, TenKTrees, by.x="species",by.y="Mendeley_tag")

dim(groupSizeSummary) #245
length(unique(groupSizeSummary$X10K_Trees_Name)) $190

groupSizeSummary


####################################
### Network Graph of Social Org. ###
####################################

typeSocialOrg	<- unique(behaviorAll$Social.organization)[c(1:3,5:8)]
SocialOrgMat	<- matrix(,length(typeSocialOrg),length(typeSocialOrg),
					dimnames=list(typeSocialOrg,typeSocialOrg))
listSpecies 	<- unique(socialOrgSummary$species)[c(2:146)]


for(i in 1:length(typeSocialOrg)){
	for(j in 1:length(typeSocialOrg)){
		nSpeciesOverlap	<- 0
		for(k in listSpecies){
			subset	<- socialOrgSummary[socialOrgSummary$species==k,]
			if(typeSocialOrg[i] %in% subset$socialOrganization & typeSocialOrg[j] %in% 									subset$socialOrganization){
				nSpeciesOverlap <- nSpeciesOverlap+1
			}
		}	
		SocialOrgMat[i,j]	<- nSpeciesOverlap			
	}	
}

library(igraph)

socialOrgNet <-	graph_from_adjacency_matrix(SocialOrgMat, weighted=TRUE, diag=FALSE, mode="undirected")
plot(socialOrgNet, weights=E(socialOrgNet)$weights)
?graph_from_adjacency_matrix