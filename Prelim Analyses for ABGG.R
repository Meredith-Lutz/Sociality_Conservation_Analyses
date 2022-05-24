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

plot(as.numeric(groupSizeIUCN$cvGroupSize), jitter(groupSizeIUCN$catNum), pch=16, xlab="Coefficient of Variation in Group Size", ylab="IUCN status")


###meangroupSize

meangroupSizeSummary			<- aggregate(groupSize$Max...of.individuals, by = list(groupSize$IUCN_Name, 										groupSize$Taxon_group), FUN = mean)
colnames(meangroupSizeSummary)	<-c("species", "Taxon","meanGroupSize")
meangroupSizeIUCN	<- merge(meangroupSizeSummary, iucn, by.x = 'species', by.y = 'scientificName')

png("meangroupSizeIUCN.png", width=8, height=8, units="in", res=300)
par(mar=c(5,4,2,2), cex=1.5)
plot(log10(as.numeric(meangroupSizeIUCN[meangroupSizeIUCN$meanGroupSize<100,]$meanGroupSize)), jitter(meangroupSizeIUCN[meangroupSizeIUCN$meanGroupSize<100,]$catNum), pch=16, xlim=c(0,2), yaxt="n" , xaxt = "n", xlab="Log of Mean Group Size", ylab="IUCN Status")
axis(2,at=0:4,labels=c("Least \n Concern", "Near \n Threatened", "Vulnerable", "Endangered", "Critically \n Endangered"))
axis(1,at=c(0,1,2),labels=c(1,10,100))

#legend(68,4.5,c("Apes", "Catarrhine \nMonkeys", "Platyrrhines", "Strepsirrhines", "Tarsiers"), col=1:5, pch=16, bty="n")
dev.off()



###Merge mean group size and variation in group size###

MeanGroupSize_Var <- merge(meangroupSizeSummary, groupSizeSummary, by.x="species", by.y="species")
head(MeanGroupSize_Var)
dim(MeanGroupSize_Var)

Strep	<- MeanGroupSize_Var[MeanGroupSize_Var$Taxon=="Strep",]
Cat		<- MeanGroupSize_Var[MeanGroupSize_Var$Taxon=="Cat",]
Tar		<- MeanGroupSize_Var[MeanGroupSize_Var$Taxon=="Tar",]
Ape		<- MeanGroupSize_Var[MeanGroupSize_Var$Taxon=="Ape",]
Plat	<- MeanGroupSize_Var[MeanGroupSize_Var$Taxon=="Plat",]
Hap		<- MeanGroupSize_Var[MeanGroupSize_Var$Taxon!="Strep",]


model1 <- lm(MeanGroupSize_Var[4:253,]$cvGroupSize ~ log10(MeanGroupSize_Var[4:253,]$meanGroupSize))
summary(model1)

xs <- seq(0,2,by=0.05)
ys <- model1$coef[1]+xs*(model1$coef[2])
png("LOGGroupSizeVScvGroupSize_Black.png", width=8, height=8, units="in", res=300)
par(mar=c(5,4,2,2), cex=1.5)
plot(log10(MeanGroupSize_Var[4:253,]$meanGroupSize), MeanGroupSize_Var[4:253,]$cvGroupSize, xlim=c(0,2), pch=16, xaxt="n", xlab="Log of Mean Group Size", ylab="Coefficient of Variation in Group Size")
points(xs, ys, pch=16, type = "l", col="midnight blue", lwd=3, lty=5)
axis(1,at=c(0,1,2),labels=c(1,10,100))
dev.off()

png("LOGGroupSizeVScvGroupSize_SupplementWithLines.png", width=8, height=8, units="in", res=300)
par(mar=c(5,4,2,2), cex=1.5)
plot(log10(MeanGroupSize_Var[4:253,]$meanGroupSize), MeanGroupSize_Var[4:253,]$cvGroupSize, xlim=c(0,2), pch=16, xaxt="n", xlab="Log of Mean Group Size", ylab="Coefficient of Variation in Group Size")
points(xs3, ys3, pch=16, type="l", col=4, lwd=3, lty=5)
points(xs2, ys2, pch=16, type="l", col=2, lwd=3, lty=5)
axis(1,at=c(0,1,2),labels=c(1,10,100))
dev.off()

model3 <- lm(Strep[2:41,]$cvGroupSize ~ log10(Strep[2:41,]$meanGroupSize))
summary(model3)
xs3 <- seq(0,2,by=0.05)
ys3 <- model3$coef[1]+xs*(model3$coef[2])
png("GroupSizeVScvGroupSize_Strep.png", width=8, height=8, units="in", res=300)
par(mar=c(5,4,2,2), cex=1.5)
plot(log10(MeanGroupSize_Var[4:253,]$meanGroupSize), MeanGroupSize_Var[4:253,]$cvGroupSize, xaxt="n", xlim=c(0,2), pch=16, xlab="Log of Mean Group Size", ylab="Coefficient of Variation in Group Size", col=ifelse(MeanGroupSize_Var[4:253,]$Taxon!="Strep","gray85",NA))
points(log10(Strep[2:41,]$meanGroupSize), Strep[2:41,]$cvGroupSize,col=4, pch=16)
points(xs3, ys3, pch=16, type="l", col=4, lwd=3, lty=5)
#points(xs2, ys2, pch=16, type="l", col=2, lwd=3, lty=5)
axis(1,at=c(0,1,2),labels=c(1,10,100))
dev.off()

dim(Cat)
png("GroupSizeVScvGroupSize_Cat.png", width=8, height=8, units="in", res=300)
par(mar=c(5,4,2,2), cex=1.5)
plot(MeanGroupSize_Var[4:253,]$meanGroupSize, MeanGroupSize_Var[4:253,]$cvGroupSize, xlim=c(0,85), pch=16, xlab="Mean Group Size", ylab="Coefficient of Variation in Group Size", col=ifelse(MeanGroupSize_Var[4:253,]$Taxon!="Cat","gray85",NA))
points(Cat[2:101,]$meanGroupSize, Cat[2:101,]$cvGroupSize,col=2, pch=16)
dev.off()

model2 <- lm(Hap[3:212,]$cvGroupSize ~ log10(Hap[3:212,]$meanGroupSize))
summary(model2)

xs2 <- seq(0,2,by=0.05)
ys2 <- model2$coef[1]+xs*(model2$coef[2])
png("GroupSizeVScvGroupSize_Hap.png", width=8, height=8, units="in", res=300)
par(mar=c(5,4,2,2), cex=1.5)
plot(log10(MeanGroupSize_Var[4:253,]$meanGroupSize), MeanGroupSize_Var[4:253,]$cvGroupSize, xaxt="n", xlim=c(0,2), pch=16, xlab="Log of Mean Group Size", ylab="Coefficient of Variation in Group Size", col=ifelse(MeanGroupSize_Var[4:253,]$Taxon=="Strep","gray85",NA))
points(log10(Hap[3:212,]$meanGroupSize), Hap[3:212,]$cvGroupSize,col=2, pch=16)
points(xs2, ys2, pch=16, type="l", col=2, lwd=3, lty=5)
axis(1,at=c(0,1,2),labels=c(1,10,100))
dev.off()

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

typeSocialOrg	<- sort(unique(behaviorAll$Social.organization))[c(2:8)]
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
set.seed(17)
layout		<-	layout.fruchterman.reingold(socialOrgNet)
png("NetworkSocialOrg_WhiteEdges.png", width=8, height=8, units="in", res=300)
plot(socialOrgNet, edge.width=E(socialOrgNet)$weight/5, vertex.color="midnight blue", layout=layout, vertex.size=eigen_centrality(socialOrgNet)$vector*35, vertex.label=NA, edge.color="white")
dev.off()

png("NetworkSocialOrg_MMMF.SMMF_weighted.png", width=8, height=8, units="in", res=300)
plot(socialOrgNet, edge.width=E(socialOrgNet)$weight/5, vertex.color="midnight blue", layout=layout, vertex.size=eigen_centrality(socialOrgNet)$vector*35, vertex.label=NA, edge.color=c(rep("white",13), rgb(169,169,169,max=255), rep("white",7)))
dev.off()

png("NetworkSocialOrg_MMMF.SMMF_unweighted.png", width=8, height=8, units="in", res=300)
plot(socialOrgNet, edge.width=3,vertex.color="midnight blue", layout=layout, vertex.size=eigen_centrality(socialOrgNet)$vector*35, vertex.label=NA, edge.color=c(rep("white",13), rgb(169,169,169,max=255), rep("white",7)))
dev.off()

png("NetworkSocialOrg_Both_edges.png", width=8, height=8, units="in", res=300)
plot(socialOrgNet, edge.width=E(socialOrgNet)$weight/5,vertex.color="midnight blue", layout=layout, vertex.size=eigen_centrality(socialOrgNet)$vector*35, vertex.label=NA, edge.color=c(rep("white",7), rgb(169,169,169,max=255), rep("white", 5), rgb(169,169,169,max=255), rep("white",7)))
dev.off()

####Pulling out Study Site info (lat/long)####

colnames(SS3001_3500) <- colnames(SS2501_3000) <- colnames(SS1501_2000) <- colnames(SS501_1000) <- colnames(SS2001_2500) <- colnames(SS1001_1500) <- colnames(SS001_500)

SSAll	<- rbind(SS001_500, SS501_1000, SS1001_1500, SS1501_2000,
                     SS2001_2500, SS2501_3000, SS3001_3500) #6903
         
SSAll$Latitude	<-	as.numeric(SSAll$Latitude)

summary(SSAll$Latitude)
SS_without_errors	<- SSAll[SSAll$Latitude <=90 & is.na(SSAll$Latitude)==FALSE,]       

write.csv(SS_without_errors,"SS_without_errors.csv")

#### Plotting Kamilar & Baden Intraspecific Data ####

KamilarData 	<- 	read.csv("KamilarBadenData_intraspecificDATA.csv")
KamilarDataMean	<-	aggregate(KamilarData$Mean.group.size, by=list(KamilarData$REAL_Species), FUN=mean)
colnames(KamilarDataMean)	<- c("Species", "MeanGroupSize")
KamilarDatacv	<- 	aggregate(KamilarData$Mean.group.size, by=list(KamilarData$REAL_Species), FUN=cv)
colnames(KamilarDatacv)		<- c("Species", "CVGroupSize")


png("KamilarBaden_IntraspecificData.png", width=8, height=8, units="in", res=300)
par(mar=c(5,4,2,2), cex=1.5)
summarizedKamilarData	<- merge(KamilarDataMean, KamilarDatacv, by.x="Species", by.y="Species")
plot(log10(summarizedKamilarData$MeanGroupSize), summarizedKamilarData$CVGroupSize, xlim=c(0,2), ylim=c(0,1.4), pch=16, xaxt="n", xlab="Log of Mean Group Size", ylab="Coefficient of Variation in Group Size")
axis(1,at=c(0,1,2),labels=c(1,10,100))
dev.off()

<<<<<<< HEAD
##### TESTING #####
=======
##### Hello
##### Sricharan's subset attempts

group <- subset(behaviorAll, select=c(Genus, species, Mean.individuals, SD.individuals, N.groups, Median.individuals))
groupFM <- subset(behaviorAll, select=c(Genus, species, Mean.adult.females, SD.adult.females, Median.adult.females, Min...adult.females, Max...adult.females, Mean.adult.males, SD.Adult.males, Median.adult.males, Min...adult.males, Max...adult.males))
groupDT <- subset(behaviorAll, select=c(Genus, species, Predation.report., Predation...aerial., Predation...terrestrial., Predation...snake.))
groupFD <- subset(behaviorAll, select=c(Genus, species, Seasonal.Feeding.Data.Available., Diet.methodology, X..plant.reproductive.parts, X..folivory, Type.of.leaves, X..insects, X..fungus, Insect.eater., Fungus.eater., Dietary.ecology.study))
groupDS <- subset(behaviorAll, select=c(Genus, species, Female.dispersal, Female.secondary.dispersal, Male.dispersal, Male.secondary.dispersal))

groupALL <- subset(behaviorAll, Allomaternal.care == "Yes", select =Article.ID)
install.packages("janitor")
library(janitor)
str(behaviorAll)
tabyl(behaviorAll, GSP, Allomaternal.care)

adorn_totals("col") %>%
adorn_percentages("row") %>%
adorn_pct_formatting(digits = 2) %>%
adorn_ns() %>%
adorn_title()

tabyl(behaviorAll, GSP, Social.learning)

tabyl(behaviorAll, GSP, Predation.report., Predation...aerial.)

tabyl(behaviorAll, GSP, Habitat.loss, Habitat.fragmentation)

tabyl(behaviorAll, GSP, Activity.pattern)

tabyl(behaviorAll, GSP, Female.dispersal, Female.secondary.dispersal)

tabyl(behaviorAll, GSP, Social.learning)


library(stringr)
behaviorAll$GSP <- str_c(behaviorAll$Genus, '', behaviorAll$species)
>>>>>>> main
