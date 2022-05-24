########################################
########################################
##### Heat Map Construction by Sri #####
########################################
########################################


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
