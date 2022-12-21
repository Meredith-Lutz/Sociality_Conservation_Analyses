setwd("~/Desktop/Cleaned PREDICT data")
#setwd('G:/My Drive/Graduate School/Research/Projects/SocialityConservationAnalyses/Data')
library("stringr")

#read in SS data
SS_001_500old 		<- read.csv("SS_001_500.csv", stringsAsFactors=FALSE)
SS_501_1000old 		<- read.csv("SS_501_1000.csv", stringsAsFactors=FALSE)
SS_001_500verified 	<- read.csv("SS_001_500_verified.csv", stringsAsFactors=FALSE)
SS_501_1000verified 	<- read.csv("SS_501_1000_verified.csv", stringsAsFactors=FALSE)
SS_1001_1500	 	<- read.csv("SS_1001_1500.csv", stringsAsFactors=FALSE)
SS_1501_2000	 	<- read.csv("SS_1501_2000.csv", stringsAsFactors=FALSE)
SS_2001_2500	 	<- read.csv("SS_2001_2500.csv", stringsAsFactors=FALSE)
SS_2501_3000	 	<- read.csv("SS_2501_3000.csv", stringsAsFactors=FALSE)
SS_3001_3500	 	<- read.csv("SS_3001_3500.csv", stringsAsFactors=FALSE)
SS_3501_4000	 	<- read.csv("SS_3501_4000.csv", stringsAsFactors=FALSE)

#force col names
colnames(SS_3501_4000) <- colnames(SS_3001_3500) <- colnames(SS_2501_3000) <- colnames(SS_2001_2500) <- colnames(SS_1501_2000) <- colnames(SS_1001_1500) <- colnames(SS_501_1000old) <- colnames(SS_001_500old)

#now add in verified data chunks
colnames(SS_001_500verified) <- colnames(SS_501_1000verified)
newformat_studysiteverified <- rbind.data.frame(SS_001_500verified, SS_501_1000verified)

#bind together
currentformat_studysite	<- rbind.data.frame(SS_001_500old, SS_501_1000old, SS_1001_1500, SS_1501_2000, SS_2001_2500, SS_2501_3000, SS_3001_3500, SS_3501_4000)

#read in edits from Open Refine
studySitesOpenRefine 		<- read.csv("oldformat_studysite.csv", stringsAsFactors=FALSE)

allArticleIDs 	<- sort(unique(c(currentformat_studysite$Article.ID,studySitesOpenRefine$Article.ID)))

revisedStudySiteNames <- data.frame()

revisedStudySiteColNames <- c("Coder.Initials", "Article.ID", "Study.site.ID", "Location.of.small.site", "Location.of.large.site", "Country")

for(i in allArticleIDs ) {
	print(i)
			nStudySitesOpenRefine <- nrow(studySitesOpenRefine[studySitesOpenRefine$Article.ID==i & 							is.na(studySitesOpenRefine$Article.ID)==FALSE,])
			nStudySitesCurrentFormat <- nrow(currentformat_studysite[currentformat_studysite$Article.ID==i & 					is.na(currentformat_studysite$Article.ID)==FALSE,])
			nStudySitesNewFormat <- nrow(newformat_studysiteverified[newformat_studysiteverified$Article.ID==i & 				is.na(newformat_studysiteverified$Article.ID)==FALSE,]) 
			if(nStudySitesOpenRefine==0) {
				if(i %in% c(193,283, 325, 327, 356)) {
					#keep currentformat for these select articles
					templine <- currentformat_studysite[currentformat_studysite$Article.ID==i, 		
						c("Coder.Initials","Article.ID", "Study.Site.ID","Location.of.study", "Country.of.study")]
					templine$LargeSite <- NA
					templine <- templine[,c(1:4,6,5)]
					colnames(templine) <-revisedStudySiteColNames
					revisedStudySiteNames <- rbind.data.frame(revisedStudySiteNames, templine)
				} 
				else{
					if(nStudySitesNewFormat != nStudySitesCurrentFormat) {
					#keep newformatverified if # of SS is different from currentformat
						templine <- newformat_studysiteverified[newformat_studysiteverified$Article.ID==i, 	
							c("Article.Coder.Initials","Article.ID", 							"Study.Site.ID","Location.of.small.study.site", 							"Location.of.large.study.site","Country")]
						 colnames(templine) <-revisedStudySiteColNames
						revisedStudySiteNames <- rbind.data.frame(revisedStudySiteNames, templine)
					} 
					else{
						templine <- currentformat_studysite[currentformat_studysite$Article.ID==i, 		
						c("Coder.Initials","Article.ID", "Study.Site.ID","Location.of.study", "Country.of.study")]
						templine$LargeSite <- NA
						templine <- templine[,c(1:4,6,5)]
						colnames(templine) <-revisedStudySiteColNames
						revisedStudySiteNames <- rbind.data.frame(revisedStudySiteNames, templine)
					}
				}
			} 
			if(nStudySitesOpenRefine>0) {
				if(nStudySitesOpenRefine != nStudySitesCurrentFormat) {
					templine <- currentformat_studysite[currentformat_studysite$Article.ID==i, 		
						c("Coder.Initials","Article.ID", "Study.Site.ID","Location.of.study", "Country.of.study")]
						templine$LargeSite <- NA
						templine <- templine[,c(1:4,6,5)]
						colnames(templine) <-revisedStudySiteColNames
						revisedStudySiteNames <- rbind.data.frame(revisedStudySiteNames, templine)
				}
				else {
					templine <- studySitesOpenRefine[studySitesOpenRefine$Article.ID==i &
							is.na(studySitesOpenRefine$Article.ID)==FALSE, 
							c("Coder.Initials","Article.ID", 							"Study.Site.ID","Location.of.study.1", 							"Location.of.study.2","Country.of.study")]
					colnames(templine) <-revisedStudySiteColNames
					revisedStudySiteNames <- rbind.data.frame(revisedStudySiteNames, templine)
				}

			}
}

finalversion <- merge(currentformat_studysite, revisedStudySiteNames, by.x=c("Coder.Initials", "Article.ID", "Study.Site.ID"), by.y=c("Coder.Initials", "Article.ID", "Study.site.ID"), all.x=TRUE)

finalversion <- finalversion[order(finalversion$Article.ID, finalversion$Coder.Initials, finalversion$Study.Site.ID),]

write.csv(finalversion, "SS_merged_open_refine.csv")
