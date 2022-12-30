data<-read.csv("Master Sheet of Interns - Sheet1.csv")

matrix<-as.matrix(c(14,34,51,32))

png("classYearBarplot.png", width=8, height=8, units="in", res=300)
par(cex=1.5)
barplot(matrix,beside=TRUE, col="midnight blue", xlab="Year when joined", ylab="Number of students",xaxt="n")
axis(1,at=c(1.5,2.5,3.5, 4.5),labels=c("First year", "Sophomore", "Junior", "Senior"),)
dev.off()

data	<- data[data$Major != "",]
png("MajorofStudents.png",width=8, height=8, units="in", res=300)
par(mar=c(10,5,2,1),cex=1.5)
barplot(as.matrix(sort(table(data$Major),decreasing=TRUE)),beside=TRUE, col="midnight blue",ylim=c(0,50), ylab="Number of students")
mtext("Major", side=1,line=8,cex=1.5)
text(x=seq(from=1.5,by=1,length.out=17),y=-7,labels=c("WFCB","Animal \n Science", "Anthropology", "EEB", "Animal \n Biology", "Environ. \n Science", "Biology", "Biochemistry", "Psychology", "Animal Sci & \n Management", "Ecological \n Management", "Global \n Disease Bio.", "Linguistics","NPB","Sociology","Spanish","Statistics"),xpd=NA,srt=90)
dev.off()

