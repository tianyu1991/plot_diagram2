NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(datasets)
library(dplyr)
library(ggplot2)

#plot1
total_emi<-sapply(split(NEI$Emissions,NEI$year),sum)
plot(names(total_emi),total_emi,type="l",xlab="year",ylab="total PM2.5 emission")
dev.copy(png,file="plot1.png")
dev.off()

#plot2
BCtimore<-subset(NEI,NEI$fips == "24510",select=c(Emissions,year))
BCtimore_emi<-group_by(BCtimore,year)
BCtimore_emi2<-summarize(BCtimore_emi,Emissions=sum(Emissions))
plot(BCtimore_emi2,type="l",xlab="year",ylab="PM2.5 emission(tons)",main="PM2.5 emission in Baltimore City")
dev.copy(png,file="plot2.png")
dev.off()

#plot3

BCtimore<-subset(NEI,NEI$fips == "24510",select=c(Emissions,year,type))
BCtimore_emi<-group_by(BCtimore,year,type)
BCtimore_emi2<-summarize(BCtimore_emi,Emissions=sum(Emissions))
qplot(year,Emissions,data=BCtimore_emi2,geom=c("point","smooth"),facets=.~type,main="PM2.5 emission from four sources in Baltimore City",ylab="PM2.5 emission(tons)")
dev.copy(png,file="plot3.png",width = 1000, height = 480)
dev.off()

#plot4
coal<-grep("Coal",SCC[,4])
scc<-as.data.frame(SCC[coal,1])
colnames(scc)<-"SCC" 
coal_emi<-merge(NEI,scc,by="SCC",all=FALSE)
coal_emi2<-group_by(coal_emi,year)
coal_emi2<-summarize(coal_emi2,Emissions=sum(Emissions))
qplot(year,Emissions,data=coal_emi2,geom=c("point","smooth"),xlab="year",ylab="PM2.5 emission(tons)",main="PM2.5 emission from coal combustion-related sources")
dev.copy(png,file="plot4.png")
dev.off()

#plot5
motor<-grep("On-Road",SCC[,4])
scc<-as.data.frame(SCC[motor,1])
colnames(scc)<-"SCC" 
motor_emi<-merge(NEI,scc,by="SCC",all=FALSE)
BC_motor_emi<-subset(motor_emi,motor_emi$fips == "24510",select=c(Emissions,year))
BC_motor_emi2<-group_by(BC_motor_emi,year)
BC_motor_emi2<-summarize(BC_motor_emi2,Emissions=sum(Emissions))
qplot(year,Emissions,data=BC_motor_emi2,geom=c("point","smooth"),xlab="year",ylab="PM2.5 emission(tons)",main="PM2.5 emission from motor vehicle sources in Baltimore City ")
dev.copy(png,file="plot5.png")
dev.off()

#plot6
LA_motor_emi<-subset(motor_emi,motor_emi$fips == "06037",select=c(Emissions,year))
LA_motor_emi2<-group_by(LA_motor_emi,year)
LA_motor_emi2<-summarize(LA_motor_emi2,Emissions=sum(Emissions))
png(file = "plot6.png", width = 960, height = 480)
par(mfrow=c(1,2))
with(BC_motor_emi2,plot(year,Emissions,col="blue",type="l",xlab="year",ylab="PM2.5 emission(tons)",sub="emissions from motor vehicle sources in Baltimore City"))
with(LA_motor_emi2,plot(year,Emissions,col="red",type="l",xlab="year",ylab="PM2.5 emission(tons)",sub="emissions from motor vehicle sources in Los Angeles County"))
legend("topright",lty=1,col=c("blue","red"),legend=c("Baltimore City","Los Angeles County"))
dev.off()
