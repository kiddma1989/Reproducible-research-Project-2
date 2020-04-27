#read file
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destfile <- "data.csv"
download.file(url, destfile)
data <- read.csv("data.csv", sep = ",")
event<-data[,c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
#summary of fatility and injuries with event
library(reshape2)
fat_injur<-melt(event,id='EVTYPE',measure.vars = c('INJURIES','FATALITIES'))
fat_injur_sum<-dcast(fat_injur,EVTYPE~variable,sum)


#Events with highest fatality and injury
fat_sum<-fat_injur_sum[order(-fat_injur_sum$FATALITIES),][,c('EVTYPE','FATALITIES')]
injur_sum<-fat_injur_sum[order(-fat_injur_sum$INJURIES),][,c('EVTYPE','INJURIES')]


#Calculate the total damage of observations
library(dplyr)
damage<-event%>%select(EVTYPE,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)
symbol<-sort(unique(as.character(damage$PROPDMGEXP)))
number<-c(0,0,0,1,10,10,10,10,10,10,10,10,10,1e9,1e2,1e2,1e3,1e6,1e6)
damage$PROPDMGEXP<-number[match(damage$PROPDMGEXP,symbol)]
damage$CROPDMGEXP<-number[match(damage$CROPDMGEXP,symbol)]
damage<-damage%>%mutate(PROPDMGtotal=PROPDMG*PROPDMGEXP)%>%
        mutate(CROPDMGtotal=CROPDMG*CROPDMGEXP)%>%
        mutate(DamageTotal=PROPDMGtotal+CROPDMGtotal)
#Events with highest damage
DamageTotal<-damage%>%group_by(EVTYPE)%>%
        summarize(TotalDamage=sum(DamageTotal))%>%
        arrange(-TotalDamage)
DamageTotal$TotalDamage=DamageTotal$TotalDamage/1e9 #to billion


#Plot events with highest fatality and injury
par(mfrow=c(1,2),mar=c(12,6,4,1))
barplot(fat_sum$FATALITIES[1:10],names.arg = fat_sum$EVTYPE[1:10],las=3,
        main = 'Top 10 Events with Highest fatalities',
        ylab = 'Total Number of Fatalities')
barplot(injur_sum$INJURIES[1:10],names.arg = injur_sum$EVTYPE[1:10],las=3,
        main = 'Top 10 Events with Highest Injuries',
        ylab = 'Total Number of Injuries')


#Plot events with highest damage
par(mfrow=c(1,1),mar=c(12,6,4,1))
barplot(DamageTotal$TotalDamage[1:10],names.arg = DamageTotal$EVTYPE[1:10],las=3,
        main = 'Top 10 Events with Highest Economic Impact',
        ylab = 'Total Economic Impact(Billion USD)')