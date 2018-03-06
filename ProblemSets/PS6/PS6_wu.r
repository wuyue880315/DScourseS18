###This data from kaggle. link: https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings/downloads/Video_Games_Sales_as_at_22_Dec_2016.csv
###I can't download this file by using commands.

library(data.table)
library(bitops)
library(RCurl)
library(ggplot2)

gamedata<-read.csv('Video_Games_Sales_as_at_22_Dec_2016.csv') #Format into data frame

###############
#Cleaning Data#
###############

#Add "None" to gamedata$developer levels.
levels<-levels(gamedata$Developer)
levels[length(levels)+1]<-"None"
gamedata$Developer<-factor(gamedata$Developer,levels = levels)

#Replace "NA" by "None"
gamedata$Developer[is.na(gamedata$Developer)]<-"None"

#I consider the missing values are completey at ramdom(MCAR).
#Drop the entire offending row and our final results will not be biased.
gamedata<-gamedata[!is.na(gamedata$User_Count),]
gamedata<-gamedata[!is.na(gamedata$User_Score),]
gamedata<-gamedata[!is.na(gamedata$Critic_Score),]
gamedata<-gamedata[!is.na(gamedata$Critic_Count),]


#Due to some observations are highly skewed, we use log and scale to transform.

gamedata$logUser_Count<-log(gamedata$User_Count)
gamedata$logUser_Score<-log(gamedata$User_Score)
gamedata$logCritis_Count<-log(gamedata$Critic_Count)


gamedata$NormNA_sale<-scale(gamedata$NA_Sales)
gamedata$NormEU_sale<-scale(gamedata$EU_Sales)
gamedata$NormJP_sale<-scale(gamedata$JP_Sales)
gamedata$Normother_sale<-scale(gamedata$Other_Sales)
gamedata$NormGlobal_sale<-scale(gamedata$Global_Sales)

################
#visualizations#
################

###Frequency of game genre
par(mar=c(4,4,4,1),cex=0.5)
plot(gamedata$Genre,
     ylab="Frequency",
     xlab="genre")

###Boxplot of genre and global sale.
plot(gamedata$Genre,gamedata$NormGlobal_sale,
     ylab="Global sale",
     xlab="genre")

###Critic score and global sales
plot(gamedata$logCritis_Score,gamedata$NormGlobal_sale,
     ylab = "global sales",
     xlab = "critic score")

