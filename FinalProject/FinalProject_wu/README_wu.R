###This data from kaggle. link: https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings/downloads/Video_Games_Sales_as_at_22_Dec_2016.csv
###I can't download this file by using commands.

library(data.table)
library(bitops)
library(RCurl)
library(ggplot2)
library(plyr)

gamedata<-read.csv('Video_Games_Sales_as_at_22_Dec_2016.csv') #Format into data frame

###############
#Cleaning Data#
###############

#Add "None" to gamedata$developer levels.

levels<-levels(gamedata$Developer)
levels[length(levels)+1]<-"Unknown"
gamedata$Developer<-factor(gamedata$Developer,levels = levels)
gamedata$Developer[gamedata$Developer==""]<-"Unknown"

#I consider the missing values are completey at ramdom(MCAR).
#Drop the entire offending row and our final results will not be biased.
gamedata<-gamedata[!is.na(gamedata$User_Count),]
gamedata<-gamedata[!is.na(gamedata$User_Score),]
gamedata<-gamedata[!is.na(gamedata$Critic_Score),]
gamedata<-gamedata[!is.na(gamedata$Critic_Count),]
gamedata<-gamedata[!is.na(gamedata$Year_of_Release),]
gamedata<-data.frame(gamedata)


#Due to some observations are highly skewed, we use log and scale to transform.

gamedata$User_Score<-as.numeric(gamedata$User_Score)
gamedata$Global_Sales<-as.numeric(gamedata$Global_Sales)
gamedata$logUser_Score<-log(gamedata$User_Score)
gamedata$logCritic_Score<-log(gamedata$Critic_Score)
gamedata$logGlobal_sale<-log(gamedata$Global_Sales)
gamedata$NormGlobal_sale<-scale(gamedata$Global_Sales)

##########
# Method #
##########
#Global sales(log) = b0 + b1 * Critic score + b2 * Genre + b3 * platform + e

fit<-lm(data = gamedata, logGlobal_sale ~ Genre + Platform + Critic_Score )
summary(fit)

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

---------------
  
###User score and global sales
par(mar=c(4,4,4,1),cex=1)
plot(gamedata$User_Score,gamedata$logGlobal_sale,
     pch=4,
     ylab = "Global sales(log)",
     xlab = "User score",
     col = 'blue',
     main = 'Relationship between Global sales and User score',
     xlim = c(0,100))
abline(lm(gamedata$logGlobal_sale~gamedata$User_Score),col = "red")
-----------------
  
###Critic score and global sales
par(mar=c(4,4,4,1),cex=1)
plot(gamedata$Critic_Score,gamedata$logGlobal_sale,
     pch=4,
     ylab = "Global sales(log)",
     xlab = "Critic score",
     col = 'blue',
     main = 'Relationship between Global sales and Critic score')
abline(lm(gamedata$logGlobal_sale~gamedata$User_Score),col = "red")
--------------------
###Platform plot
Platform<-aggregate(gamedata[,],list(gamedata$Platform),mean)
count<-count(gamedata$Platform)
Platform$count<-count$freq
Platform$device<-Platform$Group.1
ggplot(Platform,aes(x=User_Score,y=logGlobal_sale,size=count))+ 
  geom_point(shape=21)+
  geom_text(data=Platform,
            aes(x=User_Score,
                y=logGlobal_sale,
                label=device),
            vjust=1.5,size=5)


--------------------------
###Genre plot
Genre<-aggregate(gamedata[,],list(gamedata$Genre),mean)
count<-count(gamedata$Genre)
Genre$count<-count$freq
Genre$type<-Genre$Group.1
ggplot(Genre,aes(x=User_Score,y=logGlobal_sale,size=count))+ 
  geom_point(shape=21)+
  geom_text(data=Genre,
            aes(x=User_Score,
                y=logGlobal_sale,
                label=type),
            vjust=1.5,size=5)

------------------------
###Publisher plot
Publisher<-aggregate(gamedata[,],list(gamedata$Publisher),mean)
count<-count(gamedata$Publisher)
Publisher$count<-count$freq
Publisher$company<-Publisher$Group.1
ggplot(Publisher,aes(x=User_Score,y=logGlobal_sale,size=count))+ 
  geom_point(shape=21)

-------------------------
###Year of release
par(mar=c(4,4,4,1),cex=1)
plot(gamedata$Year_of_Release,gamedata$logGlobal_sale,
     pch=4,
     ylab = "Global sales(log)",
     xlab = "Released year",
     col = 'blue',
     main = 'Relationship between Global sales and released year',
     xlim = c(15,40)
)

