library(MixedDataImpute)
library(mice)
library(stargazer)

wages<- read.csv('wages.csv')  #Load the file

summary(wages) #How the data looks like.

wages<-wages[!is.na(wages$tenure),] #Drop NA's in tenure.
wages<-wages[!is.na(wages$hgc),]  #Drop NA's in hgc.

summary(wages) #Make sure there is no more NA's in tenure and hgc.

###stargazer(wages[1:5,],summary = FALSE) #Make a table in latex.

#According the summary above, logwage has 560 NA's.

rate<- 560/length(wages$logwage)
rate #Missing rate of logwage is 25.13%

#The missing value in logwage is more like MNAR.  Because there is reasons that people keep their income secretly, and we can't use other variable to account the missing values.

############
#Imputation#
############
  
###Linear model for complated case.
  
wages_complate<-wages[!is.na(wages$logwage),] #Delete missing values in logwage.

linear_complate<-lm(logwage ~ hgc + college + poly(tenure,2) + age + married, data = wages_complate)

###Linear model after mean imputation.

wages_mean <- wages 
wages_mean$logwage[is.na(wages_mean$logwage)]<-mean(wages_complate$logwage) #Replace the NA's by mean of the complated case.

linear_mean<-lm(logwage ~ hgc + college + poly(tenure,2)  + age + married, data = wages_mean)

###Linear model after prediction of regression model we have got from complated case.

summary(linear_complate) #The model we use to predict.
wages_ols <- wages       #Creat a new frame.
wages_ols1 <- predict(linear_complate,wages)  #Predict
#Replace the missing value by the result of prediction.
for(i in 1:2229) {
  if (is.na(wages_ols$logwage[i])){
    wages_ols$logwage[i]<-wages_ols1[i]
  }
}

linear_ols<-lm(logwage ~ hgc + college + poly(tenure,2)  + age + married, data = wages_ols)

#Mice

wages_imp <- mice(wages)

linear_mice<-with(wages_imp,lm(logwage ~ hgc + college + poly(tenure,2)  + age + married))
summary(linear_mice)

#Stargazer

stargazer(linear_complate,linear_mean,linear_ols)

