library(rmongodb)
mongo <- mongo.create()
userColl <- "sm.user"
dataColl <- "sm.data"
##load mongodb data to data frame
userDF <- do.call(rbind.data.frame,mongo.find.all(mongo,userColl))
##Clean
userDF <- within(userDF,rm("X_id"))
dataDF <- do.call(rbind.data.frame,mongo.find.all(mongo,dataColl))
##Clean
dataDF <- within(dataDF,rm("X_id"))
##Clean
userDF <- userDF[-as.numeric(rownames(userDF[userDF$x_NMI==20453,])),]
##Before code the attributes, copy the data frame
userChar <- userDF
##Coding the attributes
userDF <- codingAttribute(userDF)
##Add Average Usage Column
userDF <- addAverageColumn(userDF)
userChar <- addAverageColumn(userChar)
##new table contains the average daily usage per user
userAverageHourlyUsage <- averageHalfHourUsagePerUser()
##a list contains the totally average daily usage
averageHourlyUsageAll <- totalAverageHourlyUsage()
##reveal the peak time and off-peak time
plot(averageHourlyUsageAll)
summary(averageHourlyUsageAll)
##the median arount 0.25
averageHourlyUsageAll>0.25
##first 14 and last 2 are false, which could be defined as off-peak hours
## and the peak hours should be 7:00 AM to 11:00 PM

##model the characteristics and usage
summary(userChar[,2:7])
##Try to see the correlation and linear relation between the Ave # and other attributes.
plot(userDF[,2:7])
##get the correlation matrix
cor(userDF[,2:7])
model = lm(Ave.Daily.Usage ~ Building + number_of_people + pool + type_of_AC + No_of_AC,data=userDF)
##view the model result
model
##to see the regression Diagnostics
par(mfrow=c(2,2)) ## in one page
plot(model) ## plot
par(mfrow=c(1,1)) ## turn back to 1 chart per page
##try to predict
predict(model,list(Building=2,number_of_people=2,pool=0,type_of_AC=2,No_of_AC=2))
##result is 10.0465
##To see the real number with the same attributs
View(userDF[userDF$Building==2&userDF$number_of_people==2&userDF$pool==0&userDF$type_of_AC==2&userDF$No_of_AC==2,])
boxplot(userDF[userDF$Building==2&userDF$number_of_people==2&userDF$pool==0&userDF$type_of_AC==2&userDF$No_of_AC==2,7])
summary(userDF[userDF$Building==2&userDF$number_of_people==2&userDF$pool==0&userDF$type_of_AC==2&userDF$No_of_AC==2,7])
##Mdeian is 9.818 and Mean is 9.829 that quite close to the predict result
##So we can see the model works.

##Analyze the Price for fixed plan for low power consumption users
##for company, calc the revenue of low power consumption users
sum(userDF[userDF$Ave.Daily.Usage<11,7])*0.45
##result is 299.6704
##Fixed plan would be 0.45*11
0.45*11*nrow(userDF[userDF$Ave.Daily.Usage<11,])
##result is 440.55 which is much higher (arount 150%) than before.

