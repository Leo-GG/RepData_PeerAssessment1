
data <- read.csv(unz("activity.zip", "activity.csv"))


## 1. Histogram of steps per day
perDay<-aggregate(data$steps,list(data$date),sum, na.rm=TRUE, na.action=NULL)
hist(perDay$x[perDay$x>0],xlab="Steps",ylab="Frequency (days)",main="Histogram of steps per day")
meanSteps=mean(perDay$x[perDay$x>0])
medianSteps=median(perDay$x[perDay$x>0])
legend("topright",c(paste("Mean=",as.character(meanSteps)),paste("Median=",as.character(medianSteps) )) )


## 2. Mean activity per day
# Summarize per interval
perInt<-aggregate(data$steps,list(data$interval),mean, na.rm=TRUE, na.action=NULL)
# Make a copy of original data 
fData<-data.frame(data)
# Fill missing (NA) values on the new dataframe using the average value
for (i in c(1:nrow(fData))){
  if(is.na(fData$steps[i])){
    fData$steps[i]<-perInt$x[perInt$Group.1==fData[i,"interval"]]
  }  
}
# Re-compute the average and plot the daily pattern
perIntFilled<-aggregate(fData$steps,list(fData$interval),mean, na.rm=TRUE, na.action=NULL)
plot(perIntFilled$Group.1,perIntFilled$x, type="l",xlab="Interval",ylab="Steps",main="Average activity pattern",col="red")

## 3. Histogram of steps per day after filling NA values
perDayF<-aggregate(fData$steps,list(fData$date),sum)
hist(perDayF$x,xlab="Steps",ylab="Frequency (days)",main="Histogram of steps per day")
meanSteps=mean(perDayF$x)
medianSteps=median(perDayF$x)
legend("topright",c(paste("Mean=",as.character(meanSteps)),paste("Median=",as.character(medianSteps) )) )


## 4. Weekday/weekend patterns
fData$day=weekdays(as.Date(fData$date))
fData$dayType<-sapply(fData$day,function(x){ if( x=="Saturday"|x=="Sunday" ) "weekend" else "weekday"})
# Subindex weekends and aggregate data
dataWeekend<-fData[fData$dayType=="weekend",]
intWeekend<-aggregate(dataWeekend$steps,list(dataWeekend$interval),mean)
# Subindex weekdays and aggregate data
dataWeekday<-fData[fData$dayType=="weekday",]
intWeekday<-aggregate(dataWeekday$steps,list(dataWeekday$interval),mean)
# Make a plot as indicated
par(mfrow=c(2,1))
plot(intWeekday$Group.1,intWeekday$x, type="l",xlab="Interval",ylab="Steps",main="Weekday",col="red")
plot(intWeekend$Group.1,intWeekend$x, type="l",xlab="Interval",ylab="Steps",main="Weekend",col="red")

