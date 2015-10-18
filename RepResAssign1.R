# Reproducible research - peer assessment 1
# code assumes that you have downloaded the file and have 
# set the working directory to be the same as the file .


# load the libraries that are used in the code

library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

library("chron", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")


#when I turn this into the final file, add in a hyperlink to show where
# the file came from

#read the data in
ActivityData <- read.csv("activity.csv", header = TRUE, sep = ",",
                         colClasses = c("numeric","character","numeric") )

#take a look at the dimensions of the file and some of the data - does it look right?
dim(ActivityData)
head(ActivityData, 10)
tail(ActivityData, 1000)


# now make sure R knows that the date column is a date...note 
# that need to use upper case Y as is full year.
ActivityData$date <- as.Date(ActivityData$date, format = "%Y-%m-%d")


# now make a new data frame to store the steps per day
# use this to produce a histogram of the steps per day. Comment on the plot

StepsPerDay <- data.frame()
StepsPerDay <- aggregate(steps ~ date, ActivityData,sum)
head(StepsPerDay)


plot(StepsPerDay$date, StepsPerDay$steps, "l")
qplot(StepsPerDay$steps, geom = "histogram", binwidth = 500, 
      main = "Histogram for total steps per day",
      xlab = "Total Steps per Day",
      ylab = "Count of days",
      fill = "Steps",
      col = I("black"))


#mean and median
meanStepsPerDay <- mean(StepsPerDay$steps)
meanStepsPerDay
medianStepsPerDay <- median(StepsPerDay$steps)
medianStepsPerDay




#now for the average daily activity pattern
# need to now figure out the average steps for each 5 minute interval,
# so now need to treat each entry in the last column of ActivityData as a factor
ActivityData$interval <- as.factor(ActivityData$interval)

#then create another data frame to store the average steps per interval
StepsPerInterval <- aggregate(steps ~ interval, ActivityData,mean, na.rm = TRUE)
head(StepsPerInterval)

plot(StepsPerInterval$interval, StepsPerInterval$steps, "l")

StepsPerInterval <- as.data.frame(StepsPerInterval)




# want the interval which has the highest average number of steps.
#visual check suggests no ties, so just do this:

maxStepInt <- max(StepsPerInterval$steps, na.rm = TRUE)
maxStepInt
maxRow <- subset(StepsPerInterval, steps == maxStepInt)
maxRow



#So far, the missing values have been ignored. But how many are there?
missingData <- sum(is.na(ActivityData$steps))
missingData

# there are 2304. Mmm. let's fill those in with the average for the 
# interval, and see what difference it makes.

ActivityDataFull <- ActivityData 
for (i in 1:nrow(ActivityDataFull)) {
        if (is.na(ActivityDataFull$steps[i])) {
                
                #get the avg interval from StepsPerInterval
                ActivityDataFull$steps[i] <- StepsPerInterval[which(ActivityDataFull$interval[i] == 
                                                                            StepsPerInterval$interval), ]$steps 
        }
}


StepsPerDayFull <- data.frame()
StepsPerDayFull <- aggregate(steps ~ date, ActivityDataFull, sum )
head(StepsPerDayFull)

#redo the plot, for a visual check
plot(StepsPerDayFull$date, StepsPerDayFull$steps, "l")
qplot(StepsPerDayFull$steps, geom = "histogram", binwidth = 500, 
      main = "Histogram for total steps per day",
      xlab = "Total Steps per Day",
      ylab = "Count of days",
      fill = "Steps",
      col = I("black"))


#calculate the mean and median again, and see what the difference is
meanStepsPerDayFull <- mean(StepsPerDayFull$steps)
meanStepsPerDayFull
medianStepsPerDayFull <- median(StepsPerDayFull$steps)
medianStepsPerDayFull

#(create a little table with the results)

StepsPerIntervalFull <- aggregate(steps ~ interval, ActivityDataFull, mean, na.rm = TRUE)
head(StepsPerIntervalFull)

# are there differences between weekends and weekdays?

ActivityDataFull$daytype <- chron::is.weekend(ActivityDataFull$date)

head(ActivityDataFull)
#replace FALSE with weekday to make it easy to read, and make sure the data types are right 
ActivityDataFull$daytype <- as.factor(ActivityDataFull$daytype)
ActivityDataFull$interval <- as.numeric(ActivityDataFull$interval)
ActivityDataFull$steps <- as.numeric(ActivityDataFull$steps)

ActivityDataFull$daytype <- revalue(ActivityDataFull$daytype, c("FALSE" = "Weekday", "TRUE" = "Weekend"))
head(ActivityDataFull)


#now do the aggregate thing by daytype and by interval

intervalsDayType <- aggregate(steps ~ daytype + interval, ActivityDataFull, FUN = mean)
head(intervalsDayType)


ggplot(intervalsDayType, aes(x=interval, y=steps)) +
        geom_line(color = "grey") + 
        facet_wrap(~ daytype, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()



knit2html("PA1_template.Rmd")
