## Coursera course:  reproducible research 
## Assignment week 1

library(ggplot2)
library(dplyr)
library(Hmisc)
library(lattice)


#function to update status messages to the console
comm <- function(...) cat("[run_analysis.R]", ..., "\n")

## Loading and preprocessing the data
mydatafile <- file.path(getwd(),"reproducible-research/week1/RepData_PeerAssessment1/activity.csv")
comm("read data from activity.csv") ## Load activity from csv file

# Load the data (i.e. read.csv())
mydata <- read.csv (mydatafile, header=TRUE, na.strings="NA" )
# Process/transform the data (if necessary) into a format suitable for your analysis


## What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
mydatanona <- na.omit(mydata)
# 1. Calculate the total number of steps taken per day
mytotalstepsperday <- mydatanona %>% group_by(date) %>% summarise(avg = sum(steps)) #summarize data
# 2. If you do not understand the difference between a histogram and a barplot, 
# research the difference between them. Make a histogram of the total number of steps taken each day
hist(mytotalstepsperday$avg, breaks=30, xlab="total steps per day", main="")
# 3. Calculate and report the mean and median of the total number of steps taken per day
mean(mytotalstepsperday$avg) # mean of the total steps per day
# [1] 10766.19
# dates that have no valid data at all (like 2012-10-01) have not been included in the calculation
median(mytotalstepsperday$avg) # median of the total steps per day 
# [1] 10765

## What is the average daily activity pattern? 
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average 
# number of steps taken, averaged across all days (y-axis)
myavgstepsperinterval <- mydatanona %>% group_by(interval) %>% summarise(avg = mean(steps)) #summarize data by average # steps
plot(myavgstepsperinterval, type="l", ylab="average of total steps per interval")
# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum 
# number of steps?
mytotalstepsperinterval <- mydatanona %>% group_by(interval) %>% summarise(avg = sum(steps)) #summarize data by sum # steps
subset(mytotalstepsperinterval, avg==max(mytotalstepsperinterval))$interval #print the 5 minute interval with the max number of steps
# [1] 835

## Imputing missing values
# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
mydata <- read.csv (mydatafile, header=TRUE, na.strings="NA" )
table(is.na(mydata$steps))
# FALSE  TRUE 
# 15264  2304
# 2304 missing values

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use 
# the mean/median for that day, or the mean for that 5-minute interval, etc.

# Strategy: myavgstepsperinterval has an average # steps for each interval across all days. 
# I will join myavgstepsperinterval to the data and then replace the missing NA's 
# myfixeddata is a copy of the original data set

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
myfixeddata<- mydata
myfixeddata <- myfixeddata %>% left_join(myavgstepsperinterval, by = "interval")
myfixeddata$steps[is.na(myfixeddata$steps)] <- myfixeddata$avg
table(is.na(myfixeddata$steps))
# FALSE 
# 17568 

# 4. Make a histogram of the total number of steps taken each day and Calculate 
# and report the mean and median total number of steps taken per day. Do these 
# values differ from the estimates from the first part of the assignment? What 
# is the impact of imputing missing data on the estimates of the total daily 
# number of steps?

par(mfrow=c(1,2))
mytotalstepsperdayfixed  <- myfixeddata  %>% group_by(date) %>% summarise(avg = sum(steps)) #summarize data
hist(mytotalstepsperday$avg, breaks = 30, ylim= c(0,20))
hist(mytotalstepsperdayfixed$avg, breaks = 30)
mean(mytotalstepsperdayfixed$avg) # mean of the total steps per day
# [1] 10766.19
median(mytotalstepsperdayfixed$avg) # median of the total steps per day 
# [1] 10766.19

## Are there differences in activity patterns between weekdays and weekends?
# 1. Create a new factor variable in the dataset with two levels - "weekday" and 
# "weekend" indicating whether a given date is a weekday or weekend day.
myfixeddata$date <- as.Date(myfixeddata$date, format = "%Y-%m-%d")
myfixeddata$weekday <- weekdays(myfixeddata$date)
myfixeddata$blnweekday <- ifelse((weekdays(myfixeddata$date) %in% c("Saturday","Sunday")), "weekend","weekday")
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, averaged 
# across all weekday days or weekend days (y-axis). See the README file in the 
# GitHub repository to see an example of what this plot should look like using 
# simulated data.
myavgstepsperintervalonweekday <- myfixeddata  %>% group_by(blnweekday, interval) %>% summarise(avg = mean(steps)) #summarize data
xyplot(avg~interval | factor(blnweekday), data=myavgstepsperintervalonweekday, 
       main="average steps per interval", xlab="Interval",  ylab="Number of steps",layout=c(1,2),type="l")
