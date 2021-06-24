#Loading and preprocessing the data
dat <- read.csv("activity.csv")
dat$date <- as.POSIXct(dat$date, "%Y/%m/%d")
dat$day <- weekdays(dat$date)


#What is mean total number of steps taken per day?
datdf <- with(dat, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(datdf) <- c("date", "total")
with(datdf, hist(total,
     main = "Total number of steps taken per day",
     xlab = "Total steps taken per day",
     col = "blue",
     ylim = c(0,20),
     breaks = seq(0,25000, by=2500)))

#Calculate and report the mean and median total number of steps taken per day
mean(datdf$total)
median(datdf$total)


#What is the average daily activity pattern?
library(tidyverse)
datdf2 <- dat %>% 
  group_by(interval) %>%
  drop_na(steps) %>%
  summarise(average = mean(steps))

with(datdf2, plot(interval, average, type = "l", lwd = 2,
                  xlab="Interval",
                  ylab="Average number of steps",
                  main="Average daily activity pattern"))

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
datdf2$interval[which.max(datdf2$average)]


#Imputing missing values
#total number of missing values
sum(is.na(dat$steps))

#Devise a strategy for filling in all of the missing values in the dataset.
filled_steps <- datdf2$average[match(dat$interval, datdf2$interval)]

#Create a new dataset that is equal to the original dataset but with the missing data filled in
filled_df <- transform(dat, steps = ifelse(is.na(dat$steps), yes = filled_steps, no = dat$steps))

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
datdf3 <- aggregate(steps ~ date, filled_df, sum)
names(datdf3) <- c("date", "steps")
with(datdf3, hist(steps,
                 main = "Total number of steps taken per day",
                 xlab = "Total steps taken per day",
                 col = "blue",
                 ylim = c(0,20),
                 breaks = seq(0,25000, by=2500)))

mean(datdf3$steps)
median(datdf3$steps)


#Are there differences in activity patterns between weekdays and weekends?
dat$daytype <- sapply(dat$date, function(x) {
  if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
    {y <- "Weekend"}
      else {y <- "Weekday"}
    y
  })

#Make a panel plot containing a time series plot
dat_by_day <-  aggregate(steps ~ interval + daytype, dat, mean, na.rm = TRUE)

ggplot(dat_by_day, aes(x = interval , y = steps, color = daytype)) + 
  geom_line() + ggtitle("Average Daily Steps by Day Type") + 
  xlab("Interval") + 
  ylab("Average Number of Steps") +
  facet_wrap(~daytype, ncol = 1, nrow=2)


