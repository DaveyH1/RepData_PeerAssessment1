library(knitr)
opts

dir()
unzip("activity.zip")
act <- read.csv("activity.csv")


str(act)


?aggregate

stepDate <- aggregate(act$steps, by = list(date = act$date), sum)


hist(stepDate$x, breaks = 61, main = "Total Steps by Day", xlab = "Steps")



library(ggplot2)
ggplot(stepDate, aes(x  = x)) +
        geom_histogram()


meanStep <- round(mean(stepDate$x, na.rm = T),3)
medianSteps <- round(median(stepDate$x, na.rm = T),3)

class(avgSteps)

avgSteps <- aggregate(act$steps, by = list(interval = act$interval), mean, na.rm = T)


ggplot(avgSteps, aes(x = interval, y = x, group = 1)) +
        geom_line()


avgSteps[which(avgSteps$x == max(avgSteps$x)),]$interval

# Get the missing data
missingData <- act[which(is.na(act$steps) == TRUE),]

# Get the data with no missing values
correctData <- act[!is.na(act$steps), ]

# Create imputed by calculating the mean of the intervals
imputed <- aggregate(correctData$steps, by = list(interval = correctData$interval), mean)

# add the imputed column to the end of the missing data to created fixed data
fixedData <- merge(x = missingData, y = imputed, by = "interval")

# remove the NA values
fixedData <- fixedData[, -2]

# Reorder the data frame to allow binding
fixedData <- fixedData[, c(3,2,1)]

# Rename x to steps to allow binding
colnames(fixedData)[1] <- "steps"

# Create the new table with no NA's
newAct <- rbind(correctData, fixedData)



stepDateNew <- aggregate(newAct$steps, by = list(date = newAct$date), sum)


median(stepDateNew$x)


str(newAct)

# Get the weekedays from the date
newAct$weekDay <- weekdays(as.Date(newAct$date, format = "%Y-%m-%d"), abbr = T)

# Use gsub to replace Sat and Sun with Weekends
newAct$weekDay <- gsub(paste(unlist(list("Sat","Sun")), collapse = "|"),"Weekend", newAct$weekDay)

# Now Repeat with other days for Weekday
newAct$weekDay <- gsub(paste(unlist(list("Mon","Tue","Wed","Thu","Fri")), collapse = "|"),"Weekday", newAct$weekDay)

newAct$weekDay <- as.factor(newAct$weekDay)
unique(newAct$weekDay)

str(newAct)


weekDayAvgSteps <- aggregate(newAct$steps, by = list(interval = newAct$interval, day = newAct$weekDay), mean)


qplot(interval, x, data = weekDayAvgSteps, geom = "line") +
        facet_wrap(~ day, nrow = 2) +
        xlab("Interval") +
        ylab("Number of Steps (Mean)")

