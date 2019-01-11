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
