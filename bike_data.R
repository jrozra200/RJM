setwd("~/Google Drive/Grad School/Programming Practice/RJMetrics Interview/")

library(ggmap)
library(ggplot2)
library(plyr)

# QUESTIONS OF THE DATA #
# How many Bikes were stolen in the data set?
# What is the range of dates in the data?
# Is there missing data?
# What was the biggest/smallest value of bike stolen
# What was the average value of bike stolen?
# What day had the most/least count of bikes stolen? What is the average amount per day?
# How much $ in value of bikes stolen each day?
# What hour of the day are the most bikes stolen?
# What day of the week are the most bikes stolen?
# What month of the year are teh most bikes stolen?
# What can I see from the UCR codes?
# What can I see from the distrcit codes?
# Where were the most fines? Can I see them on a heat map?

## LOAD THE DATA
early_data <- read.csv("Bicycle_Thefts.csv")
later_data <- read.csv("Bicycle_Thefts_9_15_2014_-_2_8_2015.csv")

## CHECK OUT THE DATA
head(early_data)
head(later_data)

## SUMMARY OF THE DATA
summary(early_data)
summary(later_data)

## RENAME THINGS TO MAKE THEM EASIER TO DEAL WITH
names(early_data) <- c("DC_Number", "DC_Key", "Location", "Theft_Date", 
                       "Theft_Year", "District", "Stolen_Value", "Theft_Hour", 
                       "UCR", "lat", "lon", "Location_1")
names(later_data) <- c("FID", "DC_Number", "DC_Key", "Location", "Theft_Date", 
                       "Theft_Year", "District", "Stolen_Value", "Theft_Hour", 
                       "UCR", "Location_1", "Y", "GlobalID", "x2", "y2")

## CHANGE THE DATE FIELD INTO A DATE VARIABLE IN THE R CONTEXT
early_data$Theft_Date <- as.POSIXct(strptime(as.character(early_data$Theft_Date), 
                                             format = "%m/%d/%Y %I:%M:%S %p"))
later_data$Theft_Date <- as.POSIXct(strptime(as.character(later_data$Theft_Date), 
                                             format = "%m/%d/%Y %I:%M:%S %p"))

## MAKE LATITUDE AND LONGITUDE FIELDS IN THE later_data
later_latlong <- later_data$Location_1
later_latlong <- gsub(pattern = "\\(", replacement = "", x = later_latlong)
later_latlong <- gsub(pattern = "\\)", replacement = "", x = later_latlong)
later_latlong <- strsplit(later_latlong, ", ")
later_latlong <- ldply(later_latlong, rbind)
names(later_latlong) <- c("lon", "lat")
later_latlong$lon <- as.numeric(as.character(later_latlong$lon))
later_latlong$lat <- as.numeric(as.character(later_latlong$lat))

later_data <- cbind(later_data, later_latlong)

## GET A COMBINED DATA SET FOR EASE OF USE
combined_data <- rbind(early_data, later_data[, c(2:10, 17, 16, 11)])

par(mfrow = c(1,2))
hist(combined_data$Stolen_Value, xlab = "Value of Stolen Bike ($)", 
     main = "Histogram of Stolen Bike Value")
boxplot(combined_data$Stolen_Value, ylab = "Value of Stolen Bike ($)", 
        main = "Boxplot of Stolen Bike Value")
quantile(combined_data$Stolen_Value, c(0.1, 0.25, 0.5, 0.75, 0.9))

# What was the biggest/smallest value of bike stolen
maxvalue <- max(combined_data$Stolen_Value)
minvalue_not_zero <- min(combined_data$Stolen_Value[combined_data$Stolen_Value != 0])

par(mfrow = c(1,1))
boxplot(combined_data$Stolen_Value ~ combined_data$UCR)

count_by_UCR <- ddply(combined_data, .(UCR), summarize, count = length(UCR))

combined_data$month <- months(combined_data$Theft_Date)
combined_data$dow <- weekdays(combined_data$Theft_Date)

count_by_month <- ddply(combined_data, .(month), summarize, 
                        count615 = length(month[UCR == 615]), 
                        count625 = length(month[UCR == 625]),
                        count635 = length(month[UCR == 635]))
count_by_month$num <- c(4, 8, 12, 2, 1, 7, 6, 3, 5, 11, 10, 9)
count_by_month <- count_by_month[order(count_by_month$num), ]
count_by_month$num <- NULL
rownames(count_by_month) <- count_by_month$month
count_by_month$month <- NULL
count_by_month <- t(count_by_month)
rownames(count_by_month) <- c("615", "625", "635")
barplot(count_by_month, col = c("blue", "red", "yellow"))
legend(1, 1000, rownames(count_by_month), fill = c("blue", "red", "yellow"), 
       bty = "n")

count_by_weekday <- ddply(combined_data, .(dow), summarize, 
                          count615 = length(dow[UCR == 615]),
                          count625 = length(dow[UCR == 625]),
                          count635 = length(dow[UCR == 635]))
count_by_weekday$num <- c(6, 2, 7, 1, 5, 3, 4)
count_by_weekday <- count_by_weekday[order(count_by_weekday$num), ]
count_by_weekday$num <- NULL
rownames(count_by_weekday) <- count_by_weekday$dow
count_by_weekday$dow <- NULL
count_by_weekday <- t(count_by_weekday)
rownames(count_by_weekday) <- c("615", "625", "635")
barplot(count_by_weekday, col = c("blue", "red", "yellow"))
legend(0, 1250, rownames(count_by_weekday), fill = c("blue", "red", "yellow"), 
       bty = "n")

count_by_hour <- ddply(combined_data, .(Theft_Hour), summarize, 
                       count615 = length(Theft_Hour[UCR == 615]),
                       count625 = length(Theft_Hour[UCR == 625]),
                       count635 = length(Theft_Hour[UCR == 635]))
rownames(count_by_hour) <- count_by_hour$Theft_Hour
count_by_hour$Theft_Hour <- NULL
count_by_hour <- t(count_by_hour)
rownames(count_by_hour) <- c("615", "625", "635")
barplot(count_by_hour, col = c("blue", "red", "yellow"))
legend(0, 600, rownames(count_by_weekday), fill = c("blue", "red", "yellow"), 
       bty = "n")

phil2 <- get_map(location = "Philadelphia", maptype = "roadmap", zoom = 12)
map2 <- ggmap(phil2, extent = "device") + 
        geom_density2d(data = early_data, aes(x = lon, y = lat)) + 
        stat_density2d(data = early_data, aes(fill = ..level.., alpha = ..level..),
                       size = 0.01, geom = "polygon") + 
        scale_fill_gradient(low = "green", high = "red", guide = FALSE) + 
        scale_alpha(range = c(0, 0.3), guide = FALSE)

phil1 <- get_map(location = "Philadelphia", maptype = "roadmap", zoom = 13)
map1 <- ggmap(phil1, extent = "device") + 
        geom_density2d(data = early_data, aes(x = lon, y = lat)) + 
        stat_density2d(data = early_data, aes(fill = ..level.., alpha = ..level..),
                       size = 0.01, geom = "polygon") + 
        scale_fill_gradient(low = "green", high = "red", guide = FALSE) + 
        scale_alpha(range = c(0, 0.3), guide = FALSE)

count_by_district <- ddply(combined_data, .(District), summarize, 
                           count = length(District))




