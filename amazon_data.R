setwd("~/Google Drive/Grad School/Programming Practice/RJMetrics Interview/")

library(plyr)

amazonData <- read.csv("amazonReviews.csv")
head(amazonData)
summary(amazonData)

amazonData$time <- as.POSIXct(amazonData$time, origin = "1970-01-01")

########################################
## SUSPICIOUS PATTERN: SERIAL REVIERS ##
########################################

count_by_userID <- ddply(amazonData, .(userId), summarize, 
                         profilename_count = length(unique(profileName)), 
                         timerange = max(time) - min(time), 
                         total_reviews = length(userId), 
                         products_reviewed_count = length(unique(productId)),
                         avg_score = mean(score))

# SOME IDEAS - PEOPLE REVIEWING TOO OFTEN
## PEOPLE REVIEWING THE SAME PRODUCT OVER AND OVER
## PEOPLE ARE ONLY GIVING POSITIVE REVIEWS
## HOW LONG FOR THE GUY WHO DID 233 REVIEWS?
## MARKING THINGS HELPFUL TOO OFTEN


############################################
## SUSPICIOUS PATTERN: HELPFULNESS LEVELS ##
############################################

# Problem - 845 different helpfulness levels
numlevels <- length(unique(amazonData$helpfulness))

# Solution - change it to a percentage so it is normalized
new_level <- strsplit(as.character(amazonData$helpfulness), "/")
new_level <- ldply(new_level, rbind)
names(new_level) <- c("num", "denom")
new_level$num <- as.numeric(as.character(new_level$num))
new_level$denom <- as.numeric(as.character(new_level$denom))
amazonData$helpfulness1 <- new_level$num / new_level$denom
amazonData <- cbind(amazonData, new_level)

head(amazonData[!is.nan(amazonData$helpfulness1) & amazonData$helpfulness1 > 1, ])

length(unique(amazonData$score))
