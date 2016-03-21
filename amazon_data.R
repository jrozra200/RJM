library(plyr)

amazonData <- read.csv("amazonReviews.csv")
head(amazonData)
summary(amazonData)

## CHANGE THE TIME INTO HUMAN READABLE
amazonData$time <- as.Date.POSIXct(amazonData$time, origin = "1970-01-01")

## SPACE IN THE userId FIELD:
amazonData[amazonData$userId == "AY12DBB0U420B", ]      ## GIVES NOTHING
amazonData[amazonData$userId == " AY12DBB0U420B", ]     ## GIVES SOMETHING
amazonData$userId <- gsub(pattern = " ", replacement = "", x = amazonData$userId)

## HELPFULNESS SCORE IS A STRING
new_level <- strsplit(as.character(amazonData$helpfulness), "/")
new_level <- ldply(new_level, rbind)
names(new_level) <- c("num_times_helpful", "num_people_voting")
new_level$num_times_helpful <- as.numeric(as.character(new_level$num_times_helpful))
new_level$num_people_voting <- as.numeric(as.character(new_level$num_people_voting))
amazonData$helpfulness1 <- new_level$num_times_helpful / new_level$num_people_voting
amazonData <- cbind(amazonData, new_level)

## HELPFULNESS SCORES THAT ARE NaN OR GREATER THAN 1
amazonData$helpfulness1[is.nan(amazonData$helpfulness1)] <- 0
amazonData$helpfulness1[amazonData$helpfulness1 > 1] <- 1

## PEOPLE REVIEWING THE SAME PRODUCT MULTIPLE TIMES
count_by_userID <- ddply(amazonData, .(userId), summarize, 
                         profilename_count = length(unique(profileName)), 
                         timerange = max(time) - min(time), 
                         total_reviews = length(userId), 
                         products_reviewed_count = length(unique(productId)),
                         avg_score = mean(score))

dim(count_by_userID[count_by_userID$total_reviews > count_by_userID$products_reviewed_count, ])
head(count_by_userID[count_by_userID$total_reviews > count_by_userID$products_reviewed_count, ])