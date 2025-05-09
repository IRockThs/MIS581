#Set working directory and activate necessary packages.
setwd("~/Homework/Capstone")
library(tidyverse)
library(scales)
library(rpart)
library(broom)
library(rpart.plot)



#Import data, dropping the End timestamp because it will not be used. Then, convert the start timestamp to a datetime format from character. Rename the variable Timestamp and drop the original column. Data is now ready for expansion and review.
boo <- read.csv("Transportation_Network_Providers_Trips.csv") %>%
  select(c(1,3:6))

boo$Trip.Start.Timestamp <- mdy_hms(boo$Trip.Start.Timestamp, tz=Sys.timezone())
boo$Timestamp <- boo$Trip.Start.Timestamp
boo <- boo %>%
  select(2:6)
gc()

summary(boo)

boo1 <- boo %>%
  na.exclude

boo1$IsWeekDay <- ifelse(wday(boo1$Timestamp, week_start = 1)<=4,1,0)
boo1$IsWeekEnd <- ifelse(wday(boo1$Timestamp, week_start = 1)>4,1,0)
boo1$Hour <- hour(boo1$Timestamp)

boo1$isMorning <- ifelse(boo1$Hour>4 & boo1$Hour<12,1,0)
boo1$isAfternoon <- ifelse(boo1$Hour>11 & boo1$Hour<17,1,0)
boo1$isEvening <- ifelse(boo1$Hour>16 & boo1$Hour<22,1,0)
boo1$isNight <- ifelse(boo1$isMorning == 0 & boo1$isAfternoon == 0 & boo1$isEvening ==0,1,0)
boo1$IsTipped <- ifelse(boo1$Tip>0,1,0)
boo1 <- boo1 %>%
  select(1:3,6,7,9:13) %>%
  filter(Trip.Seconds <= 3600) %>%
  filter(Trip.Miles <= 40) %>%
  filter(Fare <= 100)
rm(boo)
gc()

print(binom.test(43588639,170464959,0.16))

boo1 <- rowid_to_column(boo1,"ID")

train <- boo1 %>% slice_sample(prop = 0.3)
test <- anti_join(boo1,train, by = "ID")
gc()
write.csv(train, file = "Train data.csv")
write.csv(test, file = "Test data.csv")
write.csv(boo1, file = "Full data.csv")

rm(boo1)
rm(test)
gc()

#Due to the size of the dataset, it cripples the computer to run the entire thing. Once the training and test datasets are created, they are written to a csv file so they can be removed from the environment until needed again. 

#This is also why the gc() function is called constantly - clearing memory left over from processing those large datasets.

wdaymodel <- glm(IsTipped ~ IsWeekEnd, family = "binomial", data = train)

summary(wdaymodel)

test <- read.csv("Test data.csv", header = TRUE)
pred_test <- predict(wdaymodel, test, type = "response")
rm(train)
wdayconfuse <- table(test$IsTipped,pred_test)
wdayconfuse2 <- as.data.frame(wdayconfuse)
colnames(wdayconfuse2) <- c("Actual","Predicted","Count")


ggplot(wdayconfuse2, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "black", size = 8) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "IsWeekEnd Confusion Matrix", x = "Actual", y = "Predicted")

rm(test)
rm(wdaymodel)
rm(pred_test)
rm(wdayconfuse)
rm(wdayconfuse2)
gc()

train <- read.csv("Train data.csv", header = TRUE)
gc()


todmodel <- glm(IsTipped ~ isAfternoon + isEvening + isNight, family = "binomial", data = train)
todmodel2 <- glm(IsTipped ~ isMorning + isAfternoon + isEvening, family = "binomial", data = train)
todmodel3 <- glm(IsTipped ~ isMorning + isAfternoon + isNight, family = "binomial", data = train)
todmodel4 <- glm(IsTipped ~ isMorning + isEvening + isNight, family = "binomial", data = train)
summary(todmodel2)

test <- read.csv("Test data.csv", header = TRUE)
gc()
pred_test <- predict(todmodel2, test, type = "response")
gc()
todconfuse <- table(test$IsTipped,pred_test)
todconfuse2 <- as.data.frame(todconfuse)
gc()
colnames(todconfuse2) <- c("Actual", "Predicted", "Count")

ggplot(todconfuse2, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "black", size = 8) +
  scale_fill_gradient(low = "white", high = "navy") +
  labs(title = "Time Of Day Confusion Matrix", x = "Actual", y = "Predicted")

rm(test)
rm(todconfuse)
rm(todconfuse2)
rm(pred_test)
rm(todmodel2)
gc()

train <- read.csv("Train data.csv", header = TRUE)
gc()
durationmodel <- glm(IsTipped ~ Trip.Seconds, family = "binomial", data = train)
rm(train)
test <- read.csv("Test data.csv", header = TRUE)
gc()
pred_test <- predict(durationmodel, test, type = "response")
gc()
durationconfuse <- table(test$IsTipped,pred_test)
durationconfuse2 <- as.data.frame(durationconfuse)
gc()
colnames(durationconfuse2) <- c("Actual", "Predicted", "Count")

rm(test)
rm(durationconfuse)
rm(durationconfuse2)
rm(pred_test)
rm(durationmodel)
gc()

train <- read.csv("Train data.csv", header = TRUE)
gc()
distancemodel <- glm(IsTipped ~ Trip.Miles, family = "binomial", data = train)
rm(train)
test <- read.csv("Test data.csv", header = TRUE)
gc()
pred_test <- predict(distancemodel, test, type = "response")
gc()
distanceconfuse <- table(test$IsTipped,pred_test)
distanceconfuse2 <- as.data.frame(distanceconfuse)
gc()
colnames(distanceconfuse2) <- c("Actual", "Predicted", "Count")

rm(test)
rm(distanceconfuse)
rm(distanceconfuse2)
rm(pred_test)
rm(distancemodel)
gc()

train <- read.csv("Train data.csv", header = TRUE)
gc()
faremodel <- glm(IsTipped ~ Fare, family = "binomial", data = train)
rm(train)
test <- read.csv("Test data.csv", header = TRUE)
gc()
pred_test <- predict(faremodel, test, type = "response")
gc()
fareconfuse <- table(test$IsTipped,pred_test)
fareconfuse2 <- as.data.frame(fareconfuse)
gc()
colnames(fareconfuse2) <- c("Actual", "Predicted", "Count")
summary(faremodel)

rm(test)
rm(fareconfuse)
rm(fareconfuse2)
rm(pred_test)
rm(faremodel)
gc()

# CART section

train <- read.csv("Train data.csv", header = TRUE)
train <- train %>% select(3:12)
tree <- rpart(IsTipped ~. , data = train, method = "class")
summary(tree)
regtree <- rpart(IsTipped ~., data = train, cp = 0.001)
rpart.plot(regtree)
rm(train)
gc()


#The following portions are for descriptive visualizations and tests on the entire dataset. They have been prepared differently than the test dataset so they are separated down here.
#Add weekday, then create a list 1-7 to use as values for the labels in the histogram. Plot the frequency of trips on each weekday.
boo$weekday <- wday(boo$Timestamp)
Weeekday <- c(1:7) 

ggplot(boo, aes(x=weekday))+geom_histogram(fill = "lightblue", color = "darkblue")+labs(title="Frequency Histogram: Weekday (Monday = 1)", x="weekday", y = "Frequency") + scale_y_continuous(labels = label_comma()) + scale_x_continuous("Weekday",labels = as.character(Weeekday), breaks = Weeekday)

## For fares, there are a significant number of outliers on the high end (right tail skew). The 3rd quartile is at $20.00. The highest fare I have seen is just over $80.00. For the histogram I will go up to $100.00, but the actual analysis will likely cut at 77.5 due to the number of observations being below 100K for each value over that.

lean <- boo %>%
  select(3) %>%
  filter(Fare <=100)
ggplot(lean, aes(x=Fare))+geom_histogram(fill = "forestgreen", color = "darkblue") + labs(title = "Frequency Histogram: Fare Bins", x = "Fare", y = "Frequency") + scale_y_continuous(labels = label_comma())

rm(lean) #Removed to save memory
rm(Weeekday)

##Distance and Time will both be handled similarly to fares. The longest trips I've had are about 40 miles and 45 minutes (on Uber). 50 mile and 1 hour are reasonable points to cut for histogram plotting, since other values would be outliers.
Distance <- boo %>%
  select(2) %>%
  filter(Trip.Miles <=50)

ggplot(Distance, aes(x=Trip.Miles)) + geom_histogram(fill = "goldenrod", color = "darkblue", binwidth = 2) + labs(title="Frequency Histogram: Distance", x = "Distance", y = "Frequency") + scale_y_continuous(labels = label_comma())

rm(Distance)

Time = boo %>%
  select(1) %>%
  filter(Trip.Seconds <=3600)

mins <- c(1:60)

ggplot(Time, aes(x=Trip.Seconds))+geom_histogram(fill = "lavender", color = "darkblue", binwidth = 60)+labs(title="Frequency Histogram: Trip Duration", x="Time (seconds)", y = "Frequency") + scale_y_continuous(labels = label_comma())

rm(mins)
rm(Time)

#For the percent of rides tipped, the easiest way to find the answer is to count the number of tips (Tip>0) and then divide that by the observations minus the NA fares (which equals the NA tips). Formatted to 3 decimal places.
tips = boo %>%
  select(Tip) %>%
  filter(Tip >0)
tippercent <- 44823425/(174369516-560352)
print(percent(tippercent, accuracy=0.001))
rm(tips)



