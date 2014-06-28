source('weekOfDay.R')

setwd("/home/labuser/kaggle/walmart")



trainMerged <- read.csv("trainMerged.csv")
#train <- na.omit(train_data)
testMerged <- read.csv('testMerged.csv')
#newTrain <- train [as.Date(Date) > '2011-10-10', ]

newtrainMerged <- merge(x = trainMerged, y = getWeekNumber(), x.by = Date, y.by = week_name)

# Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
# Labor Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
# Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
# Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13

holidayDates_train <- unique(subset(trainMerged, IsHoliday==TRUE, select = Date))$Date
holidayDates_test <- unique(subset(testMerged, IsHoliday==TRUE, select = Date))$Date





# set.seed(1)
# train_rows <- sample(1:nrow(train), nrow(train)*0.6)
# test_rows <- (-train_rows)
# y <- train$Weekly_Sales[test_rows] 

dat <- subset(trainMerged, Store ==1 ,  select =c(Dept, Weekly_Sales, MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5, Date, IsHoliday))
dat$y <- ifelse(dat$IsHoliday == TRUE, 1 ,0)
par(mfrow=c(3,3))
plot(dat$MarkDown1, col=(dat$y + 1))
plot(dat$MarkDown1, col=(dat$y + 1))
plot(dat$MarkDown2, col=(dat$y + 1))
plot(dat$MarkDown3, col=(dat$y + 1))
plot(dat$MarkDown4, col=(dat$y + 1))
plot(dat$MarkDown5, col=(dat$y + 1))
plot(dat$Weekly_Sales, col=(dat$y + 1))


par(mfrow=c(3,3))
dat1 <- subset(dat, subset = (Dept ==1 & IsHoliday==TRUE) ,select =c(Dept, Weekly_Sales, MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5, Date, IsHoliday))
dat2 <- subset(dat, subset = (Dept ==2 & IsHoliday==TRUE) ,select =c(Dept, Weekly_Sales, MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5, Date, IsHoliday))
dat3 <- subset(dat, subset = (Dept ==3 & IsHoliday==TRUE) ,select =c(Dept, Weekly_Sales, MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5, Date, IsHoliday))
dat4 <- subset(dat, subset = (Dept ==4 & IsHoliday==TRUE) ,select =c(Dept, Weekly_Sales, MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5, Date, IsHoliday))
dat5 <- subset(dat, subset = (Dept ==5 & IsHoliday==TRUE) ,select =c(Dept, Weekly_Sales, MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5, Date, IsHoliday))
dat6 <- subset(dat, subset = (Dept ==6 & IsHoliday==TRUE) ,select =c(Dept, Weekly_Sales, MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5, Date, IsHoliday))
dat7 <- subset(dat, subset = (Dept ==7 & IsHoliday==TRUE) ,select =c(Dept, Weekly_Sales, MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5, Date, IsHoliday))
dat8 <- subset(dat, subset = (Dept ==8 & IsHoliday==TRUE) ,select =c(Dept, Weekly_Sales, MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5, Date, IsHoliday))
dat9 <- subset(dat, subset = (Dept ==9 & IsHoliday==TRUE) ,select =c(Dept, Weekly_Sales, MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5, Date, IsHoliday))


dat1$y <- ifelse(dat1$IsHoliday == TRUE, 1 ,0)
dat2$y <- ifelse(dat2$IsHoliday == TRUE, 1 ,0)
dat3$y <- ifelse(dat3$IsHoliday == TRUE, 1 ,0)
dat4$y <- ifelse(dat4$IsHoliday == TRUE, 1 ,0)
dat5$y <- ifelse(dat5$IsHoliday == TRUE, 1 ,0)
dat6$y <- ifelse(dat6$IsHoliday == TRUE, 1 ,0)
dat7$y <- ifelse(dat7$IsHoliday == TRUE, 1 ,0)
dat8$y <- ifelse(dat8$IsHoliday == TRUE, 1 ,0)
dat9$y <- ifelse(dat9$IsHoliday == TRUE, 1 ,0)


plot(dat1$Weekly_Sales, col=(dat1$y + 1))
plot(dat2$Weekly_Sales, col=(dat2$y + 1))
plot(dat3$Weekly_Sales, col=(dat3$y + 1))
plot(dat4$Weekly_Sales, col=(dat4$y + 1))
plot(dat5$Weekly_Sales, col=(dat5$y + 1))
plot(dat6$Weekly_Sales, col=(dat6$y + 1))
plot(dat7$Weekly_Sales, col=(dat7$y + 1))
plot(dat8$Weekly_Sales, col=(dat8$y + 1))
plot(dat9$Weekly_Sales, col=(dat9$y + 1))


plot(dat1$MarkDown1, col=(dat1$y + 1))
plot(dat1$MarkDown2, col=(dat1$y + 1))
plot(dat1$MarkDown3, col=(dat1$y + 1))
plot(dat1$MarkDown4, col=(dat1$y + 1))
plot(dat1$MarkDown5, col=(dat1$y + 1))
plot(dat1$Weekly_Sales, col=(dat1$y + 1))










train <- subset(trainMerged, select = -Date)
# train <- train[train_rows, ]
# test <- train[test_rows, ]

train$Store = as.factor(train$Store)
train$Dept = as.factor(train$Dept)
train$Type = as.factor(train$Type)

#train <- na.omit(train)
w <- ifelse(train$IsHoliday == TRUE, 5 ,0)

lm.wt_fit <- lm (Weekly_Sales ~ .,  data = train)
lm.fit <- lm (Weekly_Sales ~ .,  data = train)

test  <- testMerged
test[is.na(test)] <- 0
test$Store = as.factor(test$Store)
test$Dept = as.factor(test$Dept)
test$Type = as.factor(test$Type)
lm.pred <- predict.lm(lm.fit, newdata = test)

write.csv(lm.pred , file = 'output.csv')


mean((lm.pred - y)^2)

