
getWeekNumber <- function(){
d<- seq(as.Date("2010-01-08"), as.Date("2013-12-31"), "week") 
k <- c(1 : length(d))
k <- k %% 52
k <- ifelse(k == 0, 52, k)
holiday_weeknumber = data.frame(d, k)
names(holiday_weeknumber) <- c('week_name','week_number')
return (holiday_weeknumber)
}