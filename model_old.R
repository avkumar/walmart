require(zoo)

setwd("C:/Users/arakumar/Desktop/RedHatSharedFolder/walmart_labs")


train <- read.csv('train_stack.csv', stringsAsFactors = FALSE)
is.na(train)
newtrain <- complete.cases(train)

test <- read.table('test_stack.xls', header = TRUE, sep = "\t", stringsAsFactors = FALSE)
test <- zoo(test)
test <- na.locf(test)
test <- as.data.frame(test)
test[is.na(test)] <- 0
str(test)



columns = c("Store", "Date", "Temperature", "Fuel_Price", "CPI",  "Unemployment", "IsHoliday", "Dept", "Type", "size" )
#fmla = as.formula ( log (5000 + Weekly_Sales) ~ log (4 + Temperature) + log (1 + Fuel_Price) + log (1 + CPI) + log (1 + Unemployment)+  log (1 + size )   + HOLIDAY + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11 + d12 + d13 + d14 + d15 + d16 + d17 + d18 + d19 + d20 + d21 + d22 + d23 + d24 + d25 + d26 + d27 + d28 + d29 + d30 + d31 + d32 + d33 + d34 + d35 + d36 + d37 + d38 + d39 + d40 + d41 + d42 + d43 + d44 + d45 + d46 + d47 + d48 + d49 + d50 + d51 + d52 + d53 + d54 + d55 + d56 + d57 + d58 + d59 + d60 + d61 + d62 + d63 + d64 + d65 + d66 + d67 + d68 + d69 + d70 + d71 + d72 + d73 + d74 + d75 + d76 + d77 + d78 + d79 + d80 + d81 + d82 + d83 + d84 + d85 + d86 + d87 + d88 + d89 + d90 + d91 + d92 + d93 + d94 + d95 + d96 + d97 + d98 + d99 + s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11 + s12 + s13 + s14 + s15 + s16 + s17 + s18 + s19 + s20 + s21 + s22 + s23 + s24 + s25 + s26 + s27 + s28 + s29 + s30 + s31 + s32 + s33 + s34 + s35 + s36 + s37 + s38 + s39 + s40 + s41 + s42 + s43 + s44 + s45 + typeA + typeB + typeC )

w <- ifelse(train$IsHoliday == TRUE, 5 ,0)

fmla = as.formula ( as.numeric( log (5000 + Weekly_Sales) ) ~ as.numeric( log (100 + Temperature) )+
                      as.numeric( log (1 + Fuel_Price) )+ as.numeric( log (1 + CPI)) + 
                      as.numeric( log (1 + Unemployment) )+  as.numeric( log (1 + size )) 
                    + as.numeric(  HOLIDAY ) + as.numeric(  d1 ) + as.numeric(  d2 ) +
                      as.numeric(  d3 ) + as.numeric(  d4 ) + as.numeric(  d5 ) + as.numeric(  d6 ) + 
                      as.numeric(  d7 ) + as.numeric(  d8 ) + as.numeric(  d9 ) + as.numeric(  d10 ) +
                      as.numeric(  d11 ) + as.numeric(  d12 ) + as.numeric(  d13 ) + as.numeric(  d14 ) +
                      as.numeric(  d15 ) + as.numeric(  d16 ) + as.numeric(  d17 ) + as.numeric(  d18 ) +
                      as.numeric(  d19 ) + as.numeric(  d20 ) + as.numeric(  d21 ) + as.numeric(  d22 ) + 
                      as.numeric(  d23 ) + as.numeric(  d24 ) + as.numeric(  d25 ) + as.numeric(  d26 ) +
                      as.numeric(  d27 ) + as.numeric(  d28 ) + as.numeric(  d29 ) + as.numeric(  d30 ) +
                      as.numeric(  d31 ) + as.numeric(  d32 ) + as.numeric(  d33 ) + as.numeric(  d34 ) +
                      as.numeric(  d35 ) + as.numeric(  d36 ) + as.numeric(  d37 ) + as.numeric(  d38 ) +
                      as.numeric(  d39 ) + as.numeric(  d40 ) + as.numeric(  d41 ) + as.numeric(  d42 ) +
                      as.numeric(  d43 ) + as.numeric(  d44 ) + as.numeric(  d45 ) + as.numeric(  d46 ) +
                      as.numeric(  d47 ) + as.numeric(  d48 ) + as.numeric(  d49 ) + as.numeric(  d50 ) + as.numeric(  d51 ) + as.numeric(  d52 ) + as.numeric(  d53 ) + as.numeric(  d54 ) + as.numeric(  d55 ) + as.numeric(  d56 ) + as.numeric(  d57 ) + as.numeric(  d58 ) + as.numeric(  d59 ) + as.numeric(  d60 ) + as.numeric(  d61 ) + as.numeric(  d62 ) + as.numeric(  d63 ) + as.numeric(  d64 ) + as.numeric(  d65 ) + as.numeric(  d66 ) + as.numeric(  d67 ) + as.numeric(  d68 ) + as.numeric(  d69 ) + as.numeric(  d70 ) + as.numeric(  d71 ) + as.numeric(  d72 ) + as.numeric(  d73 ) + as.numeric(  d74 ) + as.numeric(  d75 ) + as.numeric(  d76 ) + as.numeric(  d77 ) + as.numeric(  d78 ) + as.numeric(  d79 ) + as.numeric(  d80 ) + as.numeric(  d81 ) + as.numeric(  d82 ) + as.numeric(  d83 ) + as.numeric(  d84 ) + as.numeric(  d85 ) + as.numeric(  d86 ) + as.numeric(  d87 ) + as.numeric(  d88 ) + as.numeric(  d89 ) + as.numeric(  d90 ) + as.numeric(  d91 ) + as.numeric(  d92 ) + as.numeric(  d93 ) + as.numeric(  d94 ) + as.numeric(  d95 ) + as.numeric(  d96 ) + as.numeric(  d97 ) + as.numeric(  d98 ) + as.numeric(  d99 ) + as.numeric(  s1 ) + as.numeric(  s2 ) + as.numeric(  s3 ) + as.numeric(  s4 ) + as.numeric(  s5 ) + as.numeric(  s6 ) + as.numeric(  s7 ) + as.numeric(  s8 ) + as.numeric(  s9 ) + as.numeric(  s10 ) + as.numeric(  s11 ) + as.numeric(  s12 ) + as.numeric(  s13 ) + as.numeric(  s14 ) + as.numeric(  s15 ) + as.numeric(  s16 ) + as.numeric(  s17 ) + as.numeric(  s18 ) + as.numeric(  s19 ) + as.numeric(  s20 ) + as.numeric(  s21 ) + as.numeric(  s22 ) + as.numeric(  s23 ) + as.numeric(  s24 ) + as.numeric(  s25 ) + as.numeric(  s26 ) + as.numeric(  s27 ) + as.numeric(  s28 ) + as.numeric(  s29 ) + as.numeric(  s30 ) + as.numeric(  s31 ) + as.numeric(  s32 ) + as.numeric(  s33 ) + as.numeric(  s34 ) + as.numeric(  s35 ) + as.numeric(  s36 ) + as.numeric(  s37 ) + as.numeric(  s38 ) + as.numeric(  s39 ) + as.numeric(  s40 ) + as.numeric(  s41 ) + as.numeric(  s42 ) + as.numeric(  s43 ) + as.numeric(  s44 ) + as.numeric(  s45 ) + as.numeric(  typeA ) + as.numeric(  typeB ) + as.numeric(  typeC ))
lm.wt_fit <- lm (fmla, weights =w, data= train)
lm.fit <- lm (fmla,  data= train)
summary(lm.fit)
new <- predict(model, newdata = test)


 dat <- subset(trainMerged, Store ==1, select =c(Weekly_Sales,MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5, Date, IsHoliday))
 dat$y <- ifelse(dat$IsHoliday == TRUE, 1 ,0)
 par(mfrow=c(3,3))
 plot(dat$MarkDown1, col=(dat$y + 1))

 








out <- cbind(test['Store'], test['dept'], test['Date'], exp(new) - 5000)
write.csv(out , file = 'output.csv')

