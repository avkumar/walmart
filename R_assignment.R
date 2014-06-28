
setwd("C:/Users/arakumar/Desktop/RedHatSharedFolder/walmart_labs")
library('ISLR')
college=read.csv ("College.csv")
rownames = college[,1]
college =college[, -1]
fix(college)
 Elite =rep ("No",nrow(college ))
 Elite [college$Top10perc >50]=" Yes"
 Elite =as.factor (Elite)
 college =data.frame(college ,Elite)
par(mfrow=c(2,2))
plot(Elite, Outstate)

auto = read.csv("Auto.csv")
#fix(auto)
summary(auto)
attach(college)
unique(unlist(cylinders, use.names = FALSE))
attach(auto)
lm.fit3 = lm( mpg~poly(as.numeric(horsepower), 5), data = auto)

Boston = read.csv()

library(MASS)
attach(Boston)
colnames(Boston)


set.seed(1)
x = rnorm(100)

 
eps <- rnorm(100, 0, 0.25)
y <-  -1 + 0.5*x + x^2 +  eps
lm.1 <- lm(y~x)
lm.2 <- lm(y~poly(x,2,raw=TRUE))
summary(lm.2)


library(vars)

plot (x, y)
abline(lm(y ~x), col ='green')
abline(a = -1, b = 0.5 ,col = 'red')
summary(lm(y~poly(x,20)))
summary(lm(y~x))

ls(lm.1)

lm.fit <- lm(y~poly(x,1))
sum(lm.fit$coefficients)
summary(lm(y~I(x^30)))
summary(lm(y~poly(x,20,raw=FALSE)))



set.seed (1)
 x1=runif (100)
 x2 =0.5* x1+rnorm (100) /10
 y=2+2* x1 +0.3* x2+rnorm (100)
cor(x1, x2)


x1=c(x1 , 0.1)
 x2=c(x2 , 0.8)
 y=c(y,6)






x <- c(1:10)
y <- c(11:20)
lm ( y ~ x + 0)
lm(x ~ y + 0)

Boston
attach(Boston)
pairs(Boston)

plot(nox, crim, data = Boston)
lm(crim ~ zn, data = Boston)
summary(lm(crim ~., data = Boston))

zn            0.044855   0.018734   2.394 0.017025 *  
  indus        -0.063855   0.083407  -0.766 0.444294    
chas         -0.749134   1.180147  -0.635 0.525867    
nox         -10.313535   5.275536  -1.955 0.051152 .  
rm            0.430131   0.612830   0.702 0.483089    
age           0.001452   0.017925   0.081 0.935488    
dis          -0.987176   0.281817  -3.503 0.000502 ***
  rad           0.588209   0.088049   6.680 6.46e-11 ***
  tax          -0.003780   0.005156  -0.733 0.463793    
ptratio      -0.271081   0.186450  -1.454 0.146611    
black        -0.007538   0.003673  -2.052 0.040702 *  
  lstat         0.126211   0.075725   1.667 0.096208 .  
medv 



lm(crim ~ zn, data = Boston)$coefficient['zn']

y = sin(x)
plot(y, x)
?plot

x <- c(1)
plot(x, y = 0, to = 1, from = y, xlim = NULL, ylab = NULL)



x = seq(0, 2*pi, length = 2000)

#y = sin(x)
x1 <- 2*pi*runif(1)
x2 <-  2*pi*runif(1)
df1 <- as.data.frame(c(x1,x2))
df2 <- as.data.frame( c(sin(x1) , sin(x2)))
df <- cbind(df1,df2)
names(df) <- c('x', 'y')

lm1.fit <- lm(y ~ x, data = df)
abline(lm1.fit)
lm2.fit <- lm(y ~ 1, data = df)
abline(lm2.fit)

summary(lm.fit)

plot(x)
x1
runif(x, 0, 2000)
library(MASS)




