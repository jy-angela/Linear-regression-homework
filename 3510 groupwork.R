### graphical display of the observed data
tf_raw<-read.table("traffic.txt", header=FALSE)
tf_raw

tf_raw$V5 <- ifelse(tf_raw$V5 == 1, 1, 0)

tf <- data.frame(
  y = tf_raw$V1,
  x1 = tf_raw$V2,
  x2 = tf_raw$V3,
  x3 = tf_raw$V4,  
  x4 = tf_raw$V5  
)

plot(tf)


### Fit a multiple linear regression model
mlr<-lm(y~x1+x2+x3+x4, data = tf)
summary(mlr)
mlrs<-summary(mlr)

## comment
#1. From the Adjusted R-squared which is 0.7442, which is better than R-squared, 
#74.42% of variation can be explained by taking predictor variables into account
#Hence, MLR is relatively fitted. 
#2. F statistic is 88.29, which is very high and p value is much smaller alpha, 
#As the F ratio is very high, we can reject the null hypothesis test and predict that 
#MLR is very significant
#3. From the t statistic, we can see that x3 is not significant as the p-value is much larger.


### Normality checking
qqnorm(residuals(mlr), ylab='Residuals')
qqline(residuals(mlr))
## comment
# The distribution of error terms is not normal distribution and they are not evenly
# distributed. When theretical quantiles less than -1, the residuals are under the qqline, 
# and for theretical quantiles more than roughlyy 1.3, the residuals are on the upper of the qqline.
# for data points larger than 1.3, there are largerly apart from the linearilty
# Hence, it doesn't follow a normal distribution. 



### Draw some plots of residuals
par(mfrow=c(1,3))
plot(residuals(mlr),ylab='Residuals',xlab='Time')
plot(residuals(mlr), fitted(mlr), ylab='Residuals', xlab='Fitted values')
plot(residuals(mlr),tf$x1,ylab='Residuals',xlab='x1')
plot(residuals(mlr),tf$x2,ylab='Residuals',xlab='x2')
plot(residuals(mlr),tf$x3,ylab='Residuals',xlab='x3')
plot(residuals(mlr),tf$x4,ylab='Residuals',xlab='x4')
par(mfrow=c(1,1))


# 1. When we rearrange the data in time, we can see that residuals are time dependent. At time
# 60, there is a large spread for residuals compared to nearbys. Hence, the heteroscedasticity
# illustrates the lack of independence
# 2. The MLR is not linear as the errors' distribution are not random as they are largely above 0. 
# Also, the variance of error terms are not constant, as the spread of these errors are not even
# Also, 

### Durbin-Waston tests
library(lmtest)
dwtest(y ~ x1+x2+x3+x4, data=tf)

# A DW statistic close to 2 suggests that there is no autocorrelation in the residuals (null hypothesis). 

# p-value: The small p-value (3.101e-05) suggests that the DW statistic is significantly different from the 
# expected value under the assumption of no autocorrelation. In this case, it indicates that there is strong 
# evidence to reject the null hypothesis in favor of the alternative hypothesis (positive autocorrelation).



### Some F-tests.
# From previous t test, we can see that x3 variable is not so significant
# to establish the F-tests, we can assume H0: beta_3 = 0 (x3 not significant)

mlr1 <- lm(y ~ x1+x2+x4,data=tf)
anova(mlr1,mlr)
# as we see that p-value is really large, so we cannot reject the null-hypothesis 

# we can test whether there are some complicated relationship
# H0: beta_2 = 90 * beta_3

mlr2 <- lm(y ~ I(90*x2+x3)+x1+x4,data=tf) 
summary(mlr2)
anova(mlr2,mlr)

#also, we can test the relationship 

#comment, hence, we cannot reject H0 at the level of 

#test whether coefficients are constant
# From the results of MLR, we get beta_2=9.158e+03, 
#we test H0: beta_2 = 8500
mlr3 <- lm(y~offset(8500 * x2) + x1 + x3 + x4, data = tf)
summary(mlr3)
anova(mlr3,mlr)

# hence, we cannot reject H0 at 0.05 level



### Prediction
con <- c(1,50000, 3, 60, 0)
lhat <- sum(con*coef(mlr))
lhat
#lhat is 56327.66

t05 <- qt(0.975,116)

bm <- t05*mlrs$sigma*sqrt(con%*%mlrs$cov.unscaled%*%con)
c(lhat-bm,lhat+bm)

c3 <- 1
bm <- t05*mlrs$sigma*sqrt(con%*%mlrs$cov.unscaled%*%con+c3) 
c(lhat-bm,lhat+bm)
con <- data.frame(x1=50000,x2=3,x3=60, x4=0) 
predict(mlr,con,interval='confidence',level=0.95) 
predict(mlr,con,interval='prediction',level=0.95)


### As we know that X3 variable is not so significant
### Prediction
con1 <- c(1,50000, 3, 0)
lhat1 <- sum(con*coef(mlr))
lhat1


t05 <- qt(0.975,116)

bm <- t05*mlrs$sigma*sqrt(con%*%mlrs$cov.unscaled%*%con)
c(lhat-bm,lhat+bm)

c3 <- 1
bm <- t05*mlrs$sigma*sqrt(con%*%mlrs$cov.unscaled%*%con+c3) 
c(lhat-bm,lhat+bm)
con <- data.frame(x1=50000,x2=3,x3=60, x4=0) 
predict(mlr,con,interval='confidence',level=0.95) 
predict(mlr,con,interval='prediction',level=0.95)


#### As we know the x3 is not significant, when we remove this variable and find the mlr model

m1lr<-lm(y~x1+x2+x4, data = tf)
summary(m1lr)
mlrs<-summary(m1lr)



