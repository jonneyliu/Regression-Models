###Quiz 2

##Q1 & Q2
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
summary(lm(y~x))


##Q3
data(mtcars)
head(mtcars)
fit <- lm(mpg~wt,mtcars)
summary(fit)
int <- summary(fit)$coef[1,1] + c(1,-1) *qt(0.975,df = fit$df)*summary(fit)$coef[1,2] ##estimate Beta1 CI
slope <- summary(fit)$coef[2,1] + c(1,-1) *qt(0.975,df = fit$df)*summary(fit)$coef[2,2] ##estimate Beta2 CI
int
newdata = data.frame(wt = mean(mtcars$wt))
predict(fit,newdata, interval="confidence")

##Q4
?mtcars
# The estimated expected change in mpg per 1,000 lb increase in weight.

##Q5
# summary(fit)$coef
# summary(fit)$coef[1,1]
# summary(fit)$coef[2,1]
# xn <- 3
# yn <- summary(fit)$coef[1,1] + xn*summary(fit)$coef[2,1]
# yn
# 3*summary(fit)$coef[2,1] + c(1,-1) *qt(0.975,df = fit$df)*summary(fit)$coef[1,2] ##estimate Beta2 CI

fit <- lm(mpg ~ wt, data=mtcars)
newdata <- data.frame(wt = 3)
predict(fit, newdata, interval = "prediction")


##Q6
fit <- lm(mpg ~ wt, data=mtcars)
summary(fit)
fit2 <- lm(mpg ~ I(wt/2), data=mtcars)
summary(fit2)
int <- summary(fit2)$coef[1,1] + c(1,-1) *qt(0.975,df = fit2$df)*summary(fit2)$coef[1,2] ##estimate Beta1 CI
slope <- summary(fit2)$coef[2,1] + c(1,-1) *qt(0.975,df = fit2$df)*summary(fit2)$coef[2,2] ##estimate Beta2 CI


##Q7
##Q8
fit <- lm(mpg ~ wt, data=mtcars)
summary(fit)
fit2 <- lm(mpg ~ I(wt+10), data=mtcars)
summary(fit2)
# Y = b0 + b1 * X + e 
# b1 * (X + c) + (beta0 - beta1 * c)
# New intercept: beta0 - c*beta1


##Q9
fit <- lm(mpg ~ wt, data=mtcars)
fitResS<-fit$residuals^2
fit1 <- lm(mpg ~ 1, data = mtcars)
fitResS1 <- fit1$residuals^2
sum(fitResS)/sum(fitResS1)
