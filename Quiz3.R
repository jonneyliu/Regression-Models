##Regression Models Quiz 3

##Q1
data(mtcars)
head(mtcars)

fit <- lm(mpg ~ as.factor(cyl) + wt,mtcars)
fit <- lm(mpg ~ cyl + wt,mtcars)
summary(fit)
4*fit$coef[2]
str(mtcars)


##Q2
fit <- lm(mpg ~ as.factor(cyl) + wt,mtcars)

summary(fit)
fit1 <- lm(mpg ~ as.factor(cyl),mtcars)
summary(fit1)


##Q3

Question 3
Consider the mtcars data set. Fit a model with mpg as the outcome that considers number of cylinders as a 
factor variable and weight as confounder. Now fit a second model with mpg as the outcome model that considers the 
interaction between number of cylinders (as a factor variable) and weight. Give the P-value for the likelihood ratio test 
comparing the two models and suggest a model using 0.05 as a type I error rate significance benchmark.

fit <- lm(mpg ~ as.factor(cyl) + wt,mtcars)
summary(fit)
fit3 <- lm(mpg ~ as.factor(cyl) + wt + as.factor(cyl)*wt,mtcars)
summary(fit3)
The P-value is larger than 0.05. So, according to our criterion, we would fail to reject, which suggests that the interaction 
terms may not be necessary.



# Question 4
Consider the mtcars data set. Fit a model with mpg as the outcome that includes number of cylinders as a factor variable
and weight inlcuded in the model as

lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

How is the wt coefficient interpretted?
The estimated expected change in MPG per one ton increase in weight for a specific number of cylinders (4, 6, 8).



# Question 5
Consider the following data set

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y~x)
head(hatvalues(fit))


# Question 6
# Consider the following data set


x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
Give the slope dfbeta for the point with the highest hat value.

fit <- lm(y~x)
dfbetas(fit)


# Question 7
# Consider a regression relationship between Y and X with and without adjustment for a third variable Z. 
# Which of the following is true about comparing the regression coefficient between Y and X with and without adjustment for Z.
