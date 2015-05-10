####Week 2 notes

n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))
sum(ey * ex) / sum(ex ^ 2)

coef(lm(ey ~ ex - 1))

coef(lm(y ~ x + x2 + x3)) 
require(datasets); data(swiss); ?swiss
summary(lm(Fertility ~ . , data = swiss))


n <- 100; x2 <- 1 : n; x1 <- .01 * x2 + runif(n, -.1, .1); y = -x1 + x2 + rnorm(n, sd = .01)
summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef


##Examples
require(datasets); data(swiss); ?swiss
require(datasets); data(swiss); require(GGally); require(ggplot2)
g = ggpairs(swiss, lower = list(continuous = "smooth"),params = c(method = "loess"))
g

Calling lm

summary(lm(Fertility ~ . , data = swiss))

summary(lm(Fertility ~ . , data = swiss))$coefficients

summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients

How can adjustment reverse the sign of an effect? Let's try a simulation.

n <- 100; x2 <- 1 : n; x1 <- .01 * x2 + runif(n, -.1, .1); y = -x1 + x2 + rnorm(n, sd = .01)
summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef

plot(x1,x2)

Hard coding the dummy variables

summary(lm(count ~ 
             I(1 * (spray == 'B')) + I(1 * (spray == 'C')) + 
             I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
             I(1 * (spray == 'F'))
           , data = InsectSprays))$coef

##Can use as.factor instead

Reordering the levels

spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data = InsectSprays))$coef

### Hunger example in Swirl()
lmM <-lm(Numeric[Sex=="Male"] ~ Year[Sex=="Male"],hunger)
lmInter <- lm(Numeric ~ Year+Sex + Sex*Year)
