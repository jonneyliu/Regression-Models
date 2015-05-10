library(ggplot2)

fit_all <- lm(mpg~.,mtcars)
fit_cyl <- lm(mpg~cyl,mtcars)
fit_hp <- lm(mpg~hp,mtcars)
fit1 <- lm(mpg~am,mtcars)
fit1a <- lm(mpg~am+wt+hp,mtcars)
fit1b <- lm(mpg~am+wt+cyl,mtcars)
fit1c <- lm(mpg~am+wt+cyl+hp,mtcars)
fit2 <- lm(mpg~am*wt+wt*hp,mtcars)
fit2a <- lm(mpg~am+wt*hp,mtcars)
fit2b <- lm(mpg~am*wt+hp,mtcars)
fit3 <- lm(mpg~am*wt+wt*cyl,mtcars)

summary(fit_all)
summary(fit_cyl)
summary(fit_hp)
summary(fit1)
summary(fit1a)
summary(fit1b)
summary(fit1c)
summary(fit2)
summary(fit2a)
summary(fit2b)
summary(fit3)

plot(mtcars$cyl,mtcars$mpg)
plot(mtcars$cyl,mtcars$wt)
plot(mtcars$hp,mtcars$mpg)
plot(mtcars$hp,mtcars$cyl)

g1 <- ggplot(data=mtcars, aes(x=wt, y=mpg, colour=hp)) + geom_point() + stat_smooth(method="lm",col="Red") + labs(title="Fig 4.1: Interaction am vs wt") + theme_bw(col="Red") 
g1
summary(fit)

mtcars