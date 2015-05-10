data(mtcars)
head(mtcars)
class(mtcars)
library(MASS)
install.packages("corrplot")
library(corrplot)

?mtcars
sapply(mtcars,class)
str(mtcars)
##need to convert some classes from numeric to factors
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)

?boxplot()
boxplot(mpg ~ am,data=mtcars)

t.test(mpg~am,mtcars)$p.value ##Welch two sample t-test null hypothesis Miu0 = Miu1

fit <- lm(mpg ~ am, data =mtcars)
fitall <- lm(mpg ~ ., data=mtcars)
fiti <- lm(mpg ~ cyl*hp+cyl*wt+cyl*am+hp*wt+hp*am+wt*am,mtcars)
fiti1 <- lm(mpg ~ am+wt+hp+wt*am,mtcars)
fiti2 <- lm(mpg ~  wt*am,mtcars)
fit1 <- lm(mpg ~ qsec + wt*am, mtcars)
fit11 <- lm(mpg ~ qsec + am + wt:am, mtcars)
fit2 <- lm(mpg ~ qsec*wt + wt*am + qsec ,mtcars)
model <- step(fitall)
model2 <- stepAIC(fitall,direction="backward")
anova(fit,fitall,fiti,fiti2,fit1,model,model2)
model2[[1]]
mcor<-cor(mtcars)
round(mcor,digits=2)
pairs(mtcars, main = "mtcars data")
corrplot(mcor,addCoef.col="black", method="shade", shade.col=NA)
dev.off()
summary(model); plot(model)

summary(model2); plot(model2)
summary(fiti); plot(fiti)
summary(fiti1); plot(fiti1)
summary(fiti2); plot(fiti2)
summary(fit1); plot(fit1)
summary(fit11); plot(fit11)
summary(fit2); plot(fit2)
sapply(mtcars,unique)


pairs(mtcars, main = "mtcars data")
cor(mtcars)

coplot(mpg ~ disp  | as.factor(cyl), data = mtcars,
       panel = panel.smooth, rows = 1)

coplot(mpg ~ disp , data = mtcars,
       panel = panel.smooth, rows = 1)






fullModel <- lm(mpg ~ ., data=mtcars)
stepModel <- step(fullModel, k=log(nrow(mtcars)))
amIntWtModel<-lm(mpg ~ wt + qsec + am + wt:am, data=mtcars)
amModel<-lm(mpg ~ am, data=mtcars)
anova(amModel, stepModel, fullModel, amIntWtModel) 
confint(amIntWtModel) # results hidden
summary(amIntWtModel)$coef
$$mpg_e = `r round(summary(fit_final)$coef[1],3)` + `r round(summary(fit_final)$coef[2],3)`*am + `r round(summary(fit_final)$coef[3],3)`*hp+ `r round(summary(fit_final)$coef[4],3)`*wt+`r round(summary(fit_final)$coef[5],3)`*am*wt$$
plot(amIntWtModel)
  
  - `r round(summary(fit_final)$coef[5],3)` = `r round(summary(fit_final)$coef[2],3)' - r round(summary(fit_final)$coef[5],3)` higher mpg
* A manual car that weighs 5000lbs will have a lower mpg of `r round(summary(fit_final)$coef[2],3) - 5 * r round(summary(fit_final)$coef[5],3)`
* A manual car that weighs 5000lbs will have a lower mpg of `r round(summary(fit_final)$coef[2],3) - 5 * r round(summary(fit_final)$coef[5],3)`

fit1 <- lm(mpg ~.,data=mtcars)
fit2 <- lm(mpg ~disp+hp+drat+wt+qsec+vs+am+gear+carb,data=mtcars) #cyl dropped
fit3 <- lm(mpg ~disp+hp+drat+wt+qsec+am+gear+carb,data=mtcars)    #vs  dropped
fit4 <- lm(mpg ~disp+hp+drat+wt+qsec+am+gear,data=mtcars)         #carb dropped
fit5 <- lm(mpg ~disp+hp+drat+wt+qsec+am,data=mtcars)              #gear dropped
fit6 <- lm(mpg ~disp+hp+wt+qsec+am,data=mtcars)                   #drat dropped
fit7 <- lm(mpg ~hp+wt+qsec+am,data=mtcars)                        #hp   dropped
fit8 <- lm(mpg ~wt+qsec+am,data=mtcars) 
fit9 <- lm(mpg ~wt+qsec+am+am:wt,data=mtcars)
anova(fit8,fit9,fit1)
anova(fit1,fit8,fit9)$Pr[[2]]
