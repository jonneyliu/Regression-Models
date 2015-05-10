###Swirl regression models outlier


?influence.measures
Do ?influence.measures to see the full suite of influence measures in stats. The measures include
rstandard - standardized residuals, residuals divided by their standard deviations)
rstudent - standardized residuals, residuals divided by their standard deviations, where the ith data point 
was deleted in the calculation of the standard deviation for the residual to follow a t distribution
hatvalues - measures of leverage
dffits - change in the predicted response when the $i^{th}$ point is deleted in fitting the model.
dfbetas - change in individual coefficients when the $i^{th}$ point is deleted in fitting the model.
cooks.distance - overall change in teh coefficients when the $i^{th}$ point is deleted.
resid - returns the ordinary residuals
resid(fit) / (1 - hatvalues(fit)) where fit is the linear model fit returns the PRESS residuals, i.e. the leave 
one out cross validation residuals - the difference in the response and the predicted response at data point $i$,
where it was not included in the model fitting.


fit <- lm(y ~ x, out2) 
plot(fit, which=1) 

##remove outlier on first column
fitno <- lm(y ~ x, out2[-1,]) 

##dfbeta function returns array of change in coefficient when particular sample in omitted
head(dfbeta(fit))
resno <- out2[1, "y"] - predict(fitno, out2[1,])

##"Influence" ====

1-resid(fit)[1]/resno 

##hatvalues calculates influence for every sample
head(hatvalues(fit))

sigma <- sqrt(deviance(fit)/df.residual(fit))
rstd <- resid(fit)/(sigma * sqrt(1-hatvalues(fit))) 
##instead can yse rstandard function
head(cbind(rstd, rstandard(fit))) 

##plot 
plot(fit, which =3)

##studentized residuals
sigma1 <- sqrt(deviance(fitno)/df.residual(fitno))
resid(fit)[1]/(sigma1*sqrt(1-hatvalues(fit)[1]))
##equivalent to...
rstudent(fit)[1] 


###last one: Cook's distance
step1: calculate difference between predicted values of fit & fitno
dy <-predict(fitno,out2)-predict(fit,out2)
##cook's distance:
sum(dy^2)/(2*sigma^2) 
##can be done with function:
cooks.distance(fit)[1] 
plot(fit,which=5)
