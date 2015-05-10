# ###week 3 variance inflation
# The Rumsfeldian triplet
# 
# There are known knowns. These are things we know that we know. There are known unknowns. 
# That is to say, there are things that we know we don't know. But there are also unknown unknowns. 
# There are things we don't know we don't know. Donald Rumsfeld
# 
# In our context
# 
# (Known knowns) Regressors that we know we should check to include in the model and have.
# (Known Unknowns) Regressors that we would like to include in the model, but don't have.
# (Unknown Unknowns) Regressors that we don't even know about that we should have included in the model.
# General rules
# 
# Omitting variables results in bias in the coeficients of interest - unless their regressors are uncorrelated with the
# omitted ones.
# This is why we randomize treatments, it attempts to uncorrelate our treatment indicator with variables that we
# don't have to put in the model.
# (If there's too many unobserved confounding variables, even randomization won't help you.)
# Including variables that we shouldn't have increases standard errors of the regression variables.
# Actually, including any new variables increasese (actual, not estimated) standard errors of other regressors. 
# So we don't want to idly throw variables into the model.
# The model must tend toward perfect fit as the number of non-redundant regressors approaches $n$.
# $R^2$ increases monotonically as more regressors are included.
# The SSE decreases monotonically as more regressors are included.
# 
# 
# 
# 
# Variance inflation factors
# 
# Notice variance inflation was much worse when we included a variable that was highly related to x1.
# We don't 
# know $\sigma$, so we can only estimate the increase in the actual standard error of the coefficients for including a regressor.
# However, $\sigma$ drops out of the relative standard errors. If one sequentially adds variables, one can check the variance (or sd) inflation for including each one.
# When the other regressors are actually orthogonal to the regressor of interest, then there is no variance inflation.
# The variance inflation factor (VIF) is the increase in the variance for the ith regressor compared to the ideal setting where it is orthogonal to the other regressors.
# (The square root of the VIF is the increase in the sd ...)
# Remember, variance inflation is only part of the picture. We want to include certain variables, even if they dramatically inflate our variance.



data(swiss); 
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
a <- summary(fit1)$cov.unscaled[2,2]
fit2 <- update(fit, Fertility ~ Agriculture + Examination)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
  c(summary(fit2)$cov.unscaled[2,2],
    summary(fit3)$cov.unscaled[2,2]) / a 

library(car)
fit <- lm(Fertility ~ . , data = swiss)
vif(fit)
data(swiss)
fit1 <- lm(Fertility ~ Agriculture, data = swiss)

fit3 <- lm(Fertility ~ Agriculture + Examination + Education,swiss)
fit5 <- lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality,swiss)

####anova or Analysis of variance quantifies significance of additional regressors
anova(fit1, fit3, fit5) ##checks significance if added correlated regressors

##lastly we use Sharpiro-Wilk test to check for residual normality
shapiro.test(fit3$residuals) ##fails to reject normality



