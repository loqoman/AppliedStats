# Get US arrest data
data("USArrests")

# > Comparing Assault Rates (predictor) to Murder Rates
# - Assault rates(x) vs. Murder Rates(y)
plot(USArrests$Assault,USArrests$Murder,main="Murder Rates vs. Assault Rates In US States ", 
     xlab="Number of Assault Arrests", ylab="Murder Rate", pch=19, 
     col="red",bg="darkblue", col.main="blue")

# > Regression analysis of Assault rates predicting murder rates
# lm(y~x)
regression = lm(USArrests$Murder~USArrests$Assault)

summary(regression)
anova(regression)

# > Check conditions needed to run linear regression inference
plot(USArrests$Murder, resid(regression), ylab="Residuals",xlab="Predicted",
     main="Residual plot of linear regression on US State Assault Rate vs. Murder Rate")

# Viewing data

# Gives Mean
mean(USArrests$Assault)

# Gives st. dv.
sd(USArrests$Assault)

# Finding S_{xx}
sx = sum((USArrests$Assault-mean(USArrests$Assault))^2)
# Finding sum(y-yhat)^2
yhatsum = sum((USArrests$Murder-(0.631+0.041*USArrests$Assault))^2)


# Running a Confidence Interval
confint(regression,level=0.95)

# Running an Inference Test on our Linear model ("Is there significant statistical correlation?")
inference(x=0.041, alternative = "twosided", type="ht", statistic = "mean", conf_level = 0.05)

