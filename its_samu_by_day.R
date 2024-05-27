## Load Libraries
library(nlme)
library(car)
library(lubridate)

## import data
data <- read.csv("/Users/nzeyimanajanvier/Desktop/its_samu/by_day/samu_day.csv")
data$dayv <- c(range = seq(1, 123, by = 1))
# First visualization
plot(data$dayv,data$time,
     ylab="Time Response",
     ylim=c(0,80),
     xlab="day",
     type="l",
     col="red",
     xaxt="n"
)

# add x-axis label
axis(1, at=1:123, labels=data$dayv )


# add in each record point
points(data$dayv,data$time,
       col="red",
       pch=20)

# label Samu increase in number of cars
abline(v=62.5, lty=2)

# create ordinary least squares regression models
model_ols <- lm(time ~ dayv + level + trend, data=data)

# SEE SUMMARY OF THE MODEL
summary(model_ols)

# GET CONFIDENCE INTERVAL FOR THE COEFFICIENTS
confint(model_ols)

# Check for autocorrelation
# RUN THE DURBIN WATSON TEST
dwt(model_ols,max.lag=12,alternative="two.sided")

# SET PLOTTING TO TWO TWO RECORDS PER PAGE
par(mfrow=c(2,1))

# PRODUCE PLOTS
acf(residuals(model_ols),)
acf(residuals(model_ols), type="partial")

# FIT ARMA MODEL WILH lag=1
model_p1 <- gls(time ~ dayv + level + trend,
                 data=data, correlation=corARMA(p=1,form=~dayv), method="ML")
summary(model_p1)

# DIAGNOSTIC CHECKS
model_p2 <- update(model_p1, correlation=corARMA(p=2,form=~dayv))
anova(model_p1, model_p2)

# FIT A FINAL ARMA (with lag=3) MODEL
model_p1 <- gls(time ~ dayv + level + trend,, 
                data=data,
                correlation = corARMA(p=1, form=~dayv),
                method="ML") 
summary(model_p1)

par(mfrow=c(1,1))
# PLOT THE RESULTS
plot(data$dayv,data$time,
     ylab="Time to Response",
     ylim=c(0,80),
     xlab="Days",
     pch=20,
     col="pink",
     xaxt="n")

# ADD X-AXIS YEAR LABELS
axis(1, at=1:123, labels=data$dayv )

# LABEL THE WEATHER CHANGE
abline(v=62.5, lty=2)

# PLOT THE FIRST LINE SEGMENT
lines(data$dayv[1:62], fitted(model_p1)[1:62], col="red", lwd=2)

# PLOT THE SECOND LINE SEGMENT
lines(data$dayv[63:123], fitted(model_p1)[63:123], col="red", lwd=2)

# PlOT THE COUNTERFACTUAL LINE SEGMENT
segments(1,
         model_ols$coef[1]+model_p1$coef[2],123,
         model_ols$coef[1]+model_p1$coef[2]*123,
         lty=2,
         lwd=2,
         col="red")

library(tidyverse)

# perform t_test comparing pre and post intervention period
pre_intervention_data <- data %>% filter(dayv<=62)
post_intervention_data <- data %>% filter(dayv>62)
t_test_result <- t.test(pre_intervention_data$time, post_intervention_data$time)
print(t_test_result)

###### Making prediction in ITs

# Predicted value at 25 years after the weather change
pred <- fitted(model_p1)[80]

# Then estimate the counterfactual at the same time point
cfac <- model_p1$coef[1] + model_p1$coef[2]*80

# Absolute change at 70 days
pred - cfac

# Relative change at 70 days
(pred - cfac) / cfac

















