#' R scripts to generate plots used in lecture note #2

#' Required libraries
#' See R-script note1.R for details on loading `whas100`

library(devtools)
## install_version("survMisc", version = "0.4.6")
library(tidyverse)
library(survival)
library(survMisc)
library(lubridate)
library(survminer)
library(reReg)
library(Hmisc)

load("whas100.RData")
whas100 <- as.tibble(whas100)

km <- survfit(Surv(lenfol, fstat) ~ 1, whas100)

summary(km, time = c(365, 2190))
weiSurv <- Weibull2(c(365, 2190), c(.8, .505))

pdf("Weibull2.pdf")
plot(km$time, km$surv, 's', lwd = 1.2, xlab = "Time", ylab = "Survival probability")
lines(0:3000, weiSurv(0:3000), 's', col = 2, lwd = 1.2)
points(c(365, 2190), c(.8, .505), pch = 16, cex = 1.2)
legend("topright", c("Kaplan-Meier", "Weibull2"), bty = "n", lwd = 1.2, col = 1:2, lty = 1)
dev.off()


fit.exp <- survreg(Surv(lenfol, fstat) ~ (age + gender)^2 + bmi, data = whas100, dist = "exp")
fit.exp$var


fit.wei <- survreg(Surv(lenfol, fstat) ~ (age + gender)^2 + bmi, data = whas100)
