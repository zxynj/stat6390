#' R scripts to generate plots used in lecture note #5

#' Required libraries
#' See R-script note1.R for details on loading `whas100`

library(tidyverse)
library(survival)

load("whas100.RData")
whas100 <- as.tibble(whas100)

fm <- Surv(lenfol, fstat) ~ (age + gender)^2 + bmi
fit.cox <- coxph(fm, data = whas100)
fit.aft <- survreg(fm, data = whas100)

coef(fit.cox)
coef(fit.aft)

summary(fit.cox)
