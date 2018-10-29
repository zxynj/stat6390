#' R scripts to generate plots used in lecture note #5

#' Required libraries
#' See R-script note1.R for details on loading `whas100`

library(tidyverse)
library(survival)

load("whas100.RData")
whas100 <- as.tibble(whas100)

## fm <- Surv(lenfol, fstat) ~ (age + gender)^2 + bmi
fm <- Surv(lenfol, fstat) ~ gender
fit.cox <- coxph(fm, data = whas100)
fit.aft <- survreg(fm, data = whas100)

coef(fit.cox)
coef(fit.aft)

summary(fit.cox)

fit.surv <- survfit(Surv(lenfol, fstat) ~ 1, data = whas100)
with(fit.surv, -sum(n.event * log(n.risk)))

1 - exp(-diff(fit.cox$loglik)) ^ (2 / 100)
