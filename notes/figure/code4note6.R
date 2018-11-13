#' R scripts to generate plots used in lecture note #6

#' Required libraries
#' See R-script note1.R for details on loading `whas100`

library(tidyverse)
library(survival)

load("whas100.RData")
whas100 <- as.tibble(whas100)

fit <- coxph(Surv(lenfol, fstat) ~ age + bmi + gender, data = whas100)
AIC(fit)
step(fit)

fit$nevent

extractAIC(fit, k = log(fit$nevent))
step(fit, k = log(fit$nevent))

res <- resid(fit, type = "schoenfeld")
res <- as.tibble(res) %>% mutate(Time = as.numeric(rownames(res)))

qplot(Time, age, data = res) + geom_hline(yintercept = 0)
qplot(Time, bmi, data = res) + geom_hline(yintercept = 0)

ggplot(data = reshape2::melt(res, id = "Time")) + geom_boxplot(aes(x = variable, y = value))

stdres <- as.tibble(resid(fit, type = "schoenfeld") %*% fit$var * fit$nevent)
names(stdres) <- c("age", "bmi", "gender")
stdres$Time <- res$Time

qplot(Time, age, data = stdres) + geom_hline(yintercept = 0)
ggplot(data = reshape2::melt(stdres, id = "Time")) + geom_boxplot(aes(x = variable, y = value))

zph <- cox.zph(fit)
head(zph$y)
plot(zph)
