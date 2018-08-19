#' R scripts to generate plots used in lecture note #1


#' Required libraries
#' survMisc V 0.4.6 was installed to get the whas100 dataset,
#' then was updated to load survminer

library(devtools)
## install_version("survMisc", version = "0.4.6")
library(tidyverse)
library(survival)
library(survMisc)
library(lubridate)
library(survminer)
library(reReg)

#' whas100 dataset
#'
#' Load the dataset from survMisc V 0.4.6, then save it locally.
#' data(whas100, package = "survMisc")
#' save(was100, file = "whas100.RData")

load("whas100.RData")
whas100 <- as.tibble(whas100)

#' life time distributions
n <- 100
sa <- density(-rexp(n, .1))
sb <- density(rexp(n, .15))
sc <- density(c(rnorm(n, mean = 30, sd = 2), rnorm(n, mean = 3, sd = 5)))
    
dat <- tibble(Y = c(sa$x + abs(min(sa$x)), sb$x + abs(min(sb$x)), sc$x + abs(min(sc$x))),
              density = c(sa$y, sb$y, sc$y),
              study = rep(1:3, c(length(sa$x), length(sb$x), length(sc$x))))
dat$study <- as.factor(dat$study)
ggplot(dat, aes(x = Y, y = density, color = study)) + geom_line(size = 1.1) +
    facet_grid(rows = vars(study)) + theme(legend.position="none")
ggsave("note1-1.pdf")

dat <- dat %>% mutate(logY = log(Y))
ggplot(dat, aes(x = logY, y = density, color = study)) + geom_line(size = 1.1) +
    facet_grid(rows = vars(study)) + theme(legend.position="none")
ggsave("note1-2.pdf")

#' Event plots

reDat <- with(whas100, reSurv(lenfol, id, rep(0, 100), fstat))
plot(reDat)
ggsave("tab1-1-3-2.pdf")

plotEvents(reSurv(lenfol, id, rep(0, 100), fstat) ~ bmi2,
           data = whas100 %>% mutate(bmi2 = factor(bmi > 30, labels = c("High BMI", "Normal BMI"))))
ggsave("tab1-1-3-3.pdf")

pdf("tab1-1-3.pdf")
whas100 %>% with(Surv(lenfol, fstat)) %>% plot
dev.off()

whas100 %>% with(Surv(lenfol, fstat)) %>% ggsurvevents

data(heart)
head(heart)
heart %>% with(Surv(start, stop, event))
