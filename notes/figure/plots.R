library(devtools)
## install_version("survMisc", version = "0.4.6")
library(tidyverse)
library(survival)
library(survMisc)
library(lubridate)
library(survminer)

## Note 1
## fake life times
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

## Plotting with whas100
## data(whas100, package = "survMisc")
## data(whas100)
## save(whas100, file = "whas100.RData")
load("whas100.RData")
head(whas100)

whas100 <- as.tibble(whas100)
whas100 <- whas100 %>% mutate(Time = as.duration(mdy(foldate) - mdy(admitdate)) / ddays(1),
                              foldate = parse_date_time(foldate, "mdY"),
                              admitdate = parse_date_time(admitdate, "mdY"))
       
whas100 <- whas100 %>% mutate(fstat = as.factor(fstat))
levels(whas100$fstat) <- c("Censor", "Death")

ggplot(whas100 %>% filter(id <= 20)) +
    geom_segment(aes(x = admitdate, xend = foldate, y = id, yend = id), size = 1) +
    scale_y_continuous(breaks = 1:20) + ## scale_y_reverse() + 
    geom_point(aes(x = foldate, y = id, shape = fstat), size = 2.5, stroke = 1.7) +
    scale_shape_manual(values = c(1, 4)) +
    geom_point(aes(x = admitdate, y = id), size = 3) + xlab("Time (Year)") + ggtitle("Calendar time")
ggsave("tab1-1-1.pdf")

ggplot(whas100 %>% filter(id <= 20)) +
    geom_segment(aes(x = 0, xend = lenfol, y = id, yend = id), size = 1) +
    scale_y_continuous(breaks = 1:20) + ## scale_y_reverse() + 
    geom_point(aes(x = lenfol, y = id, shape = fstat), size = 2.5, stroke = 1.7) +
    scale_shape_manual(values = c(1, 4)) +
    geom_point(aes(x = 0, y = id), size = 3) + xlab("Time (Year)") + ggtitle("Patient (Follow-up) time")
ggsave("tab1-1-2.pdf")

max(whas100$foldate)

## Coding with whas10
whas100 <- as.tibble(whas100)

pdf("tab1-1-3.pdf")
whas100 %>% with(Surv(lenfol, fstat)) %>% plot
dev.off()

whas100 %>% with(Surv(lenfol, fstat)) %>% ggsurvevents

whas100 %>% with(Surv(lenfol, fstat)) %>% ggflexsurvplot


data(heart)
head(heart)
heart %>% with(Surv(start, stop, event))

heart %>% with(Surv(start, stop, type = "interval"))


## Note 2
t0 <- seq(0, 7, length = 1e3)
dat <- tibble(x = rep(t0, 4),
              y = c(dexp(t0, .5), pexp(t0, .5), 1 - pexp(t0, .5), dexp(t0, .5) / (1 - pexp(t0, .5))),
              Function = rep(1:4, each = 1e3))
dat$Function <- as.factor(dat$Function)
levels(dat$Function) <- c("f(t)", "F(t)", "S(t)", "h(t)")

ggplot(data = dat, aes(x = x, y = y, color = Function)) + geom_line(size = 1.1) + xlab("t") + ylab("")

load("whas100.RData")
whas100 <- as.tibble(whas100)

whas100 %>% filter(fstat > 0) %>% top_n(10)

whas10 <- whas100 %>% filter(fstat > 0) %>% filter(row_number() <= 10)
whas10 %>% mutate(surv = 1 - ecdf(lenfol)(lenfol))
whas10 %>% mutate(surv = 1 - ecdf(lenfol)(lenfol)) %>% select(lenfol, surv) %>% plot
whas10 %>% mutate(surv = 1 - ecdf(lenfol)(lenfol)) %>% with(plot(lenfol, surv, 's'))

whas10 %>% mutate(surv = 1 - ecdf(lenfol)(lenfol)) %>% ggplot(aes(lenfol, surv)) + geom_step()
ggsave("whas10-ecdf.pdf")

whas100 %>% filter(fstat > 0) %>% mutate(surv = 1 - ecdf(lenfol)(lenfol)) %>%
    ggplot(aes(lenfol, surv)) + geom_step() + geom_smooth()
ggsave("whas100-ecdf.pdf")



whas100 %>% filter(fstat > 0) %>% mutate(surv = 1 - rank(lenfol, ties.method = "max") / sum(lenfol)) %>%
    ggplot(aes(lenfol, surv)) + geom_step() + geom_smooth()
