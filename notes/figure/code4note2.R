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

load("whas100.RData")
whas100 <- as.tibble(whas100)

#' reformat whas100 for better display
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


ggplot(whas100 %>% filter(id <= 20)) +
    geom_segment(aes(x = 0, xend = lenfol, y = id, yend = id), size = 1) +
    scale_y_continuous(breaks = 1:20) + ## scale_y_reverse() + 
    geom_point(aes(x = lenfol, y = id, shape = fstat), size = 2.5, stroke = 1.7) +
    scale_shape_manual(values = c(1, 4)) +
    geom_point(aes(x = 0, y = id), size = 3) + xlab("Time (Year)") +
    ggtitle("Patient (Follow-up) time") +
    geom_vline(xintercept = 800, color = 2, lwd = 1.2) 
ggsave("tab1-1-4.pdf")

whas20_2 <- whas100 %>% filter(id <= 20)
whas20_2$fstat[c(4, 9, 13)] <- whas20_2$fstat[c(16, 16, 16)]

ggplot(whas20_2) + 
    geom_segment(aes(x = 0, xend = lenfol, y = id, yend = id), size = 1) +
    scale_y_continuous(breaks = 1:20) + ## scale_y_reverse() + 
    geom_point(aes(x = lenfol, y = id, shape = fstat), size = 2.5, stroke = 1.7) +
    scale_shape_manual(values = c(1, 4)) +
    geom_point(aes(x = 0, y = id), size = 3) + xlab("Time (Year)") +
    ggtitle("Patient (Follow-up) time (modified data)") +
    geom_vline(xintercept = 800, color = 2, lwd = 1.2) 
ggsave("tab1-1-5.pdf")


#' Kaplan Meier's
#' Re-load whas100 for the following 

load("whas100.RData")
whas100 <- as.tibble(whas100)

whas100 %>% filter(fstat > 0) %>% top_n(10)

whas10 <- whas100 %>% filter(fstat > 0) %>% filter(row_number() <= 10)
whas10 %>% mutate(surv = 1 - ecdf(lenfol)(lenfol))
whas10 %>% mutate(surv = 1 - ecdf(lenfol)(lenfol)) %>% select(lenfol, surv) %>% plot
whas10 %>% mutate(surv = 1 - ecdf(lenfol)(lenfol)) %>% with(plot(lenfol, surv, 's'))

whas10 %>% mutate(surv = 1 - ecdf(lenfol)(lenfol)) %>% ggplot(aes(lenfol, surv)) + geom_step()
ggsave("whas10-ecdf.pdf")

whas100 %>% filter(id <= 20) %>%
    mutate(surv = 1 - ecdf(lenfol)(lenfol)) %>% ggplot(aes(lenfol, surv)) + geom_step() +
    geom_vline(xintercept = 800)

whas100 %>% filter(fstat > 0) %>% mutate(surv = 1 - ecdf(lenfol)(lenfol)) %>%
    ggplot(aes(lenfol, surv)) + geom_step() + geom_smooth()
ggsave("whas100-ecdf.pdf")

km <- survfit(Surv(lenfol, fstat) ~ 1, data = whas100, subset = id <= 20)
summary(km)

pdf("km1.pdf")
plot(km)
dev.off()
ggsurvplot(km)
ggsave("km2.pdf")

km <- survfit(Surv(lenfol, fstat) ~ 1, data = whas100)
pdf("km3.pdf")
plot(km)
dev.off()
ggsurvplot(km)
ggsave("km4.pdf")

cox <- coxph(Surv(lenfol, fstat) ~ 1, data = whas100)
H0 <- basehaz(cox)
pdf("km5.pdf")
plot(km, lwd = 1.5)
lines(H0$time, exp(-H0$hazard), col = 2, 's', lwd = 1.5)
legend("bottomleft", bty = "n", lty = 1, lwd = 1.5, col = 1:2, c("Kaplan-Meier", "Nelson-Aalon"))
dev.off()

km0 <- survfit(Surv(lenfol, fstat) ~ 1, data = whas100, conf.type = "none")
km <- lapply(c("plain", "log", "log-log", "logit", "arcsin"), function(x) update(km0, conf.type = x))
plot(km0)
invisible(lapply(km, lines))

plot(km0)
invisible(lapply(1:4, function(x) lines(km[[x]], col = 6 - x, lwd = 1.1)))
lines(km0, lwd = 1.2)
legend("bottomleft", c("log", "log-log", "logit", "arcsin"), col = 5:2, lwd = 1.1, bty = "n")

kmplot <- do.call(rbind, lapply(km, function(x) with(x, cbind(time, surv, lower, upper))))
kmplot <- as.tibble(kmplot) %>% mutate(type = as.factor(rep(1:5, each = 95)))
levels(kmplot$type) <- c("plain", "log", "log-log", "logit", "arcsin")

ggplot(kmplot, aes(x = time, y = surv)) + geom_step(size = 1.2) + ylab("Probability") +
    geom_step(aes(x = time, y = lower, col = type), size = .8) +
    geom_step(aes(x = time, y = upper, col = type), size = .8) 
ggsave("kmplot.pdf")

ggplot(kmplot, aes(x = time, y = surv)) + geom_step(size = 1.2) + ylab("Probability") +
    geom_step(aes(x = time, y = lower, col = type), size = .8) +
    geom_step(aes(x = time, y = upper, col = type), size = .8) +
    xlim(0, 500) + ylim(0.655, 1) # + theme(legend.position="none")
ggsave("kmplot2.pdf")


km <- survfit(Surv(lenfol, fstat) ~ gender, data = whas100)
pdf("km-gender1.pdf")
plot(km)
dev.off()
ggsurvplot (km)
ggsave("km-gender2.pdf")


survdiff(Surv(lenfol, fstat) ~ gender, data = whas100)
survdiff(Surv(lenfol, fstat) ~ gender, data = whas100, rho = .5)
survdiff(Surv(lenfol, fstat) ~ gender, data = whas100, rho = 1)
