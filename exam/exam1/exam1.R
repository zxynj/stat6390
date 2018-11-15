#' R codes used for exam 1
#' 

library(tidyverse)
library(survival)
load("whas100.RData")
whas100 <- as.tibble(whas100)

#' Problem 1
#' Compute log-rank statistic without using the `survival` package

attach(whas100)
dat <- tibble(Time = unique(sort(lenfol[fstat > 0]))) %>%
    mutate(ni = colSums(outer(lenfol, Time, ">=")),
           di = colSums(outer(lenfol, Time, "==")),
           ni.1 = colSums(outer(lenfol[gender == 1], Time, ">=")),
           di.1 = colSums(outer(lenfol[gender == 1], Time, "==")),
           ni.0 = ni - ni.1, di.0 = di - di.1,
           E = ni.1 * di / ni, V = ni.1 * ni.0 * di * (ni - di) / ni / ni / (ni - 1))
detach(whas100)

(Q <- with(dat, sum(di.1 - E)^2 / sum(V)))
survdiff(Surv(lenfol, fstat) ~ gender, data = whas100)$chisq

#' Problem 2
#'
#' a
with(dat, sum(di.1 - E))
#' b
dat <- dat %>% mutate(km0 = cumprod((ni.0 - di.0) / ni.0),
                      km1 = cumprod((ni.1 - di.1) / ni.1))
max(abs(dat$km0 - dat$km1))
#' c
with(dat, Time[which.max(km1 <= .5)] - Time[which.max(km0 <= .5)])
#' d
#'
with(dat, sum(-diff(rev(Time)) * rev(km1 - km0)[-1]))

#' Problem 3
#' First write a function to calculate the 4 statistics
#'
#' @param obs is the observed survival time
#' @param event is the censoring indicator
#' @param x is the categorical covariate; taking values of 0 and 1
getD <- function(obs, event, x) {
    dat0 <- tibble(Time = unique(sort(obs[event > 0]))) %>%
        mutate(ni = colSums(outer(obs, Time, ">=")),
               di = colSums(outer(obs, Time, "==")),
               ni.1 = colSums(outer(obs[x > 0], Time, ">=")),
               di.1 = colSums(outer(obs[x > 0], Time, "==")),
               ni.0 = ni - ni.1, di.0 = di - di.1,
               E = ni.1 * di / ni, V = ni.1 * ni.0 * di * (ni - di) / ni / ni / (ni - 1),
               km0 = pmax(0, cumprod((ni.0 - di.0) / ni.0), na.rm = TRUE), 
               km1 = pmax(0, cumprod((ni.1 - di.1) / ni.1), na.rm = TRUE))               
    attach(dat0)
    d1 <- sum(di.1 - E)
    d2 <- max(abs(km0 - km1))
    d3 <- Time[which.max(km1 <= .5)] - Time[which.max(km0 <= .5)]
    d4 <- sum(-diff(rev(Time)) * rev(km1 - km0)[-1])
    detach(dat0)
    c(d1 = d1, d2 = d2, d3 = d3, d4 = d4)
}
## check if we can recover results in problem 2
d0 <- with(whas100, getD(lenfol, fstat, gender))

#' a; 5000 replications
set.seed(1)
system.time(permd <- t(replicate(5000, unlist(with(whas100, getD(lenfol, fstat, sample(gender)))))))

#' summary
summary(permd)
#' histogram
ggdat <- gather(permd, "d", factor_key = TRUE) %>% mutate(d0 = unlist(d0[d]))
ggplot(ggdat, aes(value)) +
    geom_histogram(alpha = .5) + geom_vline(aes(xintercept = d0), lty = I(2)) + 
    facet_wrap(d ~., scale = "free")
    
#' b; calculating p-values
sapply(1:4, function(x) 2 * min(sum(d0[[x]] < permd[,x]), sum(d0[[x]] > permd[,x])) / 5000)

#' Problem 4
#' First write a function to calculate U
getU <- function(obs, event, x) {
    t1 <- obs[x > 0]
    d1 <- event[x > 0]
    t0 <- obs[x == 0]
    d0 <- event[x == 0]
    sum(outer(t0, t1[d1 > 0], ">")) - sum(outer(t0[d0 > 0], t1, "<"))
}

#' a
U0 <- with(whas100, getU(lenfol, fstat, gender))
U0

#' b
set.seed(1)
permU <- replicate(5000, with(whas100, getU(lenfol, fstat, sample(gender))))
summary(permU)

2 * min(sum(U0 < permU), sum(U0 > permU)) / 5000
ggplot() + aes(permU) + geom_histogram(alpha = .5) + geom_vline(aes(xintercept = U0), lty = I(2))
