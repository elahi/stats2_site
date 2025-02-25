##' Robin Elahi
##' 24 Feb 2025
##' Code for McElreath video lecture 13 (2023)
##' Multilevel Adventures

library(rethinking)

#### BANGLADESH ####

# simple varying intercepts model
data(bangladesh)
d <- bangladesh
dat <- list(
  C = d$use.contraception,
  D = as.integer(d$district) )

mCD <- ulam(
  alist(
    C ~ bernoulli(p),
    logit(p) <- a[D],
    vector[61]:a ~ normal(abar,sigma),
    abar ~ normal(0,1),
    sigma ~ exponential(1)
  ) , data=dat , chains=4 , cores=4 )

precis(mCD, 2)

# urban effect
dat <- list(
  C = d$use.contraception,
  D = as.integer(d$district),
  U = ifelse(d$urban==1,1,0) )

# total U, centered
mCDU <- ulam(
  alist(
    C ~ bernoulli(p),
    logit(p) <- a[D] + b[D]*U,
    vector[61]:a ~ normal(abar,sigma),
    vector[61]:b ~ normal(bbar,tau),
    c(abar,bbar) ~ normal(0,1),
    c(sigma,tau) ~ exponential(1)
  ) , data=dat , chains=4 , cores=4 )

precis(mCDU, 1)

traceplot(mCDU, pars = c("bbar", "abar", "tau", "sigma"))
trankplot(mCDU, pars = c("bbar", "abar", "tau", "sigma"))

# total U, non-centered
mCDUnc <- ulam(
  alist(
    C ~ bernoulli(p),
    logit(p) <- a[D] + b[D]*U,
    # define effects using other parameters
    save> vector[61]:a <<- abar + za*sigma,
    save> vector[61]:b <<- bbar + zb*tau,
    # z-scored effects
    vector[61]:za ~ normal(0,1),
    vector[61]:zb ~ normal(0,1),
    # ye olde hyper-priors
    c(abar,bbar) ~ normal(0,1),
    c(sigma,tau) ~ exponential(1)
  ) , data=dat , chains=4 , cores=4 )

precis(mCDUnc, 1)
traceplot(mCDUnc, pars = c("bbar", "abar", "tau", "sigma"))
trankplot(mCDUnc, pars = c("bbar", "abar", "tau", "sigma"))

# Compare Stan code
stancode(mCD)
stancode(mCDU)
stancode(mCDUnc)

#### DEVILS FUNNEL ####

## R code 13.26
set.seed(12)
m13.7 <- ulam(
  alist(
    v ~ normal(0,3),
    x ~ normal(0, exp(v))
  ), data=list(N=1) , chains=4 )
precis( m13.7 )

## R code 13.27
set.seed(13)
m13.7nc <- ulam(
  alist(
    v ~ normal(0,3),
    z ~ normal(0,1),
    gq> real[1]:x <<- z*exp(v)
  ), data=list(N=1) , chains=4 )
precis( m13.7nc )

stancode(m13.7)
stancode(m13.7nc)

## Plot x against v
post_c <- extract.samples(m13.7)
post_nc <- extract.samples(m13.7nc)

par(mfrow = c(1,2))
plot(post_c$x, post_c$v, xlim = c(-20, 20), ylim = c(-6, 6), cex = 0.2, col = "red")
plot(post_nc$x, post_nc$v, xlim = c(-20, 20), ylim = c(-6, 6), cex = 0.2, col = "blue")

#### CHALLENGE ####

##' Recreate the following plots from Lecture_13-GLMM2.pdf
##' https://raw.githubusercontent.com/rmcelreath/stat_rethinking_2023/main/slides/Lecture_13-GLMM2.pdf
##' Slide 62 (probability of contraceptive use by rural/urban districts)
##' Slide 64 (posterior and prior of contraceptive use by rural/urban)
##' Slide 65 (scatterplot of prob_urban vs prob_rural)
##' You can use base R (adapt McElreath code) or use tidy code (e.g., tidybayes.rethinking)
