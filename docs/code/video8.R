##' Robin Elahi
##' 2 Feb 2025
##' Code for McElreath video lecture 8 (2023)
##' MCMC

library(rethinking)

#### WINE ####

data(Wines2012)
d <- Wines2012

dat <- list(
  S=standardize(d$score),
  J=as.numeric(d$judge),
  W=as.numeric(d$wine),
  X=ifelse(d$wine.amer==1,1,2),
  Z=ifelse(d$judge.amer==1,1,2)
)

mQ <- ulam(
  alist(
    S ~ dnorm(mu,sigma),
    mu <- Q[W],
    Q[W] ~ dnorm(0,1),
    sigma ~ dexp(1)
  ) , data=dat , chains=4 , cores=4 )

precis(mQ,2)
traceplot(mQ)
trankplot(mQ)

## 2nd model

mQO <- ulam(
  alist(
    S ~ dnorm(mu,sigma),
    mu <- Q[W] + O[X],
    Q[W] ~ dnorm(0,1),
    O[X] ~ dnorm(0,1),
    sigma ~ dexp(1)
  ) , data=dat , chains=4 , cores=4 )

dev.off()
plot(precis(mQO,2))

## 3rd model

mQOJ <- ulam(
  alist(
    S ~ dnorm(mu,sigma),
    mu <- (Q[W] + O[X] - H[J])*D[J],
    Q[W] ~ dnorm(0,1),
    O[X] ~ dnorm(0,1),
    H[J] ~ dnorm(0,1),
    D[J] ~ dexp(1),
    sigma ~ dexp(1)
  ) , data=dat , chains=4 )

plot(precis(mQOJ,2))

#### SR2 9.4 ####

## R code 9.11
library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )

## R code 9.12
m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dd )
precis( m8.3 , depth=2 )

## R code 9.13
dat_slim <- list(
  log_gdp_std = dd$log_gdp_std,
  rugged_std = dd$rugged_std,
  cid = as.integer( dd$cid )
)
str(dat_slim)

## R code 9.14
m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dat_slim , chains=1 )

## R code 9.15
precis( m9.1 , depth=2 )

## R code 9.16
m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dat_slim , chains=4 , cores=4 )

## R code 9.17
show( m9.1 )

## R code 9.18
precis( m9.1 , 2 )

## R code 9.19
pairs( m9.1 )

## R code 9.20
traceplot( m9.1 )

## R code 9.21
trankplot( m9.1 )

#### SR2 9.5 ####

## R code 9.22
y <- c(-1,1)
set.seed(11)
m9.2 <- ulam(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- alpha ,
    alpha ~ dnorm( 0 , 1000 ) ,
    sigma ~ dexp( 0.0001 )
  ) , data=list(y=y) , chains=3 )

## R code 9.23
precis( m9.2 )

## R code 9.24
set.seed(11)
m9.3 <- ulam(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- alpha ,
    alpha ~ dnorm( 1 , 10 ) ,
    sigma ~ dexp( 1 )
  ) , data=list(y=y) , chains=3 )
precis( m9.3 )

## R code 9.25
set.seed(41)
y <- rnorm( 100 , mean=0 , sd=1 )

## R code 9.26
set.seed(384)
m9.4 <- ulam(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a1 + a2 ,
    a1 ~ dnorm( 0 , 1000 ),
    a2 ~ dnorm( 0 , 1000 ),
    sigma ~ dexp( 1 )
  ) , data=list(y=y) , chains=3 )
precis( m9.4 )

## R code 9.27
m9.5 <- ulam(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a1 + a2 ,
    a1 ~ dnorm( 0 , 10 ),
    a2 ~ dnorm( 0 , 10 ),
    sigma ~ dexp( 1 )
  ) , data=list(y=y) , chains=3 )
precis( m9.5 )

## R code 9.28
mp <- ulam(
  alist(
    a ~ dnorm(0,1),
    b ~ dcauchy(0,1)
  ), data=list(y=1) , chains=1 )

## R code 9.29
m5.8s <- ulam(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dexp( 1 )
  ) , data=d, chains=4,
  start=list(a=10,bl=0,br=0.1,sigma=1) )

## R code 9.30
m5.8s2 <- ulam(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dexp( 1 )
  ) , data=d, chains=4,
  constraints=list(br="lower=0"),
  start=list(a=10,bl=0,br=0.1,sigma=1) )

