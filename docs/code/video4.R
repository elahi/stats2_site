##' Robin Elahi
##' 18 Jan 2025
##' Code for McElreath video lecture 4 (2023)
##' Categories and curves

library(rethinking)
#library(tidyverse)

#### GENERATIVE SIMULATION ####

# Generative code
# S=1 female; S=2 male
sim_HW <- function(S,b,a) {
  N <- length(S)
  H <- ifelse(S==1,150,160) + rnorm(N,0,5)
  W <- a[S] + b[S]*H + rnorm(N,0,5)
  data.frame(S,H,W)
}

S <- rbern(100)+1
dat <- sim_HW(S,b=c(0.5,0.6),a=c(0,0))
head(dat)

with(dat, plot(H, W, col = S))

#### TESTING ####
# female sample
S <- rep(1,100)
simF <- sim_HW(S,b=c(0.5,0.6),a=c(0,0))
# male sample
S <- rep(2,100)
simM <- sim_HW(S,b=c(0.5,0.6),a=c(0,0))
# effect of sex (male-female)
mean(simM$W - simF$W)

# Run the model
# observe sample
S <- rbern(100)+1
dat <- sim_HW(S,b=c(0.5,0.6),a=c(0,0))
# estimate posterior
m_SW <- quap(
  alist(
    W ~ dnorm(mu,sigma),
    mu <- a[S],
    a[S] ~ dnorm(60,10),
    sigma ~ dunif(0,10)
  ), data=dat )
precis(m_SW,depth=2)

# Not in lecture slides
# test simulated data with height 
# doesn't quite recover betas

head(dat)
dat$Hbar = mean(dat$H)
m_SHW <- quap(
  alist(
    W ~ dnorm(mu,sigma),
    mu <- a[S] + b[S]*(H-Hbar),
    a[S] ~ dnorm(60,10),
    b[S] ~ dlnorm(0,1),
    sigma ~ dunif(0,10)
  ), data=dat )
precis(m_SHW,depth=2)

#### ANALYZE HOWELL ####

# W ~ S

data(Howell1)
d <- Howell1
d <- d[ d$age>=18 , ]
dat <- list(
  W = d$weight,
  S = d$male + 1 ) # S=1 female, S=2 male

m_SW <- quap(
  alist(
    W ~ dnorm(mu,sigma),
    mu <- a[S],
    a[S] ~ dnorm(60,10),
    sigma ~ dunif(0,10)
  ), data=dat )

# posterior means
post <- extract.samples(m_SW)

# inspect this list object
str(post)
head(post$sigma)
head(post$a)

# plot histogram
dens( post$a[,1] , xlim=c(39,50) , lwd=3 , col=2 , xlab="posterior mean weight (kg)" )
dens( post$a[,2] , lwd=3 , col=4 , add=TRUE )

# posterior W distributions
W1 <- rnorm( 1000 , post$a[,1] , post$sigma )
W2 <- rnorm( 1000 , post$a[,2] , post$sigma )
dens( W1 , xlim=c(20,70) , ylim=c(0,0.085) , lwd=3 , col=2 , xlab="posterior predicted weight (kg)" )
dens( W2 , lwd=3 , col=4 , add=TRUE )

# causal contrast (in means)
mu_contrast <- post$a[,2] - post$a[,1]
dens( mu_contrast , xlim=c(3,10) , lwd=3 , col=1 , xlab="posterior mean weight contrast (kg)" )

# W contrast
W_contrast <- W2 - W1
dens( W_contrast , xlim=c(-25,35) , lwd=3 , col=1 , xlab="posterior weight contrast (kg)" )

Wdens <- density(W_contrast,adj=0.5)
polygon(c(Wdens$x[Wdens$x>0], max(Wdens$x), 0), c(Wdens$y[Wdens$x>0], 0, 0), col = 4, border = NA )
polygon(c(Wdens$x[Wdens$x<0], 0, min(Wdens$x)), c(Wdens$y[Wdens$x<0], 0, 0), col = 2, border = NA )

# proportion above zero
sum( W_contrast > 0 ) / 1000
# proportion below zero
sum( W_contrast < 0 ) / 1000

# W ~ S + H

data(Howell1)
d <- Howell1
d <- d[ d$age>=18 , ]
dat <- list(
  W = d$weight,
  H = d$height,
  Hbar = mean(d$height),
  S = d$male + 1 ) # S=1 female, S=2 male

m_SHW <- quap(
  alist(
    W ~ dnorm(mu,sigma),
    mu <- a[S] + b[S]*(H-Hbar),
    a[S] ~ dnorm(60,10),
    b[S] ~ dlnorm(0,1),
    sigma ~ dunif(0,10)
  ), data=dat )
precis(m_SHW, depth = 2)

# Contrasts at each height
xseq <- seq(from=130,to=190,len=50)
muF <- link(m_SHW,data=list(S=rep(1,50),H=xseq,Hbar=mean(d$height)))
str(muF)
head(muF)

with(dat, plot(H, W, col = S))
lines( xseq , apply(muF,2,mean) , lwd=3 , col=1)

muM <- link(m_SHW,data=list(S=rep(2,50),H=xseq,Hbar=mean(d$height)))
lines( xseq , apply(muM,2,mean) , lwd=3 , col=2 )

# Contrasts
mu_contrast <- muF - muM
plot( NULL , xlim=range(xseq) , ylim=c(-6,8) , xlab="height (cm)", ylab="weight contrast (F–M)" )
for ( p in c(0.5,0.6,0.7,0.8,0.9,0.99) ) 
  shade( apply(mu_contrast,2,PI,prob=p) , xseq )
abline(h=0,lty=2)

#### FULL LUXURY BAYES ####

# full system as SCM

data(Howell1)
d <- Howell1
d <- d[ d$age>=18 , ]
dat <- list(
  W = d$weight,
  H = d$height,
  Hbar = mean(d$height),
  S = d$male + 1 ) # S=1 female, S=2 male

m_SHW_full <- quap(
  alist(
    
    # weight
    W ~ dnorm(mu,sigma),
    mu <- a[S] + b[S]*(H-Hbar),
    a[S] ~ dnorm(60,10),
    b[S] ~ dlnorm(0,1),
    sigma ~ dunif(0,10),
    
    # height
    H ~ dnorm(nu,tau),
    nu <- h[S],
    h[S] ~ dnorm(160,10),
    tau ~ dunif(0,10)
    
  ), data=dat )

# compute total causal effect of S on W
post <- extract.samples(m_SHW_full)
Hbar <- dat$Hbar
n <- 1e4

with( post , {
  # simulate W for S=1
  H_S1 <- rnorm(n, h[,1] , tau )
  W_S1 <- rnorm(n, a[,1] + b[,1]*(H_S1-Hbar) , sigma)
  # simulate W for S=2
  H_S2 <- rnorm(n, h[,2] , tau)
  W_S2 <- rnorm(n, a[,2] + b[,2]*(H_S2-Hbar) , sigma)
  # compute contrast
  W_do_S <<- W_S2 - W_S1
})

dens(W_do_S)

# automated way
HWsim <- sim(m_SHW_full,
             data=list(S=c(1,2)),
             vars=c("H","W"))

W_do_S_auto <- HWsim$W[,2] - HWsim$W[,1]

