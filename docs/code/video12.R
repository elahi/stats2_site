##' Robin Elahi
##' 17 Feb 2025
##' Code for McElreath video lecture 12 (2023)
##' Multilevel Models

library(rethinking)

#### REED FROGS ####

data(reedfrogs)
d <- reedfrogs
d$tank <- 1:nrow(d)

dat <- list(
  S = d$surv,
  D = d$density,
  T = d$tank )

mST <- ulam(
  alist(
    S ~ dbinom( D , p ) ,
    logit(p) <- a[T] ,
    a[T] ~ dnorm( a_bar , sigma ) ,
    a_bar ~ dnorm( 0 , 1.5 ) ,
    sigma ~ dexp( 1 )
  ), data=dat , chains=4 , log_lik=TRUE )

precis(mST, depth = 2)

# No memory (no pooling)
mSTnomem <- ulam(
  alist(
    S ~ dbinom( D , p ) ,
    logit(p) <- a[T] ,
    a[T] ~ dnorm( a_bar , 1 ) ,
    a_bar ~ dnorm( 0 , 1.5 )
  ), data=dat , chains=4 , log_lik=TRUE )

compare( mST , mSTnomem , func=WAIC )

# Stratify by predators
dat$P <- ifelse(d$pred=="pred",1,0)
mSTP <- ulam(
  alist(
    S ~ dbinom( D , p ) ,
    logit(p) <- a[T] + bP*P ,
    bP ~ dnorm( 0 , 0.5 ),
    a[T] ~ dnorm( a_bar , sigma ) ,
    a_bar ~ dnorm( 0 , 1.5 ) ,
    sigma ~ dexp( 1 )
  ), data=dat , chains=4 , log_lik=TRUE )

compare( mST , mSTP , func=WAIC )

# Note difference in sigma values
precis(mSTP)
precis(mST)

#### BONUS: RANDOM CONFOUNDS ####

N_groups <- 30
N_id <- 200
a0 <- (-2)
bZY <- (-0.5)
g <- sample(1:N_groups, size=N_id, replace=TRUE) # sample into groups
Ug <- rnorm(N_groups, 1.5) # group confounds
Ug; mean(Ug)

X <- rnorm(N_id, Ug[g] ) # individual varying trait
Ug
g

Z <- rnorm(N_groups) # group varying trait (observed)
Z

Y <- rbern(N_id, p=inv_logit( a0 + X + Ug[g] + bZY*Z[g] ) )
table(g)
sum(table(g))

dat <- list(Y=Y,X=X,g=g,Ng=N_groups,Z=Z)

# fixed effects
mf <- ulam(
  alist(
    Y ~ bernoulli(p),
    logit(p) <- a[g] + bxy*X + bzy*Z[g],
    a[g] ~ dnorm(0,10),
    c(bxy,bzy) ~ dnorm(0,1)
  ) , data=dat , chains=4 , cores=4 )

# naive model 
mn <- ulam(
  alist(
    Y ~ bernoulli(p),
    logit(p) <- bxy*X + bzy*Z[g],
    c(bxy,bzy) ~ dnorm(0,1)
  ) , data=dat , chains=4 , cores=4 )

# varying effects (non-centered - next week!)
mr <- ulam(
  alist(
    Y ~ bernoulli(p),
    logit(p) <- a[g] + bxy*X + bzy*Z[g],
    transpars> vector[Ng]:a <<- abar + z*tau,
    z[g] ~ dnorm(0,1),
    c(bxy,bzy) ~ dnorm(0,1),
    abar ~ dnorm(0,1),
    tau ~ dexp(1)
  ) , data=dat , chains=4 , cores=4 , sample=TRUE )

# The Mundlak Machine
xbar <- sapply( 1:N_groups , function(j) mean(X[g==j]) )
dat$Xbar <- xbar
mrx <- ulam(
  alist(
    Y ~ bernoulli(p),
    logit(p) <- a[g] + bxy*X + bzy*Z[g] + buy*Xbar[g],
    transpars> vector[Ng]:a <<- abar + z*tau,
    z[g] ~ dnorm(0,1),
    c(bxy,buy,bzy) ~ dnorm(0,1),
    abar ~ dnorm(0,1),
    tau ~ dexp(1)
  ) , data=dat , chains=4 , cores=4 , sample=TRUE )

# The Latent Mundlak Machine
mru <- ulam(
  alist(
    # Y model
    Y ~ bernoulli(p),
    logit(p) <- a[g] + bxy*X + bzy*Z[g] + buy*u[g],
    transpars> vector[Ng]:a <<- abar + z*tau,
    # X model
    X ~ normal(mu,sigma),
    mu <- aX + bux*u[g],
    vector[Ng]:u ~ normal(0,1),
    # priors
    z[g] ~ dnorm(0,1),
    c(aX,bxy,buy,bzy) ~ dnorm(0,1),
    bux ~ dexp(1),
    abar ~ dnorm(0,1),
    tau ~ dexp(1),
    sigma ~ dexp(1)
  ) , data=dat , chains=4 , cores=4 , sample=TRUE )

# Reproduce plots on slide 78
post_mf <- extract.samples(mf)
post_mn <- extract.samples(mn)
post_mr <- extract.samples(mr)
post_mrx <- extract.samples(mrx)
post_mru <- extract.samples(mru)

# Plot
dens(post_mf$bxy, xlim = c(0, 3), ylim = c(0, 3), lwd = 2)
dens(post_mn$bxy, col = "gray", lwd = 2, add = TRUE)
dens(post_mr$bxy, col = "red", lwd = 2, add = TRUE)
dens(post_mrx$bxy, col = "blue", lwd = 2, add = TRUE)
dens(post_mru$bxy, col = "green", lwd = 2, add = TRUE)
abline(v = 1, lty = 2)

# Plot
dens(post_mf$bzy, xlim = c(-3, 3), ylim = c(0, 2.5), lwd = 2)
dens(post_mn$bzy, col = "gray", lwd = 2, add = TRUE)
dens(post_mr$bzy, col = "red", lwd = 2, add = TRUE)
dens(post_mrx$bzy, col = "blue", lwd = 2, add = TRUE)
dens(post_mru$bzy, col = "green", lwd = 2, add = TRUE)
abline(v = -0.5, lty = 2)
