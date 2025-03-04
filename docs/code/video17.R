##' Robin Elahi
##' 4 March 2025
##' Code for McElreath video lecture 17 (2023)
##' Measurement and misclassification
##' (skipped misclassification)

library(rethinking)

#### PARENT CHILD INCOME ####

# simple parent-child income example
# recall bias on parental income
N <- 500
P <- rnorm(N)
C <- rnorm(N,0*P)
Pstar <- rnorm(N, 0.8*P + 0.2*C )
mCP <- ulam(
  alist(
    C ~ normal(mu,sigma),
    mu <- a + b*P,
    a ~ normal(0,1),
    b ~ normal(0,1),
    sigma ~ exponential(1)
  ) , data=list(P=Pstar,C=C) , chains=4 )
precis(mCP)

#### WAFFLE DIVORCE ####
data(WaffleDivorce)
d <- WaffleDivorce

## R code 15.3
dlist <- list(
  D_obs = standardize( d$Divorce ),
  D_sd = d$Divorce.SE / sd( d$Divorce ),
  M = standardize( d$Marriage ),
  A = standardize( d$MedianAgeMarriage ),
  N = nrow(d)
)

m15.1 <- ulam(
  alist(
    # model for D* (observed)
    D_obs ~ dnorm( D_true , D_sd ),
    # model for D (unobserved)
    vector[N]:D_true ~ dnorm( mu , sigma ),
    mu <- a + bA*A + bM*M,
    a ~ dnorm(0,0.2),
    bA ~ dnorm(0,0.5),
    bM ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ) , data=dlist , chains=4 , cores=4 )

precis( m15.1 , depth=2 )

dlist2 <- list(
  D_obs = standardize( d$Divorce ),
  D_sd = d$Divorce.SE / sd( d$Divorce ),
  M_obs = standardize( d$Marriage ),
  M_sd = d$Marriage.SE / sd( d$Marriage ),
  A = standardize( d$MedianAgeMarriage ),
  N = nrow(d)
)

m15.2 <- ulam(
  alist(
    # D* model (observed)
    D_obs ~ dnorm( D_true , D_sd ),
    # D model (unobserved)
    vector[N]:D_true ~ dnorm( mu , sigma ),
    mu <- a + bA*A + bM*M_true[i],
    a ~ dnorm(0,0.2),
    bA ~ dnorm(0,0.5),
    bM ~ dnorm(0,0.5),
    sigma ~ dexp( 1 ),
    # M* model (observed)
    M_obs ~ dnorm( M_true , M_sd ),
    # M model (unobserved)
    vector[N]:M_true ~ dnorm( nu , tau ),
    nu <- aM + bAM*A,
    aM ~ dnorm(0,0.2),
    bAM ~ dnorm(0,0.5),
    tau ~ dexp( 1 )
  ) , data=dlist2 , chains=4 , cores=4 )

precis( m15.2 , depth=2 ) 

precis( m15.1 , depth=1 )
precis( m15.2 , depth=1 )

#### EXTRA PAIR PATERNITY ####



