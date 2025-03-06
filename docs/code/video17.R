##' Robin Elahi
##' 5 March 2025
##' Code for McElreath video lecture 17 (2023)
##' Measurement and misclassification
##' (skipped misclassification)
 
library(rethinking)
library(dagitty)

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

#### WAFFLE DIVORCE - REVIEW ####
data(WaffleDivorce)
d <- WaffleDivorce 
d$pop_log <- log(d$Population)

# Review causal effects
dag5.1 <- dagitty( "dag{ A -> D; A -> M; M -> D }" )
coordinates(dag5.1) <- list( x=c(A=0,D=1,M=2) , y=c(A=0,D=1,M=0) )
drawdag( dag5.1 )

# What do we need to condition on to estimate:
# causal effect of A on D?
# causal effect of M on D?
adjustmentSets(dag5.1, exposure = "A", outcome = "D")
adjustmentSets(dag5.1, exposure = "M", outcome = "D")

# Prep for OG model
dat <- list(
  D = standardize( d$Divorce ),
  M = standardize( d$Marriage ),
  A = standardize( d$MedianAgeMarriage )
)

## R code 5.10
# Draw the hhDAG!
m5.3 <- ulam(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = dat , chains = 4, cores = 4)
precis( m5.3 )

#### WAFFLE DIVORCE - ERROR ON DIVORCE ####

# Motivational plots
par(mfrow = c(1,2))

# points
plot( d$Divorce ~ d$MedianAgeMarriage , ylim=c(4,15) ,
      xlab="Median age marriage" , ylab="Divorce rate" )
# standard errors
for ( i in 1:nrow(d) ) {
  ci <- d$Divorce[i] + c(-1,1)*d$Divorce.SE[i]
  x <- d$MedianAgeMarriage[i]
  lines( c(x,x) , ci )
}

# points
plot( d$Divorce ~ d$pop_log , ylim=c(4,15) ,
      xlab="Log population" , ylab="Divorce rate" )
# standard errors
for ( i in 1:nrow(d) ) {
  ci <- d$Divorce[i] + c(-1,1)*d$Divorce.SE[i]
  x <- d$pop_log[i]
  lines( c(x,x) , ci )
}

# Start with error on divorce
## R code 15.3
dlist <- list(
  D_obs = standardize( d$Divorce ),
  D_sd = d$Divorce.SE / sd( d$Divorce ),
  M = standardize( d$Marriage ),
  A = standardize( d$MedianAgeMarriage ),
  N = nrow(d)
)

# Draw the hhDAG!
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

#### WAFFLE DIVORCE - ERROR ON DIVORCE AND MARRIAGE ####

dlist2 <- list(
  D_obs = standardize( d$Divorce ),
  D_sd = d$Divorce.SE / sd( d$Divorce ),
  M_obs = standardize( d$Marriage ),
  M_sd = d$Marriage.SE / sd( d$Marriage ),
  A = standardize( d$MedianAgeMarriage ),
  N = nrow(d)
)

# Draw the hhDAG!
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

precis( m5.3 , depth=1 )
precis( m15.1 , depth=1 )
precis( m15.2 , depth=1 )

#### EXTRA PAIR PATERNITY ####



