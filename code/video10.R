##' Robin Elahi
##' 8 Feb 2025
##' Code for McElreath video lecture 10 (2023)
##' Hidden counts and confounds

library(rethinking)

#### GENERATE ADMISSIONS - ability ####

set.seed(12)
N <- 2000 # number of applicants
# even gender distribution
G <- sample( 1:2 , size=N , replace=TRUE )
# sample ability, high (1) to average (0)
u <- rbern(N,0.1)
# gender 1 tends to apply to department 1, 2 to 2
# and G=1 with greater ability tend to apply to 2 as well
D <- rbern( N , ifelse( G==1 , u*1 , 0.75 ) ) + 1
# matrix of acceptance rates [dept,gender]
p_u0 <- matrix( c(0.1,0.1,0.1,0.3) , nrow=2 )
p_u1 <- matrix( c(0.3,0.3,0.5,0.5) , nrow=2 )
p_u <- list( p_u0 , p_u1 )
# simulate acceptance
p <- sapply( 1:N , function(i) p_u[[1+u[i]]][D[i],G[i]] )
A <- rbern( N , p )

table(G, D, u)
p_u0
p_u1

# Model
dat_sim <- list( A=A , D=D , G=G )
# total effect gender
m1 <- ulam(
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G],
    a[G] ~ normal(0,1)
  ), data=dat_sim , chains=4 , cores=4 )
# direct effects - now confounded!
m2 <- ulam(
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G,D],
    matrix[G,D]:a ~ normal(0,1)
  ), data=dat_sim , chains=4 , cores=4 )

precis(m1, depth = 3)
precis(m2, depth = 3)

# Plot
post2 <- extract.samples(m2)
dens(inv_logit(post2$a[,1,1]),lwd=3,col=4,xlim=c(0,0.5),xlab="probability admission")
dens(inv_logit(post2$a[,2,1]),lwd=3,col=4,lty=2,add=TRUE)
dens(inv_logit(post2$a[,1,2]),lwd=3,col=2,add=TRUE)
dens(inv_logit(post2$a[,2,2]),lwd=3,col=2,lty=2,add=TRUE)

#### Next sim ####

datl <- dat_sim
datl$D2 <- ifelse(datl$D==2,1,0)
datl$N <- length(datl$D)
datl$b <- c(1,1)
datl$g <- c(1,0)

mGDu <- ulam(
  alist(
    # A model
    A ~ bernoulli(p),
    logit(p) <- a[G,D] + b[G]*u[i],
    matrix[G,D]:a ~ normal(0,1),
    # D model
    D2 ~ bernoulli(q),
    logit(q) <- delta[G] + g[G]*u[i],
    delta[G] ~ normal(0,1),
    # declare unobserved u
    vector[N]:u ~ normal(0,1)
  ), data=datl , chains=4 , cores=4 ) 

#### TOOLS ####
data(Kline)
d <- Kline
d$P <- scale( log(d$population) )
d$contact_id <- ifelse( d$contact=="high" , 2 , 1 )
dat <- list(
  T = d$total_tools ,
  P = d$P ,
  C = d$contact_id )

# intercept only
m11.9 <- ulam(
  alist(
    T ~ dpois( lambda ),
    log(lambda) <- a,
    a ~ dnorm( 3 , 0.5 )
  ), data=dat , chains=4 , log_lik=TRUE )

# interaction model
m11.10 <- ulam(
  alist(
    T ~ dpois( lambda ),
    log(lambda) <- a[C] + b[C]*P,
    a[C] ~ dnorm( 3 , 0.5 ),
    b[C] ~ dnorm( 0 , 0.2 )
  ), data=dat , chains=4 , log_lik=TRUE )

compare( m11.9 , m11.10 , func=PSIS )

## Plot

k <- PSIS( m11.10 , pointwise=TRUE )$k
plot( dat$P , dat$T , xlab="log population (std)" ,
      ylab="total tools" ,
      col=ifelse( dat$C==1 , 4 , 2 ) , lwd=4+4*normalize(k) ,
      ylim=c(0,75) , cex=1+normalize(k) )
# set up the horizontal axis values to compute predictions at
P_seq <- seq( from=-1.4 , to=3 , len=100 )
# predictions for C=1 (low contact)
lambda <- link( m11.10 , data=data.frame( P=P_seq , C=1 ) )
lmu <- apply( lambda , 2 , mean )
lci <- apply( lambda , 2 , PI )
lines( P_seq , lmu , lty=2 , lwd=1.5 )
shade( lci , P_seq , xpd=TRUE , col=col.alpha(4,0.3) )
# predictions for C=2 (high contact)
lambda <- link( m11.10 , data=data.frame( P=P_seq , C=2 ) )
lmu <- apply( lambda , 2 , mean )
lci <- apply( lambda , 2 , PI )
lines( P_seq , lmu , lty=1 , lwd=1.5 )
shade( lci , P_seq , xpd=TRUE , col=col.alpha(2,0.3))

#### TOOLS - SCIENTIFIC MODEL ####

f <- function(a=0.02,b=0.5,g=0.2,P=1e4,t_max=50) {
  T <- rep(0,t_max)
  for ( i in 2:t_max )
    T[i] <- T[i-1] + a*P^b - g*T[i-1]
  return(T)
}

plot( NULL , xlim=c(0,50) , ylim=c(0,10) ,
      xlab="time" , ylab="Tools" )
T <- f(P=1e3)
lines( 1:50 , T , lwd=3 , col=2 )
T <- f(P=1e4)
lines( 1:50 , T , lwd=3 , col=2 )

# innovation/loss model
dat2 <- list( T=d$total_tools, P=d$population,
              C=d$contact_id )

m11.11 <- ulam(
  alist(
    T ~ dpois( lambda ),
    lambda <- exp(a[C])*P^b[C]/g,
    a[C] ~ dnorm(1,1),
    b[C] ~ dexp(1),
    g ~ dexp(1)
  ), data=dat2 , chains=4 , cores=4 )

precis(m11.11, 2)

#### BONUS ####

# Fork
cols <- c(4,2)
N <- 300
Z <- rbern(N)
X <- rnorm(N,2*Z-1)
Y <- rnorm(N,2*Z-1)
plot( X , Y , col=cols[Z+1] , lwd=3 )
abline(lm(Y[Z==1]~X[Z==1]),col=2,lwd=3)
abline(lm(Y[Z==0]~X[Z==0]),col=4,lwd=3)
abline(lm(Y~X),lwd=3)

# Pipe
cols <- c(4,2)
N <- 300
X <- rnorm(N)
Z <- rbern(N,inv_logit(X))
Y <- rnorm(N,(2*Z-1))
plot( X , Y , col=cols[Z+1] , lwd=3 )
abline(lm(Y[Z==1]~X[Z==1]),col=2,lwd=3)
abline(lm(Y[Z==0]~X[Z==0]),col=4,lwd=3)
abline(lm(Y~X),lwd=3)

# Collider
cols <- c(4,2)
N <- 300
X <- rnorm(N)
Y <- rnorm(N)
Z <- rbern(N,inv_logit(2*X+2*Y-2))
plot( X , Y , col=cols[Z+1] , lwd=3 )
abline(lm(Y[Z==1]~X[Z==1]),col=2,lwd=3)
abline(lm(Y[Z==0]~X[Z==0]),col=4,lwd=3)
abline(lm(Y~X),lwd=3)

# Haunting
# base rate differences erasing effect of X
N <- 1000
X <- rnorm(N)
Z <- rbern(N)
p <- inv_logit(X + ifelse(Z==1, -1,5))
Y <- rbern(N,p)

mX <- quap(
  alist(
    Y ~ dbern(p),
    logit(p) <- a + b*X,
    a ~ dnorm(0,1),
    b ~ dnorm(0,1)
  ) , data=list(Y=Y,X=X) )

mXZ <- quap(
  alist(
    Y ~ dbern(p),
    logit(p) <- a + b[Z]*X,
    a ~ dnorm(0,1),
    b[Z] ~ dnorm(0,1)
  ) , data=list(Y=Y,X=X,Z=Z+1) )

mXZ2 <- quap(
  alist(
    Y ~ dbern(p),
    logit(p) <- a[Z] + b[Z]*X,
    a[Z] ~ dnorm(0,1),
    b[Z] ~ dnorm(0,1)
  ) , data=list(Y=Y,X=X,Z=Z+1) )

