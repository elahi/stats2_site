##' Robin Elahi
##' 22 Jan 2025
##' Code for McElreath video lecture 5 (2023)
##' Elemental confounds

library(rethinking)

#### SIMULATE CONFOUNDS ####

# FORK example

n <- 1000
Z <- rbern( n , 0.5 )
X <- rbern( n , (1-Z)*0.1 + Z*0.9 )
Y <- rbern( n , (1-Z)*0.1 + Z*0.9 )

table(X,Y)
table(X,Y,Z)
cor(X,Y)
cor(X[Z==0],Y[Z==0])
cor(X[Z==1],Y[Z==1])

round(table(X[Z==1],Y[Z==1])/sum(Z==1),2)

# PIPE example

n <- 1000
X <- rbern( n , 0.5)
Z <- rbern( n , (1-X)*0.1 + X*0.9 )
Y <- rbern( n , (1-Z)*0.1 + Z*0.9 )

table(X,Y)
table(X,Y,Z)
cor(X,Y)
cor(X[Z==0],Y[Z==0])
cor(X[Z==1],Y[Z==1])

# COLLIDER example

n <- 1000
X <- rbern( n , 0.5 )
Y <- rbern( n , 0.5 )
Z <- rbern( n , ifelse(X+Y>0,0.9,0.2) )

table(X,Y)

table(X,Y,Z)

cor(X,Y)
cor(X[Z==0],Y[Z==0])
cor(X[Z==1],Y[Z==1])

# DESCENDENT example

n <- 1000
X <- rbern( n , 0.5 )
Z <- rbern( n , (1-X)*0.1 + X*0.9 )
Y <- rbern( n , (1-Z)*0.1 + Z*0.9 )
A <- rbern( n , (1-Z)*0.1 + Z*0.9 )

table(X,Y)

table(X,Y,A)

cor(X,Y)
cor(X[A==0],Y[A==0])
cor(X[A==1],Y[A==1])

#### D-SEP PLOTS ####

a <- 0.7
cols <- c( col.alpha(4,a) , col.alpha(2,a) )

# FORK

cols <- c(4,2)

N <- 300
Z <- rbern(N)
X <- rnorm(N,2*Z-1)
Y <- rnorm(N,(2*Z-1))

plot( X , Y , col=cols[Z+1] , lwd=3 )

abline(lm(Y[Z==1]~X[Z==1]),col="white",lwd=5)
abline(lm(Y[Z==1]~X[Z==1]),col=2,lwd=3)

abline(lm(Y[Z==0]~X[Z==0]),col="white",lwd=5)
abline(lm(Y[Z==0]~X[Z==0]),col=4,lwd=3)

abline(lm(Y~X),lwd=5,col="white")
abline(lm(Y~X),lwd=3)

# PIPE

N <- 300
X <- rnorm(N)
Z <- rbern(N,inv_logit(X))
Y <- rnorm(N,(2*Z-1))

plot( X , Y , col=cols[Z+1] , lwd=3 )

abline(lm(Y[Z==1]~X[Z==1]),col="white",lwd=5)
abline(lm(Y[Z==1]~X[Z==1]),col=2,lwd=3)

abline(lm(Y[Z==0]~X[Z==0]),col="white",lwd=5)
abline(lm(Y[Z==0]~X[Z==0]),col=4,lwd=3)

abline(lm(Y~X),lwd=5,col="white")
abline(lm(Y~X),lwd=3)

# COLLIDER

N <- 300
X <- rnorm(N)
Y <- rnorm(N)
Z <- rbern(N,inv_logit(2*X+2*Y-2))

plot( X , Y , col=cols[Z+1] , lwd=3 )

abline(lm(Y[Z==1]~X[Z==1]),col="white",lwd=5)
abline(lm(Y[Z==1]~X[Z==1]),col=2,lwd=3)

abline(lm(Y[Z==0]~X[Z==0]),col="white",lwd=5)
abline(lm(Y[Z==0]~X[Z==0]),col=4,lwd=3)

abline(lm(Y~X),lwd=5,col="white")
abline(lm(Y~X),lwd=3)

#### EMPIRICAL-FORK ####

# DIVORCE EXAMPLE

library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

d$WHpm <- (d$WaffleHouses/d$Population)

par(mfrow = c(2,2))
plot( d$WHpm , d$Divorce , col=2 , lwd=3 , xlab="Waffle Houses per million" , ylab="Divorce rate" )
#identify( d$WHpm , d$Divorce , d$Loc , cex=0.8 )

m <- lm( Divorce ~ WHpm , d )
post <- extract.samples(m)
head(post)
xseq <- seq(from=-5,to=45,len=30)
mu <- sapply( xseq , function(x) post$Intercept + post$WHpm*x )
shade( apply(mu,2,PI) , xseq )

plot( d$Marriage , d$Divorce , col=ifelse(d$South==1,2,4) , lwd=3 , xlab="Marriage rate" , ylab="Divorce rate" )
#identify( d$Marriage , d$Divorce , d$Loc , cex=0.8 )

plot( d$MedianAgeMarriage , d$Divorce , col=ifelse(d$South==1,2,4) , lwd=3 , xlab="Median age of marriage" , ylab="Divorce rate" )
#identify( d$MedianAgeMarriage , d$Divorce , d$Loc , cex=0.8 )

plot( d$MedianAgeMarriage , d$Marriage , col=ifelse(d$South==1,2,4) , lwd=3 , xlab="Median age of marriage" , ylab="Marriage rate" )
#identify( d$MedianAgeMarriage , d$Marriage , d$Loc , cex=0.8 )

# explain stratifying by continuous variable
dev.off()
plot( sort( d$MedianAgeMarriage ) , 1:50 , yaxt="n" , lwd=3 , col=2 , ylab="" , xlab="Median age of marriage" )

div <- 2
divs <- seq(from=0,to=1,len=div+1)
for ( i in 1:(length(divs)) ) abline( v=quantile(d$MedianAgeMarriage,divs[i]) , lwd=2 )

# visualize standardizing
par(mfrow = c(1,2))
plot( d$MedianAgeMarriage , d$Divorce , col=ifelse(d$South==1,2,4) , lwd=3 , 
      xlab="Median age of marriage" , ylab="Marriage rate" )
plot( standardize(d$MedianAgeMarriage) , standardize(d$Divorce) , col=ifelse(d$South==1,2,4) , lwd=3 , 
      xlab="Median age of marriage (standardized)" , ylab="Divorce rate (standardized)" )
dev.off()

# prior predictive simulation
n <- 20
a <- rnorm(n,0,10)
bM <- rnorm(n,0,10)
bA <- rnorm(n,0,10)

plot( NULL , xlim=c(-2,2) , ylim=c(-2,2) , xlab="Median age of marriage (standardized)" , ylab="Divorce rate (standardized)" )
Aseq <- seq(from=-3,to=3,len=30)
for ( i in 1:n ) {
  mu <- a[i] + bA[i]*Aseq
  lines( Aseq , mu , lwd=2 , col=2 )
}

# better priors
n <- 20
a <- rnorm(n,0,0.2)
bM <- rnorm(n,0,0.5)
bA <- rnorm(n,0,0.5)
plot( NULL , xlim=c(-2,2) , ylim=c(-2,2) , xlab="Median age of marriage (standardized)" , ylab="Divorce rate (standardized)" )
Aseq <- seq(from=-3,to=3,len=30)
for ( i in 1:n ) {
  mu <- a[i] + bA[i]*Aseq
  lines( Aseq , mu , lwd=2 , col=2 )
}

# model
dat <- list(
  D = standardize(d$Divorce),
  M = standardize(d$Marriage),
  A = standardize(d$MedianAgeMarriage)
)

m_DMA <- quap(
  alist(
    D ~ dnorm(mu,sigma),
    mu <- a + bM*M + bA*A,
    a ~ dnorm(0,0.2),
    bM ~ dnorm(0,0.5),
    bA ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ) , data=dat )

plot(precis(m_DMA))

# posterior predictions
post <- extract.samples( m_DMA )
# sample A from data
n <- 1e3
As <- sample(dat$A,size=n,replace=TRUE)
# simulate D for M=0 (sample mean)
DM0 <- with( post ,
             rnorm(n, a + bM*0 + bA*As , sigma ) )
# simulate D for M=1 (+1 standard deviation)
# use the *same* A values
DM1 <- with( post ,
             rnorm(n, a + bM*1 + bA*As , sigma ) )
# contrast
M10_contrast <- DM1 - DM0
dens(M10_contrast,lwd=4,col=2,xlab="effect of 1sd
increase in M" )


#### EMPIRICAL-PIPE ####

# plant fungus (from SR2)

set.seed(71)
# number of plants
N <- 100

# simulate initial heights
h0 <- rnorm(N,10,2)

# assign treatments and simulate fungus and growth
treatment <- rep( 0:1 , each=N/2 )
fungus <- rbinom( N , size=1 , prob=0.5 - treatment*0.4 )
h1 <- h0 + rnorm(N, 5 - 3*fungus)

# compose a clean data frame
d <- data.frame( h0=h0 , h1=h1 , treatment=treatment , fungus=fungus )
precis(d)

## R code 6.14
sim_p <- rlnorm( 1e4 , 0 , 0.25 )
precis( data.frame(sim_p) )

## R code 6.15
m6.6 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ),
    mu <- h0*p,
    p ~ dlnorm( 0 , 0.25 ),
    sigma ~ dexp( 1 )
  ), data=d )
precis(m6.6)

## R code 6.16
m6.7 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ),
    mu <- h0 * p,
    p <- a + bt*treatment + bf*fungus,
    a ~ dlnorm( 0 , 0.2 ) ,
    bt ~ dnorm( 0 , 0.5 ),
    bf ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 )
  ), data=d )
precis(m6.7)

## R code 6.17
m6.8 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ),
    mu <- h0 * p,
    p <- a + bt*treatment,
    a ~ dlnorm( 0 , 0.2 ),
    bt ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 )
  ), data=d )
precis(m6.8)

## R code 6.18
library(dagitty)
plant_dag <- dagitty( "dag {
    H_0 -> H_1
    F -> H_1
    T -> F
}")
coordinates( plant_dag ) <- list( x=c(H_0=0,T=2,F=1.5,H_1=1) ,
                                  y=c(H_0=0,T=0,F=0,H_1=0) )
drawdag( plant_dag )

## R code 6.19
impliedConditionalIndependencies(plant_dag)

## R code 6.20
set.seed(71)
N <- 1000
h0 <- rnorm(N,10,2)
treatment <- rep( 0:1 , each=N/2 )
M <- rbern(N)
fungus <- rbinom( N , size=1 , prob=0.5 - treatment*0.4 + 0.4*M )
h1 <- h0 + rnorm( N , 5 + 3*M )
d2 <- data.frame( h0=h0 , h1=h1 , treatment=treatment , fungus=fungus )

#### EMPIRICAL-COLLIDER ####

# grant applications (from SR2, page 161)

set.seed(1914)
N <- 200 # num grant proposals
p <- 0.1 # proportion to select
# uncorrelated newsworthiness and trustworthiness 
nw <- rnorm(N)
tw <- rnorm(N)
# select top 10% of combined scores
s<-nw+tw #totalscore
q <- quantile( s , 1-p ) # top 10% threshold 
selected <- ifelse( s >= q , TRUE , FALSE )
cor( tw[selected] , nw[selected] )

plot( nw , tw , xlab="Newsworthiness" , ylab="Trustworthiness" , col=2 , lwd=3 )

plot( nw , tw , xlab="Newsworthiness" , ylab="Trustworthiness" , lwd=3 , col=ifelse(selected==TRUE,2,"gray") )
abline( lm(tw[selected]~nw[selected]), lwd=3  )


# age and happiness (from SR2, page 176)

d <- sim_happiness( seed=1977 , N_years=1000 )
precis(d)

## R code 6.22
d2 <- d[ d$age>17 , ] # only adults
d2$A <- ( d2$age - 18 ) / ( 65 - 18 )

## R code 6.23
d2$mid <- d2$married + 1
m6.9 <- quap(
  alist(
    happiness ~ dnorm( mu , sigma ),
    mu <- a[mid] + bA*A,
    a[mid] ~ dnorm( 0 , 1 ),
    bA ~ dnorm( 0 , 2 ),
    sigma ~ dexp(1)
  ) , data=d2 )
precis(m6.9,depth=2)

## R code 6.24
m6.10 <- quap(
  alist(
    happiness ~ dnorm( mu , sigma ),
    mu <- a + bA*A,
    a ~ dnorm( 0 , 1 ),
    bA ~ dnorm( 0 , 2 ),
    sigma ~ dexp(1)
  ) , data=d2 )
precis(m6.10)

# children, grandparents, education (from sr2, page 180)
## R code 6.25
N <- 200  # number of grandparent-parent-child triads
b_GP <- 1 # direct effect of G on P
b_GC <- 0 # direct effect of G on C
b_PC <- 1 # direct effect of P on C
b_U <- 2  # direct effect of U on P and C

## R code 6.26
set.seed(1)
U <- 2*rbern( N , 0.5 ) - 1
G <- rnorm( N )
P <- rnorm( N , b_GP*G + b_U*U )
C <- rnorm( N , b_PC*P + b_GC*G + b_U*U )
d <- data.frame( C=C , P=P , G=G , U=U )

## R code 6.27
m6.11 <- quap(
  alist(
    C ~ dnorm( mu , sigma ),
    mu <- a + b_PC*P + b_GC*G,
    a ~ dnorm( 0 , 1 ),
    c(b_PC,b_GC) ~ dnorm( 0 , 1 ),
    sigma ~ dexp( 1 )
  ), data=d )
precis(m6.11)

## R code 6.28
m6.12 <- quap(
  alist(
    C ~ dnorm( mu , sigma ),
    mu <- a + b_PC*P + b_GC*G + b_U*U,
    a ~ dnorm( 0 , 1 ),
    c(b_PC,b_GC,b_U) ~ dnorm( 0 , 1 ),
    sigma ~ dexp( 1 )
  ), data=d )
precis(m6.12)

#### MORE DAGS ####

# confronting confounding (from SR2, page 183)

## R code 6.29
library(dagitty)
dag_6.1 <- dagitty( "dag {
    U [unobserved]
    X -> Y
    X <- U <- A -> C -> Y
    U -> B <- C
}")
adjustmentSets( dag_6.1 , exposure="X" , outcome="Y" )

## R code 6.30
library(dagitty)
dag_6.2 <- dagitty( "dag {
    A -> D
    A -> M -> D
    A <- S -> M
    S -> W -> D
}")
adjustmentSets( dag_6.2 , exposure="W" , outcome="D" )

## R code 6.31
impliedConditionalIndependencies( dag_6.2 )
