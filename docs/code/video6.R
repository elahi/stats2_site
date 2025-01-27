##' Robin Elahi
##' 27 Jan 2025
##' Code for McElreath video lecture 6 (2023)
##' Good controls, bad controls

library(rethinking)

#### OPENING LECTURE EXAMPLE ####

# simulate confounded Y
N <- 200
b_XY <- 0
b_UY <- -1
b_UZ <- -1
b_ZX <- 1
set.seed(10)
U <- rbern(N)
Z <- rnorm(N,b_UZ*U)
X <- rnorm(N,b_ZX*Z)
Y <- rnorm(N,b_XY*X+b_UY*U)
d <- list(Y=Y,X=X,Z=Z)

str(d)
pairs(d)

# ignore U,Z
m_YX <- quap(
  alist(
    Y ~ dnorm( mu , sigma ),
    mu <- a + b_XY*X,
    a ~ dnorm( 0 , 1 ),
    b_XY ~ dnorm( 0 , 1 ),
    sigma ~ dexp( 1 )
  ), data=d )

# stratify by Z
m_YXZ <- quap(
  alist(
    Y ~ dnorm( mu , sigma ),
    mu <- a + b_XY*X + b_Z*Z,
    a ~ dnorm( 0 , 1 ),
    c(b_XY,b_Z) ~ dnorm( 0 , 1 ),
    sigma ~ dexp( 1 )
  ), data=d )

post <- extract.samples(m_YX)
post2 <- extract.samples(m_YXZ)
dens(post$b_XY,lwd=3,col=1,xlab="posterior
b_XY",xlim=c(-0.3,0.3))
dens(post2$b_XY,lwd=3,col=2,add=TRUE)

# coefficient on X is wrong
precis(m_YX) 

# coefficient on X is correct; coefficient on Z means nothing; table 2 fallacy
precis(m_YXZ) 


#### POST-TREATMENT VARIABLE ####

# simulate confounding by post-treatment variable

f <- function(n=100,bXZ=1,bZY=1) {
  X <- rnorm(n)
  u <- rnorm(n)
  Z <- rnorm(n, bXZ*X + u)
  Y <- rnorm(n, bZY*Z + u )
  bX <- coef( lm(Y ~ X) )['X']
  bXZ <- coef( lm(Y ~ X + Z) )['X']
  return( c(bX,bXZ) )
}

## Case 1
sim <- mcreplicate( 1e4 , f() , mc.cores=8 )
str(sim)
dens( sim[1,] , lwd=3 , xlab="posterior mean" , xlim=c(-1, 2) , ylim=c(0,2.7)  )
dens( sim[2,] , lwd=3 , col=2 , add=TRUE )

## Case 2
sim <- mcreplicate( 1e4 , f(bZY=0) , mc.cores=8 )
str(sim)
dens( sim[1,] , lwd=3 , xlab="posterior mean" , xlim=c(-1, 1) , ylim=c(0, 3)  )
dens( sim[2,] , lwd=3 , col=2 , add=TRUE )

#### CASE CONTROL BIAS ####

# simulate case control bias

f <- function(n=100,bXY=1,bYZ=1) {
  X <- rnorm(n)
  Y <- rnorm(n, bXY*X )
  Z <- rnorm(n, bYZ*Y )
  bX <- coef( lm(Y ~ X) )['X']
  bXZ <- coef( lm(Y ~ X + Z) )['X']
  return( c(bX,bXZ) )
}

sim <- mcreplicate( 1e4 , f() , mc.cores=8 )

dens( sim[1,] , lwd=3 , xlab="posterior mean" , xlim=c(0,1.5) , ylim=c(0,5)  )
dens( sim[2,] , lwd=3 , col=2 , add=TRUE )

#### PRECISION PARASITE ####

# simulate precision parasite

f <- function(n=100,bZX=1,bXY=1) {
  Z <- rnorm(n)
  X <- rnorm(n, bZX*Z )
  Y <- rnorm(n, bXY*X )
  bX <- coef( lm(Y ~ X) )['X']
  bXZ <- coef( lm(Y ~ X + Z) )['X']
  return( c(bX,bXZ) )
}

sim <- mcreplicate( 1e4 , f(n=50) , mc.cores=8 )

dens( sim[1,] , lwd=3 , xlab="posterior mean" , xlim=c(0.5,1.5) , ylim=c(0,4.5)  )
dens( sim[2,] , lwd=3 , col=2 , add=TRUE )

#### BIAS AMPLIFIER ####

# simulate bias amplifier

f <- function(n=100,bZX=1,bXY=1) {
  Z <- rnorm(n)
  u <- rnorm(n)
  X <- rnorm(n, bZX*Z + u )
  Y <- rnorm(n, bXY*X + u )
  bX <- coef( lm(Y ~ X) )['X']
  bXZ <- coef( lm(Y ~ X + Z) )['X']
  return( c(bX,bXZ) )
}

sim <- mcreplicate( 1e4 , f(bXY=0) , mc.cores=8 )

dens( sim[1,] , lwd=3 , xlab="posterior mean" , xlim=c(-0.5,1) , ylim=c(0,5.5)  )
dens( sim[2,] , lwd=3 , col=2 , add=TRUE )

## Visualize bias amplification

abline_w <- function(...,col=1,lwd=1,dlwd=2) {
  abline(...,col="white",lwd=lwd+dlwd)
  abline(...,col=col,lwd=lwd)
}

n <- 1000
Z <- rbern(n)
u <- rnorm(n)
X <- rnorm(n, 7*Z + u )
Y <- rnorm(n, 0*X + u )

cols <- c( col.alpha(2,0.5) , col.alpha(4,0.5) )
plot( X , Y  , col=cols[Z+1] , lwd=2 )

abline_w( lm(Y~X) , lwd=3 )

abline_w( lm(Y[Z==1]~X[Z==1]) , lwd=3 , col=4 )

abline_w( lm(Y[Z==0]~X[Z==0]) , lwd=3 , col=2 )


