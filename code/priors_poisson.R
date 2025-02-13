## Priors for Poisson linear models
## Statistical Modeling
## 2025

##### Preliminaries #####
library(rethinking)

#### Exponentiating the Normal distribution ####

## Normal(0, 10))
n <- 10000
x <- rnorm(n, 0, 10)
par(mfrow = c(1,2))
dens(x)
dens(exp(x))

## Normal(3, 0.5))
x <- rnorm(n, 3, 0.5)
par(mfrow = c(1,2))
dens(x)
dens(exp(x))

## We can use the lognormal to demonstrate the same phenomenon
dev.off()
curve( dlnorm( x , 0 , 10 ) , from=0 , to=100 , n=200, col = "red")
curve( dlnorm( x , 3 , 0.5 ) , from=0 , to=100 , n=200 , add = TRUE, col = "blue")

#### Poisson priors ####

# Default Normal priors that we have been using
set.seed(10)
N <- 100
a <- rnorm( N , 0 , 10 )
b <- rnorm( N , 0 , 1 )
plot( NULL , xlim=c(-2,2) , ylim=c(0,100) )
for ( i in 1:N ) curve( exp( a[i] + b[i]*x ) , add=TRUE , col=grau() )

# Priors from SR2 - play around with a and b
set.seed(10)
N <- 100
a <- rnorm( N , 3 , 0.5 )
b <- rnorm( N , 0 , 0.2 )
plot( NULL , xlim=c(-2,2) , ylim=c(0,100) )
for ( i in 1:N ) curve( exp( a[i] + b[i]*x ) , add=TRUE , col=grau() )
