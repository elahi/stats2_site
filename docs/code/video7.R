##' Robin Elahi
##' 28 Jan 2025
##' Code for McElreath video lecture 7 (2023)
##' Overfitting

library(rethinking)

#### PLANT - FUNGUS ####

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

plot( jitter(d$fungus) , d$h1/d$h0 , col=ifelse(d$treatment==1,2,4) , lwd=3 , xaxt="n" , xlab="" , ylab="growth" )
axis( 1 , at=c(0,1) , labels=c("no fungus","yo fungus") )

plot( jitter(d$treatment) , d$h1/d$h0 , col=ifelse(d$fungus==1,2,4) , lwd=3 , xaxt="n" , xlab="" , ylab="growth" )
axis( 1 , at=c(0,1) , labels=c("control","treatment") )

abline(lm( I(d$h1/d$h0)[d$fungus==1] ~ treatment[d$fungus==1] ))
abline(lm( I(d$h1/d$h0)[d$fungus==0] ~ treatment[d$fungus==0] ))

m6.6 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ),
    mu <- h0*p,
    p ~ dlnorm( 0 , 0.25 ),
    sigma ~ dexp( 1 )
  ), data=d )

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

m6.8 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ),
    mu <- h0 * p,
    p <- a + bt*treatment,
    a ~ dlnorm( 0 , 0.2 ),
    bt ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 )
  ), data=d )

plot(precis(m6.7,pars=c("bt","bf")))

plot(precis(m6.8,pars=c("bt")))

post <- extract.samples(m6.7)
dens( post$bt , lwd=4 , xlab="effect of treatment (posterior)" , xlim=c(-0.15,0.2) )
post <- extract.samples(m6.8)
dens( post$bt , lwd=4 , col=2 , add=TRUE )
abline( v=0 , lwd=2 , lty=3 )

compare( m6.7 , m6.8 , func=PSIS )

plot(compare( m6.7 , m6.8 , func=PSIS ))


#### OUTLIERS - WAFFLE DIVORCE ####

data(WaffleDivorce)
d <- WaffleDivorce
d$A <- standardize( d$MedianAgeMarriage )
d$D <- standardize( d$Divorce )
d$M <- standardize( d$Marriage )

m5.1 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bA * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

m5.2 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM * M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

m5.3 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

compare( m5.1 , m5.2 , m5.3 , func=PSIS )

PSIS_m5.3 <- PSIS(m5.3,pointwise=TRUE)
set.seed(24071847)
WAIC_m5.3 <- WAIC(m5.3,pointwise=TRUE)

plot( PSIS_m5.3$k , WAIC_m5.3$penalty , xlab="PSIS Pareto k" ,
      ylab="WAIC penalty" , col=2 , lwd=3 , cex=1.5 )

identify( PSIS_m5.3$k , WAIC_m5.3$penalty , d$Location )

plot( d$A , d$D , col=ifelse(d$Loc %in% c("ME","ID"),2,grau(0.7)) , lwd=3 , cex=1.5 , 
      xlab="Age at marriage (std)" , ylab="Divorce rate (std)" )

identify( d$A , d$D , d$Location )

#### WAFFLE DIVORCE - STUDENT T REGRESSION ####

# sim student t

curve( dnorm( x , 0 , 0.5 ) , from=-7 , to=7 , lwd=4 , xlab="value" , ylab="density" , col=grau(0.3) )
for ( i in 1:10 ) curve( dnorm( x , 0 , runif(1,0.5,2) ) , add=TRUE , lwd=4 , col=grau(0.3) )

x <- rnorm( 1e4 , 0 , runif(1e4,0.5,2) )
dens(x)
curve(dnorm(x,0,1),add=TRUE,lwd=3,col=2)

curve( dnorm( x , 0 , 1) , from=-7 , to=7 , lwd=4 , xlab="value" , ylab="density" )
curve( dstudent( x , 2 , 0 , 1 ) , add=TRUE , col=2 , lwd=4 )

y <- rstudent( 1e4 , 2 , 0 , 1 )
dens( y , xlim=c(-7,7) , lwd=4 )

# model

data(WaffleDivorce)
d <- WaffleDivorce
dat <- list(
  A = standardize( d$MedianAgeMarriage ),
  D = standardize( d$Divorce ),
  M = standardize( d$Marriage )
)

m5.3 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = dat )

m5.3t <- quap(
  alist(
    D ~ dstudent( 2 , mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = dat )

post <- extract.samples(m5.3)
dens( post$bA , xlab="bA (effect of age of marriage)" , lwd=4 , ylim=c(0,3) )
post <- extract.samples(m5.3t)
dens( post$bA , add=TRUE , lwd=4 , col=2 )


muG <- link(m5.3)
muT <- link(m5.3t)
muG_mean <- apply( muG , 2, mean )
muT_mean <- apply( muT , 2, mean )

plot( muG_mean , muT_mean , col=ifelse(d$Loc %in% c("ME","ID"),2,grau(0.7)) , 
      lwd=3 , cex=1.5 , xlab="Predicted divorce (Gaussian)" , 
      ylab="Predicted divorce (Student-t)" , xlim=c(-2,2) , ylim=c(-2,2) )
abline(a=0,b=1,lty=3,lwd=2)



