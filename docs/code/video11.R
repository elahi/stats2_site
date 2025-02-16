##' Robin Elahi
##' 15 Feb 2025
##' Code for McElreath video lecture 11 (2023)
##' Ordered categories

library(rethinking)

#### TROLLEY ####

data(Trolley)
d <- Trolley

dat <- list(
  R = d$response,
  A = d$action,
  I = d$intention,
  C = d$contact
)

mRX <- ulam(
  alist(
    R ~ dordlogit(phi,alpha),
    phi <- bA*A + bI*I + bC*C,
    c(bA,bI,bC) ~ normal(0,0.5),
    alpha ~ normal(0,1)
  ) , data=dat , chains=4 , cores=4 )

precis(mRX, 2)

# plot predictive distributions for each treatment
vals <- c(0,0,0)
Rsim <- mcreplicate( 100 ,
                     sim(mRX,data=list(A=vals[1],I=vals[2],C=vals[3])) ,
                     mc.cores=6 )
simplehist(as.vector(Rsim),lwd=8,col=2,xlab="Response")
mtext(concat("A=",vals[1],", I=",vals[2],", C=",vals[3]))

# total effect of gender
dat$G <- ifelse(d$male==1,2,1)

mRXG <- ulam(
  alist(
    R ~ dordlogit(phi,alpha),
    phi <- bA[G]*A + bI[G]*I + bC[G]*C,
    bA[G] ~ normal(0,0.5),
    bI[G] ~ normal(0,0.5),
    bC[G] ~ normal(0,0.5),
    alpha ~ normal(0,1)
  ) , data=dat , chains=4 , cores=4 )

precis(mRXG, 2)

#### ORDERED PREDICTOR ####

edu_levels <- c( 6 , 1 , 8 , 4 , 7 , 2 , 5 , 3 )
edu_new <- edu_levels[ d$edu ]
dat$E <- edu_new
dat$a <- rep(2,7) # dirichlet prior
mRXE <- ulam(
  alist(
    R ~ ordered_logistic( phi , alpha ),
    phi <- bE*sum( delta_j[1:E] ) +
      bA*A + bI*I + bC*C,
    alpha ~ normal( 0 , 1 ),
    c(bA,bI,bC,bE) ~ normal( 0 , 0.5 ),
    vector[8]: delta_j <<- append_row( 0 , delta ),
    simplex[7]: delta ~ dirichlet( a )
  ), data=dat , chains=4 , cores=4 )

precis(mRXE,2)

dat$Y <- standardize(d$age)
mRXEYGt <- ulam(
  alist(
    R ~ ordered_logistic( phi , alpha ),
    phi <- bE[G]*sum( delta_j[1:E] ) +
      bA[G]*A + bI[G]*I + bC[G]*C +
      bY[G]*Y,
    alpha ~ normal( 0 , 1 ),
    bA[G] ~ normal( 0 , 0.5 ),
    bI[G] ~ normal( 0 , 0.5 ),
    bC[G] ~ normal( 0 , 0.5 ),
    bE[G] ~ normal( 0 , 0.5 ),
    bY[G] ~ normal( 0 , 0.5 ),
    vector[8]: delta_j <<- append_row( 0 , delta ),
    simplex[7]: delta ~ dirichlet( a )
  ), data=dat , chains=4 , cores=4 , threads=2 )

precis(mRXEYGt, 2)
