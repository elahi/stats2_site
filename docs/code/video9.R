##' Robin Elahi
##' 5 Feb 2025
##' Code for McElreath video lecture 9 (2023)
##' Modeling events

library(rethinking)

#### GENERATE ADMISSIONS ####

# generative model, basic mediator scenario
N <- 1000 # number of applicants
# even gender distribution
G <- sample( 1:2 , size=N , replace=TRUE )
# gender 1 tends to apply to department 1, 2 to 2
D <- rbern( N , ifelse( G==1 , 0.3 , 0.8 ) ) + 1
# matrix of acceptance rates [dept,gender]
accept_rate <- matrix( c(0.1,0.3,0.1,0.3) , nrow=2 )
# simulate acceptance
A <- rbern( N , accept_rate[D,G] )
accept_rate

table(G,D)
table(G,A)

#### logit ####

## Sim 1
a <- rnorm(1e4,0,10)
b <- rnorm(1e4,0,10)
xseq <- seq(from=-3,to=3,len=100)
p <- sapply( xseq , function(x)
  inv_logit(a+b*x) )
plot( NULL , xlim=c(-2.5,2.5) , ylim=c(0,1) ,
      xlab="x value" , ylab="probability" )
for ( i in 1:10 ) lines( xseq , p[i,] , lwd=3 ,
                         col=2 )

## Sim 2
a <- rnorm(1e4,0,1.5)
b <- rnorm(1e4,0,0.5)
xseq <- seq(from=-3,to=3,len=100)
p <- sapply( xseq , function(x)
  inv_logit(a+b*x) )
plot( NULL , xlim=c(-2.5,2.5) , ylim=c(0,1) ,
      xlab="x value" , ylab="probability" )
for ( i in 1:10 ) lines( xseq , p[i,] , lwd=3 ,
                         col=2 )

#### MODEL GENERATED ADMISSIONS ####

## Total effect
dat_sim <- list( A=A , D=D , G=G )

m1 <- ulam(
    alist(
        A ~ bernoulli(p),
        logit(p) <- a[G],
        a[G] ~ normal(0,1)
      ), data=dat_sim , chains=4 , cores=4 )
precis(m1, depth = 2)

## Direct effect
m2 <- ulam(
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G, D],
    matrix[G,D]:a ~ normal(0,1)
  ), data=dat_sim , chains=4 , cores=4 )

precis(m2, depth = 3)

inv_logit(coef(m2))

## Aggregate data
dat_sim2 <- aggregate( A ~ G + D , dat_sim , sum )
dat_sim2$N <- aggregate( A ~ G + D , dat_sim , length )$A
dat_sim2

## Use binomial on aggregated data
m2_bin <- ulam(
  alist(
    A ~ binomial(N,p),
    logit(p) <- a[G,D],
    matrix[G,D]:a ~ normal(0,1)
  ), data=dat_sim2 , chains=4 , cores=4 )
precis(m2_bin, depth = 3)

#### MODEL UBC ADMISSIONS ####

data(UCBadmit)
d <- UCBadmit
dat <- list(
  A = d$admit,
  N = d$applications,
  G = ifelse(d$applicant.gender=="female",1,2),
  D = as.integer(d$dept)
)

# total effect gender
mG <- ulam(
  alist(
    A ~ binomial(N,p),
    logit(p) <- a[G],
    a[G] ~ normal(0,1)
  ), data=dat , chains=4 , cores=4 )
traceplot(mG)
trankplot(mG)
precis(mG, depth = 2)

post1 <- extract.samples(mG)
PrA_G1 <- inv_logit( post1$a[,1] )
PrA_G2 <- inv_logit( post1$a[,2] )
diff_prob <- PrA_G1 - PrA_G2
dev.off()
dens(diff_prob,lwd=4,col=2,xlab="Gender contrast (probability)")

# direct effect gender
# direct effects
mGD <- ulam(
  alist(
      A ~ binomial(N,p),
      logit(p) <- a[G,D],
      matrix[G,D]:a ~ normal(0,1)
    ), data=dat , chains=4 , cores=4 )
traceplot(mGD)
trankplot(mGD)
precis(mGD, depth = 3)

post2 <- extract.samples(mGD)
PrA <- inv_logit( post2$a )
diff_prob_D_ <- sapply( 1:6 , function(i) PrA[,1,i] - PrA[,2,i] )
plot(NULL,xlim=c(-0.2,0.3),ylim=c(0,25),xlab="Gender contrast (probability)",ylab="Density")
for ( i in 1:6 ) dens( diff_prob_D_[,i] , lwd=4, col=1+i , add=TRUE )
abline(v = 0, lty = 2)

#### CAUSAL EFFECT OF G* ####

# number of applicatons to simulate
total_apps <- sum(dat$N)
# number of applications per department
apps_per_dept <- sapply( 1:6 , function(i)
  sum(dat$N[dat$D==i]) )
# simulate as if all apps from women
p_G1 <- link(mGD,data=list(
  D=rep(1:6,times=apps_per_dept),
  N=rep(1,total_apps),
  G=rep(1,total_apps)))
# simulate as if all apps from men
p_G2 <- link(mGD,data=list(
  D=rep(1:6,times=apps_per_dept),
  N=rep(1,total_apps),
  G=rep(2,total_apps)))
# summarize
dens( p_G1 - p_G2 , lwd=4 , col=2 ,
      xlab="effect of gender perception" )
abline(v = 0, lty = 2)

# simulate as if all apps from women
p_G1 <- link(mGD,data=list(
  D=rep(1:6,times=apps_per_dept),
  N=rep(1,total_apps),
  G=rep(1,total_apps)))
# simulate as if all apps from men
p_G2 <- link(mGD,data=list(
  D=rep(1:6,times=apps_per_dept),
  N=rep(1,total_apps),
  G=rep(2,total_apps)))

# show each dept density with weight as in population
w <- xtabs( dat$N ~ dat$D ) / sum(dat$N)
plot(NULL,xlim=c(-0.2,0.3),ylim=c(0,25),xlab="
Gender contrast (probability)",ylab="Density")
for ( i in 1:6 ) dens( diff_prob_D_[,i] ,
                       lwd=2+20*w[i] , col=1+i , add=TRUE )
abline(v=0,lty=3)

#### BONUS CATS ####

library(rethinking)
data(AustinCats)
d <- AustinCats
dat <- list(
  days = d$days_to_event,
  adopted = ifelse( d$out_event=="Adoption" , 1 , 0 ),
  color_id = ifelse( d$color=="Black" , 1 , 2 ) )

meow <- ulam(
  alist(
    days|adopted==1 ~ exponential(lambda),
    days|adopted==0 ~ custom(exponential_lccdf(!Y|lambda)),
    lambda <- 1.0/mu,
    log(mu) <- a[color_id],
    a[color_id] ~ normal(0,1)
  ) , data=dat , chains=4 , cores=4 )

post <- extract.samples(meow)
plot(NULL,xlim=c(0,100),ylim=c(0,1),xlab=
       "days until adoption",ylab="fraction")
for ( i in 1:50 ) curve(exp(-x/
                              exp(post$a[i,1])),add=TRUE,col=1)
for ( i in 1:50 ) curve(exp(-x/
                              exp(post$a[i,2])),add=TRUE,col=2)
