##' Robin Elahi
##' 8 March 2025
##' Code for McElreath video lecture 18 (2023)
##' Missing data

library(rethinking)
library(dagitty)
library(tidyverse)
theme_set(theme_bw())

#### DAG ATE MY HW ####

## Dog usually benign
# Dog eats random homework
# aka missing completely at random
# Draw DAG
dag1 <- dagitty( "dag {
    H [unobserved]
    S -> H -> H_star <- D }")
coordinates(dag1) <- list(x = c(S = 1, H = 2, H_star = 2, D = 1),
                          y = c(S = 1, H = 1, H_star = 2, D = 2))
drawdag(dag1)
# Simulate according to DAG
N <- 100
S <- rnorm(N)
H <- rnorm(N, 0.5*S)
# dog eats 50% of homework at random
D <- rbern(N, 0.5) # play around with the percentage eaten!
Hstar <- H
Hstar[D==1] <- NA
# Make dataframe
d1 <- tibble(S, H, Hstar) %>% 
  gather(key = "dataset", "grade", H:Hstar) %>% 
  mutate(dataset = ifelse(dataset == "H", "complete", "incomplete"))
# Plot
d1 %>%
  ggplot(aes(S, grade, color = dataset, shape = dataset)) + 
  geom_smooth(method = "lm", se = TRUE) +  
  geom_point(size = 2) +
  scale_shape(solid = FALSE) + 
  scale_color_manual(values = c("black", "red")) + 
  labs(x = "time spent studying") 


## Dog path can be benign
# Dog eats homework of students who study too much
# aka missing at random
# Draw DAG
dag2 <- dagitty( "dag {
    H [unobserved]
    S -> H -> H_star <- D <- S }")
coordinates(dag2) <- list(x = c(S = 1, H = 2, H_star = 2, D = 1),
                          y = c(S = 1, H = 1, H_star = 2, D = 2))
drawdag(dag2)
# Simulate according to DAG
N <- 100
S <- rnorm(N)
H <- rnorm(N,0.5*S)
# dog eats 80% of homework where S>0
D <- rbern(N, ifelse(S>0, 0.8, 0) ) # play around with the percentage eaten!
Hstar <- H
Hstar[D==1] <- NA
# Make dataframe
d2 <- tibble(S, H, Hstar) %>% 
  gather(key = "dataset", "grade", H:Hstar) %>% 
  mutate(dataset = ifelse(dataset == "H", "complete", "incomplete"))
# Plot
d2 %>%
  ggplot(aes(S, grade, color = dataset, shape = dataset)) + 
  geom_smooth(method = "lm", se = TRUE) +  
  geom_point(size = 2) +
  scale_shape(solid = FALSE) + 
  scale_color_manual(values = c("black", "red")) + 
  labs(x = "time spent studying") 

## Non-linear relationships and poor modeling, less benign
# Dog eats homework of students who study too much
# BUT NOW NONLINEAR WITH CEILING EFFECT
# Draw DAG
dag3 <- dagitty( "dag {
    H [unobserved]
    S -> H -> H_star <- D <- S }")
coordinates(dag3) <- list(x = c(S = 1, H = 2, H_star = 2, D = 1),
                          y = c(S = 1, H = 1, H_star = 2, D = 2))
drawdag(dag3)
# Simulate according to DAG
N <- 100
S <- rnorm(N)
H <- rnorm(N,(1-exp(-0.7*S))) # play around with the coefficient and plot!
plot(S, H)
# dog eats 100% of homework where S>0
D <- rbern(N, ifelse(S > 0, 1, 0) ) # play around with the percentage eaten!
Hstar <- H
Hstar[D==1] <- NA
# Make dataframe
d3 <- tibble(S, H, Hstar) %>% 
  gather(key = "dataset", "grade", H:Hstar) %>% 
  mutate(dataset = ifelse(dataset == "H", "complete", "incomplete"))
# Plot
d3 %>%
  ggplot(aes(S, grade, color = dataset, shape = dataset)) + 
  geom_smooth(method = "lm", se = TRUE) +  
  geom_point(size = 2) +
  scale_shape(solid = FALSE) + 
  scale_color_manual(values = c("black", "red")) + 
  labs(x = "time spent studying") 

## Usually not benign
# Dog eats bad homework
# aka missing not at random
# Draw DAG
dag4 <- dagitty( "dag {
    H [unobserved]
    S -> H -> H_star <- D <- H }")
coordinates(dag4) <- list(x = c(S = 1, H = 2, H_star = 2, D = 1),
                          y = c(S = 1, H = 1, H_star = 2, D = 2))
drawdag(dag4)
# Simulate according to DAG
#set.seed(2)
N <- 100
S <- rnorm(N)
H <- rnorm(N,0.5*S)
# dog eats 90% of homework where H<0
D <- rbern(N, ifelse(H<0, 0.9, 0) ) # play around with the percentage eaten!
Hstar <- H
Hstar[D==1] <- NA
# Make dataframe
d4 <- tibble(S, H, Hstar) %>% 
  gather(key = "dataset", "grade", H:Hstar) %>% 
  mutate(dataset = ifelse(dataset == "H", "complete", "incomplete"))
# Plot
d4 %>%
  ggplot(aes(S, grade, color = dataset, shape = dataset)) + 
  geom_smooth(method = "lm", se = TRUE) +  
  geom_point(size = 2) +
  scale_shape(solid = FALSE) + 
  scale_color_manual(values = c("black", "red")) + 
  labs(x = "time spent studying") 


#### FROM TEXT ####

## R code 15.8
N <- 100
S <- rnorm( N )
H <- rbinom( N , size=10 , inv_logit(S) )

## R code 15.9
D <- rbern( N , prob = 0.5) # dogs completely random
Hm <- H
Hm[D==1] <- NA

## R code 15.10
D <- ifelse( S > 0 , 1 , 0 ) # dog eats H of students who study (S) alot
Hm <- H
Hm[D==1] <- NA

## R code 15.11
# Simulate unobserved confound (X = noise)
set.seed(501)
N <- 1000 
X <- rnorm(N)
S <- rnorm(N)
H <- rbinom( N , size=10 , inv_logit( 2 + S - 2*X ) )
D <- ifelse( X > 1 , 1 , 0 )
Hm <- H
Hm[D==1] <- NA

## R code 15.12
dat_list <- list(
  H = H,
  S = S )

m15.3 <- ulam(
  alist(
    H ~ binomial( 10 , p ),
    logit(p) <- a + bS*S,
    a ~ normal( 0 , 1 ),
    bS ~ normal( 0 , 0.5 )
  ), data=dat_list , chains=4 )
precis( m15.3 )

## R code 15.13
dat_list0 <- list( H = H[D==0] , S = S[D==0] )

m15.4 <- ulam(
  alist(
    H ~ binomial( 10 , p ),
    logit(p) <- a + bS*S,
    a ~ normal( 0 , 1 ),
    bS ~ normal( 0 , 0.5 )
  ), data=dat_list0 , chains=4 )
precis( m15.4 )

## R code 15.14
D <- ifelse( abs(X) < 1 , 1 , 0 )

## R code 15.15
N <- 100
S <- rnorm(N)
H <- rbinom( N , size=10 , inv_logit(S) )
D <- ifelse( H < 5 , 1 , 0 )
Hm <- H; Hm[D==1] <- NA

## R code 15.16
library(rethinking)
data(milk)
d <- milk
d$neocortex.prop <- d$neocortex.perc / 100
d$logmass <- log(d$mass)
dat_list <- list(
  K = standardize( d$kcal.per.g ),
  B = standardize( d$neocortex.prop ),
  M = standardize( d$logmass ) )

## R code 15.17
m15.5 <- ulam(
  alist(
    K ~ dnorm( mu , sigma ),
    mu <- a + bB*B + bM*M,
    B ~ dnorm( nu , sigma_B ),
    c(a,nu) ~ dnorm( 0 , 0.5 ),
    c(bB,bM) ~ dnorm( 0, 0.5 ),
    sigma_B ~ dexp( 1 ),
    sigma ~ dexp( 1 )
  ) , data=dat_list , chains=4 , cores=4 )

## R code 15.18
precis( m15.5 , depth=2 )

## R code 15.19
obs_idx <- which( !is.na(d$neocortex.prop) )
dat_list_obs <- list(
  K = dat_list$K[obs_idx],
  B = dat_list$B[obs_idx],
  M = dat_list$M[obs_idx] )
m15.6 <- ulam(
  alist(
    K ~ dnorm( mu , sigma ),
    mu <- a + bB*B + bM*M,
    B ~ dnorm( nu , sigma_B ),
    c(a,nu) ~ dnorm( 0 , 0.5 ),
    c(bB,bM) ~ dnorm( 0, 0.5 ),
    sigma_B ~ dexp( 1 ),
    sigma ~ dexp( 1 )
  ) , data=dat_list_obs , chains=4 , cores=4 )
precis( m15.6 )

## R code 15.20
plot( coeftab(m15.5,m15.6) , pars=c("bB","bM") )

## R code 15.21
post <- extract.samples( m15.5 )
B_impute_mu <- apply( post$B_impute , 2 , mean )
B_impute_ci <- apply( post$B_impute , 2 , PI )

# B vs K
plot( dat_list$B , dat_list$K , pch=16 , col=rangi2 ,
      xlab="neocortex percent (std)" , ylab="kcal milk (std)" )
miss_idx <- which( is.na(dat_list$B) )
Ki <- dat_list$K[miss_idx]
points( B_impute_mu , Ki )
for ( i in 1:12 ) lines( B_impute_ci[,i] , rep(Ki[i],2) )

# M vs B
plot( dat_list$M , dat_list$B , pch=16 , col=rangi2 ,
      ylab="neocortex percent (std)" , xlab="log body mass (std)" )
Mi <- dat_list$M[miss_idx]
points( Mi , B_impute_mu )
for ( i in 1:12 ) lines( rep(Mi[i],2) , B_impute_ci[,i] )

## R code 15.22
m15.7 <- ulam(
  alist(
    # K as function of B and M
    K ~ dnorm( mu , sigma ),
    mu <- a + bB*B_merge + bM*M,
    
    # M and B correlation
    MB ~ multi_normal( c(muM,muB) , Rho_BM , Sigma_BM ),
    matrix[29,2]:MB <<- append_col( M , B_merge ),
    
    # define B_merge as mix of observed and imputed values
    vector[29]:B_merge <- merge_missing( B , B_impute ),
    
    # priors
    c(a,muB,muM) ~ dnorm( 0 , 0.5 ),
    c(bB,bM) ~ dnorm( 0, 0.5 ),
    sigma ~ dexp( 1 ),
    Rho_BM ~ lkj_corr(2),
    Sigma_BM ~ dexp(1)
  ) , data=dat_list , chains=4 , cores=4 )
precis( m15.7 , depth=3 , pars=c("bM","bB","Rho_BM" ) )

## R code 15.23
B_missidx <- which( is.na( dat_list$B ) )

## R code 15.24
data(Moralizing_gods)
str(Moralizing_gods)

## R code 15.25
table( Moralizing_gods$moralizing_gods , useNA="always" )

## R code 15.26
symbol <- ifelse( Moralizing_gods$moralizing_gods==1 , 16 , 1 )
symbol <- ifelse( is.na(Moralizing_gods$moralizing_gods) , 4 , symbol )
color <- ifelse( is.na(Moralizing_gods$moralizing_gods) , "black" , rangi2 )
plot( Moralizing_gods$year , Moralizing_gods$population , pch=symbol ,
      col=color , xlab="Time (year)" , ylab="Population size" , lwd=1.5 )

## R code 15.27
with( Moralizing_gods ,
      table( gods=moralizing_gods , literacy=writing , useNA="always" ) )

## R code 15.28
haw <- which( Moralizing_gods$polity=="Big Island Hawaii" )
columns <- c("year","writing","moralizing_gods")
t( Moralizing_gods[ haw , columns ] )

## R code 15.29
set.seed(9)
N_houses <- 100L
alpha <- 5
beta <- (-3)
k <- 0.5
r <- 0.2
cat <- rbern( N_houses , k )
notes <- rpois( N_houses , alpha + beta*cat )
R_C <- rbern( N_houses , r )
cat_obs <- cat
cat_obs[R_C==1] <- (-9L)
dat <- list(
  notes = notes,
  cat = cat_obs,
  RC = R_C,
  N = as.integer(N_houses) )

## R code 15.30
m15.8 <- ulam(
  alist(
    # singing bird model
    ## cat known present/absent:
    notes|RC==0 ~ poisson( lambda ),
    log(lambda) <- a + b*cat,
    ## cat NA:
    notes|RC==1 ~ custom( log_sum_exp(
      log(k) + poisson_lpmf( notes | exp(a + b) ),
      log(1-k) + poisson_lpmf( notes | exp(a) )
    ) ),
    
    # priors
    a ~ normal(0,1),
    b ~ normal(0,0.5),
    
    # sneaking cat model
    cat|RC==0 ~ bernoulli(k),
    k ~ beta(2,2)
  ), data=dat , chains=4 , cores=4 )
