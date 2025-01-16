##' Robin Elahi
##' 15Jan 2025
##' Code for McElreath video lecture 3 (2023)
##' Linear regression

library(rethinking)

#### GENERATIVE SIMULATION ####

# Generative code
sim_weight <- function(H,b,sd) {
  U <- rnorm( length(H) , 0 , sd )
  sd
  W <- b*H + U
  return(W)
}

# Generate the data
H <- runif( 200 , min=130 , max=170 )
W <- sim_weight( H , b=0.5 , sd=5 )
plot( W ~ H , col=2 , lwd=3 )

# Prior predictive distribution
n <- 1e3
a <- rnorm(n,0,10)
b <- runif(n,0,1)
plot( NULL , xlim=c(130,170) , ylim=c(50,90) ,
      xlab="height (cm)" , ylab="weight (kg)" )
for ( j in 1:50 ) abline( a=a[j] , b=b[j] , lwd=2 , col=2 )

# Simulation-based calibration
# simulate a sample of 10 people
set.seed(93)
H <- runif(10,130,170)
W <- sim_weight(H,b=0.5,sd=5)
plot( W ~ H , col=2 , lwd=3 )

# run the model
m3.1 <- quap(
  alist(
    W ~ dnorm(mu,sigma),
    mu <- a + b*H,
    a ~ dnorm(0,10),
    b ~ dunif(0,1),
    sigma ~ dunif(0,10)
  ) , data=list(W=W,H=H) )

# summary
precis( m3.1 )

#### DATA ANALYSIS ####

# Analyze the data
data(Howell1)
d2 <- Howell1[Howell1$age >= 18, ]
d2 <- d2 %>%
  rename(W = weight, H = height)
dat <- list(W=d2$W,H=d2$H)

m3.2 <- quap(
  alist(
    W ~ dnorm(mu,sigma),
    mu <- a + b*H,
    a ~ dnorm(0,10),
    b ~ dunif(0,1),
    sigma ~ dunif(0,10)
  ) , data = dat )
precis( m3.2 )

# Posterior predictive distribution
post <- extract.samples(m3.2)
plot( d2$H , d2$W , col=2 , lwd=3 ,
      xlab="height (cm)" , ylab="weight (kg)" )
for ( j in 1:20 )
  abline( a=post$a[j] , b=post$b[j] , lwd=1 )

height_seq <- seq(130,190,len=20)
W_postpred <- sim( m3.2 ,
                   data=list(H=height_seq) )
W_PI <- apply( W_postpred , 2 , PI )
lines( height_seq , W_PI[1,] , lty=2 , lwd=2 )
lines( height_seq , W_PI[2,] , lty=2 , lwd=2 )

#### BONUS: TIDY PLOTTING ####
# Tidybayes.rethinking
# https://mjskay.github.io/tidybayes.rethinking/articles/tidy-rethinking.html#introduction-1
# install.packages("devtools")     # only necessary if you don't have devtools already
# devtools::install_github("mjskay/tidybayes.rethinking")
library(tidybayes.rethinking)
library(tidyverse)
library(tidybayes)
library(ggdist)
library(modelr)
library(cowplot)
theme_set(theme_tidybayes() + panel_border())

# Rename model to use for this exercise
m <- m3.2
m

# What is this function doing?
?extract.samples
str(rethinking::extract.samples(m))
summary(m)

# Tidy samples
m %>%
  spread_draws(a, b, sigma) %>%
  head(10)

# Compatibility intervals
d2 %>% 
  data_grid(., H = seq_range(H, n = 21)) %>% 
  add_linpred_draws(m)

# Prediction intervals
d2_pred <- d2 %>% 
  data_grid(., H = seq_range(H, n = 21)) %>% 
  add_predicted_draws(m)

# Final plot
d2 %>% 
  ggplot(aes(x = H, y = W)) + 
  stat_lineribbon(aes(y = .prediction), .width = c(.95), alpha = 0.5, 
                  color = "black", fill = "gray", data = d2_pred) + 
  geom_point(shape = 1, col = "red", size = 2) +
  theme(legend.position = "none") + 
  labs(x = "Height", y = "Weight")
