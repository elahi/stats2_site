## Lecture code from Statistical Rethinking (McElreath 2020)
## Chapter 7: Entropy, accuracy, divergence
## Statistical Modeling
## 2025

##### Preliminaries #####
library(rethinking)

#### ENTROPY ####

## R code 7.12
# Entropy: average log-probability of an event
# Probability of rain (p1) or shine (p2) [or weighted coin; 7E2]
p <- c( 0.3 , 0.7 )
-sum( p*log(p) )

# Write a function
get_entropy <- function(p) -sum(p * log(p))
get_entropy(p)

## 7E3
# 4-sided die, what is the entropy?

## 7E4
# loaded die; 4 never shows

## 7H3
i1 <- c( 0.2 , 0.2 , 0.2 , 0.2 , 0.2 )
i2 <- c( 0.8 , 0.1 , 0.05 , 0.025 , 0.025 )
i3 <- c( 0.05 , 0.15 , 0.7 , 0.05 , 0.05 )
d <- as.data.frame(rbind(i1, i2, i3))
names(d) <- c("spA", "spB", "spC", "spD", "spE")
rownames(d) <- c("island1", "island2", "island3")
d

## 7H3 - part 2


## 7H3 - part 3
## Using p (target) to describe q (model)


