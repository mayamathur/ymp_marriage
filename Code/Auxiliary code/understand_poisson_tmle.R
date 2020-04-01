
# Goal: Why does tmle only return ATE for link = poisson?
n = 1000
X = rbinom( n=n, size = 1, prob = 0.5)
W = rbinom( n=n, size = 1, prob = 0.5)
b1 = log(2)
b2 = log(1.3)
Y = rpois( n=n, lambda = exp(b1 * X + b2 * W) )

glm( Y ~ X + W, family = "poisson" )

library(tmle)
library(SuperLearner)

mod = tmle( Y = Y,
      A = X,
      W = as.data.frame(W), 
      family = "poisson" )

# this is what's being estimated for the ATE
mean( Y[X == 1] ) - mean( Y[ X==0 ] )

# versus we want this
log( mean( Y[X == 1] ) ) - log( mean( Y[X == 0] ) )

mod$estimates$ATT




# for comparison
library(locfit)  # for expit
Y2 = rbinom( n=n, size = 1, prob = expit(b1 * X + b2 * W) )

glm( Y2 ~ X + W, family = "binomial" )

library(tmle)
library(SuperLearner)

mod = tmle( Y = Y2,
            A = X,
            W = as.data.frame(W), 
            family = "binomial" )

lm( Y2 ~ X )

mod$estimates$ATT

# so ATT is just linear prob effect
# so for Poisson, it's E[Y1] - E[Y0]

# so I overall have no idea what the ATT is here