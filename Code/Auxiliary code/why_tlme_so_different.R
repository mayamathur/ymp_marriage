
# Figure out why TMLE p-values are so much smaller
# social integration had an especially large change in p-value

########################### SET UP ###########################
Cnames = c( "age",
            "nhwhite",
            "colled",
            "nim89",
            "region2",
            "region3",
            "region4",
            "mdinc2",
            "mdinc3",
            "mdinc4",
            "exam89",
            "alco89",
            "smoke89",
            "act89_d" )

Xname = "mars93_2"

# focus on at outcome that had large difference
Yname = "socint13z"

# read in existing imputations
setwd(stochastic.results.dir)
load("imputed_datasets.RData" )
library(here)
stochastic.results.dir = here("Results/Marriage/R objects from analysis")



########################### FIT THE MODELS ###########################

bhats = data.frame( "TMLE" = rep(NA,m), "nonTMLE" = rep(NA,m) )
ses = data.frame( "TMLE" = rep(NA,m), "nonTMLE" = rep(NA,m) )

for ( i in 1:m ){
  res.nonTMLE = dataset_result( d = complete(imps,i),
                                X = Xname,
                                C = Cnames,
                                Ys = Yname,  # all outcome names
                                alpha = .05,
                                center.stats = FALSE,
                                bhat.orig = NA,
                                model = "OLS",
                                TMLE = FALSE )
  
  bhats[ i, "nonTMLE" ] = res.nonTMLE$bhats
  ses[ i, "nonTMLE" ] = res.nonTMLE$SEs
  
  res.TMLE = dataset_result( d = complete(imps,i),
                                X = Xname,
                                C = Cnames,
                                Ys = "socint13z",  # all outcome names
                                alpha = .05,
                                center.stats = FALSE,
                                bhat.orig = NA,
                                model = "OLS",
                                TMLE = TRUE )
  
  bhats[ i, "TMLE" ] = res.TMLE$bhats
  ses[ i, "TMLE" ] = res.TMLE$SEs
}

# pool them
colMeans(bhats)
colMeans(ses)

# wow
# TMLE SEs are on average 2 orders of magnitude smaller than non-TMLE for this outcome


########################### SANITY CHECK: FIT MODELS TO FIRST IMPUTED DATASET MANUALLY ###########################

covars = c(Xname, Cnames)
d = complete(imps,1)

##### Manually Fit OLS (non-TMLE) #####
# yes, matches results
summary( lm( d[[Yname]] ~ ., data = d[ , covars] ) )


##### Manually Fit TMLE #####
require(tmle)
require(SuperLearner)

# defaults to super-learning
mod = tmle( Y = d[[Yname]],
            A = d[[Xname]],
            W = d[ , Cnames ] )

# for continuous outcome
b = mod$estimates$ATE$psi
SE = mod$estimates$ATE$var.psi
pval = mod$estimates$ATE$pvalue

# won't be exactly same as in bhats and ses because of stochastic nature,
#  but should be clsoe

# OOOPSSSSS!!! I FORGOT TO TAKE SQUARE ROOT FOR TMLE :)


