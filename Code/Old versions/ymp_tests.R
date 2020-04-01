

########################### SIMULATE DATA ########################### 

library(NRejections)
setwd("~/Dropbox/Personal computer/Independent studies/Ying's marriage paper/Code")
source("ymp_helper.R")

cor = make_corr_mat( nX = 5,
                     nY = 15,
                     rho.XX = 0.05,
                     rho.YY = 0.1,
                     rho.XY = 0.2,
                     prop.corr = 10/15 )
n=300
d = sim_data( n = n, cor = cor )
Xname = "X1"
# all other X-variables are covariates
Cnames = c("X2", "X3", "X4", "X5")
Ylin = names(d)[ grep( "Y", names(d) ) ]

# # add some Poisson outcomes
# d$Y6 = rpois( n = n, lambda = 5 )
# d$Y7 = rpois( n = n, lambda = 8 )
# Ypois = c("Y6", "Y7")
# 
# # add some binary outcomes
# d$Y8 = rbinom( n = n, size = 1, prob = 0.5 )
# d$Y9 = rbinom( n = n, size = 1, prob = 0.3 )
# Ybin = c("Y8", "Y9")

# induce MCAR missingness to all observations
d = degradefunction( d, 0.1 * nrow(d) * ncol(d) )
# check proportion missingness
table(is.na(d))




########################### MAKE MI DATASETS ########################### 

# we are only going to do this part once so that we can reuse imputations

# NEED TO UPDATE ENTIRE OS, THEN R, FOR NEW MICE TO WORK, I SUSPECT
# SO TEMPORARILY USING AMELIA
library(mice)
m = 5
imps = mice(d, m)

# check predictor matrix and method
imps$predictorMatrix
imps$method  # all PMM, as desired


# library(Amelia)
# m = 5
# imps = amelia( d, m )


########################### MOCK ANALYSES ########################### 

###### User-chosen parameters #####

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "OLS"

# set flavor ("vanilla" or "TMLE")
flavor = "vanilla"

# missing data method ("MI" or "CC")
missingness = "MI"

# outcome names
Ynames = Ylin

# total number of outcomes (not just the ones with this link; this is for Bonferroni penalization)
#( n.tot.outcomes = sum( length(Ylin), length(Ypois), length(Ybin) ) )
( n.tot.outcomes = length(Ylin) )

# familywise alpha
alpha = 0.05




###### Analyze all imputed datasets and pool resamples #####

res = list()

p.bt = c()
t.bt = c()
rej.bt = c()

bhats.unp = c()
ses.unp = c()



for ( i in 1:m ) {
  
  
  # for Amelia, first line should be "d = imps$imputations[i][[1]]" (with second index always 1)
  res[[i]] = analyze_CC_dataset( d = complete(imps, i), 
                                    X = Xname,
                                    C = Cnames,
                                    Ys = Ynames,
                                    alpha = 0.05,
                                    resample = TRUE,
                                    model = link,
                                    TMLE = FALSE )
  
  bhats = res[[i]]$samp.res$bhats
  ( bhats.unp = rbind( bhats.unp, bhats ) )
  
  # b / se = tval
  # se = b / tval
  ses = res[[i]]$samp.res$bhats / res[[i]]$samp.res$tvals
  ( ses.unp = rbind( ses.unp, ses ) )
  
  
  ##### Concatenate the resamples #####
  ( p.bt = cbind( p.bt, res[[i]]$resamps$p.bt ) )
  ( t.bt = cbind( t.bt, res[[i]]$resamps$t.bt ) )
  ( rej.bt = cbind( rej.bt, res[[i]]$resamps$rej.bt ) )
}

# now p.bt, t.bt have 1 row per outcome; 1 column per resample (B=100)
# rej.bt has 1 row per outcome; 1 column per resample (B=100)


# look at results
bhats.unp
ses.unp



########################### POOL COEFFICIENTS VIA RUBIN'S RULES ########################### 

( bhats.pool = colMeans( bhats.unp ) )

( ses.pool = vapply( 1:length(Ynames),
                  FUN = function(x) rubin_se( bhats.unp[,x], ses.unp[,x] ),
                  FUN.VALUE = 99 ) )

# minus 2 because 1 is for intercept and 1 is for exposure of interest
# checked against one of the fitted models :) 
( df = nrow(d) - 2 - length(Cnames) )  

# CI limits
( lo.pool = bhats.pool - qt( p = 1 - alpha/2, df = df ) )
( hi.pool = bhats.pool + qt( p = 1 - alpha/2, df = df ) )

# p-values
( pvals.pool = 2 * ( 1 - pt( q = abs( bhats.pool / ses.pool ),
                           df = df ) ) )

# did Bonferroni reject?
( bonf.rej = pvals.pool < (alpha / n.tot.outcomes) )


########################### E-VALUES ########################### 


# first compute SMD
# ~~~ NOTE THAT THESE ARE NON-IMPUTED SDS
sds = apply( d[, Ynames], 2, function(x) sd(x, na.rm = TRUE) )
  

evals.pt = vapply( 1:length(bhats),
                   function(i) evalues.MD( est = bhats[i] / sds[i],
                                           se = ses.pool[i],
                                           true = 0)["E-values", "point"],
                   FUN.VALUE = -99 )
  
  
# for each bhat, one of these will be NA (the one corresponding to the the wrong
#  CI limit)
evals.lo = vapply( 1:length(bhats),
                   function(i) evalues.MD( est = bhats[i] / sds[i],
                                           se = ses.pool[i],
                                           true = 0)["E-values", "lower"],
                   FUN.VALUE = -99 )  


evals.hi = vapply( 1:length(bhats),
                   function(i) evalues.MD( est = bhats[i] / sds[i],
                                           se = ses.pool[i],
                                           true = 0)["E-values", "upper"],
                   FUN.VALUE = -99 )  


# combine them
evals.CI = evals.lo
evals.CI[is.na(evals.CI)] = evals.hi[is.na(evals.lo)]

# # example
# d1 = c(NA, 1, 3, NA)
# d2 = c(0, NA, NA, 5)
# d1[is.na(d1)] = d2[is.na(d1)]



########################### POOL RESAMPLING METHODS VIA SIMPLE CONCATENATION ########################### 

if ( link == "OLS" ) {
  ##### Romano #####
  
  # test stats are already centered
  library(StepwiseTest)
  rom = FWERkControl( bhats.pool / ses.pool,  # first argument is sample t-values, here pooled from MI
                      as.matrix( t.bt ),  # these now have resamples from all m imputations
                      k = 1,
                      alpha = alpha )
  rom.rej = as.vector(rom$Reject)
  
  ##### Ours #####
  
  ( theta.hat = sum( pvals.pool < alpha ) )
  
  # global test
  ( crit = quantile( rej.bt, 1 - alpha ) )
  
  # p-values for observed rejections
  ( jt.pval = sum( rej.bt >= theta.hat ) /
      length( rej.bt ) )
  
  # null interval
  ( ni.lo = quantile( rej.bt, alpha / 2 ) )
  ( ni.hi = quantile( rej.bt, 1 - alpha / 2 ) )
  
} else {
  rom.rej = NA
  theta.hat = NA
  jt.pval = NA
  ni.lo = NA
  ni.hi = NA
}



########################### PRETTIFY OUTPUT ########################### 


digits = 2

# ~~~ NOTE THAT LAST 4 COLUMNS ARE OUTCOME-WIDE, SO THOSE VALUES ARE REPEATED FOR
#  ALL VARIABLES IN THAT GROUP
( table2 = data.frame( Outcome = Ynames, 
                      Est.and.CI = paste( round(bhats.pool, digits),
                                          " [",
                                          round(lo.pool, digits),
                                          ", ",
                                          round(hi.pool, digits),
                                          "]",
                                          sep = ""
                                          ),
                      pval = round( pvals.pool, 3 ),
                      
                      Reject.Bonf = bonf.rej, 
                      
                      Reject.Romano = rom.rej,
                      
                      # ours
                      Theta.hat = theta.hat,
                      
                      Global.pval = jt.pval, 
                        
                      Null.int = paste( " [",
                                        round(ni.lo, digits),
                                        ", ",
                                        round(ni.hi, digits),
                                        "]",
                                        sep = ""
                                        ),
     
                        
                      Excess.hits = theta.hat - ni.hi
                      ) )




( table3 = data.frame( Outcome = Ynames, 
                     
                     Evalue.point = round( evals.pt, digits ), 
                     
                     Evalue.CI = round( evals.CI, digits )
                     ) )





###### Set up the results files #####

# check if the overall results file already exists; if not, make one
results.dir = "~/Dropbox/Personal computer/Independent studies/Ying's marriage paper/Results"
setwd(results.dir)

string = paste( "table2",
                #link,
                #flavor,
                missingness,
                ".csv",
                sep = "_" )

write.csv(table2, string)


string = paste( "table3",
                #link,
                #flavor,
                missingness,
                ".csv",
                sep = "_" )

write.csv(table3, string)


