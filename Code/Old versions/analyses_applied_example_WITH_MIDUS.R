# Contact: Maya Mathur (mmathur@stanford.edu)

# To do: make lists of covariates, etc. 

############################## SET YOUR DIRECTORY LOCATIONS ############################## 

rm(list=ls())

# location of data
data.dir = "~/Dropbox/Personal computer/Independent studies/Ying's marriage paper/Data"

# location of code
code.dir = "~/Dropbox/Personal computer/Independent studies/Ying's marriage paper/Code"

# location to save results
results.dir = "~/Dropbox/Personal computer/Independent studies/Ying's marriage paper/Results"

# location to save imputations and resamples
# (or where to look for them if not running from scratch)
stochastic.results.dir = "~/Dropbox/Personal computer/Independent studies/Ying's marriage paper/Results/R objects from analysis"

# location of codebook
# ~~~ still coming from other paper
codebook.dir = "~/Dropbox/Personal computer/Independent studies/Tyler's outcome-wide paper/Linked to OSF (OWP)/Applied example/MIDUS codebooks"

setwd(code.dir)
source("helper_applied_example.R")
                                                                                                                  
# are we doing a local test using the MIDUS data?
local.test = TRUE

############################## READ IN DATA ############################## 

# stored in the other paper's repository
setwd(data.dir)

if ( local.test == TRUE ) {
  # read in data
  d = read.csv("flourish_prepped.csv", header=TRUE); nrow(d)
  nrow(d) # should be 2948
  
  # spurious variable
  toss = c( "X" )
  d = d[ , !names(d) %in% toss ]
  
  # lists of variable names
  # outcome names
  Ylin = c("flourish_z",
           "emotion_z",
           "social_z",
           "psych_z",
           "B1SPOSAFz",
           "B1SQ1z",
           "B1SSWBMSz",
           "B1SSWBSIz",
           "B1SSWBAOz",
           "B1SSWBSCz",
           "B1SSWBSAz",
           "B1SPWBA1z",
           "B1SPWBE1z",
           "B1SPWBG1z",
           "B1SPWBR1z",
           "B1SPWBU1z",
           "B1SPWBS1z")
  
  # Ybin is just the binary outcomes to be modeled with logistic regression
  #  i.e., those with prevalence < 10%
  Ybin = c( "B1SA62G",
            "B1PANXTD" )
  
  # these are also all binary, but will be modeled with Poisson regression
  #  because prevalence >10%
  Ycount = c( "B1SBMIc", 
              "smoke", 
              "binge_c",
              "oth_sub",
              "B1PDEPDX" )
  
  # names of adjusted covariates (without exposure of interest)
  # see SAS file 9
  Cnames = c( 
    "A1PAGE_M2",
    "A1PRSEX",
    "raceA",
    "A1SE2",
    "A1SE3",
    "A1SE4",
    "A1PC1",
    "sibling",
    "CEDUC4cat",
    "A1PC14",
    "A1SE9",
    "A1SE7",
    "A1SE8c",
    "mom_smk",
    "dad_smk",
    "B1PA58",
    "A1SE6" )
  
  Xname = "A1SEPA_z"
}

# variable names for the marriage dataset
if ( local.test == FALSE ) {
  
  # variable name lists from Ying's file "Table2_complete_nabs.sas"
  Ylin = c( "fhapp13z",
           "purposez",
           "hopel13z",
           "socint13z",
           "supportz",
           "cesdz",
           "anx13z",
           "loneliz",
           "nahei11az",
           "number13z" )
  
  # this also includes the common ones because tmle package doesn't support 
  # RR estimation for Poisson link
  Ybin = c( "depx13", # common ones
           "exam",
           "alco11_d",
           "act13_d",
           "obese",
           "diabetes",
           "asthma",
           "cancer",
    
           # rare ones
          "smoke",
           "ssleep",
           "death",
           "heart",
           "stroke"
          )
  
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
}




########################### SET ANALYSIS PARAMETERS ########################### 

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "poisson"

# TMLE or standard MLE?
TMLE = TRUE
resample = FALSE

# should we overwrite previous results files?
write.results = TRUE

# should we run from saved imputations or re-impute?
impute.from.scratch = TRUE
resample.from.scratch = FALSE


# missing data method ("MI" or "CC")
missingness = "MI"

# name of exposure
Xname = "A1SEPA_z"

# number of imputations
m = 5


# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05



########################### PROCESS THE USER'S INPUT ########################### 

# adjustments for TMLE
# ~~~ obviously change this because it dichotomizes the exposure
if ( TMLE == TRUE ) {
  # for YMP paper, exposure will actually be binary
  median = median(d$A1SEPA_z, na.rm = TRUE )
  d$A1SEPA_z[ d$A1SEPA_z < median ] = 0
  d$A1SEPA_z[ d$A1SEPA_z >= median ] = 1
  
  # code as 1/0 for TMLE package
  d$B1SA62G = binarize(d$B1SA62G)
  d$B1PANXTD = binarize(d$B1PANXTD)
}


# names of outcomes with this link
if ( link == "OLS" ) Ynames = Ylin
if ( link == "poisson" ) Ynames = Ycount
if ( link == "logistic" ) Ynames = Ybin

# total number of outcomes (for Bonferroni penalization)
# not just the ones with the above-specified link
( n.tot.outcomes = length( c(Ylin, Ycount, Ybin) ) )




########################### MAKE MI DATASETS ########################### 

##### How Much Missing Data? #####

# proportions of missing data
prop.missing = apply( d, 2, function(x) sum( is.na(x) ) / length(x)  )
min(prop.missing)
mean(prop.missing)
max(prop.missing)
# note that there is almost no missing data

# proportion of subjects with no missing data: 86% 
sum( complete.cases(d) ) / nrow(d)

# missing data on exposure
prop.missing[ names(d) == Xname ]


if ( missingness == "MI" ) {
  ##### Make Imputations #####
  library(mice)
  
  if ( impute.from.scratch == TRUE ) {
    ##### Generate Imputations #####
    ini = mice(d, m=1, maxit = 0 )
    ini$loggedEvents
    
    # check methods
    ini$method
    
    pred = quickpred(d)
    
    # ~~~~ UPDATE THIS FOR NEW DATASET
    
    if ( local.test == TRUE ) {
      # avoid using composites to impute other variables (causes mice to get angry
      #  probably due to collinearity)
      composites = c("flourish_z", "emotion_z", "psych_z", "social_z")
      pred[, composites ] = 0
    }

    m = 5
    imps = mice( d, m, predictorMatrix = pred )
    
    # any complaints?
    head(imps$loggedEvents)
    
    # first imputed dataset
    head( complete(imps, 1) )
    # if this line returns an error about complete() not being applicable
    #  for a mids objects (which is a lie), restart R
    
    # check method
    imps$method
    
    if ( write.results == TRUE ) {
      
      # save imputations for reproducibility
      setwd(stochastic.results.dir)
      save( imps, file = "imputed_datasets.RData" )
      
      # # also save imputed datasets as csvs for Ying
      # setwd(stochastic.results.dir)
      # setwd("Imputed datasets as csvs")
      
      for (i in 1:m) {
        write.csv( complete(imps,i),
                   paste("imputed_dataset_", i, ".csv", sep="") )
      }
    }
    
  } else {  # if not imputing from scratch
    
    # read in existing imputations to avoid running the below code
    setwd(stochastic.results.dir)
    load("imputed_datasets.RData" )
  } 
}



###########################  ANALYSES ########################### 

###### If Doing CC Analyses #####

if ( missingness == "CC" ) {
  res = analyze_CC_dataset( #d = d[ complete.cases(d), ],  # for sanity check on complete data
    d = d, 
    X = Xname,
    C = Cnames,
    Ys = Ynames,
    alpha = alpha.within,
    resample = resample,
    B = B.resamp,  
    model = link,
    TMLE = TMLE ) 
}


###### Analyze all imputed datasets and pool resamples #####

if ( missingness == "MI" ) {
  # initialize results
  res = list()
  
  p.bt = c()
  t.bt = c()
  rej.bt = c()
  
  bhats.unp = c()
  ses.unp = c()
  
  
  for ( i in 1:m ) {
    
    cat("\n Analyzing imputation ", i)
    
    # "resample" is whether we're ACTUALLY going to resample, 
    #  so is also FALSE if link isn't OLS
    resample = resample.from.scratch
    if ( link != "OLS" | TMLE == TRUE ) resample = FALSE
    
    
    # for Amelia, first line should be "d = imps$imputations[i][[1]]" (with second index always 1)
    res[[i]] = analyze_CC_dataset( #d = d[ complete.cases(d), ],  # for sanity check on complete data
      d = complete(imps, i), 
      X = Xname,
      C = Cnames,
      Ys = Ynames,
      alpha = alpha.within,
      resample = resample,
      B = B.resamp,  
      model = link,
      TMLE = TMLE )
    
    # one for each covariate
    bhats = res[[i]]$samp.res$bhats
    ( bhats.unp = rbind( bhats.unp, bhats ) )
    
    if ( TMLE == FALSE ){
      # b / se = tval
      # se = b / tval
      ses = res[[i]]$samp.res$bhats / res[[i]]$samp.res$tvals
      ( ses.unp = rbind( ses.unp, ses ) )
    } else {
      ses = res[[i]]$samp.res$SEs
      ( ses.unp = rbind( ses.unp, ses ) )
    }

    
    if (resample == TRUE) {
      ##### Concatenate the resamples #####
      ( p.bt = cbind( p.bt, res[[i]]$resamps$p.bt ) )
      ( t.bt = cbind( t.bt, res[[i]]$resamps$t.bt ) )
      ( rej.bt = cbind( rej.bt, res[[i]]$resamps$rej.bt ) )
    }
    
  }
  
  # now p.bt, t.bt have 1 row per outcome; 1 column per resample (B=100)
  # rej.bt has 1 row per outcome; 1 column per resample (B=100)
  
  # look at results
  bhats.unp
  ses.unp
  
  # sanity check on resamples: p-values should be uniform
  if ( resample == TRUE & link == "OLS" ) {
    hist(p.bt)
    require(testthat)
    expect_equal( mean(p.bt), 0.5, tolerance = 0.05 )
    expect_equal( mean(rej.bt), alpha.within * length(Ynames), 0.08 )
  }
  
  # save stochastic resamples for reproducibility
  if ( write.results == TRUE & link == "OLS" & resample == TRUE & resample.from.scratch == TRUE ) {
    setwd( stochastic.results.dir )
    
    # remove period from alpha-level
    if (alpha.within == 0.05) alpha.string = "alpha005"
    if (alpha.within == 0.01) alpha.string = "alpha001"
    
    write.csv( p.bt,
               paste( "resampled_OLS_pvals", "_", alpha.string, ".csv", sep="" ),
               row.names = FALSE )
    
    write.csv( t.bt,
               paste( "resampled_OLS_tvals", "_", alpha.string, ".csv", sep="" ),
               row.names = FALSE )
    
    write.csv( rej.bt,
               paste( "resampled_OLS_rej", "_", alpha.string, ".csv", sep="" ),
               row.names = FALSE )
  }
}



########################### POOL COEFFICIENTS VIA RUBIN'S RULES ########################### 


if ( missingness == "MI" ) {
  ( bhats.pool = colMeans( bhats.unp ) )
  
  ( ses.pool = vapply( 1:length(Ynames),
                       FUN = function(x) rubin_se( bhats.unp[,x], ses.unp[,x] ),
                       FUN.VALUE = 99 ) )
  
  
  
  ##### CI limits #####
  
  # t-based inference
  if ( link == "OLS" ) {
    # minus 2 because 1 is for intercept and 1 is for exposure of interest
    # checked against one of the fitted models :) 
    ( df = nrow(d) - 2 - length(Cnames) )  
    
    ( lo.pool = bhats.pool - qt( p = 1 - alpha/2, df = df ) * ses.pool )
    ( hi.pool = bhats.pool + qt( p = 1 - alpha/2, df = df ) * ses.pool )
    
    # p-values
    ( pvals.pool = 2 * ( 1 - pt( q = abs( bhats.pool / ses.pool ),
                                 df = df ) ) )
  } 
  
  if ( link != "OLS" ) {
    
    ( lo.pool = bhats.pool - qnorm( p = 1 - alpha/2 ) * ses.pool )
    ( hi.pool = bhats.pool + qnorm( p = 1 - alpha/2 ) * ses.pool )
    
    # p-values
    ( pvals.pool = 2 * ( 1 - pnorm( q = abs( bhats.pool / ses.pool ) ) ) )
    
  }
  
  
  # did Bonferroni reject?
  ( bonf.rej = pvals.pool < (alpha / n.tot.outcomes) )
  
}





########################### E-VALUES ########################### 

require(EValue)

if ( link == "OLS" & missingness == "MI" ) {
  
  # all continuous variables are already standardized by sd(Y), so no need to 
  #  standardize point estimates
  
  evals.pt = vapply( 1:length(bhats),
                     function(i) evalues.MD( est = bhats.pool[i],
                                             se = ses.pool[i],
                                             true = 0)["E-values", "point"],
                     FUN.VALUE = -99 )
  
  
  # for each bhat, one of these will be NA (the one corresponding to the the wrong
  #  CI limit)
  evals.lo = vapply( 1:length(bhats),
                     function(i) evalues.MD( est = bhats.pool[i],
                                             se = ses.pool[i],
                                             true = 0)["E-values", "lower"],
                     FUN.VALUE = -99 )  
  
  
  evals.hi = vapply( 1:length(bhats),
                     function(i) evalues.MD( est = bhats.pool[i],
                                             se = ses.pool[i],
                                             true = 0)["E-values", "upper"],
                     FUN.VALUE = -99 ) 
}



if ( link == "poisson" & missingness == "MI" ) {
  
  evals.pt = vapply( 1:length(bhats.pool),
                     function(i) evalues.RR( est = exp( bhats.pool[i] ),
                                             lo = exp( lo.pool[i] ),
                                             hi = exp( hi.pool[i] ),
                                             true = 1)["E-values", "point"],
                     FUN.VALUE = -99 )
  
  
  # for each bhat, one of these will be NA (the one corresponding to the the wrong
  #  CI limit)
  evals.lo = vapply( 1:length(bhats.pool),
                     function(i) evalues.RR( est = exp( bhats.pool[i] ),
                                             lo = exp( lo.pool[i] ),
                                             hi = exp( hi.pool[i] ),
                                             true = 1)["E-values", "lower"],
                     FUN.VALUE = -99 )
  
  
  evals.hi = vapply( 1:length(bhats.pool),
                     function(i) evalues.RR( est = exp( bhats.pool[i] ),
                                             lo = exp( lo.pool[i] ),
                                             hi = exp( hi.pool[i] ),
                                             true = 1)["E-values", "upper"],
                     FUN.VALUE = -99 ) 
}



if ( link == "logistic" & missingness == "MI" ) {
  
  evals.pt = vapply( 1:length(bhats.pool),
                     function(i) evalues.OR( est = exp( bhats.pool[i] ),
                                             lo = exp( lo.pool[i] ),
                                             hi = exp( hi.pool[i] ),
                                             rare = FALSE,
                                             true = 1)["E-values", "point"],
                     FUN.VALUE = -99 )
  
  
  # for each bhat, one of these will be NA (the one corresponding to the the wrong
  #  CI limit)
  evals.lo = vapply( 1:length(bhats.pool),
                     function(i) evalues.OR( est = exp( bhats.pool[i] ),
                                             lo = exp( lo.pool[i] ),
                                             hi = exp( hi.pool[i] ),
                                             rare = FALSE,
                                             true = 1)["E-values", "lower"],
                     FUN.VALUE = -99 )
  
  
  evals.hi = vapply( 1:length(bhats.pool),
                     function(i) evalues.OR( est = exp( bhats.pool[i] ),
                                             lo = exp( lo.pool[i] ),
                                             hi = exp( hi.pool[i] ),
                                             rare = FALSE,
                                             true = 1)["E-values", "upper"],
                     FUN.VALUE = -99 ) 
}



# combine them
evals.CI = evals.lo
evals.CI[is.na(evals.CI)] = evals.hi[is.na(evals.lo)]

# # example
# d1 = c(NA, 1, 3, NA)
# d2 = c(0, NA, NA, 5)
# d1[is.na(d1)] = d2[is.na(d1)]




########################### CODE SANITY CHECKS ########################### 

# choose one outcome at a time and look at estimates and inference
#  on the main exposure of interest

if ( TMLE == FALSE ) {
  for ( i in 1:length(Ynames) ) {
    
    cat( "\n Checking", link, "outcome number ", i)
    
    # results for this outcome
    bhat.man = c()
    se.man = c()
    pvals.man = c()
    
    # fit model to each imputed dataset
    for ( j in 1:m ) {
      
      dat = complete(imps, j)
      
      covars = c(Xname, Cnames)
      
      if ( link == "OLS" ) mod = lm( dat[[ Ynames[i] ]] ~ ., data = dat[ , covars] )
      if ( link == "poisson" ) {
        # if binary variable, make sure it's 0/1 instead of "b.No"/"a.Yes"
        if ( length( unique( dat[[ Ynames[i] ]] ) ) == 2 ) tempY = binarize( dat[[ Ynames[i] ]] )
        else tempY = dat[[ Ynames[i] ]]
        
        mod = glm( tempY ~ ., data = dat[ , covars], family = "poisson" )
      }
      if ( link == "logistic" ) mod = glm( dat[[ Ynames[i] ]] ~ ., data = dat[ , covars], family = "binomial" )
      
      # save within-imputation results for this imputation
      # only saving results for exposure of interest
      bhat.man = c( bhat.man, coef(mod)[["A1SEPA_z"]] )
      se.man = c( se.man, summary(mod)$coefficients["A1SEPA_z","Std. Error"] )
    }
    
    # pool results
    bhat.pool.man = mean( bhat.man )
    
    # Rubin's Rules by hand
    Ubar = mean(se.man^2)
    B = ( 1 / (m-1) ) * sum( ( bhat.man - mean(bhat.man) )^2 )
    total.var = Ubar + ( 1 + (1/m) ) * B
    se.pool.man = sqrt(total.var)
    
    # CIs by hand
    if ( link == "OLS" ) {
      df = nrow(dat) - length(covars) - 1
      t = abs( bhat.pool.man / se.pool.man )
      pval.man = 2 * ( 1 - pt( t,
                               df = df ) )
      
      lo.man = bhat.pool.man - qt(.975, df = df) * se.pool.man
      hi.man = bhat.pool.man + qt(.975, df = df) * se.pool.man
    }
    
    if ( link == "poisson" | link == "logistic" ) {
      z = abs( bhat.pool.man / se.pool.man )
      pval.man = 2 * ( 1 - pnorm( z ) )
      
      lo.man = bhat.pool.man - qnorm(.975) * se.pool.man
      hi.man = bhat.pool.man + qnorm(.975) * se.pool.man
    }
    
    
    # check bhats
    require(testthat)
    expect_equal( bhats.pool[i], bhat.pool.man )
    expect_equal( ses.pool[i], se.pool.man )
    expect_equal( lo.pool[i], lo.man )
    expect_equal( hi.pool[i], hi.man )
    expect_equal( pvals.pool[i], pval.man )
    
  }  
}



########################### PRETTIFY OUTPUT ########################### 

digits = 2

if ( link == "OLS" & missingness == "MI" ) {
  
  est = round(bhats.pool, digits)
  
  ci = paste( "[",
              my_round( lo.pool, digits ),
              ", ",
              my_round( hi.pool, digits ),
              "]",
              sep = ""
  )
} 


if ( link != "OLS" & missingness == "MI" ) {
  
  est = round( exp(bhats.pool), digits)
  
  ci = paste( "[",
              my_round( exp(lo.pool), digits),
              ", ",
              my_round( exp(hi.pool), digits),
              "]",
              sep = ""
  )
}



if ( link == "OLS" & missingness == "CC" ) {
  
  est = round(res$samp.res$bhats, digits)
  
  # get SEs from pvals
  SE = abs(res$samp.res$bhats) / qnorm( 1 - res$samp.res$pvals / 2 )
  
  # sanity check
  # pvals: 2 * (1 - pnorm( abs(res$samp.res$bhats) / SE ) )
  
  lo = my_round( res$samp.res$bhats - qnorm(.975) * SE, digits )
  hi = my_round( res$samp.res$bhats + qnorm(.975) * SE, digits )
  
  ci = paste( "[",
              lo,
              ", ",
              hi,
              "]",
              sep = ""
  )
} 



if ( missingness == "MI" ) {
  pvals = vapply( pvals.pool,
                  function(x) format_pval( x, digits = 3, star.cutoffs = stars ),
                  "asdf" )
} else {
  pvals = vapply( res$samp.res$pvals,
                  function(x) format_pval( x, digits = 3, star.cutoffs = stars ),
                  "asdf" )
}



# NOTE THAT LAST 4 COLUMNS ARE OUTCOME-WIDE, SO THOSE VALUES ARE REPEATED FOR
#  ALL VARIABLES IN THAT GROUP

# set p-value star cutoffs; last is Bonferroni
stars = c(0.01, 0.05, 0.05 / length( c(Ylin, Ybin, Ycount) ) )


( table2 = data.frame( Outcome = Ynames, 
                       
                       Est = est,
                       
                       CI = ci,
                       
                       pval = pvals,
                       
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

# error about short row names is expected

# if doing CC, add a column saying the sample size
if ( missingness == "CC" ) table2$n = samp.res$n



digits2 = 2
( table3 = data.frame( Outcome = Ynames, 
                       
                       Evalue.point = round( evals.pt, digits2 ), 
                       
                       Evalue.CI = round( evals.CI, digits2 )
) )




###### Merge in table-friendly variable names #####

setwd(codebook.dir)
require(readxl)
cd = read_xlsx("Analysis dataset codebook.xlsx")

require(plyr)
table2$Outcome = mapvalues( table2$Outcome, from = cd$Variable, to = cd$`Long name`)
table3$Outcome = mapvalues( table3$Outcome, from = cd$Variable, to = cd$`Long name`)
# warnings are expected because codebook contains extra variables


###### Write Results #####

if ( write.results == TRUE ) {
  setwd(results.dir)
  
  flavor = ifelse( TMLE == TRUE, "tmle", "nontmle")
  
  string = paste( "table2",
                  link,
                  flavor,
                  missingness,
                  ".csv",
                  sep = "_" )
  
  write.csv(table2, string)
  
  
  string = paste( "table3",
                  link,
                  flavor,
                  missingness,
                  ".csv",
                  sep = "_" )
  
  write.csv(table3, string)
}



########################### OTHER STATS MENTIONED IN PROSE ########################### 


# median correlation magnitude
Ys = d[ , c(Ylin, Ycount) ]

# binaries need to be 0/1 for this
binaries = Ycount[ !Ycount == "flourish_d" ]
Ys[ , binaries ] = apply(Ys[ , binaries ], 2, function(x) binarize(x) )

# use only complete cases
Ys = Ys[ complete.cases(Ys), ]

# correlations without the the diagonal elements
corrs = as.numeric( abs( cor( Ys ) ) )
corrs = corrs[ !corrs == 1 ]
summary(corrs)


# 
# ########################### CC ANALYSIS SANITY CHECK ########################### 
# 
# link = "OLS"
# 
# # names of outcomes with this link
# if ( link == "OLS" ) Ynames = Ylin
# if ( link == "poisson" ) Ynames = Ycount
# if ( link == "logistic" ) Ynames = Ybin
# 
# CCres = analyze_CC_dataset( d = d,  # for sanity check on complete data
#                     #d = complete(imps, i), 
#                     X = Xname,
#                     C = Cnames,
#                     Ys = Ynames,
#                     alpha = 0.05,
#                     resample = resample,
#                     B = B.resamp,  
#                     model = link,
#                     TMLE = TMLE )
# 
# data.frame( outcome = mapvalues( Ynames, cd$Variable, cd$`Long name`),
#             est = CCres$samp.res$bhats,
#             pval = CCres$samp.res$pvals)
