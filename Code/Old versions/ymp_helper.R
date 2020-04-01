

# ~~~ CHECK ME
# see Marshall "Combining estimates" paper, pg 3
# ests: ests from m imputations
# ses: ses from m imputations
rubin_se = function( ests, ses ){
  
  m = length(ests)
  
  # within-imputation variance
  Ubar = (1/m) * sum( ses^2 )
  
  # between-imputation variance
  B = (1 / (m-1)) * sum( ( ests - mean(ests) )^2 )
  
  # overall SE
  return( sqrt( Ubar + (1 + (1/m)) * B ) )
}



# pass the CC data (or imputed dataset)

# will run this once for each of three outcome types
analyze_CC_dataset = function( d,
                               X,
                               C = NA,
                               Ys, 
                               alpha = 0.05,
                               resample = FALSE,
                               model = "OLS",
                               TMLE = FALSE ) {

  # # ~~~ TESTING ONLY - OLS
  # X = Xname
  # C = Cnames
  # Ys = Ylin
  # alpha = 0.05
  # center.stats = FALSE
  # bhat.orig = NA
  # resample = FALSE
  # model = "OLS"
  # TMLE = FALSE
  # # ~~~~~ END TESTING
  
  # # ~~~ TESTING ONLY - POISSON
  # X = Xname
  # C = Cnames
  # Ys = Ypois
  # alpha = 0.05
  # center.stats = FALSE
  # bhat.orig = NA
  # resample = FALSE
  # model = "poisson"
  # TMLE = FALSE
  # # ~~~~~ END TESTING
  # 
  # # ~~~ TESTING ONLY - LOGISTIC
  # X = Xname
  # C = Cnames
  # Ys = Ybin
  # alpha = 0.05
  # center.stats = FALSE
  # bhat.orig = NA
  # resample = FALSE
  # model = "logistic"
  # TMLE = FALSE
  # # ~~~~~ END TESTING
  
  
  
  ##### Fit All Models #####
  # seems to work (at least in sense of not hitting errors) for all 3 outcome types :) 
  samp.res = dataset_result( d = d,
                             X = X,
                             C = C,
                             Ys = Ys,  # all outcome names
                             alpha = alpha,
                             center.stats = FALSE,
                             bhat.orig = NA,
                             model = model,
                             TMLE = TMLE )
  
  
  ##### Fit All TMLE Models #####
  
  ##### Generate Resamples #####
  
  if ( resample == TRUE ) {
    library(NRejections)
    
    resamps = NRejections::resample_resid(  X = X,
                                            C = C,
                                            Ys = Ys,
                                            d = d,
                                            alpha = alpha,
                                            resid = samp.res$resid,
                                            bhat.orig = samp.res$b,
                                            B=100,
                                            cores = 8 )
  } else {
    resamps = NA
  }
  
  ##### Return All The Things #####
  # res
  # resamples
  
  return( list( samp.res = samp.res,
                resamps = resamps ) )
  
}








fit_model = function( X,
                      C = NA,
                      Y,
                      Ys,  # used for subsetting bhat.orig
                      d,
                      center.stats = FALSE,
                      bhat.orig = NA,  # bhat.orig is a single value now for just the correct Y
                      model, 
                      TMLE,
                      alpha = 0.05 ) {
  
 # browser()

  # # ~~~ TESTING ONLY
  # X = Xname
  # C = Cnames
  # Y = "Y6"
  # Ys = Ypois
  # alpha = 0.05
  # center.stats = FALSE
  # bhat.orig = NA
  # resample = FALSE
  # model = "poisson"
  # TMLE = FALSE
  # # ~~~~~ END TESTING
  
  
  if ( length(X) > 1 ) stop("X must have length 1")
  
  # all covariates, including the one of interest
  if ( all( is.na(C) ) ) covars = X else covars = c( X, C )
  
  ################# OLS w/o TMLE ################# 
  if ( model == "OLS" & TMLE == FALSE ) {
    if ( all( is.na(C) ) ) m = lm( d[[Y]] ~ d[[X]] )
    # https://stackoverflow.com/questions/6065826/how-to-do-a-regression-of-a-series-of-variables-without-typing-each-variable-nam
    else m = lm( d[[Y]] ~ ., data = d[ , covars] )
    
    # stats for covariate of interest
    if ( all( is.na(C) ) ) m.stats = summary(m)$coefficients[ 2, ]
    else m.stats = summary(m)$coefficients[ X, ]
    
    # should we center stats by the original-sample estimates?
    if( !center.stats ) {
      b = m.stats[["Estimate"]]
      tval = m.stats[["t value"]]
      SE = m.stats[["Std. Error"]]
    }
    if( center.stats ) {
      b = m.stats[["Estimate"]] - bhat.orig[ which(Ys==Y) ]
      SE = m.stats[["Std. Error"]]
      tval = b / SE
    }
    
    df = nrow(d) - length(covars) - 1
    pval = 2 * ( 1 - pt( abs( b / SE ), df ) )

  }
  
  
  ################# Logistic Regression w/o TMLE ################# 
  # bookmark
  if ( model == "logistic" & TMLE == FALSE ) {
    if ( all( is.na(C) ) ) {
      m = glm( d[[Y]] ~ d[[X]], family = "binomial" )
    } else {
      # https://stackoverflow.com/questions/6065826/how-to-do-a-regression-of-a-series-of-variables-without-typing-each-variable-nam
      m = glm( d[[Y]] ~ ., data = d[ , covars], family = "binomial" )
    }
    
    # stats for covariate of interest
    if ( all( is.na(C) ) ) {
      m.stats = summary(m)$coefficients[ 2, ]
    } else {
      m.stats = summary(m)$coefficients[ X, ]
    } 
    
    # ~~~~ NOTE THAT THESE ARE ACTUALLY Z-VALS, NOT TVALS
    # should we center stats by the original-sample estimates?
    if( !center.stats ) {
      b = m.stats[["Estimate"]]
      tval = m.stats[["z value"]]
      SE = m.stats[["Std. Error"]]
    }
    if( center.stats ) {
      b = m.stats[["Estimate"]] - bhat.orig[ which(Ys==Y) ]
      SE = m.stats[["Std. Error"]]
      tval = b / SE
    }
    
    pval = 2 * ( 1 - pnorm( abs( b / SE ) ) )
    df = NA
  }
  
  
  ################# Poisson Regression w/o TMLE ################# 
  # bookmark
  if ( model == "poisson" & TMLE == FALSE ) {
    
    if ( all( is.na(C) ) ) {
      m = glm( d[[Y]] ~ d[[X]], family = "poisson" )
    } else {
      # https://stackoverflow.com/questions/6065826/how-to-do-a-regression-of-a-series-of-variables-without-typing-each-variable-nam
      m = glm( d[[Y]] ~ ., data = d[ , covars], family = "poisson" )
    }
    
    # stats for covariate of interest
    if ( all( is.na(C) ) ) {
      m.stats = summary(m)$coefficients[ 2, ]
    } else {
      m.stats = summary(m)$coefficients[ X, ]
    } 
    
    # ~~~~ NOTE THAT THESE ARE ACTUALLY Z-VALS, NOT TVALS
    # should we center stats by the original-sample estimates?
    if( !center.stats ) {
      b = m.stats[["Estimate"]]
      tval = m.stats[["z value"]]
      SE = m.stats[["Std. Error"]]
    }
    if( center.stats ) {
      b = m.stats[["Estimate"]] - bhat.orig[ which(Ys==Y) ]
      SE = m.stats[["Std. Error"]]
      tval = b / SE
    }
    
    pval = 2 * ( 1 - pnorm( abs( b / SE ) ) )
    df = NA
  }

  
  stats = data.frame( outcome = Y,
                      b = b,
                      SE = SE,  
                      df = df,
                      tval = tval,
                      pval = pval,
                      reject = pval < alpha )
  
  return( list( stats = stats,
                resids = residuals(m) ) )
}




# fits one type of model for all specified Ys

# modified from NRejections
# model: "OLS", "logistic", "poisson"

dataset_result = function( d,
                           X,
                           C = NA,
                           Ys,  # all outcome names
                           alpha = 0.05,
                           center.stats = TRUE,
                           bhat.orig = NA,
                           model = "OLS",
                           TMLE = FALSE ) { 
  
  
  if ( length(X) > 1 ) stop("X must have length 1")
  
  # for each outcome, fit regression model
  # see if each has p < alpha for covariate of interest
  if ( any( all( is.na(C) ) ) ) covars = X
  else covars = c( X, C )
  
  # get the correct bhat for the outcome we're using
  
  # this is a list of lists:
  #  length is equal to number of outcomes
  #  each entry is another list
  # with elements "pval" (scalar) and "resid" (length matches number of subjects)
  
  lists = lapply( X = Ys,
                  FUN = function(y) fit_model( X = X,
                                               C = C,
                                               Y = y,
                                               Ys = Ys,
                                               d = d,
                                               center.stats = center.stats,
                                               bhat.orig = bhat.orig,
                                               model = model, 
                                               TMLE = TMLE,
                                               alpha = alpha ) )
  
  # "flatten" the list of lists
  u = unlist(lists)
  pvals = as.vector( u[ names(u) == "stats.pval" ] )
  
  tvals = as.vector( u[ names(u) == "stats.tval" ] )
  bhats = as.vector( u[ names(u) == "stats.b" ] )
  pvals = as.vector( u[ names(u) == "stats.pval" ] )
  
  # save residuals
  # names of object u are resid.1, resid.2, ..., hence use of grepl 
  mat = matrix( u[ grepl( "resid", names(u) ) ], byrow=FALSE, ncol=length(Ys) ) 
  resid = as.data.frame(mat)
  names(resid) = Ys
  
  # returns vector for number of rejections at each alpha level
  # length should match length of .alpha
  n.reject = vapply( X = alpha, FUN = function(a) sum( pvals < a ), FUN.VALUE=-99 )
  
  return( list( rej = n.reject,
                tvals = tvals,
                bhats = bhats,
                pvals = pvals,
                resid = resid ) )
}



# induce missingness
# https://stackoverflow.com/questions/18837896/degrading-data-randomly-with-pre-existing-missingness

degradefunction <- function(x, del.amount){
  # 1) indicate which cells are NA (works with matrix or df)
  preNAs     <- is.na(x)
  # 2) how many cells are eligible to be degraded?
  OpenSpots  <- prod(dim(x)) - sum(preNAs)
  # 3) of these, select del.amount for replacement with NA
  newNas     <- sample(1:OpenSpots, size = del.amount, replace = FALSE)
  # 4) impute these NAs, ignoring the original NAs
  x[!preNAs][newNas] <- NA
  x
}





