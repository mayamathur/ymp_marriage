
########################### MAKE MI DATASETS ########################### 

if ( missingness == "MI" ) {
  ##### Make Imputations #####
  library(mice)
  
  if ( impute.from.scratch == TRUE ) {
    ##### Generate Imputations #####
    ini = mice(d, m=1, maxit = 0 )
    ini$loggedEvents
    if ( !is.null(ini$loggedEvents) ) stop("Imputation trouble: Dry run has logged events! Adjust the code in make_imputations.R.")
    
    # check default methods
    ini$method
    
    # # set imputation method
    # # this works better with "collinear" variables than regular PMM, it seems
    # # quotes because mice sometimes flags as "collinear"/"constant" variables that aren't
    # mymethod = ini$method
    # mymethod[ mymethod == "pmm" ] = "midastouch"  
    
    # make smart predictor matrix
    pred = quickpred(d)
    
    # # vars that mice considers too collinear don't get imputed at all
    # #  intervene to stop this behavior
    # problem.vars = ini$loggedEvents$out
    # pred[problem.vars,] = 1
    # pred[,problem.vars] = 1
    
    imps = mice( d,
                 m=M,
                 predictorMatrix = pred,
                 #ridge = 1e-02,
                 method = "pmm")
    
    # any complaints?
    head(imps$loggedEvents)
    if ( !is.null(imps$loggedEvents) ) stop("Imputation trouble: Imputations have logged events! Adjust the code in make_imputations.R.")
    
    
    # make sure there is no missing data in the imputations
    any.missing = apply( complete(imps,1), 2, function(x) any(is.na(x)) ) # should be FALSE
    if ( any(any.missing) == TRUE ) stop("Imputed datasets have missing data! Look at logged events.")
  
    
    # first imputed dataset
    head( complete(imps, 1) )
    # if this line returns an error about complete() not being applicable
    #  for a mids objects (which is a lie), restart R

    if ( write.results == TRUE ) {
      
      # save imputations for reproducibility
      setwd(stochastic.results.dir)
      save( imps, file = "imputed_datasets.RData" )
      
      # also save imputed datasets as csvs for Ying
      setwd(stochastic.results.dir)
      setwd("Imputed datasets as csvs")
      
      for (i in 1:M) {
        write.csv( complete(imps,i),
                   paste("imputed_dataset_", i, ".csv", sep="") )
      }
    }
    
  } # end loop for impute.from.scratch = TRUE
  
  # read in existing imputations
  # we're doing this even if impute.from.scratch=TRUE to have same data format
  # i.e., a list of imputed datasets instead of a mids object
  setwd(stochastic.results.dir)
  setwd("Imputed datasets as csvs")
  
  library(readr)
  imps = lapply( list.files(),
                 function(x) suppressMessages(read_csv(x)) )
}

