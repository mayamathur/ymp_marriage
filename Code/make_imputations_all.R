
 
# # FOR LOCAL USE
# root.dir = "/Users/mmathur/Dropbox/Personal computer/Independent studies/Ying's marriage paper"
# 
# setwd(root.dir)
# data.dir = paste(root.dir, "[PRIVATE] Data and results/Data/Raw from Ying", sep="/")
# code.dir = paste(root.dir, "Linked to OSF (YMP)/Code", sep="/")
# results.dir = paste(root.dir, "[PRIVATE] Data and results/Results/Marriage - cheating controlled", sep="/")
# full.imputeds.dir = paste(root.dir, "[PRIVATE] Data and results/Data/Imputed full datasets as csv", sep="/")
# codebook.dir = paste(root.dir, "[PRIVATE] Data and results/Data", sep="/")


# # COMMENTED OUT FOR SPEED
# # marriage data
# library(sas7bdat)
# setwd(data.dir)
# # this is for both marriage and divorce? since not restricted?
# d = read.sas7bdat( "merged.sas7bdat" )
# nrow(d)  # 116,412
# # save it to avoid long read-in process
# write.csv( d, "nonrestrict_data.csv", row.names = FALSE )

setwd(data.dir)
library(readr)

# # REAL VERSION:
# d = suppressMessages( read.csv("nonrestrict_data.csv") )
# # create table one before taking random sample
# library(tableone)
# CreateTableOne(data=d, includeNA=TRUE)

# ~~~ TEMP ONLY:
# ~~~ for dry run: FAKE ONLY
# library(dplyr)
# d = sample_n( d, size = 3000 )  # TEMP ONLY!!!
# write.csv(d, "nonrestrict_data_TEST_SUBSAMPLE.csv")
# TEMP ONLY: 
d = suppressMessages( read.csv("nonrestrict_data_TEST_SUBSAMPLE.csv") )


library(testthat)
#expect_equal( nrow(d), 116412) # based on my own analysis

d = as.data.frame(d)

# keep only those with non-missing marital status in 1989
d = d %>% filter( !is.na(mars89) )
#expect_equal( nrow(d), 116144)

# fake = d # safekeeping

M=10



########################### MAKE MI DATASETS ########################### 

#### Variables to impute

# variable name lists from Ying's file "Table2_complete_nabs.sas"
Ylin = c( "fhapp13",
          "hopel13",
          "socint13",
          "support",
          "cesd",
          "anx13",
          "loneli",
          "number13" )

# this also includes the common outcomes because tmle package doesn't support
#  estimating RR for Poisson link (only ATE)
Ybin = c( # common ones
  "depx13",
  "exam",
  "act13_d",
  "nAHEI11a",
  "obese",
  "diabetes",
  "asthma",
  "cancer",
  
  # rare and non-rare ones
  "smoke",
  "alco09",
  "ssleep",
  "death",
  "heart",
  "stroke"
)

Ycount = NA

Xname = "mars93_2"

##### Set Names of Adjusted Covariates #####
# abuse_c is very missing, so is only in MI covariate set, not CC
# Ying's covariate string from Table2_complete_nabs.sas:
# mars93_2 age nhwhite colled nim89 region2 region3 region4 mdinc2 mdinc3 mdinc4 exam89 alco89 smoke89 act89_d number
Cnames.MI = c( "age",
               "nhwhite",
               "colled",
               "nim89",
               "region",
               "mdinc_c",
               "exam89",
               "alco89",
               "smoke89",
               "act89_d",
               "number",
               "abuse_c",
               
               "mars89" # marital status in 1989 (for cheating-controlled method)
)

# Ying's covariate string from Table2_mi.sas:
# mars93_2 age nhwhite colled nim89 abuse_c region2 region3 region4 mdinc2 mdinc3 mdinc4 exam89 alco89 smoke89 act89_d number
Cnames.CC = Cnames.MI[ !Cnames.MI == "abuse_c" ]

# last one is exposure for divorce analyses
# so that we can just impute once
analysis.vars = c(Cnames.CC, Cnames.MI, Xname, Ylin, Ybin, Ycount, "mars93", "mars89")

d = d[ , names(d) %in% analysis.vars ]


##### Make Imputations #####
library(mice)

##### Generate Imputations #####
ini = mice(d, m=1, maxit = 0 )
ini$loggedEvents
if ( !is.null(ini$loggedEvents) ) warning("Imputation trouble: Dry run has logged events! Adjust the code in make_imputations_all.R.")

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
# #  intervene to warning this behavior
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
if ( !is.null(imps$loggedEvents) ) warning("Imputation trouble: Imputations have logged events! Adjust the code in make_imputations_all.R.")


# make sure there is no missing data in the imputations
any.missing = apply( complete(imps,1), 2, function(x) any(is.na(x)) ) # should be FALSE
if ( any(any.missing) == TRUE ) warning("Imputed datasets have missing data! Look at logged events.")


# first imputed dataset
head( complete(imps, 1) )
# if this line returns an error about complete() not being applicable
#  for a mids objects (which is a lie), restart R



if ( write.results == TRUE ) {
  
  # also save imputed datasets as csvs for Ying
  setwd(full.imputeds.dir)
  
  for (i in 1:M) {
    write.csv( complete(imps,i),
               paste("imputed_dataset_", i, ".csv", sep="") )
  }
}


# # read in existing imputations
# # we're doing this even if impute.from.scratch=TRUE to have same data format
# # i.e., a list of imputed datasets instead of a mids object
# setwd(stochastic.results.dir)
# setwd("Imputed full datasets as csv")
# 
# library(readr)
# imps = lapply( list.files(),
#                function(x) suppressMessages(read_csv(x)) )










# ######### OLD VERSIONS:
# 
# setwd(root.dir)
# data.dir = paste(root.dir, "[PRIVATE] Data and results/Data/For cheating-method analyses/Raw from Ying", sep="/")
# code.dir = paste(root.dir, "Linked to OSF (YMP)/Code", sep="/")
# results.dir = paste(root.dir, "[PRIVATE] Data and results/Results/Marriage - cheating controlled", sep="/")
# stochastic.results.dir = paste(root.dir, "[PRIVATE] Data and results/Data", sep="/")
# codebook.dir = paste(root.dir, "[PRIVATE] Data and results/Data", sep="/")
# 
# 
# 
# # # COMMENTED OUT FOR SPEED
# # # marriage data
# # library(sas7bdat)
# # setwd(data.dir)
# # d = read.sas7bdat( "marital_status_nonrestrict.sas7bdat" )
# # nrow(d)  # 116,680
# # # save it to avoid long read-in process
# # write.csv( d, "marital_status_nonrestrict_data.csv", row.names = FALSE )
# 
# setwd(data.dir)
# library(readr)
# d = suppressMessages( read.csv("marital_status_nonrestrict_data.csv") )
# 
# library(testthat)
# expect_equal( nrow(d), 116680) # based on my own analysis
# 
# d = as.data.frame(d)
# 
# fake = d # safekeeping
# 
# # for dry run
# library(dplyr)
# d = sample_n( d, size = 3000 )  # TEMP ONLY!!!
# M=10
# 
# ########################### MAKE MI DATASETS ########################### 
# 
# #### Variables to impute
# 
# # variable name lists from Ying's file "Table2_complete_nabs.sas"
# Ylin = c( "fhapp13",
#           "hopel13",
#           "socint13",
#           "support",
#           "cesd",
#           "anx13",
#           "loneli",
#           "number13" )
# 
# # this also includes the common outcomes because tmle package doesn't support
# #  estimating RR for Poisson link (only ATE)
# Ybin = c( # common ones
#   "depx13",
#   "exam",
#   "act13_d",
#   "nAHEI11a",
#   "obese",
#   "diabetes",
#   "asthma",
#   "cancer",
#   
#   # rare and non-rare ones
#   "smoke",
#   "alco09",
#   "ssleep",
#   "death",
#   "heart",
#   "stroke"
# )
# 
# Ycount = NA
# 
# Xname = "mars93_2"
# 
# ##### Set Names of Adjusted Covariates #####
# # abuse_c is very missing, so is only in MI covariate set, not CC
# # Ying's covariate string from Table2_complete_nabs.sas:
# # mars93_2 age nhwhite colled nim89 region2 region3 region4 mdinc2 mdinc3 mdinc4 exam89 alco89 smoke89 act89_d number
# Cnames.MI = c( "age",
#                "nhwhite",
#                "colled",
#                "nim89",
#                "region2",
#                "region3",
#                "region4",
#                "mdinc2",
#                "mdinc3",
#                "mdinc4",
#                "exam89",
#                "alco89",
#                "smoke89",
#                "act89_d",
#                "number",
#                "abuse_c",
#                
#                "mars89" # marital status in 1989 (for cheating-controlled method)
# )
# 
# # Ying's covariate string from Table2_mi.sas:
# # mars93_2 age nhwhite colled nim89 abuse_c region2 region3 region4 mdinc2 mdinc3 mdinc4 exam89 alco89 smoke89 act89_d number
# Cnames.CC = Cnames.MI[ !Cnames.MI == "abuse_c" ]
# 
# # last one is exposure for divorce analyses
# # so that we can just impute once
# analysis.vars = c(Cnames.CC, Cnames.MI, Xname, Ylin, Ybin, Ycount, "mars93", "mars89")
# 
# d = d[ , names(d) %in% analysis.vars ]
# 
# 
#   ##### Make Imputations #####
#   library(mice)
# 
#     ##### Generate Imputations #####
#     ini = mice(d, m=1, maxit = 0 )
#     ini$loggedEvents
#     if ( !is.null(ini$loggedEvents) ) warning("Imputation trouble: Dry run has logged events! Adjust the code in make_imputations.R.")
#     
#     # check default methods
#     ini$method
#     
#     # # set imputation method
#     # # this works better with "collinear" variables than regular PMM, it seems
#     # # quotes because mice sometimes flags as "collinear"/"constant" variables that aren't
#     # mymethod = ini$method
#     # mymethod[ mymethod == "pmm" ] = "midastouch"  
#     
#     # make smart predictor matrix
#     pred = quickpred(d)
#     
#     # # vars that mice considers too collinear don't get imputed at all
#     # #  intervene to warning this behavior
#     # problem.vars = ini$loggedEvents$out
#     # pred[problem.vars,] = 1
#     # pred[,problem.vars] = 1
#     
#     imps = mice( d,
#                  m=M,
#                  predictorMatrix = pred,
#                  #ridge = 1e-02,
#                  method = "pmm")
#     
#     # any complaints?
#     head(imps$loggedEvents)
#     if ( !is.null(imps$loggedEvents) ) warning("Imputation trouble: Imputations have logged events! Adjust the code in make_imputations.R.")
#     
#     
#     # make sure there is no missing data in the imputations
#     any.missing = apply( complete(imps,1), 2, function(x) any(is.na(x)) ) # should be FALSE
#     if ( any(any.missing) == TRUE ) warning("Imputed datasets have missing data! Look at logged events.")
#   
#     
#     # first imputed dataset
#     head( complete(imps, 1) )
#     # if this line returns an error about complete() not being applicable
#     #  for a mids objects (which is a lie), restart R
# 
#     if ( write.results == TRUE ) {
#       
#       # also save imputed datasets as csvs for Ying
#       setwd(stochastic.results.dir)
#       setwd("Imputed full datasets as csv")
#       
#       for (i in 1:M) {
#         write.csv( complete(imps,i),
#                    paste("imputed_dataset_", i, ".csv", sep="") )
#       }
#     }
#     
# 
#   # read in existing imputations
#   # we're doing this even if impute.from.scratch=TRUE to have same data format
#   # i.e., a list of imputed datasets instead of a mids object
#   setwd(stochastic.results.dir)
#   setwd("Imputed datasets as csvs")
#   
#   library(readr)
#   imps = lapply( list.files(),
#                  function(x) suppressMessages(read_csv(x)) )
# }
# 
