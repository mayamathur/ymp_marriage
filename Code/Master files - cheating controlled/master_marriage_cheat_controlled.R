
############################## SET YOUR DIRECTORY LOCATIONS ############################## 

# # FOR CHANNING USE
# # location of data
# data.dir = "/udd/nhych/Marital_status/"
# data.name = "cc.sas7bdat"
# 
# # location of code
# code.dir = "/udd/nhych/Maya/Code"
# 
# # location to save results
# results.dir = "/udd/nhych/Maya/Results"
# 
# # location to save imputations and resamples
# # (or where to look for them if not running from scratch)
# stochastic.results.dir = "/udd/nhych/Maya/Results/Objects"
# 
# # location of codebook
# #codebook.dir = "~/Dropbox/Personal computer/Independent studies/Tyler's outcome-wide paper/Linked to OSF (OWP)/Applied example/MIDUS codebooks"
# 
# setwd(code.dir)
# source("helper_applied_example.R")
# 
# # read in data
# setwd(data.dir)
# 
# library(sas7bdat)
# d = read.sas7bdat(data.name)


# FOR LOCAL USE
root.dir = "/Users/mmathur/Dropbox/Personal computer/Independent studies/Ying's marriage paper"

setwd(root.dir)
data.dir = paste(root.dir, "[PRIVATE] Data and results/Data/For cheating-method analyses/Raw from Ying", sep="/")
code.dir = paste(root.dir, "Linked to OSF (YMP)/Code", sep="/")
results.dir = paste(root.dir, "[PRIVATE] Data and results/Results/Marriage - cheating controlled", sep="/")
stochastic.results.dir = paste(root.dir, "[PRIVATE] Data and results/Results/Marriage - cheating controlled/R objects from analysis", sep="/")
codebook.dir = paste(root.dir, "[PRIVATE] Data and results/Data", sep="/")

# # COMMENTED OUT FOR SPEED
# # marriage data
# library(sas7bdat)
# setwd(data.dir)
# d = read.sas7bdat( "marital_status_nonrestrict.sas7bdat" )
# nrow(d)  # 116,680
# # save it to avoid long read-in process
# write.csv( d, "marital_status_nonrestrict_data.csv", row.names = FALSE )

setwd(data.dir)
library(readr)
d = suppressMessages( read_csv("marital_status_nonrestrict_data.csv") )

library(testthat)
expect_equal( nrow(d), 116680) # based on my own analysis

d = as.data.frame(d)

# load the helper code
setwd(code.dir)
source("helper_applied_example.R")



############################## MAKE MARRIED-AT-1993 VARIABLE ############################## 

# making variables for both marriage and divorce analyses
# since now we are using the same cohort for both

##### Married in 1993 (exposure) #####

# # from Ying's file "NSH2.sas"
# /*EXPOSURE: Marital status (1993)*/
# if nvmar93=1 then mars93=1;  *never married;
# else if marry93=1 then mars93=2; *married;
# else if div93=1 then mars93=3; *divorced or separated;
# else if widow93=1 then mars93=4; *widowed;

table(d$mars93)
d$mars93_2 = (d$mars93 == 2)
table(d$mars93_2)  # married at 1993

##### Divorced in 1993 (exposure) #####

# # from Ying's file "NSH2.sas"
# /*EXPOSURE: Marital status (1993)*/
# if nvmar93=1 then mars93=1;  *never married;
# else if marry93=1 then mars93=2; *married;
# else if div93=1 then mars93=3; *divorced or separated;
# else if widow93=1 then mars93=4; *widowed;

table(d$mars93)
d$mars93_3 = (d$mars93 == 3)
table(d$mars93_3)  # divorced at 1993


##### Marital status in 1989 (baseline control variable) #####

# # from Ying's file "NSH2.sas"
# * Prior marital status (1989);
# if mar89=1 then mars89=1; *never married;
# else if mar89=2 then mars89=2; *married;
# else if mar89 in (3, 4) then mars89=3; *divorced or separated;
# else if mar89=5 then mars89=4;*widowed;

d$mars89 = NA
d$mars89[ d$mar89 == 1 ] = "a.Never married at 89"
d$mars89[ d$mar89 == 2 ] = "b.Married at 89"
d$mars89[ d$mar89 %in% c(3,4) ] = "c.Divorced or separated at 89"
d$mars89[ d$mar89 == 5 ] = "d.Widowed"

table(d$mars89, d$mar89, useNA = "ifany")


############################## SET VARIABLE NAMES ############################## 

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
               "region2",
               "region3",
               "region4",
               "mdinc2",
               "mdinc3",
               "mdinc4",
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
analysis.vars = c(Cnames.CC, Cnames.MI, Xname, Ylin, Ybin, Ycount, "mars93_3")

# look for name mismatches
#  i.e., see which ones aren't in dataset
# should be NA
expect_equal( TRUE, is.na( analysis.vars[ !analysis.vars %in% names(d) ] ) )


# remove any unnecessary variables 
# otherwise MI breaks down
d = d[, names(d) %in% analysis.vars ]



########################### DESCRIPTIVE ########################### 

##### Missingness on Each Variable #####

# prevent scientific notation
options(scipen=999)

sort( apply( d[ , names(d) ], 
             2, 
             function(x) sum(is.na(x)) / length(x) ),
      decreasing = TRUE )

##### Missingness on Each Outcome #####
sort( apply( d[ , names(d) %in% c(Ylin, Ybin) ], 
             2, 
             function(x) sum(is.na(x)) / length(x) ),
      decreasing = TRUE )


##### Look At Correlations Among Variables #####
# # this is to foreshadow problems with mice() due to collinearity
# # needs to be done in this code chunk, prior to making variables categorical
# corr = round( cor( d[ complete.cases(d), ] ), 2 )
# corr[ corr > 0.5 & corr < 1 ]
# 
# # look at highest correlation (not 1) for each variable
# apply( corr, 2, function(x) max( abs(x[x < 1]) ) )


##### Which Binary Outcomes are Rare? #####
# this is used when computing E-values for logistic regression
# from most common to least common
prevalence = sort( apply( d[ , names(d) %in% Ybin ],
                          2,
                          function(x) sum(x[!is.na(x)]) / length(x[!is.na(x)]) ),
                   decreasing = TRUE )
# save list of rare binaries for use in analysis post-processing
setwd(results.dir)
write.csv( data.frame( name = names( prevalence[ prevalence < .1 ] ) ),
           "list_of_rare_binaries.csv",
           row.names = FALSE )

########################### MAKE IMPUTATIONS (OR READ THEM IN) ########################### 

# imputation parameters
missingness = "MI" # missing data method ("MI" or "CC")
impute.from.scratch = TRUE

# number of imputations
# increase above 5 because some vars have >30% missingness
# see Graham "How many imputations are really needed?"
M = 10

# should we overwrite previous results files?
write.results = TRUE

# this script automatically handles case in which we're not making
#  imputations from scratch
setwd(code.dir)
source("make_imputations.R")

# look at one imputed dataset
library(tableone)
CreateTableOne(data=imps[[1]], includeNA=TRUE)


########################### RECODE VARIABLES ###########################

##### Recode the Original Dataset #####
# look at original codings
library(tableone)
CreateTableOne(data=d, includeNA = TRUE)

# recode binaries
# fn automatically checks whether variable actually is binary

# # not elegant, but using apply causes the numerics to 
# # become factors
# for ( i in 1:ncol(d) ) {
#   d[,i] = recode_binary(d[,i])
# }

# also recode the CC dataset, including standardization
d = make_derived_vars(d,
                      var.names = Ylin)

CreateTableOne(data=d)


##### Recode the Imputed Datasets #####
setwd(stochastic.results.dir)
setwd("Imputed datasets as csvs")

# read in each relevant imputed dataset as csv and modify it directly
for ( i in 1:M ) {
  imp = as.data.frame( suppressMessages( read_csv( paste("imputed_dataset_", i, ".csv", sep="") ) ) )
  
  # # recode the binaries
  # for ( j in 1:ncol(imp) ) {
  #   imp[,j] = binarize(imp[,j])
  # }
  
  imp = make_derived_vars(imp,
                          var.names = Ylin)
  # overwrite the old one
  write.csv( imp, paste("imputed_dataset_", i, ".csv", sep="") )
}

# look at the last imputation
CreateTableOne(data=imp,
               includeNA = TRUE)  # second argument only works for categoricals

# check Table 1 on dataset with missingness
( tab1 = CreateTableOne(data=d,
                        includeNA = TRUE) ) # second argument only works for categoricals
# save as csv
tab1mat = print( tab1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
setwd( results.dir )
write.csv(tab1mat, file = "table1_sanity.csv")


# read them back in to have in convenient list
# read in existing imputations
# we're doing this even if impute.from.scratch=TRUE to have same data format
# i.e., a list of imputed datasets instead of a mids object
setwd(stochastic.results.dir)
setwd("Imputed datasets as csvs")

imps = lapply( list.files(),
               function(x) suppressMessages(read_csv(x)) )

CreateTableOne(data = imps[[3]])

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
#                                  MI RESULTS                                #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

# ########################### RUN #0 (TEST): SET ANALYSIS PARAMETERS ###########################
# 
# # dry run: OLS with no resampling
# 
# # set link ("OLS", "poisson", "logistic")
# # spelling needs to match options within function fit_model
# link = "OLS"
# 
# # TMLE or standard MLE?
# TMLE = FALSE
# 
# # resampling parameters
# # should we run from saved imputations or re-impute?
# resample = FALSE
# resample.from.scratch = FALSE
# 
# # familywise alpha
# # we always set this to 0.05
# alpha = 0.05
# 
# # alpha for individual tests
# # we did both 0.05 and 0.01
# alpha.within = 0.05
# 
# setwd(code.dir)
# source("analyses_applied_example.R")


########################### RUN #1: OLS / resample = FALSE / TMLE = FALSE ########################### 

# we will now run analyses_applied_example.R once for each of four model specifications:
# OLS / resample = TRUE / TMLE = FALSE
# OLS / resample = FALSE / TMLE = TRUE
# logistic / resample = FALSE / TMLE = FALSE
# logistic / resample = FALSE / TMLE = TRUE

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "OLS"

# TMLE or standard MLE?
TMLE = FALSE

# should we overwrite previous results files?
write.results = TRUE

# resampling parameters
# should we run from saved imputations or re-impute?

# no resampling
resample = FALSE
resample.from.scratch = FALSE
# B.resamp = 1000 # number of resamples (~~ increase this)

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analyses_applied_example.R")


########################### RUN #2: OLS / resample = FALSE / TMLE = TRUE ########################### 

# we will now run analyses_applied_example.R once for each of four model specifications:
# OLS / resample = TRUE / TMLE = FALSE
# OLS / resample = FALSE / TMLE = TRUE
# logistic / resample = FALSE / TMLE = FALSE
# logistic / resample = FALSE / TMLE = TRUE

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "OLS"

# TMLE or standard MLE?
TMLE = TRUE

# should we overwrite previous results files?
write.results = TRUE

# resampling parameters
resample = FALSE
resample.from.scratch = FALSE

setwd(code.dir)
source("analyses_applied_example.R")


########################### RUN #3: logistic / resample = FALSE / TMLE = FALSE ########################### 

# we will now run analyses_applied_example.R once for each of four model specifications:
# OLS / resample = TRUE / TMLE = FALSE
# OLS / resample = FALSE / TMLE = TRUE
# logistic / resample = FALSE / TMLE = FALSE
# logistic / resample = FALSE / TMLE = TRUE

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "logistic"

# TMLE or standard MLE?
TMLE = FALSE


# should we overwrite previous results files?
write.results = TRUE

setwd(code.dir)
source("analyses_applied_example.R")




########################### RUN #4: logistic / resample = FALSE / TMLE = TRUE ########################### 

# we will now run analyses_applied_example.R once for each of four model specifications:
# OLS / resample = TRUE / TMLE = FALSE
# OLS / resample = FALSE / TMLE = TRUE
# logistic / resample = FALSE / TMLE = FALSE
# logistic / resample = FALSE / TMLE = TRUE

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "logistic"

# bookmark

# TMLE or standard MLE?
TMLE = TRUE


# should we overwrite previous results files?
write.results = TRUE

# resampling parameters
# should we run from saved imputations or re-impute?
resample = FALSE
resample.from.scratch = FALSE

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("helper_applied_example.R")
source("analyses_applied_example.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
#                                  CC RESULTS                                #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

missingness = "CC"

########################### RUN #5 (TEST): OLS CC ###########################

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "OLS"

# TMLE or standard MLE?
TMLE = FALSE

setwd(code.dir)
source("analyses_applied_example.R")



########################### RUN #6: LOGISTIC CC ###########################

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "logistic"

# TMLE or standard MLE?
TMLE = FALSE


setwd(code.dir)
source("analyses_applied_example.R")




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
#                             MERGE RESULTS FILES                            #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

analysis.dir.name = "Marriage"


setwd(code.dir)
source("merge_results.R")





