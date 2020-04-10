

############################## SET UP ############################## 

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

# remove all variables except global ones used in analysis_master.R
rm(list=setdiff(ls(),
                c("this.analysis",
                "analyses",
                ".a",
                "data.dir", 
                "code.dir",
                "custom.imputeds.dir",
                "full.imputeds.dir",
                "resampling.results.dir",
                "codebook.dir",
                "results.dir",
                "root.dir"
                )))




if (this.analysis == "Marriage - main") restrict = "never.married"
if (this.analysis == "Divorce - main") restrict = "married"
if ( grepl(pattern = "cheating", this.analysis) == TRUE ) restrict = "no"

# root.dir = "/Users/mmathur/Dropbox/Personal computer/Independent studies/Ying's marriage paper"
# 
# setwd(root.dir)
# data.dir = paste(root.dir, "[PRIVATE] Data and results/Data/Raw from Ying", sep="/")
# code.dir = paste(root.dir, "Linked to OSF (YMP)/Code", sep="/")
# results.dir = paste(root.dir, "[PRIVATE] Data and results/Results", this.analysis, sep="/")
# 
# codebook.dir = paste(root.dir, "[PRIVATE] Data and results/Data", sep="/")
# 
# full.imputeds.dir = paste(root.dir, "[PRIVATE] Data and results/Data/Imputed full datasets as csv", sep="/")
# 
# resampling.results.dir = paste(root.dir, "[PRIVATE] Data and results/Data/Stochastic resampling results", sep="/")
# 
# # where to save the imputed datasets that have been appropriately restricted, if needed, 
# #  for this particular analysis
# custom.imputeds.dir = paste(root.dir, "[PRIVATE] Data and results/Data/Imputed restricted datasets as csv", this.analysis, sep="/")


library(doParallel)

############################## SET VARIABLE NAMES ############################## 

# this is done first to help us recode and restrict the main dataset

# variable name lists from Ying's file "Table2_complete_nabs.sas"
Ylin = c( "fhapp13",
          "hopel13",
          "socint13",
          "support",
          "cesd",
          "anx13",
          "loneli",
          "number13" )

# IMPORTANT: this also includes the common outcomes because tmle package doesn't support
#  estimating RR for Poisson link (only ATE)
Ybin = c( # common ones
  "depx13",
  "exam",
  "act13_d",
  "nAHEI11a",
  "obese",
  "diabetes",
  "cancer",
  
  # rare and non-rare ones
  "smoke",
  "alco09",
  "ssleep",
  "death",
  "heart",
  "stroke" )

Ycount = NA

# set either divorce or marriage as the exposure
if ( grepl(pattern = "Marriage", x = this.analysis) == TRUE ) Xname = "mars93_2"
if ( grepl(pattern = "Divorce", x = this.analysis) == TRUE ) Xname = "mars93_3"

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
               "abuse_c" )

# for cheating-controlled analyses, also need to control for marital status in '89
if ( this.analysis %in% c("Marriage - cheating controlled",
                          "Divorce - cheating controlled") ) {
  Cnames.MI = c(Cnames.MI,
                "mars89_2",  # omit one level as reference level
                "mars89_3",
                "mars89_4")
}



# Ying's covariate string from Table2_mi.sas:
# mars93_2 age nhwhite colled nim89 abuse_c region2 region3 region4 mdinc2 mdinc3 mdinc4 exam89 alco89 smoke89 act89_d number

Cnames.CC = Cnames.MI[ !Cnames.MI == "abuse_c" ]

analysis.vars = c(Cnames.CC, Cnames.MI, Xname, Ylin, Ybin, Ycount)




############################## READ IN DATA ############################## 

# COMMENTED OUT FOR SPEED
# # marriage data
# library(sas7bdat)
# setwd(data.dir)
# d = read.sas7bdat( "marriage_full_data.sas7bdat" )
# nrow(d)  # 14,986
# # save it to avoid long read-in process
# write.csv( d, "marriage_full_data.csv", row.names = FALSE )

setwd(data.dir)
library(readr)
#d.full = suppressMessages( read.csv("nonrestrict_data.csv") )
# ~~~ TEST ONLY:
d.full = suppressMessages( read.csv("nonrestrict_data_TEST_SUBSAMPLE.csv") )
d.full = d.full %>% filter( !is.na(mars89) )

library(testthat)
#expect_equal( nrow(d.full), 116412) # from my own analysis

# load the helper code
setwd(code.dir)
source("helper_applied_example.R")


# make derived variables and restrict the dataset
d = make_derived_vars(d.full,
                      var.names = Ylin,
                      restrict = restrict)
nrow(d)  
# sanity check
expect_equal( all(d$mars89) == 1, TRUE )

d = as.data.frame(d)



# ########################### CC INITIAL SANITY CHECK: FIT ONE ANALYSIS MODEL BY HAND ########################### 
# y = d$support
# y.centered = ( y - mean(y, na.rm = TRUE) ) / sd( y, na.rm = TRUE )
# 
# ( m = lm( y.centered ~ mars93_2 + age + nhwhite + colled + nim89 + region2 + region3 + region4 + mdinc2 + mdinc3 + mdinc4 + exam89 + alco89 + smoke89 + act89_d + number, data = d ) )
# summary(m)
# 
# # confirm manually
# m$df.residual + 16
# 
# 
# 
# # look for variable name mismatches
# #  i.e., see which ones aren't in dataset
# # should be NA
# expect_equal( TRUE, is.na( analysis.vars[ !analysis.vars %in% names(d) ] ) )
# analysis.vars[ !analysis.vars %in% names(d) ]
# 
# # remove any unnecessary variables 
# # otherwise MI breaks down
# d = d[, names(d) %in% analysis.vars ]



########################### DESCRIPTIVE ########################### 

d = d[, names(d) %in% analysis.vars ]

##### Missingness on Each Variable #####
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
# this is to foreshadow problems with mice() due to collinearity
# needs to be done in this code chunk, prior to making variables categorical
corr = round( cor( d[ complete.cases(d), ] ), 2 )
corr[ corr > 0.5 & corr < 1 ]

# look at highest correlation (not 1) for each variable
apply( corr, 2, function(x) max( abs(x[x < 1]) ) )


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



########################### RECODE VARIABLES ###########################

##### Recode the Original Dataset #####
# look at original codings
library(tableone)
CreateTableOne(data=d, includeNA = TRUE)


##### Recode the Imputed Datasets #####

# ~~~ move this
setwd(full.imputeds.dir)
( M = length( list.files() ) )
missingness = "MI"
write.results = TRUE

# read in each relevant imputed dataset as csv and modify it directly
for ( i in 1:M ) {
  setwd(full.imputeds.dir)
  imp = as.data.frame( suppressMessages( read_csv( paste("imputed_dataset_", i, ".csv", sep="") ) ) )
  
  imp = make_derived_vars( imp,
                           var.names = Ylin,
                           restrict = restrict )
  # write to folder that is specific to this analysis
  setwd( custom.imputeds.dir )
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
setwd(custom.imputeds.dir)

imps = lapply( list.files(),
               function(x) suppressMessages(read_csv(x)) )

CreateTableOne(data = imps[[3]])

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
#                                  MI RESULTS                                #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

########################### RUN #0 (TEST): SET ANALYSIS PARAMETERS ###########################

# dry run: OLS with no resampling

cat("\n\n")
cat("**************** Starting RUN #0 ****************")

run.name = "run0"

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "OLS"

# TMLE or standard MLE?
TMLE = FALSE

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
source("analysis_general.R")

cat("**************** Finished RUN #0 ****************")


########################### RUN #1: OLS / resample = TRUE / TMLE = FALSE ########################### 


cat("\n\n")
cat("**************** Starting RUN #1 ****************")

run.name = "run1"

# we will now run analysis_general.R once for each of four model specifications:
# OLS / resample = TRUE / TMLE = FALSE
# OLS / resample = FALSE / TMLE = TRUE
# logistic / resample = FALSE / TMLE = FALSE
# logistic / resample = FALSE / TMLE = TRUE

# bm

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "OLS"

# TMLE or standard MLE?
TMLE = FALSE

# should we overwrite previous results files?
write.results = TRUE

# resampling parameters
# should we run from saved imputations or re-impute?

# resampling
resample = TRUE
resample.from.scratch = TRUE
B.resamp = 500 # number of resamples (~~ increase this)

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analysis_general.R")

cat("**************** Finished RUN #1 ****************")


########################### RUN #2: OLS / resample = FALSE / TMLE = TRUE ########################### 

cat("\n\n")
cat("**************** Starting RUN #2 ****************")

run.name = "run2"

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
source("analysis_general.R")

cat("**************** Finished RUN #2 ****************")

########################### RUN #3: logistic / resample = FALSE / TMLE = FALSE ########################### 

cat("\n\n")
cat("**************** Starting RUN #3 ****************")

run.name = "run3"

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "logistic"

# TMLE or standard MLE?
TMLE = FALSE


# should we overwrite previous results files?
write.results = TRUE

setwd(code.dir)
source("analysis_general.R")

cat("**************** Finished RUN #3 ****************")


########################### RUN #4: logistic / resample = FALSE / TMLE = TRUE ########################### 

cat("\n\n")
cat("**************** Starting RUN #4 ****************")

run.name = "run4"

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "logistic"

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
source("analysis_general.R")

cat("**************** Finished RUN #4 ****************")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
#                                  CC RESULTS                                #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

missingness = "CC"

########################### RUN #5: OLS CC, regular MLE ###########################

cat("\n\n")
cat("**************** Starting RUN #5 ****************")

run.name = "run5"

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "OLS"

# TMLE or standard MLE?
TMLE = FALSE

# # no resampling
# resample = FALSE
# resample.from.scratch = TRUE
# B.resamp = 500 # number of resamples (~~ increase this)

setwd(code.dir)
source("analysis_general.R")

# bm

cat("**************** Finished RUN #5 ****************")



########################### RUN #6: LOGISTIC CC, regular MLE ###########################

cat("\n\n")
cat("**************** Starting RUN #6 ****************")

run.name = "run6"

link = "logistic"

# TMLE or standard MLE?
TMLE = FALSE

resample = FALSE


setwd(code.dir)
source("analysis_general.R")

cat("**************** Finished RUN #6 ****************")


########################### RUN #7: OLS CC, TMLE ###########################


cat("\n\n")
cat("**************** Starting RUN #7 ****************")

run.name = "run7"

link = "OLS"

# TMLE or standard MLE?
TMLE = TRUE

setwd(code.dir)
source("analysis_general.R")

cat("**************** Finished RUN #7 ****************")


########################### RUN #8: LOGISTIC CC, TMLE ###########################

cat("\n\n")
cat("**************** Starting RUN #8 ****************")

run.name = "run8"

link = "logistic"

# TMLE or standard MLE?
TMLE = TRUE

setwd(code.dir)
source("analysis_general.R")

cat("**************** Finished RUN #8 ****************")





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
#                             MERGE RESULTS FILES                            #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

analysis.dir.name = this.analysis


setwd(code.dir)
source("merge_results.R")





