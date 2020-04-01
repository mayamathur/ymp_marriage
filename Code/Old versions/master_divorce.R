
############################## SET YOUR DIRECTORY LOCATIONS ############################## 

rm(list=ls())

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
# # ~~~ MAKE THIS
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
data.dir = paste(root.dir, "Data/Raw from Ying", sep="/")
code.dir = paste(root.dir, "Code", sep="/")
results.dir = paste(root.dir, "Results/Divorce", sep="/")
stochastic.results.dir = paste(root.dir, "Results/Divorce/R objects from analysis", sep="/")
codebook.dir = paste(root.dir, "Data", sep="/")

# marriage data
setwd(data.dir)
d = read.csv("raw_divorce.csv")
nrow(d)  # 73207

#fake = read.sas7bdat( here("Data/Raw from Ying/merge_mi_marriage.sas7bdat") )

# load the helper code
setwd(code.dir)
source("helper_applied_example.R")


############################## SET VARIABLE NAMES ############################## 

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

# this also includes the common outcomes because tmle package doesn't support
#  estimating RR for Poisson link (only ATE)
Ybin = c( # common ones
  "depx13",
  "exam",
  #"alco11_d",
  "act13_d",
  "obese",
  "diabetes",
  "asthma",
  "cancer",
  
  # ~~~ CHECK THAT THESE REALLY ARE RARE
  # rare and non-rare ones
  "smoke",
  "ssleep",
  "death",
  "heart",
  "stroke"
)

Ycount = NA

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

Xname = "mars93_3"

# remove any unnecessary variables 
# otherwise MI breaks down
d = d[, names(d) %in% c(Cnames, Xname, Ylin, Ybin, Ycount) ]

#purpose.inds = which( !is.na(d$purposez) )


########################### LOOK AT MISSINGNESS PATTERNS ########################### 

# commented out because slow
#library(Amelia)
#missmap(d)


# % missingness on each outcome
sort( apply( d[ , names(d) %in% c(Ylin, Ybin) ], 
       2, 
       function(x) sum(is.na(x)) / length(x) ),
      decreasing = TRUE )



########################### RUN #0 (CC TEST): SET ANALYSIS PARAMETERS ###########################

# dry run: OLS with no resampling

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "OLS"

# TMLE or standard MLE?
TMLE = FALSE

missingness = "CC"

# resampling parameters
# should we run from saved imputations or re-impute?
resample = FALSE
resample.from.scratch = FALSE
B.resamp = 500 # number of resamples (~~ increase this)

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analyses_applied_example.R")




########################### MAKE IMPUTATIONS (OR READ THEM IN) ########################### 

# imputation parameters
missingness = "MI" # missing data method ("MI" or "CC")
impute.from.scratch = FALSE
m = 5  # number imputations

# should we overwrite previous results files?
write.results = TRUE

# this script automatically handles case in which we're not making
#  imputations from scratch
setwd(code.dir)
source("make_imputations.R")




########################### RUN #1: SET ANALYSIS PARAMETERS ########################### 

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

missingness = "MI"

# should we overwrite previous results files?
write.results = TRUE

# resampling parameters
# should we run from saved imputations or re-impute?

# CHANGE both to TRUE
resample = TRUE
resample.from.scratch = FALSE
B.resamp = 500 # number of resamples (~~ increase this)

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analyses_applied_example.R")


########################### RUN #2: SET ANALYSIS PARAMETERS ########################### 

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

m=5

# should we overwrite previous results files?
write.results = TRUE

# resampling parameters
# should we run from saved imputations or re-impute?
resample = FALSE
resample.from.scratch = FALSE
B.resamp = 500 # number of resamples (~~ increase this)

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analyses_applied_example.R")


########################### RUN #3: SET ANALYSIS PARAMETERS ########################### 

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

m=5

# should we overwrite previous results files?
write.results = TRUE

# resampling parameters
# should we run from saved imputations or re-impute?
resample = FALSE
resample.from.scratch = FALSE
B.resamp = 500 # number of resamples (~~ increase this)

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analyses_applied_example.R")


########################### RUN #4: SET ANALYSIS PARAMETERS ########################### 

# we will now run analyses_applied_example.R once for each of four model specifications:
# OLS / resample = TRUE / TMLE = FALSE
# OLS / resample = FALSE / TMLE = TRUE
# logistic / resample = FALSE / TMLE = FALSE
# logistic / resample = FALSE / TMLE = TRUE

# set link ("OLS", "poisson", "logistic")
# spelling needs to match options within function fit_model
link = "logistic"

# TMLE or standard MLE?
TMLE = TRUE

m=5

# should we overwrite previous results files?
write.results = TRUE

# resampling parameters
# should we run from saved imputations or re-impute?
resample = FALSE
resample.from.scratch = FALSE
B.resamp = 500 # number of resamples (~~ increase this)

# familywise alpha
# we always set this to 0.05
alpha = 0.05

# alpha for individual tests
# we did both 0.05 and 0.01
alpha.within = 0.05

setwd(code.dir)
source("analyses_applied_example.R")

