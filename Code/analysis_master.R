
rm(list=ls())

########################### SET ALL WORKING DIRECTORIES ########################### 

# CHANGE ONLY THIS ONE, THEN SET UP SUB-DIRECTORIES AS FOLLOWS
root.dir = "/Users/mmathur/Dropbox/Personal computer/Independent studies/Ying's marriage paper"
# in data.dir, put the nonrestrict_data.csv
write.results = TRUE

code.dir = paste(root.dir, "Linked to OSF (YMP)/Code", sep="/")
data.dir = paste(root.dir, "[PRIVATE] Data and results/Data/Raw from Ying", sep="/")
full.imputeds.dir = paste(root.dir, "[PRIVATE] Data and results/Data/Imputed full datasets as csv", sep="/")
codebook.dir = paste(root.dir, "[PRIVATE] Data and results/Data", sep="/")
resampling.results.dir = paste(root.dir, "[PRIVATE] Data and results/Data/Stochastic resampling results", sep="/")


########################### NAMES OF ANALYSES TO BE RUN ########################### 

# analyses = c("Marriage - main",
#              "Marriage - cheating uncontrolled",
#              "Marriage - cheating controlled",
#              "Divorce - main",
#              "Divorce - cheating uncontrolled",
#              "Divorce - cheating controlled")

# for testing
analyses = c(#"Marriage - main",
             "Marriage - cheating uncontrolled",
             "Marriage - cheating controlled",
             "Divorce - main",
             "Divorce - cheating uncontrolled",
             "Divorce - cheating controlled")



########################### MAKE IMPUTATIONS TO BE USED FOR ALL ANALYSES ########################### 

setwd(code.dir)
source("make_imputations_all.R")


# bm: run for all the other i in auto-loop

########################### RUN EACH ONE ########################### 

#for ( .a in analyses[4:5] ){
for ( .a in analyses ){
  
  this.analysis = .a
  
  cat("\n\n")
  cat( paste( "******************** STARTING: ", this.analysis, "********************", sep = "") )

  # set working directories that are specific to this analysis
  results.dir = paste(root.dir, "[PRIVATE] Data and results/Results", this.analysis, sep="/")
  # where to save the imputed datasets that have been appropriately restricted, if needed, 
  #  for this particular analysis
  custom.imputeds.dir = paste(root.dir, "[PRIVATE] Data and results/Data/Imputed restricted datasets as csv", this.analysis, sep="/")
  
  setwd(code.dir)
  source("analysis_general_setup.R")
  
  cat("\n\n")
  cat( paste( "******************** FINISHED: ", this.analysis, "********************", sep = "") )
  
}









