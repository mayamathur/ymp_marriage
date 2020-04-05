
root.dir = "/Users/mmathur/Dropbox/Personal computer/Independent studies/Ying's marriage paper"

data.dir = paste(root.dir, "[PRIVATE] Data and results/Data/Raw from Ying", sep="/")
code.dir = paste(root.dir, "Linked to OSF (YMP)/Code", sep="/")


analyses = c("Marriage - main",
             "Marriage - cheating uncontrolled",
             "Marriage - cheating controlled",
             "Divorce - main",
             "Divorce - cheating uncontrolled",
             "Divorce - cheating controlled")


for ( i in analyses ){
  
  cat("\n\n")
  cat( paste( "******************** STARTING: ", this.analysis, "********************", sep = "") )
  
  this.analysis = i
  setwd(code.dir)
  source("analysis_general_setup.R")
  
  cat("\n\n")
  cat( paste( "******************** FINISHED: ", this.analysis, "********************", sep = "") )
}

# bm: I had just run i = analyses[1] by hand, not as a loop
# before running the others, check that nothing else needs to be adjusted in code