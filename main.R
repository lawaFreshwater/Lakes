# ======================
ANALYSIS<-"Main routine"
# Set working directory
od<-getwd()
rm(list = ls())
setwd("//file/herman/r/oa/08/02/2018/Water Quality/R/Lakes")
source("//file/herman/r/oa/08/02/2018/Water Quality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")


message("LAWA Lakes data pull")
print(Sys.time())
timeAtStart<- Sys.time()

## DATA Import
## Site Tables / Location data
source("lawa_dataPrep_WFS.R")

## Water Quality data
source("lawa_load_lakes.R")

## Preparing data for Export
source("rbindLWQ_files.R")
source("Lakes_format.R")

message("Process finished")
print(Sys.time())
print(Sys.time()-timeAtStart)