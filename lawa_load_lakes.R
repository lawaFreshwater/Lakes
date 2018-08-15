# ------------------------------
# BATCH LOADER FOR COUNCIL DATA
# ------------------------------

message("Load Surface Water Quality data from Councils")
message("-- A folder for todays date will be created and the imported files will be stashed there.")

# Encapsulating mkdir commands in the try() in order to suppress error messages on failure
# Failure's can mean
#               1. Directories already exist
#               1. R:/ drive not mapped to \\file\herman\R\OA\08\02

try(shell(paste('mkdir "H:/ericg/16666LAWA/2018/Lakes/1.Imported/"',format(Sys.Date(),"%Y-%m-%d"),sep=""), translate=TRUE),silent = TRUE)
try(shell(paste('mkdir "R:/2018/Water Quality/4.Analysis/"',format(Sys.Date(),"%Y-%m-%d"),sep=""), translate=TRUE),silent = TRUE)

ldl=readxl::read_xlsx("h:/ericg/16666LAWA/2018/Lakes/1.Imported/LakeDownloadData.xlsx",sheet = "Lakes Data")
by(data = ldl[,7:14],INDICES = ldl$rcid,FUN = function(x)apply(x,2,FUN=function(x)any(!is.na(x))))
## ----------------------------------------------------------------------------,
## Import Lake data to the "1.Imported" folder for 2018

## import destination will be in folder with todays date (created above)
importDestination <- paste("H:/ericg/16666LAWA/2018/Lakes/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/",sep="")

# #Northland
 # source("h:/ericg/16666LAWA/2018/Lakes/loadNRC.R")
 
# #Auckland
 # source("h:/ericg/16666LAWA/2018/Lakes/loadAC.R")
# 
# #Waikato
 # source("h:/ericg/16666LAWA/2018/Lakes/loadWRC.R")
# 
# #Bay of Plenty
source("h:/ericg/16666LAWA/2018/Lakes/loadBOP.R")

# #Gisborne
# source("h:/ericg/16666LAWA/2018/Lakes/loadGDC.R")

#Taranaki
# source("h:/ericg/16666LAWA/2018/Lakes/loadTRC.R")

#Hawkes Bay
# source("h:/ericg/16666LAWA/2018/Lakes/loadHBRC.R")

#Horizons
# source("h:/ericg/16666LAWA/2018/Lakes/loadHRC.R")

#Greater Wellington
# source("h:/ericg/16666LAWA/2018/Lakes/loadGW.R")

#Nelson
# source("h:/ericg/16666LAWA/2018/Lakes/loadNCC.R")

#Tasman
# source("h:/ericg/16666LAWA/2018/Lakes/loadTDC.R")

#Marlborough
# source("h:/ericg/16666LAWA/2018/Lakes/loadMDC.R")

#Canterbury
# source("h:/ericg/16666LAWA/2018/Lakes/loadECAN.R")

#Otago
source("h:/ericg/16666LAWA/2018/Lakes/loadORC.R")

#Southland
# source("h:/ericg/16666LAWA/2018/Lakes/loadES.R")

#West Coast
source("h:/ericg/16666LAWA/2018/Lakes/loadWCRC.R")
