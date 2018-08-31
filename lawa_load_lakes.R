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
# try(shell(paste('mkdir "R:/2018/Water Quality/4.Analysis/"',format(Sys.Date(),"%Y-%m-%d"),sep=""), translate=TRUE),silent = TRUE)

#This is find out what they had last year?
ldl=readxl::read_xlsx("h:/ericg/16666LAWA/2018/Lakes/1.Imported/LakeDownloadData.xlsx",sheet = "Lakes Data")
by(data = ldl[,7:14],INDICES = ldl$rcid,FUN = function(x)apply(x,2,FUN=function(x)any(!is.na(x))))
## ----------------------------------------------------------------------------,
## Import Lake data to the "1.Imported" folder for 2018

## import destination will be in folder with todays date (created above)
# importDestination <- paste("H:/ericg/16666LAWA/2018/Lakes/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/",sep="")

# #Northland
try(source("h:/ericg/16666LAWA/2018/Lakes/loadNRC.R"))
 
# #Auckland
try( source("h:/ericg/16666LAWA/2018/Lakes/loadAC.R"))
# 
# #Waikato
try(source("h:/ericg/16666LAWA/2018/Lakes/loadWRC.R"))
# 
# #Bay of Plenty
# source("h:/ericg/16666LAWA/2018/Lakes/loadBOP.R")  #Data in a file, doesnt need reloading every time

# #Gisborne
# source("h:/ericg/16666LAWA/2018/Lakes/loadGDC.R")  #No lakes

#Taranaki
try(source("h:/ericg/16666LAWA/2018/Lakes/loadTRC.R"))

#Hawkes Bay
try(source("h:/ericg/16666LAWA/2018/Lakes/loadHBRC.R"))

#Horizons
try(source("h:/ericg/16666LAWA/2018/Lakes/loadHRC.R"))

#Greater Wellington
try(source("h:/ericg/16666LAWA/2018/Lakes/loadGW.R"))

#Nelson
# source("h:/ericg/16666LAWA/2018/Lakes/loadNCC.R")  #No lakes

#Tasman
# source("h:/ericg/16666LAWA/2018/Lakes/loadTDC.R")  #No lakes

#Marlborough
# source("h:/ericg/16666LAWA/2018/Lakes/loadMDC.R")  #No lakes

#Canterbury
try(source("h:/ericg/16666LAWA/2018/Lakes/loadECAN.R"))

#Otago
try(source("h:/ericg/16666LAWA/2018/Lakes/loadORC.R"))

#Southland
try(source("h:/ericg/16666LAWA/2018/Lakes/loadES.R"))

#West Coast
try(source("h:/ericg/16666LAWA/2018/Lakes/loadWCRC.R"))
