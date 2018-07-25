#===================================================================================================
#  LAWA DATA PREPARATION - WFS
#  Horizons Regional Council
#
#  4 July 2018
#
#  Sean Hodges
#  Horizons Regional Council
#===================================================================================================

# Clearing workspace
rm(list = ls())

ANALYSIS<-"LOAD WFS"
# Set working directory

od <- getwd()
wd <- "\\\\file\\herman\\R\\OA\\08\\02\\2018\\Water Quality\\R\\Lakes"
setwd(wd)

#logfolder <- "\\\\file\\herman\\R\\OA\\08\\02\\2018\\Water Quality\\ROutput\\"
logfolder <- paste(wd,"\\",sep="")

#/* -===Include required function libraries===- */ 


source("//file/herman/R/OA/08/02/2018/Water Quality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")

## Supplementary functions



ld <- function(url,dataLocation,case.fix=TRUE){
  if(dataLocation=="web"){
    str<- tempfile(pattern = "file", tmpdir = tempdir())
    (download.file(url,destfile=str,method="wininet"))
    
    xmlfile <- xmlParse(file = str)
    unlink(str)
  } else if(dataLocation=="file"){
    cc(url)
    message("trying file",url,"\nContent type  'text/xml'\n")
    if(grepl("xml$",url)){
      xmlfile <- xmlParse(url)
    } else {
      xmlfile=FALSE
    }
  }
  return(xmlfile)
}

# Replace underscores in file - assuming underscores only in element names
us <- function(file){
  str <- readLines(file)
  y <- gsub(x=str,pattern = "[_]",replacement ="",perl = TRUE)
  writeLines(y,file)
}

cc <- function(file){
  x <- readLines(file)
  y <- gsub( "SITEID",            "SiteID",            x, ignore.case = TRUE  )
  y <- gsub( "ELEVATION",         "Elevation",         y, ignore.case = TRUE  )
  y <- gsub( "COUNCILSITEID",     "CouncilSiteID",     y, ignore.case = TRUE  )
  y <- gsub( "LAWASITEID",        "LawaSiteID",        y, ignore.case = TRUE  )
  y <- gsub( "SWMANAGEMENTZONE",  "SWManagementZone",  y, ignore.case = TRUE  )
  y <- gsub( "GWMANAGEMENTZONE",  "GWManagementZone",  y, ignore.case = TRUE  )
  y <- gsub( "CATCHMENT",         "Catchment",         y, ignore.case = TRUE  )
  y <- gsub( "NZREACH",           "NZReach",           y, ignore.case = TRUE  )
  y <- gsub( "DESCRIPTION",       "Description",       y, ignore.case = TRUE  )
  y <- gsub( "PHOTOGRAPH",        "Photograph",        y, ignore.case = TRUE  )
  y <- gsub( "SWQUALITY",         "SWQuality",         y, ignore.case = TRUE  )
  y <- gsub( "SWQUALITYSTART",    "SWQualityStart",    y, ignore.case = TRUE  )
  y <- gsub( "SWQFREQUENCYALL",   "SWQFrequencyAll",   y, ignore.case = TRUE  )
  y <- gsub( "SWQFREQUENCYLAST5", "SWQFrequencyLast5", y, ignore.case = TRUE  )
  y <- gsub( "SWQALTITUDE",       "SWQAltitude",       y, ignore.case = TRUE  )
  y <- gsub( "SWQLANDUSE",        "SWQLanduse",        y, ignore.case = TRUE  )
  y <- gsub( "RWQUALITY",         "RWQuality",         y, ignore.case = TRUE  )
  y <- gsub( "RWQUALITYSTART",    "RWQualityStart",    y, ignore.case = TRUE  )
  y <- gsub( "LWQUALITY",         "LWQuality",         y, ignore.case = TRUE  )
  y <- gsub( "LWQUALITYSTART",    "LWQualityStart",    y, ignore.case = TRUE  )
  y <- gsub( "LTYPE",             "LType",             y, ignore.case = TRUE  )
  y <- gsub( "LFENZID",           "LFENZID",           y, ignore.case = TRUE  )
  y <- gsub( "MACRO",             "Macro",             y, ignore.case = TRUE  )
  y <- gsub( "MACROSTART",        "MacroStart",        y, ignore.case = TRUE  )
  y <- gsub( "SWQUANTITY",        "SWQuantity",        y, ignore.case = TRUE  )
  y <- gsub( "SWQUANTITYSTART",   "SWQuantityStart",   y, ignore.case = TRUE  )
  y <- gsub( "REGION",            "Region",            y, ignore.case = TRUE  )
  y <- gsub( "AGENCY",            "Agency",            y, ignore.case = TRUE  ) 
  y <- gsub( "ns2.",              "",                  y, ignore.case = TRUE  ) 
  y <- gsub( "ns3.",              "",                  y, ignore.case = TRUE  ) 
  
  writeLines(y,file)
  
}


# ======================================
# Load WFS locations from CSV

## Load csv with WFS addresses
urls2018      <- "//file/herman/R/OA/08/02/2018/Water Quality/R/lawa_state/CouncilWFS.csv"
urls          <- read.csv(urls2018,stringsAsFactors=FALSE)
#urls$Agency[urls$Agency=="TDC"] <- "xTDC"   ## Commenting out Tasman DC due to Hilltop Server issues
#urls2016      <- "//file/herman/R/OA/08/02/2016/Water Quality/R/lawa_state/CouncilWFS.csv"
#urls          <- read.csv(urls2016,stringsAsFactors=FALSE)
stopGapNames  <- read.csv("//file/herman/R/OA/08/02/2018/Water Quality/R/lawa_state/agencyRegion.csv",stringsAsFactors=FALSE)

# Drop BOPRC - GIS Server erroring
#urls <- urls[-2,]
# Drop GDC - Error on SErver
#urls <- urls[-5,]

#url <- "https://hbrcwebmap.hbrc.govt.nz/arcgis/services/emar/MonitoringSiteReferenceData/MapServer/WFSServer?request=GetFeature&service=WFS&typename=MonitoringSiteReferenceData&srsName=urn:ogc:def:crs:EPSG:6.9:4326"
#url = "http://gis.horizons.govt.nz/arcgis/services/emar/MonitoringSiteReferenceData/MapServer/WFSServer?request=GetFeature&service=WFS&typename=MonitoringSiteReferenceData"

# Config for data extract from WFS
vars <- c("SiteID","CouncilSiteID","LawaSiteID","LWQuality","LType","LFENZID","Region","Agency")


### Even though the field names have been defined in the documentation, there are still differences in Field Names specified by each Council
### Either 
###  1. Define a method that determines the name of the elements in each WFS feed; OR
###  2. Note discrepencies as ERRORS and feedback to supplying Council.

### We'll go with option 2 for the moment.

### LOG START: output to ROutput folder
logfile <- paste(logfolder,"lawa_dataPrep_WFS.log",sep="")
sink(logfile)
###


for(h in 1:length(urls$URL)){
  if(grepl("^x", urls$Agency[h])){
    next
  } 
  if(!nzchar(urls$URL[h])){  ## returns false if the URL string is missing
    next
  }
  #if(h==3){
  #  next()
  #}

  # Fixing case issue with attribute names with WRC
  if(urls$Agency[h]=="WRC"){
    str<- tempfile(pattern = "file", tmpdir = tempdir())
    (download.file(urls$URL[h],destfile=str,method="wininet"))
    cc(str)
    xmldata <- xmlParse(file = str)
    unlink(str)
  } else if(urls$Agency[h]=="ECAN") {
  # Fixing field name issue with Ecan
    str<- tempfile(pattern = "file", tmpdir = tempdir())
    (download.file(urls$URL[h],destfile=str,method="wininet"))
    us(str)
    xmldata <- xmlParse(file = str)
    unlink(str)

  } else {
    #load up every other end point without needing to fix case in the file.
    xmldata<-ld(urls$URL[h],urls$Source[h])
  }
  
  if(urls$Source[h]=="file" & grepl("csv$",urls$URL[h])){
    cc(urls$URL[h])
    tmp <- read.csv(urls$URL[h],stringsAsFactors=FALSE,strip.white = TRUE,sep=",")
    
    tmp <- tmp[,c(5,7,8,9,30,10,28,29,20,21,22)]
    tmp$Lat <- sapply(strsplit(as.character(tmp$pos),' '), "[",1)
    tmp$Long <- sapply(strsplit(as.character(tmp$pos),' '), "[",2)
    tmp <- tmp[-11]
    if(!exists("siteTable")){
      siteTable<-as.data.frame(tmp,stringsAsFactors=FALSE)
    } else{
      siteTable<-rbind.data.frame(siteTable,tmp,stringsAsFactors=FALSE)
    }
    rm(tmp)
    
    
  } else {
    ### Determine the values used in the [emar:LWQuality] element
    lwq<-unique(sapply(getNodeSet(doc=xmldata, path="//emar:MonitoringSiteReferenceData/emar:LWQuality"), xmlValue))
    ns <- "emar:"
    ## if emar namespace does not occur before TypeName in element,then try without namespace
    ## Hilltop Server v1.80 excludes name space from element with TypeName tag
    if(length(lwq)==0){
      lwq<-unique(sapply(getNodeSet(doc=xmldata, path="//MonitoringSiteReferenceData/emar:LWQuality"), xmlValue))
      ns <- ""
    }
    
    # if the only value returned is a No, NO, N, False or false, then no lake records in WFS
    if(length(lwq)==1){
      if(lwq %in% c("no","No","NO","N","F","false","FALSE","False")){
        cat(urls$Agency[h],"has no records for <emar:LWQuality>\n")
      }
    } else {
      
      
      # since it appears that the possible values for Yes,No, True, False, Y, N, T,F, true, false, yes, no all have the
      # sample alphabetic order, Y, Yes, y, yes, True, true, T, t are always going to be item 2 in this character vector.
      # Handy.
      # Enforcing order in lwq
      lwq<-lwq[order(lwq,na.last = TRUE)]
      # If there are three or more values that LWQuality can take in the WFS
      # this needs to be feed back to the Council to get it resolved.
      # in the meantime, just reduce it to two items, and check if the second item starts
      # with a "y" or "t". If second item doesn't, force it.
      if(length(lwq)>=3){
        lwq<-lwq[-1]
        if(!grepl(lwq[2],pattern="^[YyTt]")) lwq[2]<-"TRUE"
      }
      # Resolving case issue of values for LWQuality element returned from Ecan
      # if(identical(lwq,c("NO","Yes","YES"))){
      #   lwq <- c("NO","YES")
      # }
      
      
      if(length(lwq)==2){
        module <- paste("[emar:LWQuality='",lwq[2],"']",sep="")
      } else {
        module <- paste("[emar:LWQuality='",lwq,"']",sep="")
      }
        
      #xmltop<-xmlRoot(xmldata)
      #c <- length(xmlSApply(xmltop, xmlSize)) # number of children for i'th E Element inside <Data></Data> tags
      cat("\n",urls$Agency[h],"\n---------------------------\n",urls$URL[h],"\n",module,"\n",sep="")
    
      # Determine number of records in a wfs with module before attempting to extract all the necessary columns needed
      if(length(sapply(getNodeSet(doc=xmldata, 
                            path=paste("//",ns,"MonitoringSiteReferenceData",module,"/emar:",vars[1],sep="")), xmlValue))==0){
        cat(urls$Agency[h],"has no records for <emar:LWQuality>\n")
    
      } else {
      
  
        # We declared vars earlier. Next section of code goes and gets these values from the WFS
        # in sequence
        #vars <- c("SiteID","CouncilSiteID","LawaSiteID","LWQuality","SWQAltitude","SWQLanduse",
        #          "SWQFrequencyAll","SWQFrequencyLast5","Region","Agency")
  
        for(i in 1:length(vars)){
          
          if(i==1){
            # for the first URL
            a<- sapply(getNodeSet(doc=xmldata, 
                                  path=paste("//emar:LawaSiteID/../../",ns,"MonitoringSiteReferenceData",module,"/emar:",vars[i],sep="")), xmlValue)
            cat(vars[i],":\t",length(a),"\n")
            #Cleaning var[i] to remove any leading and trailing spaces
            trimws(a)
            nn <- length(a)
          } else {
            # for all subsequent URL's
           
            b<- sapply(getNodeSet(doc=xmldata, 
                                  path=paste("//emar:LawaSiteID/../../",ns,"MonitoringSiteReferenceData",module,"/emar:",vars[i],sep="")), xmlValue)
            cat(vars[i],":\t",length(b),"\n")
            if(length(b)==0){
              if(vars[i]=="Region"){
                b[1:nn] <-stopGapNames[stopGapNames$Agency==urls$Agency[h],2]
              } else if(vars[i]=="Agency"){
                b[1:nn]<-stopGapNames[stopGapNames$Agency==urls$Agency[h],1]
              } else {
                b[1:nn]<-""
              }
            }
  
            #Cleaning b to remove any leading and trailing spaces
            trimws(b)
            
            a <- cbind(unlist(a),unlist(b))
          }
      
        }
        a <- as.data.frame(a,stringsAsFactors=FALSE)
        ### grab the latitude and longitude values (WFS version must be 1.1.0)
        latlong    <- sapply(getNodeSet(doc=xmldata, 
                              path=paste("//gml:Point[../../../",ns,"MonitoringSiteReferenceData",module,"]",sep="")), xmlValue)
        
        latlong    <- sapply(getNodeSet(doc=xmldata, 
                             path=paste("//gml:Point[../../emar:LawaSiteID/../../",ns,"MonitoringSiteReferenceData",module,"]",sep="")), xmlValue)
        
        
        llSiteName <- sapply(getNodeSet(doc=xmldata, 
                              path=paste("//gml:Point[../../emar:LawaSiteID/../../",ns,"MonitoringSiteReferenceData",module,"]",
                                         "/../../../",ns,"MonitoringSiteReferenceData/emar:CouncilSiteID",sep="")), xmlValue)
        latlong <- simplify2array(strsplit(latlong," "))
    
        rm(b,xmldata)
        if(nrow(a)==length(latlong[1,])){
          
          a <- cbind.data.frame(a,as.numeric(latlong[1,]),as.numeric(latlong[2,]))
          
        } else {
          b <- as.data.frame(matrix(latlong,ncol=2,nrow=length(latlong[1,]),byrow=TRUE))
          b <- cbind.data.frame(b,llSiteName,stringsAsFactors=FALSE)
          names(b) <- c("Lat","Long","CouncilSiteID")
          #Cleaning CouncilSiteID to remove any leading and trailing spaces
          b$CouncilSiteID <- trimws(b$CouncilSiteID)
          #b$SiteID <- trimws(b$SiteID)
          
          cat("Only",length(latlong[1,]),"out of",nrow(a),"sites with lat-longs.\nSome site locations missing\n")
          
          #if(h==11){  # Change back to 11 once BOPRC included again
          # if(h==12){  # Northland - might be case for all other councils too. Verify
          #   a <- merge(a,b,by.x="V2",by.y="CouncilSiteID",all.x=TRUE)
          # } else {        
          a <- merge(a,b,by.x="V1",by.y="CouncilSiteID",all.x=TRUE)
          # }
          
        }
        rm(latlong)      
        #a<-as.data.frame(a,stringsAsFactors=FALSE)
        names(a)<-c(vars,"Lat","Long")
        if(!exists("siteTable")){
          siteTable<-as.data.frame(a,stringsAsFactors=FALSE)
        } else{
          siteTable<-rbind.data.frame(siteTable,a,stringsAsFactors=FALSE)
        }
        rm(a)
      }
    }
    cat("\n---------------------------\n\n",sep="")
  }
}

### LOG FINISH: output to ROutput folder
sink()
###



#Converting values in the frequency columns to Title case
#From http://www.johnmyleswhite.com/notebook/2009/02/25/text-processing-in-r/
pseudo.titlecase = function(str)
{
  substr(str, 1, 1) = toupper(substr(str, 1, 1))
  return(str)
}

## Swapping coordinate values for Agency=Environment Canterbury Regional Council, Christchurch

agencies <- c("Environment Canterbury","Christchurch")

for(a in 1:length(agencies)){
  lon <- siteTable$Lat[siteTable$Agency==agencies[a]]
  siteTable$Lat[siteTable$Agency==agencies[a]] <- siteTable$Long[siteTable$Agency==agencies[a]]
  siteTable$Long[siteTable$Agency==agencies[a]]=lon
}

#siteTable$Long[siteTable$LawaSiteID=="NRWQN-00022"] <-siteTable$Long[siteTable$LawaSiteID=="NRWQN-00022"][2]

# For WCRC-00031 - location is wrong in WFS
# NZTM coordinates from WCRC website: 1466541,5295450
# WGS84, now:   Latitude	Longitude  	-42.48179737	171.37623113

siteTable$Lat[siteTable$LawaSiteID=="WCRC-00031"]  <- -42.48179737
siteTable$Long[siteTable$LawaSiteID=="WCRC-00031"] <- 171.37623113

## Correcting variations in Region names
siteTable$Region[siteTable$Region=="BayOfPlenty"]   <- "Bay of Plenty"
siteTable$Region[siteTable$Region=="WaikatoRegion"] <- "Waikato"
siteTable$Region[siteTable$Region=="HawkesBay"]     <- "Hawkes Bay"
siteTable$Region[siteTable$Region=="WestCoast"]     <- "West Coast"


## Output for next script
write.csv(x = siteTable,file = "LAWA_Site_Table.csv")
#write.csv(x = siteTable,file = "LAWA_Site_Table1.csv")
write.csv(x = siteTable,file = "LAWA_Site_Table_WFS_PULL.csv")

