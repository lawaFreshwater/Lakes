rm(list = ls())

## --- Functions ---
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## Convert datestring to mow seconds (number of seconds since 1-Jan-1940 00:00)
#mowSecs <- function(x){
#  s<-strptime("1940-01-01","%Y-%m-%d")
#  t<-strptime(x,"%Y-%m-%d %H:%M:%S")
#   t<-strptime(x,"%Y-%m-%d %H:%M:%S")
#  x<-(t-s)*86400
#}

require(XML)     ### XML library to write hilltop XML
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(RCurl)





#function to either create full xml file or return xml file as NULL depending
#on the result from the above funciton
requestData <- function(url){
  (download.file(url,destfile="tmpwrc",method="wininet",quiet=T))
  # pause(1)
  xmlfile <- xmlParse(file = "tmpwrc")
  unlink("tmpwrc")
  error<-as.character(sapply(getNodeSet(doc=xmlfile, path="//Error"), xmlValue))
  if(length(error)==0){
    return(xmlfile)   # if no error, return xml data
  } else {
    return(NULL)
  }
}

#There's a different set of sites for Ecoli than for the other measurements.
# sites%in%c(37348,37332,37352,37354,37355,37359,37360,37362,37365,37340,37339,37338)


fname <- "file:///H:/ericg/16666LAWA/2018/Lakes/wrcLWQ_config.csv"
df <- read.csv(fname,sep=",",stringsAsFactors=FALSE)

siteTable=read.csv("H:/ericg/16666LAWA/2018/Lakes/1.Imported/LAWA_Site_Table_Lakes.csv",stringsAsFactors=FALSE)
configsites <- subset(df,df$Type=="Site")[,1]
configsites <- as.vector(configsites)
sites = unique(siteTable$CouncilSiteID[siteTable$Agency=='WRC'])
Measurements <- subset(df,df$Type=="Measurement")[,1]

## Load libraries ------------------------------------------------
require(RODBC)   ### ODBC library for SQL connection
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(XML)     ### XML library to write hilltop XML

for(i in 1:length(sites)){
  cat('\n',i,'out of',length(sites),'\n')
  for(j in 1:length(Measurements)){
    
    # http://envdata.waikatoregion.govt.nz:8080/KiWIS/KiWIS?datasource=0&
    #   service=SOS&version=2.0&request=GetObservation&featureOfInterest=37362&
    #     procedure=LBACTO.Sample.Results.P&observedProperty=EColi&
    #       temporalfilter=om:phenomenonTime,P10Y
    
    # if(Measurements[j]=="EColi"){
    # url <- paste0("http://envdata.waikatoregion.govt.nz:8080/KiWIS/KiWIS?",
    #               "datasource=0&service=SOS&version=2.0&request=GetObservation&featureOfInterest=",sites[i],
    #               "&procedure=LBACTO.Sample.Results.P&observedProperty=EColi&temporalfilter=om:phenomenonTime,P10Y")  
    # }else{
    url <- paste0("http://envdata.waikatoregion.govt.nz:8080/KiWIS/KiWIS?",
                  "datasource=0&service=SOS&version=2.0&request=GetObservation&agency=LAWA&featureOfInterest=",sites[i],
                  "&procedure=LWQ.Sample.Results.P&observedProperty=",
                  Measurements[j], "&temporalfilter=om:phenomenonTime,P12Y")
# }
    xmlfile <- requestData(url)
    
   if(xpathApply(xmlRoot(xmlfile),path="count(//wml2:point)",xmlValue)==0){
     next
   }
    cat(Measurements[j],'\t')
    #STEP 1: Load wml2:DefaultTVPMetadata to a vector
    xattrs_qualifier         <- xpathSApply(xmlfile, "//wml2:DefaultTVPMeasurementMetadata/wml2:qualifier/@xlink:title")
    if(is.null(xattrs_qualifier)){
      xattrs_qualifier <- ""
    }
    xattrs_uom               <- xpathSApply(xmlfile, "//wml2:DefaultTVPMeasurementMetadata/wml2:uom/@code")
    xattrs_interpolationType <- xpathSApply(xmlfile, "//wml2:DefaultTVPMeasurementMetadata/wml2:interpolationType/@xlink:title")
    xattrs_default           <- c(xattrs_qualifier,xattrs_uom,xattrs_interpolationType)
    names(xattrs_default)    <- c("qualifier","uom","interpolationType")
    rm(xattrs_qualifier,xattrs_uom,xattrs_interpolationType)
    
    #STEP 2: Get wml2:MeasurementTVP metadata values
    xattrs_qualifier         <- xpathSApply(xmlfile, "//wml2:TVPMeasurementMetadata/wml2:qualifier/@xlink:title")
    
    #If xattrs_qualifier is empty, it means there are no additional qualifiers in this timeseries.
    #Test for Null, and create an empty dataframe as a consequence 
    if(is.null(xattrs_qualifier)){
      df_xattrs <- data.frame(time=character(),qualifier=character())
    } else{
      xattrs_time              <- sapply(getNodeSet(doc=xmlfile, "//wml2:TVPMeasurementMetadata/wml2:qualifier/@xlink:title/../../../../wml2:time"), xmlValue)
      #Store measurementTVPs in dataframe to join back into data after it is all retrieved
      df_xattrs <- as.data.frame(xattrs_time,stringsAsFactors = FALSE)
      names(df_xattrs) <- c("time")
      df_xattrs$qualifier <- xattrs_qualifier
      rm(xattrs_time,xattrs_qualifier)
    }    
    
    #Step3
    
    #Create vector of times
    time <- sapply(getNodeSet(doc=xmlfile, "//wml2:time"), xmlValue)
    #Create vector of  values
    value <- sapply(getNodeSet(doc=xmlfile, "//wml2:value"), xmlValue)
    
    df <- data.frame(time=time,value=value, stringsAsFactors = FALSE)
    rm(time, value)
    
    df$Site <- sites[i]
    df$Measurement <- Measurements[j]
    df$Units <- xattrs_default[2]  ## xattrs_default vector contains (qualifier_default, unit, interpolationtype)
    df <- df[,c(3,4,1,2,5)]
    
    
    
    # merge in additional qualifiers, if present, from df_xattrs
    if(nrow(df_xattrs)!=0) {
      df <- merge(df,df_xattrs,by="time",all=TRUE)
      df$qualifier[is.na(df$qualifier)] <- xattrs_default[1]
    } else {
      df$qualifier<-xattrs_default[1]
    }
    
    # Remove default metadata attributes for current timeseries
    rm(xattrs_default, df_xattrs)
    
    
    if(!exists("Data")){
      Data <- df
    } else{
      Data <- rbind.data.frame(Data, df)
    }
    
    # Remove current timeseries data frame
    rm(df, xmlfile)
    
    
    
    
  }
}



#p <- sapply(getNodeSet(doc=xmlfilec ,path="//sos:ObservationOffering/swes:name"), xmlValue)

#procedure <- c("RERIMP.Sample.Results.P", "WARIMP.Sample.Results.P")


#----------------
tm<-Sys.time()
cat("\nBuilding XML\n")
cat("Creating:",Sys.time()-tm,"\n")

con <- xmlOutputDOM("Hilltop")
con$addTag("Agency", "WRC")
#saveXML(con$value(), file="out.xml")

#-------

max<-nrow(Data)
#max<-nrows(datatbl)

i<-1
#for each site
while(i<=max){
  s<-Data$Site[i]
  # store first counter going into while loop to use later in writing out sample values
  start<-i
  
  cat(i,Data$Site[i],"\n")   ### Monitoring progress as code runs
  
  while(Data$Site[i]==s){
    #for each measurement
    #cat(datatbl$SiteName[i],"\n")
    con$addTag("Measurement",  attrs=c(SiteName=Data$Site[i]), close=FALSE)
    con$addTag("DataSource",  attrs=c(Name=Data$Measurement[i],NumItems="2"), close=FALSE)
    con$addTag("TSType", "StdSeries")
    con$addTag("DataType", "WQData")
    con$addTag("Interpolation", "Discrete")
    con$addTag("ItemInfo", attrs=c(ItemNumber="1"),close=FALSE)
    con$addTag("ItemName", Data$Measurement[i])
    con$addTag("ItemFormat", "F")
    con$addTag("Divisor", "1")
    con$addTag("Units", Data$Units[i])
    con$addTag("Format", "#.###")
    con$closeTag() # ItemInfo
    con$closeTag() # DataSource

    # for the TVP and associated measurement water quality parameters
    con$addTag("Data", attrs=c(DateFormat="Calendar", NumItems="2"),close=FALSE)
    d<- Data$Measurement[i]
    
    cat("       - ",Data$Measurement[i],"\n")   ### Monitoring progress as code runs
    
    while(Data$Measurement[i]==d){
      # for each tvp
      if(!is.na(Data$qualifier[i])){    # this will need to be expanded to deal with range of qualifiers
        if(grepl("8202",Data$qualifier[i])){                   ## GREATER THAN VALUES
          con$addTag("E",close=FALSE)
          con$addTag("T",Data$time[i])
          con$addTag("I1", paste0(">",Data$value[i]))
          con$addTag("I2", "$ND\t>\t")
          con$closeTag() # E
        } else if(grepl("16394",Data$qualifier[i])){           ## LESS THAN VALUES
          con$addTag("E",close=FALSE)
          con$addTag("T",Data$time[i])
          con$addTag("I1", paste0("<",Data$value[i]))
          con$addTag("I2", "$ND\t<\t")
          con$closeTag() # E 
        } else {                                               ## UNCENSORED VALUES
          con$addTag("E",close=FALSE)
          con$addTag("T",Data$time[i])
          con$addTag("I1", Data$value[i])
          con$addTag("I2", "\t")
          con$closeTag() # E
        }
        # Write all other result values  
      } else {                                                 ## UNCENSORED VALUES
        con$addTag("E",close=FALSE)
        con$addTag("T",Data$time[i])
        con$addTag("I1", Data$value[i])
        con$addTag("I2", "\t")
        con$closeTag() # E
        
      }
      i<-i+1 # incrementing overall for loop counter
      if(i>max){break}
    }
    # next
    con$closeTag() # Data
    con$closeTag() # Measurement
    
    if(i>max){break}
    # Next 
  }
  # store last counter going out of while loop to use later in writing out sample values
  end<-i-1
  
  # Adding WQ Sample Datasource to finish off this Site
  # along with Sample parameters
  con$addTag("Measurement",  attrs=c(SiteName=Data$Site[start]), close=FALSE)
  con$addTag("DataSource",  attrs=c(Name="WQ Sample", NumItems="1"), close=FALSE)
  con$addTag("TSType", "StdSeries")
  con$addTag("DataType", "WQSample")
  con$addTag("Interpolation", "Discrete")
  con$addTag("ItemInfo", attrs=c(ItemNumber="1"),close=FALSE)
  con$addTag("ItemName", "WQ Sample")
  con$addTag("ItemFormat", "S")
  con$addTag("Divisor", "1")
  con$addTag("Units")
  con$addTag("Format", "$$$")
  con$closeTag() # ItemInfo
  con$closeTag() # DataSource
  
  # for the TVP and associated measurement water quality parameters
  con$addTag("Data", attrs=c(DateFormat="Calendar", NumItems="1"),close=FALSE)
  # for each tvp
  ## LOAD SAMPLE PARAMETERS
  ## SampleID, ProjectName, SourceType, SamplingMethod and mowsecs
  sample<-Data[start:end,3]
  sample<-unique(sample)
  sample<-sample[order(sample)]
  ## THIS NEEDS SOME WORK.....
  for(a in 1:length(sample)){ 
    con$addTag("E",close=FALSE)
    con$addTag("T",sample[a])
    #put metadata in here when it arrives
    con$addTag("I1", "")
    con$closeTag() # E
  }
  
  con$closeTag() # Data
  con$closeTag() # Measurement    
  
}
cat("Saving: ",Sys.time()-tm,"\n")
saveXML(con$value(), file=paste0("H:/ericg/16666LAWA/2018/Lakes/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/wrcLWQ.xml"))
cat("Finished",Sys.time()-tm,"\n")
