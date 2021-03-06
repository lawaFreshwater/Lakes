## Import data from Council Spreadsheets

## Export to a Hilltop XML file with all supplied data intact.

## ----------------------------------------------------------------------------
## Write Hilltop XML for Water Quality Data

# Clearing workspace
rm(list = ls())


## Load libraries ------------------------------------------------
require(XML)     ### XML library to write hilltop XML
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(ggplot2)
library(gridExtra)
library(scales)
require(tidyr)   ### for reshaping data
library(readr)

#/* -===Include required function libraries===- */ 

source("file:///H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")

## --- Functions ---
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## Convert datestring to mow seconds (number of seconds since 1-Jan-1940 00:00)
mowSecs <- function(x){
  s<-strptime("1940-01-01","%Y-%m-%d", tz="GMT")  # USING GMT to avoid daylight time offset as default
  if(nchar(x[1])>=13){                               # arguments assume time based on NZST and NZDT for
    t<-strptime(x,"%d/%m/%Y %H:%M", tz="GMT")     # different parts of the year.
  } else {                                        # Will need to be aware of this for other work.
    t<-strptime(x,"%d/%m/%Y", tz="GMT")
  }
  x<-(t-s)*86400
}

value <- function (val){
  
  if(grepl(pattern = "^[<>]",x=val,perl = TRUE)){
    x<-gsub(pattern = "^[<>]", replacement = "", x = val)
  } else {x<-val}
  x<-trim(x)
  
}



nd <- function(val){
  n<-grepl(pattern = "^<",x=val,perl = TRUE)
}


### BAY OF PLENTY'UTF-8-BOM'
fname <- "H:/ericg/16666LAWA/2018/Lakes/1.Imported/BoPLakesData2017a.csv"
df <- read_csv(fname,guess_max = 20000)
mtRows=which(apply(df,1,function(x)length(x)==sum(is.na(x))))
if(length(mtRows)>0){
  df=df[-mtRows,]
}
rm(mtRows)

df$`site name`[which(df$`site name`=='Site 1')] <- "Lake Rotoma Site 1"

df$mowsecs   <- mowSecs(df$sdate)
#check mowSec output
cat("Number of NAs in derived mowsec field: ",sum(is.na(df$mowsecs)),"out of",length(df$mowsecs),"rows\n")

df$Date.Time <- strptime(df$sdate,"%d/%m/%Y", tz="GMT")
df$DataSource <- df$parameter
df$Qmowsecs   <- df$mowsecs   ## storing original mowsec value should any duplicate samples be found.

df$value      <- as.numeric(sapply(df$`Value (mg/m3)`,value))
df$value[which(is.na(df$value))] <- df$`Value (mg/m3)`[which(is.na(df$value))]
df$nd         <- nd(df$`Uncencored value`)

# reorder data to enable writing to Hilltop File
# Sort by indexing order - Site, Measurement, DateTime
df <- df[order(df$`site name`,df$parameter,df$mowsecs),]

sites<-unique(df$`site name`)
lawa <-unique(df$`LAWA ID`)
measurements<-unique(df$parameter)


## Build XML Document --------------------------------------------
tm<-Sys.time()
cat("Building XML\n")
cat("Creating:",Sys.time()-tm,"\n")

con <- xmlOutputDOM("Hilltop")
con$addTag("Agency", "Bay of Plenty")
#saveXML(con$value(), file="out.xml")

tab <- "\t"

max<-dim(df)[1]

i<-1
#for each site
while(i<=max){
  s<-df$`site name`[i]
  # store first counter going into while loop to use later in writing out sample values
  start<-i
  
  cat(i,df$`site name`[i],"\n")   ### Monitoring progress as code runs
  
  while(df$`site name`[i]==s){
    #for each parameter
    #cat(datatbl$SiteName[i],"\n")
    con$addTag("Measurement",  attrs=c(SiteName=df$`site name`[i]), close=FALSE)
    
    #### I need to join in the DatasourceName to the parameter name here, or perhaps in the qetl CSV
    con$addTag("DataSource",  attrs=c(Name=df$DataSource[i],NumItems="2"), close=FALSE)
    con$addTag("TSType", "StdSeries")
    con$addTag("DataType", "WQData")
    con$addTag("Interpolation", "Discrete")
    con$addTag("ItemInfo", attrs=c(ItemNumber="1"),close=FALSE)
    con$addTag("ItemName", df$parameter[i])
    con$addTag("ItemFormat", "F")
    con$addTag("Divisor", "1")
    con$addTag("Units", df$Units[i])
    #con$addTag("Units", "Joking")
    con$addTag("Format", df$Format[i])
    con$closeTag() # ItemInfo
    con$closeTag() # DataSource
    #saveXML(con$value(), file="out.xml")
    
    # for the TVP and associated parameter water quality parameters
    con$addTag("Data", attrs=c(DateFormat="mowsecs", NumItems="2"),close=FALSE)
    d<- df$parameter[i]
    
    cat("       - ",df$parameter[i],"\n")   ### Monitoring progress as code runs
    
    
    while(df$parameter[i]==d){
      
      # remember mowsec (record mowsec at end of while loop) mowsec <- df$mowsecs[i].
      # If next sample has the same mowsec 
      #       then increment mowsec by 1 AND write new result
      
      # Skipping mowsec check for first time through while loop...
      if(i!=start){
        if(df$mowsecs[i]==df$Qmowsecs[i-1]){
          ## If project the same, assume duplicate samples collected. Increment time by 1 second for each one.
          cat("          - duplicate sample time: ", df$Qmowsecs[i-1], " ",df$mowsecs[i], "\n")
          
          #Where sampleID is different, treat as independent sample at that date/time - add 1 second to time.
          df$mowsecs[i] <- df$mowsecs[i-1] + 1
          
        }
      } ## finish of counter check     
      
      # for each tvp
      # Handle Greater than symbols
      if(grepl(pattern = "^>",x =  df$`Uncencored value`[i],perl = TRUE)){
        con$addTag("E",close=FALSE)
        con$addTag("T",df$mowsecs[i])
        con$addTag("I1", gsub(pattern = "^>", replacement = "", x = df$`Uncencored value`[i]))
        con$addTag("I2", paste("$ND",tab,">",tab,
                               "Method",tab,df$Method[i],tab,
                               "Detection Limit",tab,df$DetectionLimit[i],tab,
                               "$QC",tab,df$QualityCode[i],tab,
                               "Result Value",tab,df$RawValue[i],tab,sep=""))
        con$closeTag() # E
        
        # Handle Less than symbols  
      } else if(grepl(pattern = "^<",x =  df$`Uncencored value`[i],perl = TRUE)){
        con$addTag("E",close=FALSE)
        con$addTag("T",df$mowsecs[i])
        con$addTag("I1", gsub(pattern = "^<", replacement = "", x = df$`Uncencored value`[i]))
        con$addTag("I2", paste("$ND",tab,"<",tab,
                               "Method",tab,df$Method[i],tab,
                               "Detection Limit",tab,df$DetectionLimit[i],tab,
                               "$QC",tab,df$QualityCode[i],tab,
                               "Result Value",tab,df$RawValue[i],tab,sep=""))
        con$closeTag() # E
        
        # Handle Asterixes  
      } else if(grepl(pattern = "^\\*",x =  df$`Uncencored value`[i],perl = TRUE)){
        con$addTag("E",close=FALSE)
        con$addTag("T",df$mowsecs[i])
        con$addTag("I1", gsub(pattern = "^\\*", replacement = "", x = df$`Uncencored value`[i]))
        con$addTag("I2", paste("$ND",tab,"*",tab,
                               "Method",tab,df$Method[i],tab,
                               "Detection Limit",tab,df$DetectionLimit[i],tab,
                               "$QC",tab,df$QualityCode[i],tab,
                               "Result Value",tab,df$RawValue[i],tab,sep=""))
        con$closeTag() # E
        
        # Write all other result values  
      } else {
        con$addTag("E",close=FALSE)
        con$addTag("T",df$mowsecs[i])
        if(df$parameter[i]%in%c("Secchi","pH")){
          con$addTag("I1", df$`Value (mg/m3)`[i])
        }else{
          con$addTag("I1", df$`Uncencored value`[i])
        }
        con$addTag("I2", paste("Method",tab,df$Method[i],tab,
                               "Detection Limit",tab,df$DetectionLimit[i],tab,
                               "$QC",tab,df$QualityCode[i],tab,
                               "Result Value",tab,df$RawValue[i],tab,sep=""))
        con$closeTag() # E
      }
      
      #correct<-0
      i<-i+1 # incrementing overall for loop counter
      if(i>max){break}
      
      
    }
    # next
    con$closeTag() # Data
    con$closeTag() # parameter
    
    
    if(i>max){break}
    # Next 
  }
  # store last counter going out of while loop to use later in writing out sample values
  end<-i-1
  
  # Adding WQ Sample Datasource to finish off this Site
  # along with Sample parameters
  con$addTag("Measurement",  attrs=c(SiteName=df$`site name`[start]), close=FALSE)
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
  con$addTag("Data", attrs=c(DateFormat="mowsecs", NumItems="1"),close=FALSE)
  # for each tvp
  if(0){
    ## THIS NEEDS SOME WORK.....
    ## just pulling out mowsecs Depth from, depth to, sample level, sample frequency, laketype
    sample<-df[,match(c("mowsecs","Frequency"),names(df))] ## 
    sample<-sample[start:end,]
    sample<-as.tbl(sample)
    # sample<-distinct(sample,mowsecs)
    
    sample<-sample[order(sample$mowsecs),]
    ## THIS NEEDS SOME WORK.....
    for(a in 1:nrow(sample)){ 
      con$addTag("E",close=FALSE)
      con$addTag("T",sample$mowsecs[a])
      # con$addTag("I1", paste("Depth From",tab,  sample$Depth.from[a], tab, "Depth To", tab, sample$Depth.to[a], tab, "Sample Level", tab,
      #                        sample$Samplelevel..epilimnion..thermocline..hypolimnion.[a], "Sample Frequency",tab, sample$SampleFrequency[a],
      #                        tab, "Sample type", sample$Laketype..Polymictic..Stratified..Brackish.[a], tab,  sep=""))
      con$addTag("I1", paste0("Sample Frequency",tab, sample$Frequency[a]))
      con$closeTag() # E
    }
  }
  con$closeTag() # Data
  con$closeTag() # Measurement    
  
}

cat("Saving: ",Sys.time()-tm,"\n")
saveXML(con$value(), file=paste0("H:/ericg/16666LAWA/2018/Lakes/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/boprcLWQ.xml"))
cat("Finished",Sys.time()-tm,"\n")

