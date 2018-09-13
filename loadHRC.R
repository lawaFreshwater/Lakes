## Import data from Council Hilltop Server

## Export to a Hilltop XML file with all supplied data intact.

## ----------------------------------------------------------------------------
## Write Hilltop XML for Water Quality Data

## SET LOCAL WORKING DIRECTORY




## Load libraries ------------------------------------------------
require(XML)     ### XML library to write hilltop XML
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(RCurl)



### Horizons


## To pull the data from hilltop server, I have a config csv that contains the 
## site and measurement names

fname <- "file:///H:/ericg/16666LAWA/2018/Lakes/hrcLWQ_config.csv"
df <- read.csv(fname,sep=",",stringsAsFactors=FALSE)

siteTable=read.csv("H:/ericg/16666LAWA/2018/Lakes/1.Imported/LAWA_Site_Table_Lakes.csv",stringsAsFactors=FALSE)
configsites <- subset(df,df$Type=="Site")[,1]
configsites <- as.vector(configsites)
sites = unique(siteTable$CouncilSiteID[siteTable$Agency=='HRC'])
Measurements <- subset(df,df$Type=="Measurement")[,1]


#function to either create full xml file or return xml file as NULL depending
#on the result from the above funciton
requestData <- function(url){
  (download.file(url,destfile="tmphrc",method="wininet",quiet=T))
  # pause(1)
  xmlfile <- xmlParse(file = "tmphrc")
  unlink("tmpr")
  error<-as.character(sapply(getNodeSet(doc=xmlfile, path="//Error"), xmlValue))
  if(length(error)==0){
    return(xmlfile)   # if no error, return xml data
  } else {
    return(NULL)
  }}




## ===============================================================================
## Getting Site Data 

# For each council server specified...
# Assumption is that gml:pos has coordinates recorded in lat,lon order
## Build XML Document --------------------------------------------
tm<-Sys.time()
cat("Building XML\n")
cat("Creating:",Sys.time()-tm,"\n")

con <- xmlOutputDOM("Hilltop")
con$addTag("Agency", "HRC")

rm(Data)
for(i in 1:length(sites)){
  cat(i,'out of',length(sites),'\n')
  for(j in 1:length(Measurements)){
    
    # url <- paste("http://tsdata.horizons.govt.nz/data.hts?service=Hilltop",
    # "&request=GetData",
    # "&Site=",sites[i],
    #  "&Measurement=",Measurements[j],
    #  "&From=2006-01-01",
    # "&To=2018-01-01",sep="")
    
    
    url <- paste("http://tsdata.horizons.govt.nz/boo.hts?service=SOS",
                 "&request=GetObservation&agency=LAWA&",
                 "&FeatureOfInterest=",sites[i],
                 "&ObservedProperty=",Measurements[j],
                 "&TemporalFilter=om:phenomenonTime,2006-01-01,2018-01-01",sep="")
    url <- gsub(" ", "%20", url)
    
    
    #------------------------------------------
    
    
    
    xmlfile <- requestData(url)
    
    
    if(!is.null(xmlfile)&((!grepl(ignore.case = T,pattern = "No data",xmlValue(xmlRoot(xmlfile)))))){
      #Create vector of times
      time <- sapply(getNodeSet(doc=xmlfile, "//wml2:time"), xmlValue)          #Create vector of  values
      value <- sapply(getNodeSet(doc=xmlfile, "//wml2:value",namespaces=c(wml2="http://www.opengis.net/waterml/2.0")), xmlValue)
      
      
      if(length(time)!=0){
        #Get QC metadata
        xPath <-"//wml2:qualifier"
        c<-getNodeSet(xmlfile,path=xPath)
        QC<-sapply(c,function(el) xmlGetAttr(el, "xlink:title")) 
        
        #Create dataframe holding both
        df <- as.data.frame(time, stringsAsFactors = FALSE)
        df$value <- value
        
        
        #Create vector of units
        myPath<-"//wml2:uom"
        c<-getNodeSet(xmlfile, path=myPath)
        u<-sapply(c,function(el) xmlGetAttr(el, "code"))
        u <-unique(u)
        
        df$Site <- sites[i]
        df$Measurement <- Measurements[j]
        if(length(u)>0){
          df$Units <- u
        }else{
          df$Units <- rep("",length(df$Site))
        }
        
        df <- df[,c(3,4,1,2,5)]
        
        if(!exists("Data")){
          Data <- df
        } else{
          Data <- rbind.data.frame(Data, df)
        }
        
      }  
      
    }else{
      cat('.')
      # cat("Nothing found for",Measurements[j],'at site',sites[i],'\n')
    }
  }
}


tm<-Sys.time()
cat("Building XML\n")
cat("Creating:",Sys.time()-tm,"\n")

con <- xmlOutputDOM("Hilltop")
con$addTag("Agency", "HRC")
#saveXML(con$value(), file="out.xml")

#-------
if(length(t)==0){
  next
} else{
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
      #con$addTag("Divisor", "1")
      con$addTag("Units", Data$Units[i])
      con$addTag("Format", "#.###")
      con$closeTag() # ItemInfo
      con$closeTag() # DataSource
      #saveXML(con$value(), file="out.xml")
      
      # for the TVP and associated measurement water quality parameters
      con$addTag("Data", attrs=c(DateFormat="Calendar", NumItems="2"),close=FALSE)
      d<- Data$Measurement[i]
      
      cat("       - ",Data$Measurement[i],"\n")   ### Monitoring progress as code runs
      
      while(Data$Measurement[i]==d){
        # for each tvp
    cens=''
        if(grepl('<',Data$value[i])){
          cens="$ND\t<\t"
          Data$value[i]=substr(Data$value[i],2,nchar(Data$value[i]))
        }
        if(grepl('>',Data$value[i])){
          cens="$ND\t>\t"  
          Data$value[i]=substr(Data$value[i],2,nchar(Data$value[i]))
        }
        
        con$addTag("E",close=FALSE)
        con$addTag("T",Data$time[i])
        con$addTag("I1", Data$value[i])
        con$addTag("I2", cens)
        
        con$closeTag() # E
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
      con$addTag("I1", paste("QC", QC, sep="\t"))
      con$closeTag() # E
    }
    
    con$closeTag() # Data
    con$closeTag() # Measurement    
    
  }
}

cat("Saving: ",Sys.time()-tm,"\n")
saveXML(con$value(), file=paste0("H:/ericg/16666LAWA/2018/Lakes/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/hrcLWQ.xml"))
cat("Finished",Sys.time()-tm,"\n")

