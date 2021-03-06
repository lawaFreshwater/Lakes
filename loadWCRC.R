rm(list = ls())

## Import data from Council Hilltop Server

## Export to a Hilltop XML file with all supplied data intact.

## ----------------------------------------------------------------------------
## Write Hilltop XML for Water Quality Data

## SET LOCAL WORKING DIRECTORY




## Load libraries ------------------------------------------------
require(XML)     ### XML library to write hilltop XML
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(RCurl)



tab="\t"

### WCRC


## To pull the data from hilltop server, I have a config csv that contains the 
## site and measurement names

fname <- "file:///H:/ericg/16666LAWA/2018/Lakes/wcrcLWQ_config.csv"
df <- read.csv(fname,sep=",",stringsAsFactors=FALSE)

siteTable=read.csv("H:/ericg/16666LAWA/2018/Lakes/1.Imported/LAWA_Site_Table_Lakes.csv",stringsAsFactors=FALSE)
configsites <- subset(df,df$Type=="Site")[,1]
configsites <- as.vector(configsites)
sites = unique(siteTable$CouncilSiteID[siteTable$Agency=='WCRC'])
Measurements <- subset(df,df$Type=="Measurement")[,1]

#to remove lines that don't match
# Measurements1 <- c("Ammonia Total", "Nitrogen (Total)","Phosphorous Total", "Chlorophyll a")


#To get metadata into data frame

# mname <- "WCRC lakes metadata 2016.csv"
# meta <- read.csv(mname,sep=",",stringsAsFactors=FALSE)
# 
# metaMeasurements <- unique(meta$Measurement)
# 
# ## Manually matching order of measurement names in the measurement vector to the metaMeasurement vector 
# metaMeasurements <- metaMeasurements[c(1,4,5,7)]
# 
# 
# meas <- cbind.data.frame(metaMeasurements,Measurements1, stringsAsFactors=FALSE)
# names(meas) <- c("Measurement","MeasurementName")
# #join meas to meta
# meta <- merge(meta,meas,by="Measurement",all = TRUE)




#function to either create full xml file or return xml file as NULL depending
#on the result from the above funciton
requestData <- function(url){
  (download.file(url,destfile="tmpwcrc",method="wininet",quiet=T))
  # pause(1)
  xmlfile <- xmlParse(file = "tmpwcrc")
  unlink("tmpr")
  error<-as.character(sapply(getNodeSet(doc=xmlfile, path="//Error"), xmlValue))
  if(length(error)==0){
    return(xmlfile)   # if no error, return xml data
  } else {
    return(NULL)
  }
}




## ===============================================================================
## Getting Site Data 

# For each council server specified...
# Assumption is that gml:pos has coordinates recorded in lat,lon order
## Build XML Document --------------------------------------------
tm<-Sys.time()
cat("Building XML\n")
cat("Creating:",Sys.time()-tm,"\n")

con <- xmlOutputDOM("Hilltop")
con$addTag("Agency", "WCRC")

#--------------------------------------
#Adding in metadata

for(i in 1:length(sites)){
    cat('\n',sites[i],'\t',i,"out of",length(sites),"\n")
  for(j in 1:length(Measurements)){
    
    
    url <- paste("http://hilltop.wcrc.govt.nz/wq.hts?service=Hilltop",
                 "&request=GetData",
                 "&Site=",sites[i],
                 "&Measurement=",Measurements[j],
                 "&From=2006-01-01",
                 "&To=2018-01-01",sep="")
    url <- gsub(" ", "%20", url)
    # cat(url,"\n")
    
    #------------------------------------------
    #Adding measurements values
    
    xmlfile <- requestData(url)
    
    if(!is.null(xmlfile)){
      cat(Measurements[j],'\t')
      xmltop<-xmlRoot(xmlfile)
      
      m<-xmltop[['Measurement']]
      
      
      # Create new node to replace existing <Data /> node in m
      DataNode <- newXMLNode("Data",attrs=c(DateFormat="Calendar",NumItems="2"))
      
      
      #cat(saveXML(DataNode),"\n")
      tab="\t"
      
      
      ## Make new E node
      # Get Time values
      ansTime <- lapply(c("T"),function(var) unlist(xpathApply(m,paste("//",var,sep=""),xmlValue)))
      ansTime <- unlist(ansTime)
      ansValue <- lapply(c("I1"),function(var) unlist(xpathApply(m,paste("//",var,sep=""),xmlValue)))
      if(is.null(ansValue[[1]])){
        ansValue <- lapply(c("Value"),function(var) unlist(xpathApply(m,paste("//",var,sep=""),xmlValue)))
}
      ansValue <- unlist(ansValue)
      
if(0){
  #   # subset data based on site and measurement and sort on date
    #   p <- subset(meta, meta$Site.Name==sites[i] & meta$MeasurementName==Measurements[j])
    #  # p$pDTz <- as.POSIXct(strptime(p$Date.Time,format = "%d/%m/%Y",tz="GMT"))
    # #  p <- p[order(p$pDTz),]
    #   
    #   #Concatenate a column with all metadata parameters included and create vector
    #   if(nrow(p)!=0){
    #     p$I2 <- paste("LAWAID", p$LAWAID, "Measurement", p$Measurement, "Depth from", p$Depth.from, "Depth to", p$Depth.to, "Sample level", p$Samplelevel,
    #                   "Method", p$Method, "DetectionLimit", p$DetectionLimit, "QualityCode", 
    #                   p$QualityCode, "SampleFrequency", p$SampleFrequency, "Sample type", p$Sample.type, "Laketype", p$Laketype, sep=tab)
    #   }
    #   # remove unnecessary variables
    #   #p<-p[,c(1:5,14:15)]
    #   
    #   ## converting xml to dataframe in order to match datatimes for wq measurement parameters
    #   mdata <- xmlToDataFrame(m[['Data']],stringsAsFactors=FALSE)
    #   mdata$pDTz <- as.POSIXct(strptime(mdata$T,format = "%Y-%m-%dT%H:%M:%S",tz="GMT"))
    #   
    #   if(!is.null(p$I2)){
    #     mdata$I2 <- p$I2
    #   }
    #   mdata <- mdata[complete.cases(mdata$T),]
     } 
      # loop through TVP nodes
      for(N in 1:xmlSize(m[['Data']])){  ## Number of Time series values
        # loop through all Children - T, Value, Parameters ..    
        addChildren(DataNode, newXMLNode(name = "E",parent = DataNode))
        addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "T",ansTime[N]))
        
        #Check for < or > or *
        ## Handle Greater than symbol
        if(grepl(pattern = "^\\>",x =  ansValue[N],perl = TRUE)){
          ansValue[N] <- substr(ansValue[N],2,nchar(ansValue[N]))
          item2 <- paste("$ND",tab,">",tab,sep="")
          
          # Handle Less than symbols  
        } else if(grepl(pattern = "^\\<",x =  ansValue[N],perl = TRUE)){
          ansValue[N] <- substr(ansValue[N],2,nchar(ansValue[N]))
          item2 <- paste("$ND",tab,"<",tab,sep="")
          
          # Handle Asterixes  
        } else if(grepl(pattern = "^\\*",x =  ansValue[N],perl = TRUE)){
          ansValue[N] <- gsub(pattern = "^\\*", replacement = "", x = ansValue[N])
          item2 <- paste("$ND",tab,"*",tab,sep="")
        } else{
          item2 <- ""
        }
        addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "I1",ansValue[N]))
        
        ## Checking to see if other elements in <Data> ie. <Parameter> elements
        if(xmlSize(m[['Data']][[N]])>2){    ### Has attributes to add to Item 2
          for(n in 3:xmlSize(m[['Data']][[N]])){      
            #Getting attributes and building string to put in Item 2
            attrs <- xmlAttrs(m[['Data']][[N]][[n]])  
            
            item2 <- paste(item2,attrs[1],tab,attrs[2],tab,sep="")
            
          }
        }
        
        ## Manually adding supplied parameters
        # if(nchar(item2)==0){
        #   item2 <- mdata$I2[N]
        # }else{
        #   item2 <- paste(item2,tab,mdata$I2[N],tab,sep="")
        # }
        # 
        ## Writing I2 node
        addChildren(DataNode[[xmlSize(DataNode)]], newXMLNode(name = "I2",item2))
        
      } 
      #saveXML(DataNode)
      
      oldNode <- m[['Data']]
      newNode <- DataNode
      replaceNodes(oldNode, newNode)
      
      con$addNode(m) 
      # cat("Completed measurement",Measurements[j],"for",sites[i],"\n\n")
    }
  }
}
cat("\nSaving: ",Sys.time()-tm,"\n")
saveXML(con$value(), file=paste0("H:/ericg/16666LAWA/2018/Lakes/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/wcrcLWQ.xml"))
cat("Finished",Sys.time()-tm,"\n")


x <- readLines(paste0("H:/ericg/16666LAWA/2018/Lakes/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/wcrcLWQ.xml"))
y <- gsub( "NumItems=\"1\"", "NumItems=\"2\"", x, ignore.case = TRUE  )
y <- gsub( "SimpleTimeSeries", "WQData", y, ignore.case = TRUE  )

writeLines(y,con = paste0("H:/ericg/16666LAWA/2018/Lakes/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/wcrcLWQ.xml"))






