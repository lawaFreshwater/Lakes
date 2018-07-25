#remove history
rm(list = ls())

#Set working directory
od<-getwd()
setwd("//file/herman/R/OA/08/02/2018/Water Quality/R/Lakes")

#reqired packages
require(XML)     ### XML library to write hilltop XML
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(RCurl)
require(reshape2)

#Function
translate <- function(d){
  # Translate Measurement Names
  # This is deliberately presented in verbose style in order
  # to enable edits as requred.
  m<-unique(d$Measurement)
  dfm <- as.data.frame(m, stringsAsFactors = FALSE)
  dfm$LMeasurement <- ""
  
  rgx <- "^pH"
  dfm$LMeasurement[grepl(pattern=rgx,dfm$m)] <- "pH" 
  
  rgx <- "^Chloro"
  dfm$LMeasurement[grepl(pattern=rgx,dfm$m)] <- "Chlorophyll-A"
  
  rgx <- "Secchi|Black|Transparency"
  dfm$LMeasurement[grepl(pattern=rgx,dfm$m,ignore.case=TRUE)] <- "Clarity"
  
  rgx <- "coli"
  dfm$LMeasurement[grepl(pattern=rgx,dfm$m,ignore.case=TRUE)] <- "E. coli"
  
  rgx <- "cyano"
  dfm$LMeasurement[grepl(pattern=rgx,dfm$m,ignore.case=TRUE)] <- "Cyano"
  
  rgx <- "Ammon|NH4"
  dfm$LMeasurement[grepl(pattern=rgx,dfm$m)] <- "Ammoniacal Nitrogen"
  
  rgx <- "Total\\s?[a-zA-Z]*\\s?Nitrogen|TN|Nitrogen.\\(Total\\)"
  dfm$LMeasurement[grepl(pattern=rgx,dfm$m)] <- "Total Nitrogen"
  
  rgx <- "Total\\s?[a-zA-Z]*\\s?Phos|TP|Phosphorus.\\(T"
  dfm$LMeasurement[grepl(pattern=rgx,dfm$m)] <- "Total Phosphorus"
  
  d<-merge(d,dfm,by.x="Measurement",by.y="m",all.x=TRUE)
  d$Measurement <- d$LMeasurement
  
  return(d[,1:(length(d)-1)])

}

#main
curdir<-getwd()

tab="\t"

#fname <- "AllCouncilLakes.csv"
#df <- read.csv(fname,sep=",", stringsAsFactors=FALSE)
load(file="rbindLWQfiles.Rdata") # creates dataframe called "d"
d<-e
d<-translate(d)
d$Time <- as.POSIXlt(x=d$Time,format="%Y-%m-%dT%H:%M:%S",tz="GMT")

# Load list of sites found from the Councils WFS feed
sites <- read.csv(file="LAWA_Site_Table.csv",stringsAsFactors=FALSE)

df2 <- merge(d,sites, by.x= "Site",by.y="CouncilSiteID",all.x = TRUE)

d <- dcast(df2, Region+LawaSiteID+LFENZID+Time+Site~Measurement, value.var= "Value", fun.aggregate = dplyr::first)
#d <- d[c(1,2,8,3,4,5,6,7,9:length(d))]

d <- d[order(d$LawaSiteID,d$Time),]
write.csv(d, file = "LakesFinal_2018-07-25.csv")

setwd(od)