rm(list=ls())

StartYear10 <- 2008
StartYear5 <- 2013
EndYear <- 2017
source("h:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")
source("h:/ericg/16666LAWA/2018/LAWAFunctionsEG.R")

lakeSiteTable <- read.csv(file = "h:/ericg/16666LAWA/2018/Lakes/1.Imported/LAWA_Site_Table_lakes.csv",stringsAsFactors = F)
lakeSiteTable$LawaSiteID[grep("Rotoiti site 3",lakeSiteTable$SiteID,ignore.case = T)]='EBOP-00094'
lawaIDs=read.csv("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/LAWAMasterSiteListasatMarch2018.csv",stringsAsFactors = F)
lawaIDs <- lawaIDs%>%filter(Module=="Lakes")

# save(catSiteTable,file="h:/ericg/16666LAWA/2018/Lakes/Analysis/lawa_sitetable.RData")

# lakeData=read.csv(paste0('h:/ericg/16666LAWA/2018/Lakes/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/lakesWithMetadata.csv'))

try(dir.create(paste0("H:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"))))
# write.csv(lakeData,paste0("H:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/lakesAllCouncils.csv"),row.names = F)

#Load the latest made (might need to check it works nicely across month folders) 
if(!exists('lakeData')){
  aclakeData=tail(dir(path = "H:/ericg/16666LAWA/2018/Lakes/1.Imported",pattern = "LakesWithMetadata.csv",
                      recursive = T,full.names = T,ignore.case=T),1)
  cat(aclakeData)
  lakeData=read.csv(aclakeData,stringsAsFactors = F)
  rm(aclakeData)
  lakeData$Value[which(lakeData$parameter%in%c('TN','TP'))]=
    lakeData$Value[which(lakeData$parameter%in%c('TN','TP'))]*1000  #mg/L to mg/m3   #Until 4/10/18 also NH4N
}

lakeData$month=lubridate::month(lubridate::dmy(lakeData$Date))
lakeData$Year=lubridate::year(lubridate::dmy(lakeData$Date))
lakeData$monYear=paste0(lakeData$month,lakeData$Year)
lakeData=lakeData[which(lakeData$Year>=StartYear10 & lakeData$Year<=EndYear),]
#57243 to 48654

# lakeData <- left_join(lakeData,lakeSiteTable%>%select(-Agency,-Region),by="LawaSiteID",suffix=c("",".y"))


lakeData <- lakeData%>%select(-SWQLanduse,-SWQAltitude)
# lakeData$SWQLanduse[lakeData$SWQLanduse%in%c("native","exotic/rural","Exotic","Native","Forest","Natural","forestry","Native forest","reference")]="Forest"
# lakeData$SWQLanduse[lakeData$SWQLanduse%in%c("Unstated","","<Null>")]=NA
# lakeData$SWQLanduse[lakeData$SWQLanduse%in%c("rural")]="Rural"
# lakeData$SWQLanduse[lakeData$SWQLanduse%in%c("urban")]="Urban"
# 
# lakeData$SWQAltitude[lakeData$SWQAltitude%in%c("Unstated","")]=NA
# lakeData$SWQAltitude[lakeData$SWQAltitude%in%c("lowland")]="Lowland"

# table(lakeData$SWQLanduse)
# table(lakeData$SWQAltitude)
# sum(is.na(lakeData$SWQLanduse))
# sum(is.na(lakeData$SWQAltitude))
# unique(lakeData$LawaSiteID[is.na(lakeData$SWQLanduse)])
# 
# unique(cbind(lakeData$SiteID[!lakeData$SiteID==lakeData$SiteID.y],lakeData$SiteID.y[!lakeData$SiteID==lakeData$SiteID.y]))
# unique(cbind(lakeData$CouncilSiteID[!lakeData$CouncilSiteID==lakeData$CouncilSiteID.y],lakeData$CouncilSiteID.y[!lakeData$CouncilSiteID==lakeData$CouncilSiteID.y]))
# apply(unique(cbind(lakeData$Lat[!lakeData$Lat==lakeData$Lat.y],lakeData$Lat.y[!lakeData$Lat==lakeData$Lat.y])),1,diff)%>%summary


# IMPLEMENT TROPHIC LEVEL FOR LAKES TLI 

lakeDataWide=lakeData%>%
  dplyr::group_by(LawaSiteID,Date,month,Year,parameter)%>%
  dplyr::summarise(Value=median(Value))%>%
  tidyr::spread(parameter,Value)%>%as.data.frame
TLc=2.22+2.54*log(lakeDataWide$CHLA,base = 10)
TLs=5.1+2.6*log((1/lakeDataWide$Secchi)-(1/40),base=10)
TLn=-3.61+3.01*log(lakeDataWide$TN,base=10)
TLp=0.218+2.92*log(lakeDataWide$TP,base=10)
TLI3=(TLc+TLn+TLp)/3 #If no Secchi
TLI4=(TLc+TLs+TLn+TLp)/4
lakeDataWide$TLI = TLI4
lakeDataWide$TLI[is.na(lakeDataWide$TLI)] <- TLI3[is.na(lakeDataWide$TLI)]
rm(TLc,TLs,TLn,TLp,TLI3,TLI4)


lakeDataWide$agency=lakeData$agency[match(lakeDataWide$LawaSiteID,lakeData$LawaSiteID)]
lakeDataWide$SiteName=lakeData$SiteName[match(lakeDataWide$LawaSiteID,lakeData$LawaSiteID)]
lakeDataWide$SiteID=lakeData$SiteID[match(lakeDataWide$LawaSiteID,lakeData$LawaSiteID)]
lakeDataWide$CouncilSiteID=lakeData$CouncilSiteID[match(lakeDataWide$LawaSiteID,lakeData$LawaSiteID)]
lakeDataWide$LFENZID=lakeData$LFENZID[match(lakeDataWide$LawaSiteID,lakeData$LawaSiteID)]
lakeDataWide$LType=lakeData$LType[match(lakeDataWide$LawaSiteID,lakeData$LawaSiteID)]
lakeDataWide$Region=lakeData$Region[match(lakeDataWide$LawaSiteID,lakeData$LawaSiteID)]


TLIlong=lakeDataWide%>%select(-CHLA,-ECOLI,-NH4N,-pH,-Secchi,-TN,-TP)%>%
  tidyr::gather(parameter,Value,TLI)

TLIlong$monYear=paste0(TLIlong$month,TLIlong$Year)

lakeData <- full_join(TLIlong,lakeData)
rm(TLIlong,lakeDataWide)

lakeParam <- c("TP", "NH4N", "TN", "Secchi", "CHLA", "pH", "ECOLI","TLI")

suppressWarnings(rm(lakeData_A,lakeData_med,lakeData_n,lawaLakeMonthlyMedian))
for(i in 1:length(lakeParam)){
  
  lakeData_A = lakeData[tolower(lakeData$parameter)==tolower(lakeParam[i]),]
  #CENSORING
  #Previously for state, left-censored was repalced by half the limit value, and right-censored was imputed
  lakeData_A$origValue=lakeData_A$Value
  if(any(lakeData_A$CenType=='Left')){
    lakeData_A$Value[lakeData_A$CenType=="Left"] <- lakeData_A$Value[lakeData_A$CenType=="Left"]/2
    lakeData_A <- lakeData_A%>%group_by(LawaSiteID)%>%mutate(lcenrep=max(Value[CenType=='Left']))%>%ungroup
    lakeData_A$Value[lakeData_A$CenType=='Left'] <- lakeData_A$lcenrep[lakeData_A$CenType=='Left']
    lakeData_A <- lakeData_A%>%select(-lcenrep)
  }
  if(any(lakeData_A$CenType=='Right')){
    lakeData_A$Value[lakeData_A$CenType=="Right"] <- lakeData_A$Value[lakeData_A$CenType=="Right"]*1.1
    lakeData_A <- lakeData_A%>%group_by(LawaSiteID)%>%mutate(rcenrep=min(Value[CenType=='Right']))%>%ungroup
    lakeData_A$Value[lakeData_A$CenType=='Right'] <- lakeData_A$rcenrep[lakeData_A$CenType=='Right']
    lakeData_A <- lakeData_A%>%select(-rcenrep)
  }

  lakeData_med <- summaryBy(formula=Value~SiteName+parameter+monYear,
                             id=~LawaSiteID+SiteID+CouncilSiteID+agency+Region+Year+month+LFENZID,
                             data=lakeData_A, 
                             FUN=quantile, prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)
  lakeData_med$LAWAID=lakeData_med$LawaSiteID

  lakeData_n <- summaryBy(formula=Value~SiteName+parameter+monYear,
                           id=~LawaSiteID+SiteID+CouncilSiteID+agency+Region+Year+month+LFENZID,
                           data=lakeData_A,
                           FUN=length,keep.names=T)
  lakeData_med$n=lakeData_n$Value

  rm(lakeData_n)
  rm(lakeData_A)
  gc()
  # Building dataframe to save at the end of this step 
  if(i==1){
    lawaLakeMonthlyMedian <- lakeData_med
  } else {
    lawaLakeMonthlyMedian <- rbind(lawaLakeMonthlyMedian,lakeData_med)
  }   
rm(lakeData_med)
}

# Housekeeping
# - Saving the lawaLakeMonthlyMedian table  USED in NOF calculations
write.csv(lawaLakeMonthlyMedian,file=paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),'%Y-%m-%d'),
                             "/lawaLakeMonthlyMedian",StartYear10,"-",EndYear,"ForITE",format(Sys.time(),"%Hh%Mm-%d%b%Y"),".csv"),
          row.names=F)
# lawaLakeMonthlyMedian=read.csv(tail(dir(path="h:/ericg/16666LAWA/2018/Lakes/4.Analysis",pattern = "lawaLakeMonthlyMedian",recursive = T,full.names = T),1),stringsAsFactors = F)

#Lake Trend requirements
#5 year, need 30 monthly measurements over five years

#Annual medians
# lawaLakeMonthlyMedian$SiteName

lakeAnnualMedian <- lawaLakeMonthlyMedian%>%
  filter(parameter=="TLI")%>%
  filter(!is.na(LawaSiteID))%>%
  filter(Year>=StartYear10)%>%
  dplyr::group_by(LFENZID,parameter,Year)%>%
  dplyr::summarise(Median=quantile(Value,prob=0.5,type=5,na.rm=T),n=n())%>%ungroup
lakeSiteTable$LFENZID=as.character(lakeSiteTable$LFENZID)
lakeAnnualMedian$LFENZID=as.character(lakeAnnualMedian$LFENZID)
lakeAnnualMedian <- left_join(lakeAnnualMedian,lakeSiteTable%>%select(LFENZID,SiteID,Agency,Region,Long,Lat))
write.csv(lakeAnnualMedian,paste0('h:/ericg/16666LAWA/2018/Lakes/4.Analysis/',
                                  format(Sys.Date(),'%Y-%m-%d'),'/LakeTLIAnnualMedian',format(Sys.time(),"%Hh%Mm-%d%b%Y"),'.csv'),row.names = F)
# lakeAnnualMedian=read.csv(tail(dir(path='h:/ericg/16666LAWA/2018/Lakes/4.Analysis',pattern='LakeTLIAnnualMedian',recursive = T,full.names = T),1),stringsAsFactors = F)
write.csv(lakeAnnualMedian%>%filter(Year==2017),paste0('h:/ericg/16666LAWA/2018/Lakes/4.Analysis/',format(Sys.Date(),'%Y-%m-%d'),
                                                       '/LakeTLILast12Month',format(Sys.time(),"%Hh%Mm-%d%b%Y"),'.csv'),row.names = F)


#5year medians
lmd5 <- lawaLakeMonthlyMedian%>%
  filter(!is.na(LawaSiteID))%>%
  filter(Year>=StartYear5)%>%
  dplyr::group_by(LawaSiteID,parameter)%>%
  dplyr::summarise(Median=quantile(Value,prob=0.5,type=5,na.rm=T),n=n())%>%ungroup
sum(lmd5$n>=30)/dim(lmd5)[1]
#398 out of 935
lmd5 <- lmd5%>%filter(n>=30) #Require 30 monthly values to calculate 5-year state median

names(lmd5)=c("Location","Parameter","Median",'n')
write.csv(lmd5,file=paste0('h:/ericg/16666LAWA/2018/Lakes/4.Analysis/',format(Sys.Date(),'%Y-%m-%d'),
                           '/5YearLakeState',format(Sys.time(),"%Hh%Mm-%d%b%Y"),'.csv'),row.names = F)
# lmd5=read.csv(tail(dir(path="h:/ericg/16666LAWA/2018/Lakes/4.Analysis",pattern="5yearlakestate.*csv",recursive = T,full.names = T,ignore.case = T),1),stringsAsFactors=F)


# lakeTLI5yrMedian <- lawaLakeMonthlyMedian%>%
#   filter(parameter=="TLI")%>%
#   filter(!is.na(LawaSiteID))%>%
#   filter(Year>=StartYear5)%>%
#   dplyr::group_by(LFENZID,parameter)%>%
#   dplyr::summarise(Median=quantile(Value,prob=0.5,type=5,na.rm=T),n=n())%>%ungroup
# lakeTLI5yrMedian$LFENZID=as.character(lakeTLI5yrMedian$LFENZID)
# lakeTLI5yrMedian <- left_join(lakeTLI5yrMedian,lakeSiteTable%>%select(LFENZID,SiteID,Agency,Region,Long,Lat))
# lakeTLI5yrMedian$siteOnSameLake=lakeSiteTable$SiteID[match(lakeTLI5yrMedian$LFENZID,lakeSiteTable$LFENZID)]
# write.csv(lakeTLI5yrMedian,file=paste0('h:/ericg/16666LAWA/2018/Lakes/4.Analysis/',format(Sys.Date(),'%Y-%m-%d'),'/LakeTLI5yrMedian',format(Sys.time(),"%Hh%Mm-%d%b%Y"),'.csv'),row.names = F)

# lmd5=read.csv(tail(dir(path="h:/ericg/16666LAWA/2018/Lakes/4.Analysis",pattern = "5YearLakeState.csv",recursive = T,full.names = T),1),stringsAsFactors = F)
# TLImed=read.csv(tail(dir(path="h:/ericg/16666LAWA/2018/Lakes/4.Analysis",pattern = "LakeTLI5yrMedian",recursive = T,full.names = T),1),stringsAsFactors = F)




