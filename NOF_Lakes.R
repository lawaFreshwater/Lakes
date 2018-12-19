#===================================================================================================
#  LAWA NATIONAL OBJECTIVES FRAMEWORK
#  Horizons Regional Council
#
#  4 September 2016
#
#  Creator: Kelvin Phan  2014
#
#  Updated by: Maree Patterson 2016
#              Sean Hodges
#             Eric Goodwin 2018 Cawthron Institute
#  Horizons Regional Council
#===================================================================================================

#Test does this need doing?
# file.info("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),'%Y-%m-%d'),"/NOF_STATE_2018_Rounded_NAs.csv")$mtime<
#   file.info(file=paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),'%Y-%m-%d'),
#                         "/lawaLakeMonthlyMedian",StartYear10,"-",EndYear,"ForITE",format(Sys.time(),"%Hh%Mm-%d%b%Y"),".csv"))$mtime
# 

rm(list = ls())
library(tidyr)

try(dir.create(paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),'%Y-%m-%d'))))

source("h:/ericg/16666Lawa/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/NOF_Functions.R")
NOF_PERIOD <- 5 # years

lakeSiteTable <- read.csv(file = "h:/ericg/16666LAWA/2018/Lakes/1.Imported/LAWA_Site_Table_lakes.csv",stringsAsFactors = F)
lakeSiteTable$LawaSiteID[grep("Rotoiti site 3",lakeSiteTable$SiteID,ignore.case = T)]='EBOP-00094'

lakeSiteTable$LType[lakeSiteTable$LawaSiteID%in%c('HRC-00355','LAWA-100250','HRC-00332','HRC-00339','HRC-00333','HRC-00341')]='polymictic'
lakeSiteTable$LType[lakeSiteTable$LawaSiteID%in%c('HRC-10004','HRC-00331','HRC-00335','HRC-00337')]='stratified'
lakeSiteTable$LType[lakeSiteTable$SiteID%in%c("436000", "44616","45001","7605"  )]='stratified'
lakeSiteTable$LType[lakeSiteTable$SiteID%in%c("6300"  )]='polymictic'


lakeSiteTable$LType=tolower(lakeSiteTable$LType)
table(lakeSiteTable$LType)

## Load NOF Bands
NOFbandDefinitions <- read.csv("H:/ericg/16666LAWA/2018/Lakes/NOFlakeBandDefinitions.csv", header = TRUE, stringsAsFactors=FALSE)
# Band TotalNitrogenseasonally.stratified.and.brackish TotalNitrogenpolymictic TotalPhosphorus Median.Ammoniacal.N Max.Ammoniacal.N E..coli Ecoli95 EcoliRec540 EcoliRec260  Clarity ChlAMedian    ChlAMax
# 1    A                                          x<=160                  x<=300           x<=10             x<=0.03          x<=0.05  x<=130  x<=540         x<5        x<20      x>7       x<=2      x<=10
# 2    B                                    x>160&x<=350            x>300&x<=500      x>10&x<=20      x>0.03&x<=0.24    x>0.05&x<=0.4  x<=130 x<=1000  x>=5&x<=10 x>=20&x<=30 x>3&x<=7   x>2&x<=5 x>10&x<=25
# 3    C                                    x>350&x<=750            x>500&x<=800      x>20&x<=50       x>0.24&x<=1.3     x>0.4&x<=2.2  x<=130 x<=1200 x>=10&x<=20 x>=20&x<=34 x>1&x<=3  x>5&x<=12 x>25&x<=60
# 4    D                                           x>750                   x>800            x>50              x>1.30            x>2.2   x>130  x>1200 x>=20&x<=30        x>34     x<=1       x>12       x>60
# 5    E                                           x>Inf                   x>Inf           x>Inf               x>Inf            x>Inf   x>260  x>1200        x>30        x>50   x<(-1)      x>Inf      x>Inf

# This is for setting the years you would like to undertake the assessment on for the National objectives Framework.
# Add more years using comma seperation as needed 
yr  <-c("2013", "2014","2015","2016","2017","Overall")
reps  <-length(yr)



#===================================================================================================
## Load LAWA Data
#Reference Dates
EndYear <- 2017
StartYear <- EndYear - NOF_PERIOD + 1

# loads lawaLakesdata dataframe  from LAWA_State.r  - has altered values from censoring, and calculated medians
lakesMonthlyMedians=read.csv(tail(dir(path="h:/ericg/16666LAWA/2018/Lakes/4.Analysis",
                                      pattern="lawaLakeMonthlyMedian.*csv",
                                      recursive = T,full.names = T,ignore.case = T),1),stringsAsFactors=F)
# lakesMonthlyMedians$Value[which(lakesMonthlyMedians$parameter%in%c('NH4N'))]= #,'TN','TP'
#   lakesMonthlyMedians$Value[which(lakesMonthlyMedians$parameter%in%c('NH4N'))]/1000  #mg/L to mg/m3

lakesMonthlyMedians$Date=lubridate::dmy(paste0('1-',lakesMonthlyMedians$month,'-',lakesMonthlyMedians$Year))
lakesMonthlyMedians$parameter=toupper(lakesMonthlyMedians$parameter)


# To run chl and clarity manually 
# #2 October 2018
# #Clarity bands
# clarSub=lakesMonthlyMedians%>%select(c("LawaSiteID","SiteName","parameter","Value","Date"))%>%
#   filter(tolower(parameter)=='secchi')
# clarSub$Year=lubridate::year(clarSub$Date)
# annualClar <- clarSub%>%filter(Year>=StartYear)%>%
#   group_by(LawaSiteID)%>%mutate(count=n())%>%ungroup%>%
#   group_by(LawaSiteID,Year)%>%dplyr::summarise(medVal=median(Value,na.rm=T),count=unique(count))%>%filter(count>=30)
# medianClar <- annualClar%>%group_by(LawaSiteID)%>%dplyr::summarise(medClar=median(medVal,na.rm=T),)%>%ungroup
# medianClar$ClarityScore <- cut(medianClar$medClar,breaks=c(-10,1,3,7,Inf),labels=c('D','C','B','A'))
# # write.csv(medianClar,paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),'%Y-%m-%d'),"/LakesClarBandForITE_",
# #                             format(Sys.time(),"%Hh%Mm-%d%b%Y"),".csv"),row.names = F)
# 
# # ChlA bands
# chlSub=lakesMonthlyMedians%>%select(c("LawaSiteID","SiteName","parameter","Value","Date"))%>%
#   filter(tolower(parameter)=='chla')
# chlSub$Year=lubridate::year(chlSub$Date)
# annualChl <- chlSub%>%filter(Year>=StartYear)%>%
#   group_by(LawaSiteID)%>%mutate(count=n())%>%ungroup%>%
#   group_by(LawaSiteID,Year)%>%dplyr::summarise(medVal=median(Value,na.rm=T),count=unique(count))%>%filter(count>=30)
# medianChl <- annualChl%>%group_by(LawaSiteID)%>%dplyr::summarise(medChl=median(medVal,na.rm=T))
# medianChl$MedChlScore <- cut(medianChl$medChl,breaks=c(-1,2,5,12,Inf),labels=c('A','B','C','D'))
# maxChl <- annualChl%>%group_by(LawaSiteID)%>%dplyr::summarise(maxChl=max(medVal,na.rm=T))
# maxChl$MaxChlScore <- cut(maxChl$maxChl,breaks=c(-10,10,25,60,Inf),labels=c('A','B','C','D'))
# chlNOF=cbind(medianChl,maxChl[,-1])
# chlNOF$overall=apply(chlNOF[,2:3],MARGIN = 1,FUN=max)
# # write.csv(chlNOF,paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),'%Y-%m-%d'),"/LakesChlBandForITE_",
# #                             format(Sys.time(),"%Hh%Mm-%d%b%Y"),".csv"),row.names = F)
# 
# #/2 October 2018
# 





# Subset to just have relevant parameters
# Date column in lakesMonthlyMedians already POSIXct data type
sub_swq <- lakesMonthlyMedians%>%filter(Year>=StartYear)%>%select(c("LawaSiteID","SiteName","parameter","Value","Date"))%>%
  filter(tolower(parameter)%in%tolower(c("NH4N","TN","TP","ECOLI","PH","SECCHI","CHLA")))
sub_swq$parameter[sub_swq$parameter=="NH4N"] <- "NH4"
#+++++++++++++++++++++++++++++ Dealing with data in dfswq table++++++++++++++++++++++++++++++++++++
Name_swq<- unique(sub_swq$SiteName)
uLAWAids <- unique(sub_swq$LawaSiteID)
#+++++++++++++++++++++++++++++ Ammonia adjustment for pH++++++++++++++++++++++++++++++++++++
csv="H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/NOFAmmoniaAdjustment.csv"
adjnh4=NH4adj(sub_swq,vars = c("NH4","PH"),csv)
sub_swq<-rbind(sub_swq,adjnh4)
rm(adjnh4,csv)

sub_swq$Date=strptime(sub_swq$Date,"%Y-%m-%d")

if(exists("NOFSummaryTable")) { rm("NOFSummaryTable") }
i=1
cat(length(uLAWAids),'\t')
suppressWarnings(rm(tnsite,tpsite,nh4site,ecosite,rightSite,value,Value)  )
for(i in i:length(uLAWAids)){
  cat('.')
  if(as.integer(i/100)==(i/100)){cat('\n')}
  #regex find replace  sub_swq\$((.*))\[sub_swq\$Parameter=="TON" & sub_swq\$SiteName==uLAWAids\[i\]\]
  rightSite=sub_swq[(sub_swq$LawaSiteID==uLAWAids[i]),]
  rightSite=rightSite[!is.na(rightSite$Value),]
  # create table of compliance with proposed National Objectives Frameqork
  Com_NOF <- data.frame (Year = yr,
                         Total_Nitrogen           = as.numeric(rep(NA,reps)),
                         Tot_Nitr_Band            = factor(rep(NA,reps),levels=c("A","B","C","D")),
                         Total_Phosphorus         = as.numeric(rep(NA,reps)),
                         Tot_Phos_Band            = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Median_Ammoniacal        = as.numeric(rep(NA,reps)),
                         Med_Ammoniacal_Band      = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Max_Ammoniacal           = as.numeric(rep(NA,reps)),
                         Max_Ammoniacal_Band      = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Ammonia_Toxicity_Band    = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Median_Clarity           = as.numeric(rep(NA,reps)),
                         Med_Clarity_Band         = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Median_ChlA              = as.numeric(rep(NA,reps)),
                         Med_ChlA_Band            = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         Max_ChlA                 = as.numeric(rep(NA,reps)),
                         Max_ChlA_Band            = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         ChlASummaryBand          = factor(rep(NA,reps),levels = c("A","B","C","D")),
                         E_coli                   = as.numeric(rep(NA,reps)),
                         E_coli_band              = rep(NA,reps),
                         E_coli95                 = as.numeric(rep(NA,reps)),
                         E_coli95_band            = rep(NA,reps),
                         E_coliRecHealth540       = as.numeric(rep(NA,reps)),
                         E_coliRecHealth540_Band  = rep(NA,reps),
                         E_coliRecHealth260       = as.numeric(rep(NA,reps)),
                         E_coliRecHealth260_Band  = rep(NA,reps),
                         E_coliSummaryband        = factor(rep(NA,reps),levels=c("A","B","C","D","E")),
                         stringsAsFactors = FALSE)
  
  
  ###################### Total Nitrogen  ########################################
  tnsite=rightSite[rightSite$parameter=="TN",]
  #--------Median Nitrate--------------------------------------
  Value <- tapply(tnsite$Value, format(tnsite$Date, '%Y'), na.rm=TRUE, quantile,prob=c(0.5),type=5)
  
  if(length(Value)!=0){
    #adding values into Com_NOF table
    Com_NOF$Total_Nitrogen <- Value[match(Com_NOF$Year,names(Value))]
    
    if(length(tnsite$Value)>=30){
      Com_NOF$Total_Nitrogen[nrow(Com_NOF)] <- quantile(tnsite$Value,prob=c(0.5),type=5,na.rm=T)
    }
    #find the band which each value belong to
    if(lakeSiteTable$LType[which(lakeSiteTable$LawaSiteID==uLAWAids[i])]%in%c("stratified","brackish","icoll","monomictic")){
      Com_NOF$Tot_Nitr_Band <- unlist(lapply(Com_NOF$Total_Nitrogen,NOF_FindBand,bandColumn = NOFbandDefinitions$TotalNitrogenseasonally.stratified.and.brackish))
      Com_NOF$Tot_Nitr_Band <- unlist(lapply(Com_NOF$Tot_Nitr_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
    }else{ #assuming polymictic
      Com_NOF$Tot_Nitr_Band <- unlist(lapply(Com_NOF$Total_Nitrogen,NOF_FindBand,bandColumn = NOFbandDefinitions$TotalNitrogenpolymictic))
      Com_NOF$Tot_Nitr_Band <- unlist(lapply(Com_NOF$Tot_Nitr_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
    }
  }
  rm(tnsite)
  ###################### Total Phosphorus  ########################################
  tpsite=rightSite[rightSite$parameter=="TP",]
  
  #--------Median phosphorus--------------------------------------
  Value <- tapply(tpsite$Value, format(tpsite$Date, '%Y'), na.rm=TRUE, quantile,prob=c(0.5),type=5)
  
  if(length(Value)!=0){
    #adding values into Com_NOF table
    Com_NOF$Total_Phosphorus <- Value[match(Com_NOF$Year,names(Value))]
    
    if(length(tpsite$Value)>=30){
      Com_NOF$Total_Phosphorus[nrow(Com_NOF)] <- quantile(tpsite$Value,prob=c(0.5),type=5,na.rm=T)
    }
    #find the band which each value belong to
    Com_NOF$Tot_Phos_Band <- unlist(lapply(Com_NOF$Total_Phosphorus,NOF_FindBand,bandColumn = NOFbandDefinitions$TotalPhosphorus))
    Com_NOF$Tot_Phos_Band <- unlist(lapply(Com_NOF$Tot_Phos_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
  }
  rm(tpsite)
  
  ###################### Ammonia Toxicity  ############################
  nh4site=rightSite[rightSite$parameter=="NH4adj",]
  #--------Median Ammoniacal Nitrogen--------------------------------------
  Value <- tapply(nh4site$Value,format(nh4site$Date, '%Y'), na.rm=TRUE, quantile,prob=c(0.5),type=5)
  
  if(length(Value)!=0){
    #adding values into Com_NOF table
    Com_NOF$Median_Ammoniacal = Value[match(Com_NOF$Year,names(Value))]
    
    #calculate the overall median
    if(length(nh4site$Value)>=30){
      Com_NOF$Median_Ammoniacal[nrow(Com_NOF)] <- quantile(nh4site$Value,prob=c(0.5),type=5)
    }
    
    #find the band which each value belong to
    Com_NOF$Med_Ammoniacal_Band <- unlist(lapply(Com_NOF$Median_Ammoniacal,NOF_FindBand,bandColumn=NOFbandDefinitions$Median.Ammoniacal.N)) 
    Com_NOF$Med_Ammoniacal_Band <- unlist(lapply(Com_NOF$Med_Ammoniacal_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
    
    #-------maximum annual Ammoniacal Nitrogen--------------------------------------
    Value <- tapply(nh4site$Value, format(nh4site$Date, '%Y'), na.rm=TRUE, max)
    
    #adding values into Com_NOF table
    Com_NOF$Max_Ammoniacal <- Value[match(Com_NOF$Year,names(Value))]
    
    #calculate the overall median
    if(length(nh4site$Value)>=30){
      Com_NOF$Max_Ammoniacal[nrow(Com_NOF)] <- quantile(nh4site$Value,prob=c(0.95),type=5)
    }
    
    #find the band which each value belong to
    Com_NOF$Max_Ammoniacal_Band <-unlist(lapply(Com_NOF$Max_Ammoniacal,NOF_FindBand,
                                                bandColumn=NOFbandDefinitions$Max.Ammoniacal.N)) 
    Com_NOF$Max_Ammoniacal_Band <- unlist(lapply(Com_NOF$Max_Ammoniacal_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
    
    #------------------Finding the band for Ammonia Toxicity-------------------------------
    Com_NOF$Ammonia_Toxicity_Band=apply(select(Com_NOF,Med_Ammoniacal_Band, Max_Ammoniacal_Band),1,max,na.rm=T)
  }  
  rm(nh4site)
  
  #################### Clarity ############
  clarsite=rightSite[rightSite$parameter=="SECCHI",]
  Value <- tapply(clarsite$Value, 
                  format(clarsite$Date, '%Y'), 
                  na.rm=TRUE, quantile,prob=c(0.5),type=5)
  
  if(length(Value)!=0){
    #adding values into Com_NOF table
    Com_NOF$Median_Clarity = Value[match(Com_NOF$Year,names(Value))]
    
    #calculate the overall median
    if(length(clarsite$Value)>=30){
      Com_NOF$Median_Clarity[nrow(Com_NOF)] <- quantile(clarsite$Value,prob=c(0.5),type=5)
    }
    
    #find the band which each value belong to
    Com_NOF$Med_Clarity_Band <- unlist(lapply(Com_NOF$Median_Clarity,NOF_FindBand,bandColumn=NOFbandDefinitions$Clarity)) 
    Com_NOF$Med_Clarity_Band <- unlist(lapply(Com_NOF$Med_Clarity_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
  }  
  rm(clarsite)
  
  
  #################### ChlA ############
  chlSite=rightSite[rightSite$parameter=="CHLA",]
  Value <- tapply(chlSite$Value, 
                  format(chlSite$Date, '%Y'), 
                  na.rm=TRUE, quantile,prob=c(0.5),type=5)
  
  if(length(Value)!=0){
    #adding values into Com_NOF table
    Com_NOF$Median_ChlA = Value[match(Com_NOF$Year,names(Value))]
    
    #calculate the overall median
    if(length(chlSite$Value)>=30){
      Com_NOF$Median_ChlA[nrow(Com_NOF)] <- quantile(chlSite$Value,prob=c(0.5),type=5)
    }
    
    #find the band which each value belong to
    Com_NOF$Med_ChlA_Band <- unlist(lapply(Com_NOF$Median_ChlA,NOF_FindBand,bandColumn=NOFbandDefinitions$ChlAMedian)) 
    Com_NOF$Med_ChlA_Band <- unlist(lapply(Com_NOF$Med_ChlA_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
  }
  #ChlA max
  Value <- tapply(chlSite$Value,format(chlSite$Date, '%Y'), 
                  na.rm=TRUE, max)
  
  if(length(Value)!=0){
    #adding values into Com_NOF table
    Com_NOF$Max_ChlA = Value[match(Com_NOF$Year,names(Value))]
    
    #calculate the overall median
    if(length(chlSite$Value)>=30){
      Com_NOF$Max_ChlA[nrow(Com_NOF)] <- quantile(chlSite$Value,prob=c(0.95),type=5)
    }
    
    #find the band which each value belong to
    Com_NOF$Max_ChlA_Band <- unlist(lapply(Com_NOF$Max_ChlA,NOF_FindBand,bandColumn=NOFbandDefinitions$ChlAMax)) 
    Com_NOF$Max_ChlA_Band <- unlist(lapply(Com_NOF$Max_ChlA_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
    
    #------------------Finding the band for ChlA Summary-------------------------------
    Com_NOF$ChlASummaryBand=apply(select(Com_NOF,Med_ChlA_Band, Max_ChlA_Band),1,max,na.rm=T)
    
  }  
  rm(chlSite)
  
  
  
  ######################  E.Coli #########################################
  suppressWarnings(rm(cnEc_band,cnEc95_band,cnEcRecHealth540_Band,cnEcRecHealth260_Band,ecosite,rawEcoli))
  ecosite=rightSite[rightSite$parameter=="ECOLI",]
  rawEcoli=data.frame(year=format(ecosite$Date,'%Y'),
                      value=ecosite$Value)
  rawEcoli=rawEcoli[!is.na(rawEcoli$value),]
  if(dim(rawEcoli)[1]>=60){ #data requirement for band determination, footnote 1, table  NPS. 60 over 5 years is monthly
    for(yy in 1:length(Com_NOF$Year)){
      ecv=rawEcoli$value[which(rawEcoli$year==Com_NOF$Year[yy])]
      if(length(ecv)>0){
        Com_NOF$E_coliRecHealth540[yy]=sum(ecv>540)/length(ecv)*100
        Com_NOF$E_coliRecHealth260[yy]=sum(ecv>260)/length(ecv)*100
      }
    }
    
    #Calculate overall exceedance percentage
    Com_NOF$E_coliRecHealth540[nrow(Com_NOF)] <- sum(rawEcoli$value>540)/length(rawEcoli$value)*100
    Com_NOF$E_coliRecHealth260[nrow(Com_NOF)] <- sum(rawEcoli$value>260)/length(rawEcoli$value)*100
    
    Com_NOF$E_coliRecHealth540_Band <- unlist(lapply(Com_NOF$E_coliRecHealth540,NOF_FindBand,bandColumn=NOFbandDefinitions$EcoliRec540))
    cnEcRecHealth540_Band <- unlist(lapply(Com_NOF$E_coliRecHealth540_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
    
    Com_NOF$E_coliRecHealth260_Band <- unlist(lapply(Com_NOF$E_coliRecHealth260,NOF_FindBand,bandColumn=NOFbandDefinitions$EcoliRec260))
    cnEcRecHealth260_Band <- unlist(lapply(Com_NOF$E_coliRecHealth260_Band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))  
  } #else they're left as NA
  
  #E coli median ####
  if(dim(ecosite)[1]>=60){
    Value <- tapply(ecosite$Value, format(ecosite$Date, '%Y'), na.rm=TRUE, quantile,prob=c(0.5),type=5)
    if(length(Value)!=0){
      #adding values into Com_NOF table
      Com_NOF$E_coli <- Value[match(Com_NOF$Year,names(Value))]
      #calculate the overall median
      Com_NOF$E_coli[nrow(Com_NOF)] <- quantile(ecosite$Value,prob=c(0.5),type=5,na.rm=T)
      #find the band which each value belong to
      Com_NOF$E_coli_band <- unlist(lapply(Com_NOF$E_coli,NOF_FindBand,bandColumn=NOFbandDefinitions$E..coli))
      #This median EColi can meet multiple bands
      cnEc_band <- unlist(lapply(Com_NOF$E_coli_band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
    }
    #else they're left as NA
    
    #-------95th percentile Ecoli-----------------------------------------------------
    Value <- tapply(ecosite$Value,format(ecosite$Date, '%Y'),na.rm=TRUE, quantile,prob=c(0.95),type=5)
    
    if(length(Value)!=0){
      #adding values into Com_NOF table
      Com_NOF$E_coli95 <- Value[match(Com_NOF$Year,names(Value))]
      #calculate the overall 95 percentile
      Com_NOF$E_coli95[nrow(Com_NOF)] <- quantile(ecosite$Value,prob=c(0.95),type=5,na.rm=T)
      #find the band which each value belong to
      Com_NOF$E_coli95_band <- unlist(lapply(Com_NOF$E_coli95,NOF_FindBand,bandColumn=NOFbandDefinitions$Ecoli95))
      #Ecoli95 can meet multiple bands
      cnEc95_band <- unlist(lapply(Com_NOF$E_coli95_band,FUN=function(x){min(unlist(strsplit(x,split = '')))}))
    }  
  }
  rm(ecosite)
  #---------------------------------------------------------------------------------
  #These contain the best case out of these scorings, the worst of which contributes.
  if(all(exists(c("cnEc_band","cnEc95_band","cnEcRecHealth540_Band","cnEcRecHealth260_Band")))){
    Com_NOF$E_coliSummaryband = apply(cbind(pmax(cnEc_band,cnEc95_band,cnEcRecHealth540_Band,cnEcRecHealth260_Band)),1,max)
  }else{
    Com_NOF$E_coliSummaryband = NA
  }
  
  Com_NOF$LawaSiteID <- uLAWAids[i]
  if(!exists("NOFSummaryTable")){
    NOFSummaryTable <- Com_NOF
  } else {
    NOFSummaryTable <- rbind.data.frame(NOFSummaryTable,Com_NOF,stringsAsFactors = FALSE)
  }
}

# NOFSummaryTable$medianClarity=medianClar$medClar[match(NOFSummaryTable$LawaSiteID,medianClar$LawaSiteID)]
# NOFSummaryTable$Med_Clarity_Band=medianClar$ClarityScore[match(NOFSummaryTable$LawaSiteID,medianClar$LawaSiteID)]
# NOFSummaryTable$medianChlA=chlNOF$medChl[match(NOFSummaryTable$LawaSiteID,chlNOF$LawaSiteID)]
# NOFSummaryTable$Med_ChlA_Band=chlNOF$MedChlScore[match(NOFSummaryTable$LawaSiteID,chlNOF$LawaSiteID)]
# NOFSummaryTable$maxChlA=chlNOF$maxChl[match(NOFSummaryTable$LawaSiteID,chlNOF$LawaSiteID)]
# NOFSummaryTable$Max_ChlA_Band=chlNOF$MaxChlScore[match(NOFSummaryTable$LawaSiteID,chlNOF$LawaSiteID)]
# NOFSummaryTable$ChlA_Summary_Band=chlNOF$overall[match(NOFSummaryTable$LawaSiteID,chlNOF$LawaSiteID)]


if(0){
  with(NOFSummaryTable,plot(as.factor(Tot_Nitr_Band),Total_Nitrogen,log='y'))
  with(NOFSummaryTable,plot(as.factor(Tot_Phos_Band),Total_Phosphorus,log='y'))
  with(NOFSummaryTable,plot(as.factor(Med_Ammoniacal_Band),Median_Ammoniacal,log='y'))
  with(NOFSummaryTable,plot(as.factor(Max_Ammoniacal_Band),Max_Ammoniacal,log='y'))
  with(NOFSummaryTable,plot(as.factor(Med_ChlA_Band),Median_ChlA,log='y'))
  with(NOFSummaryTable,plot(as.factor(Max_ChlA_Band),Max_ChlA,log='y'))
  with(NOFSummaryTable,plot(as.factor(Med_Clarity_Band),Median_Clarity,log='y'))
  
  table(NOFSummaryTable$Med_Ammoniacal_Band,NOFSummaryTable$Ammonia_Toxicity_Band)
  table(NOFSummaryTable$Max_Ammoniacal_Band,NOFSummaryTable$Ammonia_Toxicity_Band)
  table(NOFSummaryTable$Max_Ammoniacal_Band,NOFSummaryTable$Max_Ammoniacal_Band)
  table(NOFSummaryTable$E_coli95_band,NOFSummaryTable$E_coliSummaryband)
}

#############################Save the output table ############################

NOFSummaryTable$CouncilSiteID=lakeSiteTable$CouncilSiteID[match(NOFSummaryTable$LawaSiteID,lakeSiteTable$LawaSiteID)]
NOFSummaryTable$Agency=lakeSiteTable$Agency[match(NOFSummaryTable$LawaSiteID,lakeSiteTable$LawaSiteID)]
NOFSummaryTable$Region=lakeSiteTable$Region[match(NOFSummaryTable$LawaSiteID,lakeSiteTable$LawaSiteID)]

NOFSummaryTable$SiteID=lakeSiteTable$SiteID[match(NOFSummaryTable$LawaSiteID,lakeSiteTable$LawaSiteID)]
NOFSummaryTable <- NOFSummaryTable%>%select(LawaSiteID:SiteID,Year:E_coliSummaryband)
write.csv(NOFSummaryTable, file = paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),'%Y-%m-%d'),
                                         "/NOFLakes",format(Sys.time(),"%Hh%Mm-%d%b%Y"),".csv"),row.names=F)
# NOFSummaryTable <- read.csv(tail(dir(path="h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",pattern="NOFLakes[^Overall]",recursive = T,full.names = T),1),stringsAsFactors = F)
# NOFSummaryTable <- merge(NOFSummaryTable, siteTable) 
NOFSummaryTableSubset <- NOFSummaryTable[NOFSummaryTable$Year=="Overall",]
# NOFSummaryTableSubset <- NOFSummaryTableSubset%>%select("LawaSiteID","CouncilSiteID","SiteID",
#                                                         Year:E_coliSummaryband,
#                                                         "SWQAltitude","SWQLanduse","Agency")

write.csv(NOFSummaryTableSubset, file = paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),'%Y-%m-%d'),
                                               "/NOFLakesOverall",format(Sys.time(),"%Hh%Mm-%d%b%Y"),".csv"),row.names=F)

# Reshape Output
require(reshape2)
NOFSummaryTableLong <- melt(data=NOFSummaryTable,
                            id.vars=c("LawaSiteID","CouncilSiteID","SiteID","Agency","Year","Region"))
write.csv(NOFSummaryTableLong, file = paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),'%Y-%m-%d'),"/NOFSummaryTableLong.csv"),row.names=F)

NOFSummaryTableLongSubset <- NOFSummaryTableLong[NOFSummaryTableLong$Year=="Overall",]
NOFSummaryTableLongSubset <- NOFSummaryTableLongSubset[!is.na(NOFSummaryTableLongSubset$LawaSiteID),]
write.csv(NOFSummaryTableLongSubset, file = paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),'%Y-%m-%d'),"/NOF_STATE_2018.csv"),row.names=F)



#Round them off.  For web display?
NOFRound <- NOFSummaryTableLongSubset
NOFRound$variable <- as.character(NOFRound$variable)

variables<-as.character(unique(NOFSummaryTableLongSubset$variable))
variables <- variables[order(variables)]
# [1] "Ammonia_Toxicity_Band"   "ChlASummaryBand"         "E_coli"                  "E_coli_band"             "E_coli95"               
# [6] "E_coli95_band"           "E_coliRecHealth260"      "E_coliRecHealth260_Band" "E_coliRecHealth540"      "E_coliRecHealth540_Band"
# [11] "E_coliSummaryband"       "Max_Ammoniacal"          "Max_Ammoniacal_Band"     "Max_ChlA"                "Max_ChlA_Band"          
# [16] "Med_Ammoniacal_Band"     "Med_ChlA_Band"           "Med_Clarity_Band"        "Median_Ammoniacal"       "Median_ChlA"            
# [21] "Median_Clarity"          "Tot_Nitr_Band"           "Tot_Phos_Band"           "Total_Nitrogen"          "Total_Phosphorus" 

# Decimal places for variables
dp <- rep(NA,length(variables))
dp[variables%in%c("E_coli", "E_coli95", "E_coliRecHealth260", "E_coliRecHealth540")] <- 0
dp[variables%in%c("Max_Ammoniacal", "Median_Ammoniacal", "Total_Nitrogen", "Total_Phosphorus","Median_ChlA","Max_ChlA")] <- 4
dp[variables%in%c("Median_Clarity")] <- 2

parameterInvolved <- variables
parameterInvolved <- gsub(pattern = "Band",replacement = "",x = parameterInvolved)
parameterInvolved <- gsub(pattern = "band",replacement = "",x = parameterInvolved)
parameterInvolved <- gsub(pattern = "_$",replacement = "",x = parameterInvolved)
parameterInvolved <- gsub(pattern = "Med_",replacement = "Median_",x = parameterInvolved)
parameterInvolved <- gsub(pattern = "Tot_",replacement = "Total_",x = parameterInvolved)
parameterInvolved <- gsub(pattern = "E_coli$",replacement = "E_coliMedian",x = parameterInvolved)
parameterInvolved <- gsub(pattern = "Phosphorus",replacement = "Phos",x = parameterInvolved)
parameterInvolved <- gsub(pattern = "Nitrogen",replacement = "Nitr",x = parameterInvolved)


desc = rep('value',length(variables))
desc[grepl(variables,pattern = 'band',ignore.case = T)] <- 'band'
desc[variables%in%c("Agency", "SWQAltitude","SWQLanduse","SiteID","CATCH_LBL","CatchID",
                    "CatchType","Comment","LAWA_CATCH","Region","SOE_FW_RIV",
                    "SWQFrequencyAll","SWQFrequencyLast5","SWQuality","TermReach")] <- 'meta'

dfp <- data.frame(variables,parameterInvolved,desc,dp,stringsAsFactors=FALSE,row.names=NULL)
NOFRound <- merge(NOFRound,dfp,by.x="variable",by.y="variables",all=TRUE)

rm(variables,parameterInvolved,desc,dp)
# POST PROCESSING NOF RESULTS
# Round values to appropriate DP's

# Note that for rounding off a 5, the IEC 60559 standard is expected to be used, ‘go to the even digit’. Therefore round(0.5) is 0 and round(-1.5) is -2
# This is not the desired behaviour here. It is expected that 0.5 rounds to 1, 2.5 rounds, to 3 and so on.
# Therefore for all even values followed by exactly .5 needs to have a small number added (like 0.00001) to get them rounding in the right direction (unless there is 
# a flag in the function that provides for this behaviour), or to redefine the round function. 
# (see http://theobligatescientist.blogspot.co.nz/2010/02/r-i-still-love-you-but-i-hate-your.html)

# As all values are positive, we'll add a small number, related to the degree of rounding required.
# If I was smarter, I would redefine the round function


for(i in 1:length(dfp$variables)){
  if(!is.na(dfp$dp[i])){
    NOFRound$value[NOFRound$variable==dfp$variables[i]] <- as.character(as.numeric(NOFRound$value[NOFRound$variable==dfp$variables[i]]) + 0.000001)
    NOFRound$value[NOFRound$variable==dfp$variables[i]] <- as.character(round(as.numeric(NOFRound$value[NOFRound$variable==dfp$variables[i]]),digits = dfp$dp[i]))
  }
}


NOFRound$value[is.na(NOFRound$value)] <- "NA"
NOFRound <- NOFRound[order(NOFRound$LawaSiteID,NOFRound$parameterInvolved,NOFRound$desc),]
write.csv(NOFRound, file = paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),'%Y-%m-%d'),"/NOF_LAKES_STATE_2018_Rounded_NAs.csv"),row.names=F)

# Transform (tidyr::spread) data in NOFRound to the following form to supply to IT Effect
# LawaSiteID,SiteName,Year,Parameter,value,Band
# ARC-00001,44603,Overall,Max_AmmoniacalN,NA,NA
# ARC-00001,44603,Overall,Median_Ammoniacal,NA,NA
# ARC-00001,44603,Overall,Median_Ecoli,28,A
# ARC-00001,44603,Overall,Total_Nitrogen,0.0079,A

NOF_value <- NOFRound%>%filter(desc=="value")%>%select("LawaSiteID","CouncilSiteID","SiteID","Agency","Year","variable","value","parameterInvolved")
names(NOF_value) <- c("LawaSiteID","CouncilSiteID","SiteID","Agency","Year","Parameter","Value",'parameterInvolved')
NOF_band  <- NOFRound%>%filter(desc=="band")%>%select("LawaSiteID","CouncilSiteID","SiteID","Agency","Year","variable","value","parameterInvolved")
names(NOF_band) <- c("LawaSiteID","CouncilSiteID","SiteID","Agency","Year","BandingRule","BandScore",'parameterInvolved')

NOF_wide <- dplyr::left_join(NOF_band,NOF_value,by = c("LawaSiteID","CouncilSiteID","SiteID","Agency", "Year", "parameterInvolved"))
NOF_wide <- unique(NOF_wide)


# #add medianClar and ChlNOF
# medianClar$Year="Overall"
# medianClar$SiteID=NOF_wide$SiteID[match(medianClar$LawaSiteID,NOF_wide$LawaSiteID)]
# medianClar$CouncilSiteID=NOF_wide$CouncilSiteID[match(medianClar$LawaSiteID,NOF_wide$LawaSiteID)]
# medianClar$Agency=NOF_wide$Agency[match(medianClar$LawaSiteID,NOF_wide$LawaSiteID)]
# medianClar$BandingRule="CustomClar"
# medianClar <- medianClar%>%rename("BandScore"="ClarityScore","Value"="medClar")
# medianClar$parameterInvolved="Clarity"
# medianClar$Parameter="Clarity"
# medianClar <- medianClar%>%select(names(NOF_wide))%>%as.data.frame
# 
# chlNOF$Year="Overall"
# chlNOF$SiteID=NOF_wide$SiteID[match(chlNOF$LawaSiteID,NOF_wide$LawaSiteID)]
# chlNOF$CouncilSiteID=NOF_wide$CouncilSiteID[match(chlNOF$LawaSiteID,NOF_wide$LawaSiteID)]
# chlNOF$Agency=NOF_wide$Agency[match(chlNOF$LawaSiteID,NOF_wide$LawaSiteID)]
# # medChl,maxChl,
# chlNOFb <- chlNOF%>%select(-medChl,-maxChl)%>%gather(BandingRule,BandScore,c(MedChlScore,MaxChlScore,overall))
# chlNOFb$Value=NA
# chlNOFb$Value[chlNOFb$BandingRule=="MedChlScore"]=chlNOF$medChl[match(chlNOFb$LawaSiteID[chlNOFb$BandingRule=="MedChlScore"],chlNOF$LawaSiteID)]
# chlNOFb$Value[chlNOFb$BandingRule=="MaxChlScore"]=chlNOF$maxChl[match(chlNOFb$LawaSiteID[chlNOFb$BandingRule=="MaxChlScore"],chlNOF$LawaSiteID)]
# chlNOFb$Parameter <- "ChlA"
# chlNOFb$parameterInvolved="ChlA"
# chlNOFb$BandingRule[chlNOFb$BandingRule=="overall"]="ChlSummary"
# # chlNOFb <- chlNOFb%>%rename("BandScore"="overall","Value"="medClar")
# # chlNOF$Parameter="Clarity"
# chlNOFb <- chlNOFb%>%select(names(NOF_wide))%>%as.data.frame
# 
# if(!"Clarity"%in%unique(NOF_wide$Parameter)){
#   NOF_wide = rbind(NOF_wide,medianClar,chlNOFb)
# }
# 

write.csv(NOF_wide, file = paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),'%Y-%m-%d'),
                                  "/Lakes_NOF_forITE_",format(Sys.time(),"%Hh%Mm-%d%b%Y"),".csv"),row.names = F)
# NOF_wide <- read.csv(tail(dir("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/","Lakes_NOF_forITE_",recursive = T,full.names = T,ignore.case = T),1),stringsAsFactors = F)

if(audit <- 0){
  apply(NOFSummaryTable[,c(8,10,12,14,15,17,19,21,23,24)],MARGIN = 2,FUN=table)
  NOFSummaryTable[grep('wiritoa',NOFSummaryTable$SiteID,ignore.case = T),]
  table(lakesMonthlyMedians$parameter[grep('dudding',lakesMonthlyMedians$SiteID,ignore.case = T)])
}
rm(audit)    



#Individual site checks
grep('horowhenua',lakeSiteTable$SiteID,ignore.case = T)
lakeSiteTable$LawaSiteID[60]
NOFSummaryTable[NOFSummaryTable$LawaSiteID=="HRC-00333",]
i=which(uLAWAids=="HRC-00333")
