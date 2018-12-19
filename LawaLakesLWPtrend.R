rm(list=ls())
library(tidyverse)
source("h:/ericg/16666LAWA/LWPTrends_v1811_beta.R")
source("h:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")

#Load the latest made (might need to check it works nicely across month folders) 
if(!exists('lakeData')){
  aclakeData=tail(dir(path = "H:/ericg/16666LAWA/2018/Lakes/1.Imported",pattern = "LakesWithMetadata.csv",
                      recursive = T,full.names = T,ignore.case=T),1)
  lakeData=read.csv(aclakeData,stringsAsFactors = F)
  rm(aclakeData)
  lakeData$myDate <- as.Date(as.character(lakeData$Date),"%d-%b-%Y")
  lakeData <- GetMoreDateInfo(lakeData)
  lakeData$monYear = format(lakeData$myDate,"%b-%Y")
  
  lakeData$Season <- lakeData$Month
  SeasonString <- sort(unique(lakeData$Season))
  lakeData <- lakeData%>%dplyr::rename("CenType"="centype")
  lakeData$CenType[lakeData$CenType%in%c("Left","L")]='lt'
  lakeData$CenType[lakeData$CenType%in%c("Right","R")]='gt'
  lakeData$CenType[!lakeData$CenType%in%c("lt","gt")]='not'
  
  lakeData$NewValues=lakeData$Value
  if(mean(lakeData$Value[which(lakeData$parameter%in%c('NH4N','TN','TP'))],na.rm=T)<250){
    lakeData$Value[which(lakeData$parameter%in%c('NH4N','TN','TP'))]=lakeData$Value[which(lakeData$parameter%in%c('NH4N','TN','TP'))]*1000
  }
}

suppressWarnings(try(dir.create(paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d")))))

EndYear <- 2017
startYear5 <- EndYear - 5+1
startYear10 <- EndYear - 10+1

# https://www.lawa.org.nz/learn/factsheets/calculating-water-quality-trends/
# But LWPTrends drops all censored values for SenSlope calcn






#10 year trend ####
lakeDatafor10=lakeData%>%filter(Year>=startYear10 & Year <= EndYear & parameter!="pH")

usites=unique(lakeDatafor10$LawaSiteID)
uMeasures=unique(lakeDatafor10$parameter)
trendTable10=structure(list(LawaSiteID=NA,parameter=NA,
                            Observations = NA_integer_, KWstat = NA_real_,pvalue = NA_real_,Probability = NA_real_,
                            nObs = NA_integer_, S = NA_real_, VarS = NA_real_,D = NA_real_, tau = NA_real_, Z = NA_real_, p = NA_real_, 
                            Median = NA_real_, VarS = NA_real_, AnnualSenSlope = NA_real_, Intercept = NA_real_, Lci = NA_real_, Uci = NA_real_, TrendCategory = NA, 
                            TrendDirection = NA, Sen_Probability = NA_real_, Probabilitymax = NA_real_, Probabilitymin = NA_real_, Percent.annual.change = NA_real_), class = "data.frame")
nMax=length(table(lakeDatafor10$LawaSiteID,lakeDatafor10$parameter)[table(lakeDatafor10$LawaSiteID,lakeDatafor10$parameter)>0])
passCriteria10=data.frame(LawaSiteID=rep('',nMax),param=rep('',nMax),repFreq=rep('',nMax),
                          nFirstYear=rep(0,nMax),nLastYear=rep(0,nMax),
                          numSamples=rep(0,nMax),numYears=rep(0,nMax),
                          stringsAsFactors = F)
pcpos=1
cat(length(usites),'\n')
usite=1
for(usite in usite:length(usites)){
  cat('.')
  if(as.integer(usite/20)==(usite/20)){cat(usite,'\n')}
  subDat=lakeDatafor10%>%filter(LawaSiteID==usites[usite])
  for(uparam in 1:length(uMeasures)){
    # if(usite==739 & uparam==1){next}
    subSubDat=subDat%>%filter(subDat$parameter==uMeasures[uparam])
    if(dim(subSubDat)[1]>0){
      SSD_med <- summaryBy(formula=Value~LawaSiteID+monYear,
                           id=~Censored+CenType+
                             myDate+Year+Month+Qtr+Season+NewValues,
                           data=subSubDat, 
                           FUN=quantile, prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)
      SSD_med$Value=round(SSD_med$Value*1000000)/1000000
      firstYear=length(which(SSD_med$Year==startYear10))
      lastYear=length(which(SSD_med$Year==EndYear))
      numSamples=dim(SSD_med)[1]
      numYears=length(unique(SSD_med$Year[!is.na(SSD_med$Value)]))
      passCriteria10[pcpos,]=c(usites[usite],uMeasures[uparam],paste0(unique(subSubDat$SWQFrequencyAll),collapse=','),
                               firstYear,lastYear,numSamples,numYears)
      pcpos=pcpos+1
      rm(subSubDat)
      #For 10 year monthly we want 90% of measures and 90% of years
      if(numSamples >= 0.9*120 & numYears>=9){
        cat('+')
        suppressWarnings(rm(st,mk,ss,sk,sss))
        SeasonString <- sort(unique(SSD_med$Season))
        (st <- SeasonalityTest(x = SSD_med,main=uMeasures[uparam],ValuesToUse = "Value",do.plot =F))
        if(!is.na(st$pvalue)&&st$pvalue<0.05){
          sk <- SeasonalKendall(x = SSD_med,ValuesToUse = "Value",HiCensor = T,doPlot = F)
          sss <- SeasonalSenSlope(HiCensor=T,x = SSD_med,ValuesToUse = "Value",ValuesToUseforMedian = "Value",doPlot = F)
          newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,sk,sss)
        }else{
          (mk <- MannKendall(x = SSD_med,ValuesToUse = "Value",HiCensor = T,doPlot=F))
          (ss <- try(SenSlope(HiCensor=T,x = SSD_med,ValuesToUse = "Value",ValuesToUseforMedian = "Value",doPlot = F)))
          
          newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,mk,ss)
        }
        trendTable10=rbind(trendTable10,newRow)
        rm(newRow)
      }else{
        cat('.')
      }
      rm(SSD_med)
    }
  }
  rm(subDat)
}
rm(usites,uMeasures,usite,uparam,lakeDatafor10)
rownames(trendTable10) <- NULL
trendTable10$Probability[trendTable10$parameter!="Secchi"]=1-(trendTable10$Probability[trendTable10$parameter!="Secchi"])
save(trendTable10,file=paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend10Year.rData"))
save(passCriteria10,file=paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/PassCriteria10.rData"))


#5 year trend ####
lakeDatafor5=lakeData%>%filter(Year>=startYear5 & Year <= EndYear & parameter!="pH")

usites=unique(lakeDatafor5$LawaSiteID)
uMeasures=unique(lakeDatafor5$parameter)
trendTable5=structure(list(LawaSiteID=NA,parameter=NA,
                           Observations = NA_integer_, KWstat = NA_real_,pvalue = NA_real_,Probability = NA_real_,
                           nObs = NA_integer_, S = NA_real_, VarS = NA_real_,D = NA_real_, tau = NA_real_, Z = NA_real_, p = NA_real_,
                           Median = NA_real_, VarS = NA_real_, AnnualSenSlope = NA_real_, Intercept = NA_real_, Lci = NA_real_, Uci = NA_real_, TrendCategory = NA,
                           TrendDirection = NA, Sen_Probability = NA_real_, Probabilitymax = NA_real_, Probabilitymin = NA_real_, Percent.annual.change = NA_real_), class = "data.frame")
nMax=length(table(lakeDatafor5$LawaSiteID,lakeDatafor5$parameter)[table(lakeDatafor5$LawaSiteID,lakeDatafor5$parameter)>0])
passCriteria5=data.frame(LawaSiteID=rep('',nMax),param=rep('',nMax),repFreq=rep('',nMax),
                          nFirstYear=rep(0,nMax),nLastYear=rep(0,nMax),
                          numSamples=rep(0,nMax),numYears=rep(0,nMax),
                          stringsAsFactors = F)
pcpos=1
cat(length(usites),'\n')
for(usite in 1:length(usites)){
  if(as.integer(usite/20)==(usite/20)){cat(usite,'\n')}
  subDat=lakeDatafor5%>%filter(LawaSiteID==usites[usite])
  for(uparam in 1:length(uMeasures)){
    subSubDat=subDat%>%filter(subDat$parameter==uMeasures[uparam])
    if(dim(subSubDat)[1]>0){
      # SiteID+CouncilSiteID+Agency+Region+SWQLanduse+
      #   SWQAltitude+SWQFrequencyLast5+SWQuality+
      SSD_med <- summaryBy(formula=Value~LawaSiteID+monYear,
                           id=~Censored+CenType+
                             myDate+Year+Month+Qtr+Season+NewValues,
                           data=subSubDat, 
                           FUN=quantile, prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)
      SSD_med$Value=round(SSD_med$Value*1000000)/1000000
      firstYear=length(which(SSD_med$Year==startYear5))
      lastYear=length(which(SSD_med$Year==EndYear))
      numSamples=dim(SSD_med)[1]
      numYears=length(unique(SSD_med$Year[!is.na(SSD_med$Value)]))
      passCriteria5[pcpos,]=c(usites[usite],uMeasures[uparam],paste0(unique(subSubDat$SWQFrequencyAll),collapse=','),
                               firstYear,lastYear,numSamples,numYears)
      pcpos=pcpos+1
      rm(subSubDat)
      #For 5 year monthly we want 90% of measures 
      if(numSamples >= 0.9*60){
        cat('+')
        suppressWarnings(rm(st,mk,ss,sk,sss))
        SeasonString <- sort(unique(SSD_med$Season))
        (st <- SeasonalityTest(x = SSD_med,main=uMeasures[uparam],ValuesToUse = "Value",do.plot =F))
        if(!is.na(st$pvalue)&&st$pvalue<0.05){
          (sk <- SeasonalKendall(x = SSD_med,ValuesToUse = "Value",HiCensor = T,doPlot = F))
          (sss <- SeasonalSenSlope(HiCensor=T,x = SSD_med,ValuesToUse = "Value",ValuesToUseforMedian = "Value",doPlot = F))
          newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,sk,sss)
        }else{
          (mk <- MannKendall(x = SSD_med,ValuesToUse = "Value",HiCensor = T,doPlot=F))
          (ss <- SenSlope(HiCensor=T,x = SSD_med,ValuesToUse = "Value",ValuesToUseforMedian = "Value",doPlot = F))
          newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,mk,ss)
        }
        trendTable5=rbind(trendTable5,newRow)
        rm(newRow)
      }else{
        cat('.')
      }
      rm(SSD_med)
    }
  }
  rm(subDat)
}
rm(usites,uMeasures,usite,uparam,lakeDatafor5)
rownames(trendTable5) <- NULL
trendTable5$Probability[trendTable5$parameter!="Secchi"]=1-(trendTable5$Probability[trendTable5$parameter!="Secchi"])
save(trendTable5,file=paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend5Year.rData"))
save(passCriteria5,file=paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/PassCriteria5.rData"))




#Quarterly, to fill gaps where needed ####
lakeDataforQ10=lakeData%>%filter(Year>=startYear10 & Year <= EndYear & parameter!="pH")
lakeDataforQ10$quYear=paste0(lakeDataforQ10$Qtr,lakeDataforQ10$Year)
lakeDataforQ10$Season=lakeDataforQ10$quYear
usites=unique(lakeDataforQ10$LawaSiteID)
uMeasures=unique(lakeDataforQ10$parameter)
trendTableQ10=structure(list(LawaSiteID=NA,parameter=NA,
                            Observations = NA_integer_, KWstat = NA_real_,pvalue = NA_real_,Probability = NA_real_,
                            nObs = NA_integer_, S = NA_real_, VarS = NA_real_,D = NA_real_, tau = NA_real_, Z = NA_real_, p = NA_real_, 
                            Median = NA_real_, VarS = NA_real_, AnnualSenSlope = NA_real_, Intercept = NA_real_, Lci = NA_real_, Uci = NA_real_, TrendCategory = NA, 
                            TrendDirection = NA, SenProbability = NA_real_, Probabilitymax = NA_real_, Probabilitymin = NA_real_, Percent.annual.change = NA_real_), class = "data.frame")
nMax=length(table(lakeDataforQ10$LawaSiteID,lakeDataforQ10$parameter)[table(lakeDataforQ10$LawaSiteID,lakeDataforQ10$parameter)>0])
passCriteriaQ10=data.frame(LawaSiteID=rep('',nMax),param=rep('',nMax),repFreq=rep('',nMax),
                          nFirstYear=rep(0,nMax),nLastYear=rep(0,nMax),
                          numSamples=rep(0,nMax),numYears=rep(0,nMax),
                          stringsAsFactors = F)
pcpos=1
cat(length(usites),'\n')
for(usite in 1:length(usites)){
  cat('.')
  if(as.integer(usite/100)==(usite/100)){cat(usite,'\n')}
  subDat=lakeDataforQ10%>%filter(LawaSiteID==usites[usite])
  uparam=1
  for(uparam in uparam:length(uMeasures)){
    # if(usite==739 & uparam==1){next}
    subSubDat=subDat%>%filter(subDat$parameter==uMeasures[uparam])
    if(dim(subSubDat)[1]>0){
      SSD_med <- summaryBy(formula=Value~LawaSiteID+quYear,
                           id=~Censored+CenType+
                             myDate+Year+Month+Qtr+Season+NewValues,
                           data=subSubDat, 
                           FUN=quantile, prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)
      SSD_med$Value=round(SSD_med$Value*1000000)/1000000
      rm(subSubDat)
      firstYear=length(which(SSD_med$Year==startYear10))
      lastYear=length(which(SSD_med$Year==EndYear))
      numSamples=dim(SSD_med)[1]
      numYears=length(unique(SSD_med$Year[!is.na(SSD_med$Value)]))
      passCriteriaQ10[pcpos,]=c(usites[usite],uMeasures[uparam],paste0(unique(subDat$SWQFrequencyAll),collapse=','),
                               firstYear,lastYear,numSamples,numYears)
      pcpos=pcpos+1
      #For 10 year quarterly we want 90% of measures 
      if(numSamples >= 0.9*40){
        cat('+')
        suppressWarnings(rm(st,mk,ss,sk,sss))
        SeasonString <- sort(unique(SSD_med$Season))
        (st <- SeasonalityTest(x = SSD_med,main=uMeasures[uparam],ValuesToUse = "Value",do.plot =F))
        if(!is.na(st$pvalue)&&st$pvalue<0.05){
          sk <- SeasonalKendall(x = SSD_med,ValuesToUse = "Value",HiCensor = T,doPlot = F)
          sss <- SeasonalSenSlope(HiCensor=T,x = SSD_med,ValuesToUse = "Value",ValuesToUseforMedian = "Value",doPlot = F)
          newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,sk,sss)
        }else{
          (mk <- MannKendall(x = SSD_med,ValuesToUse = "Value",HiCensor = T,doPlot=F))
          (ss <- SenSlope(HiCensor=T,x = SSD_med,ValuesToUse = "Value",ValuesToUseforMedian = "Value",doPlot = F))
          newRow=cbind(LawaSiteID=usites[usite],parameter=uMeasures[uparam],st,mk,ss)
        }
        trendTableQ10=rbind(trendTableQ10,newRow)
        rm(newRow)
      }else{
        cat('.')
      }
      rm(SSD_med)
    }
  }
  rm(subDat)
}
rm(usites,uMeasures,usite,uparam)
rownames(trendTableQ10) <- NULL
trendTableQ10$Probability[trendTableQ10$parameter!="Secchi"]=1-(trendTableQ10$Probability[trendTableQ10$parameter!="Secchi"])
save(trendTableQ10,file=paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend10YearQ.rData"))
save(passCriteriaQ10,file=paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/PassCriteriaQ10.rData"))


#Reload if necessary and continue ####
load(tail(dir(path = "h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",pattern = "Trend10YearQ.rData",full.names = T,recursive = T),1),verbose =T)
load(tail(dir(path = "h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",pattern = "Trend5Year.rData",full.names = T,recursive = T),1),verbose = T)
load(tail(dir(path = "h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",pattern = "Trend10Year.rData",full.names = T,recursive = T),1),verbose = T)
load(tail(dir(path = "h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",pattern = "PassCriteria10.rData",full.names = T,recursive = T),1),verbose = T)
load(tail(dir(path = "h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",pattern = "PassCriteria5.rData",full.names = T,recursive = T),1),verbose = T)
load(tail(dir(path = "h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",pattern = "PassCriteriaQ10.rData",full.names = T,recursive = T),1),verbose = T)
# combTrend$Probability[combTrend$parameter!="Secchi"]=1-(combTrend$Probability[combTrend$parameter!="Secchi"])

#10yr monthly is gold standard trend. 5yr monthly is silver. 10yr quarterly is bronze standard
trendTable10$standard='gold'
trendTable5$standard='silver'
trendTableQ10$standard='bronze'

combTrend=trendTable10
# 114


combTrend=rbind(combTrend,trendTable5) #[which(!paste0(trendTable5$LawaSiteID,trendTable5$parameter)%in%paste0(combTrend$LawaSiteID,combTrend$parameter)),]
# 114 + 178 = 292

combTrend=rbind(combTrend,trendTableQ10[which(!paste0(trendTableQ10$LawaSiteID,trendTableQ10$parameter)%in%paste0(combTrend$LawaSiteID,combTrend$parameter)),])
# + 160 = 452
table(combTrend$standard)
# bronze   gold silver 
# 160        114   178 

# rm(list=ls())
if(!exists('lakeData')){
source("h:/ericg/16666LAWA/LWPTrends_v1811_beta.R")
  aclakeData=tail(dir(path = "H:/ericg/16666LAWA/2018/Lakes/1.Imported",pattern = "LakesWithMetadata.csv",
                      recursive = T,full.names = T,ignore.case=T),1)
  lakeData=read.csv(aclakeData,stringsAsFactors = F)
  rm(aclakeData)
  lakeData$myDate <- as.Date(as.character(lakeData$Date),"%d-%b-%Y")
  lakeData <- GetMoreDateInfo(lakeData)
  lakeData$monYear = format(lakeData$myDate,"%b-%Y")
  
  lakeData$Season <- lakeData$Month
  SeasonString <- sort(unique(lakeData$Season))
  lakeData <- lakeData%>%dplyr::rename("CenType"="centype")
  lakeData$CenType[lakeData$CenType%in%c("Left","L")]='lt'
  lakeData$CenType[lakeData$CenType%in%c("Right","R")]='gt'
  lakeData$CenType[!lakeData$CenType%in%c("lt","gt")]='not'
  
  lakeData$NewValues=lakeData$Value
}


lakeSiteTable=read.csv("file:///H:/ericg/16666LAWA/2018/Lakes/1.Imported/LAWA_Site_Table_Lakes.csv",stringsAsFactors = F)





trendTable5$ConfCat <- cut(trendTable5$Probability, breaks=  c(0, 0.1,0.33,0.67,0.90, 1),
                           labels = c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading"))
trendTable10$ConfCat <- cut(trendTable10$Probability, breaks=  c(0, 0.1,0.33,0.67,0.90, 1),
                            labels = c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading"))
trendTableQ10$ConfCat <- cut(trendTableQ10$Probability, breaks=  c(0, 0.1,0.33,0.67,0.90, 1),
                            labels = c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading"))
combTrend$ConfCat <- cut(combTrend$Probability, breaks=  c(0, 0.1,0.33,0.67,0.90, 1),
                             labels = c("Very likely improving", "Likely improving", "Indeterminate", "Likely degrading", "Very likely degrading"))

trendTable5$ConfCat=factor(trendTable5$ConfCat,levels=rev(c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading")))
trendTable10$ConfCat=factor(trendTable10$ConfCat,levels=rev(c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading")))
trendTableQ10$ConfCat=factor(trendTableQ10$ConfCat,levels=rev(c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading")))
combTrend$ConfCat=factor(combTrend$ConfCat,levels=rev(c("Very likely improving","Likely improving","Indeterminate","Likely degrading","Very likely degrading")))


trendTable5$period=5
trendTable10$period=10
trendTableQ10$period=10
# trendTable5=trendTable5[,-which(names(trendTable5)=="VarS")[2]]
# trendTable10=trendTable10[,-which(names(trendTable10)=="VarS")[2]]
# trendTableQ10=trendTableQ10[,-which(names(trendTableQ10)=="VarS")[2]]
# combTrend=combTrend[,-which(names(combTrend)=="VarS")[2]]
combTrend$period=10
combTrend$period[combTrend$standard=='silver']=5
combTrend$Frequency='monthly'
combTrend$Frequency[combTrend$standard=='bronze'] <- 'quarterly'


combTrend$Region =    lakeSiteTable$Region[match(combTrend$LawaSiteID,lakeSiteTable$LawaSiteID)]
combTrend$TrendScore=as.numeric(combTrend$ConfCat)-3
combTrend$TrendScore[is.na(combTrend$TrendScore)]<-(-99)
combTrendExport <- combTrend%>%select(LawaSiteID,parameter,TrendScore,Region,Frequency,period)

write.csv(combTrendExport,paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/LakesWQ_Trend_ForITE",
                         format(Sys.time(),"%Hh%Mm-%d%b%Y"),".csv"),row.names = F)
combTrendExport <- read.csv(tail(dir("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/","LakesWQ_Trend_ForITE",recursive = T,full.names = T,ignore.case = T),1),stringsAsFactors = F)
rm(combTrendExport)

stop("it's enough")

# savePlott=F
# usites=unique(combTrend$LawaSiteID)
# uMeasures=unique(combTrend$parameter)
# for(uparam in seq_along(uMeasures)){
#   subTrend=trendTable10[which(trendTable10$parameter==uMeasures[uparam]),]
#   worstDeg <- which.max(subTrend$Probability) 
#   bestImp <- which.min(subTrend$Probability)
#   nPlot = (max(subTrend$Probability,na.rm=T)>0.5)+(min(subTrend$Probability,na.rm=T)<0.5)
#   cat(subTrend$Probability[worstDeg],'\t')
#   cat(subTrend$Probability[bestImp],'\n')
#   if(savePlott){
#     tiff(paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/BestWorst",uMeasures[uparam],".tif"),
#          width = 8,height=8,units='in',res=300,compression='lzw',type='cairo')
#   }else{
#     windows()
#   }
#     par(mfrow=c(nPlot,1),mar=c(2,4,1,2))
#     theseDeg <- which(lakeData$LawaSiteID==subTrend$LawaSiteID[worstDeg] &
#                         lakeData$parameter==uMeasures[uparam] & dmy(lakeData$Date)>dmy("1-1-2008"))
#     theseImp <- which(lakeData$LawaSiteID==subTrend$LawaSiteID[bestImp] &
#                         lakeData$parameter==uMeasures[uparam] & dmy(lakeData$Date)>dmy("1-1-2008"))
#     if(max(subTrend$Probability,na.rm=T)>0.5){
#     st <- SeasonalityTest(x = lakeData[theseDeg,],main=uMeasures[uparam],ValuesToUse = "Value",do.plot =F)
#     if(!is.na(st$pvalue)&&st$pvalue<0.05){
#       SeasonalKendall(x = lakeData[theseDeg,],ValuesToUse = "Value",doPlot = F)
#       SeasonalSenSlope(HiCensor=T,x = lakeData[theseDeg,],ValuesToUse = "Value",ValuesToUseforMedian = "Value",doPlot = T,mymain = uMeasures[uparam])
#     }else{
#       MannKendall(x = lakeData[theseDeg,],ValuesToUse = "Value",doPlot=F)
#       SenSlope(HiCensor=T,x = lakeData[theseDeg,],ValuesToUse = "Value",ValuesToUseforMedian = "Value",doPlot = T,mymain = uMeasures[uparam])
#     }
#     }
#     if(min(subTrend$Probability,na.rm=T)<0.5){
#     st <- SeasonalityTest(x = lakeData[theseImp,],main=uMeasures[uparam],ValuesToUse = "Value",do.plot =F)
#     if(!is.na(st$pvalue)&&st$pvalue<0.05){
#       SeasonalKendall(x = lakeData[theseImp,],ValuesToUse = "Value",doPlot = F)
#       if(is.na(SeasonalSenSlope(HiCensor=T,x = lakeData[theseImp,],ValuesToUse = "Value",ValuesToUseforMedian = "Value",doPlot = T,mymain = uMeasures[uparam])$Sen_Probability)){
#         SenSlope(HiCensor=T,x = lakeData[theseImp,],ValuesToUse = "Value",ValuesToUseforMedian = "Value",doPlot = T,mymain = uMeasures[uparam])
#       }
#     }else{
#       MannKendall(x = lakeData[theseImp,],ValuesToUse = "Value",doPlot=F)
#       SenSlope(HiCensor=T,x = lakeData[theseImp,],ValuesToUse = "Value",ValuesToUseforMedian = "Value",doPlot = T,mymain = uMeasures[uparam])
#     }
#     }
#     if(names(dev.cur())=='tiff'){dev.off()}
#     rm(theseDeg,theseImp)
#   
#   rm(worstDeg,bestImp)
# }
# 
# 
# #Make the coloured plot
# colMPs=-0.5+(1:8)*1.2
# tb <- plot(factor(combTrend$parameter[combTrend$standard=='gold']),combTrend$ConfCat[combTrend$standard=='gold'],
#            col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),main="Ten year trends")
# tbp <- apply(X = tb,MARGIN = 1,FUN = function(x)x/sum(x))
# mbp <- apply(tbp,MARGIN = 2,FUN=cumsum)
# mbp <- rbind(rep(0,6),mbp)
# mbp = (mbp[-1,]+mbp[-6,])/2
# if(savePlott){
# tiff(paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/TenYearTrends.tif"),
#      width = 8,height=8,units='in',res=300,compression='lzw',type='cairo')
#   }
# par(mfrow=c(1,1),mar=c(5,10,4,2))
# barplot(tbp,main="Ten year trends",las=2,
#         col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),yaxt='n')
# axis(side = 2,at = mbp[-1,1],labels = colnames(tb),las=2,lty = 0)
# for(cc in 1:8){
#   text(rep(colMPs[cc],5),mbp[,cc],tb[cc,])
# }
# if(names(dev.cur())=='tiff'){dev.off()}
# 
# 
# par(mfrow=c(3,1))
# t10 <- plot(factor(trendTable10$parameter),trendTable10$ConfCat,col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),main="10 Year monthly")
# t5 <- plot(factor(trendTable5$parameter),trendTable5$ConfCat,col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),main="5 Year monthly")
# t10q <- plot(factor(trendTableQ10$parameter),trendTableQ10$ConfCat,col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),main="10 Year quarterly")
# write.csv(t5,paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend5Year.csv"))
# write.csv(t10,paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend10Year.csv"))
# write.csv(t10q,paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend10YearQuarterly.csv"))
# t5p <- apply(X = t5,MARGIN = 1,FUN = function(x)x/sum(x))
# t10p <- apply(X = t10,MARGIN = 1,FUN = function(x)x/sum(x))
# t10pq <- apply(X = t10q,MARGIN = 1,FUN = function(x)x/sum(x))
# 
# m5p <- apply(t5p,MARGIN = 2,FUN=cumsum)
# m5p <- rbind(rep(0,8),m5p)
# m5p = (m5p[-1,]+m5p[-6,])/2
# 
# m10p <- apply(t10p,MARGIN = 2,FUN=cumsum)
# m10p <- rbind(rep(0,8),m10p)
# m10p = (m10p[-1,]+m10p[-6,])/2
# 
# m10pq <- apply(t10pq,MARGIN = 2,FUN=cumsum)
# m10pq <- rbind(rep(0,8),m10pq)
# m10pq = (m10pq[-1,]+m10pq[-6,])/2
# 
# colMPs=-0.5+(1:8)*1.2
# if(savePlott){
# tiff(paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/TrendByPeriod.tif"),
#      width = 10,height=15,units='in',res=350,compression='lzw',type='cairo')
# }
# par(mfrow=c(3,1),mar=c(5,10,4,2))
# barplot(t10p,main="10 Year",las=2,
#         col=c("#dd1111FF","#ee7711FF","#bbbbbbFF","#11cc11FF","#008800FF"),yaxt='n')
# axis(side = 2,at = m10p[,1],labels = colnames(t10),las=2,lty = 0)
# for(cc in 1:8){
#   text(rep(colMPs[cc],5),m10p[,cc],paste0(t10[cc,],'\n(',round(t10p[,cc]*100,0),'%)'))
# }
# barplot(t5p,main="5 Year",las=2,
#         col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),yaxt='n')
# axis(side = 2,at = m5p[,1],labels = colnames(t5),las=2,lty = 0)
# for(cc in 1:8){
#   text(rep(colMPs[cc],5),m5p[,cc],paste0(t5[cc,],'\n(',round(t5p[,cc]*100,0),'%)'))
# }
# barplot(t10pq,main="10 Year quarterly",las=2,
#         col=c("#dd1111FF","#ee7711FF","#bbbbbbFF","#11cc11FF","#008800FF"),yaxt='n')
# axis(side = 2,at = m10pq[,1],labels = colnames(t10q),las=2,lty = 0)
# for(cc in 1:8){
#   text(rep(colMPs[cc],5),m10pq[,cc],paste0(t10q[cc,],'\n(',round(t10pq[,cc]*100,0),'%)'))
# }
# if(names(dev.cur())=='tiff'){dev.off()}
# 
# par(mfrow=c(1,1))


#Per site inspections?






