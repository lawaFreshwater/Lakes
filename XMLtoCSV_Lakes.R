rm(list=ls())
library(XML)
source("H:/ericg/16666LAWA/2018/LAWAFunctionsEG.R")


siteTable <- read.csv(file = "h:/ericg/16666LAWA/2018/Lakes/1.Imported/LAWA_Site_Table_Lakes.csv")



#Handles censored?
#V      V    V      V            V    V      V      V     V         V
c("ac","boprc","ecan","es","gwrc","hbrc","hrc","nrc","orc","trc","wcrc","wrc")

# siteTable=read.csv("H:/ericg/16666LAWA/2018/Lakes/LAWA_Site_Table_Lakes.csv",stringsAsFactors=FALSE)


lawalakenames=c("TN","NH4N","TP","CHLA","pH","Secchi","ECOLI")
agency='wcrc'
agency='boprc'
agency='ecan'

for(agency in c("ac","boprc","ecan","es","gwrc","hbrc","hrc","nrc","orc","trc","wcrc","wrc")){
  suppressWarnings({rm(forcsv)})
  forcsv=xml2csvLake(agency=agency,maxHistory = 20,quiet=T)
  cat(length(unique(forcsv$parameter)),paste(unique(forcsv$parameter),collapse=', '),'\n')
  # forcsv$parameter[grepl(pattern = 'Potentially toxic cyanobacteria biovolume',x = forcsv$parameter,ignore.case = T)] <- "CYANOTOX"
  # forcsv$parameter[grepl(pattern = 'Cyanobacteria biovolume, Total',x = forcsv$parameter,ignore.case = T)] <- "CYANOALL"
  # forcsv$parameter[grepl(pattern = 'Cyanotox',x = forcsv$parameter,ignore.case = T)] <- "CYANOTOX"
  # forcsv$parameter[grepl(pattern = 'Cyanoall',x = forcsv$parameter,ignore.case = T)] <- "CYANOALL"
  forcsv$parameter[grepl(pattern = 'Transparency',x = forcsv$parameter,ignore.case = T)] <- "Secchi"
  forcsv$parameter[grepl(pattern = 'Secchi',x = forcsv$parameter,ignore.case = T)] <- "Secchi"
  forcsv$parameter[grepl(pattern = 'loroph',x = forcsv$parameter,ignore.case = T)] <- "CHLA"
  forcsv$parameter[grepl(pattern = 'CHLA',x = forcsv$parameter,ignore.case = T)] <- "CHLA"
  forcsv$parameter[grepl(pattern = 'coli',x = forcsv$parameter,ignore.case = T)] <- "ECOLI"
  # forcsv$parameter[grepl(pattern = 'Soluble Phosphorus',x = forcsv$parameter,ignore.case = T)] <- "DRP"
  # forcsv$parameter[grepl(pattern = 'Dissolved Reactive',x = forcsv$parameter,ignore.case = T)] <- "DRP"
  forcsv$parameter[grepl(pattern = 'phosphorus',x = forcsv$parameter,ignore.case = T)] <- "TP"
  forcsv$parameter[grepl(pattern = 'phosphorous',x = forcsv$parameter,ignore.case = T)] <- "TP"
  forcsv$parameter[grepl(pattern = 'TP',x = forcsv$parameter,ignore.case = F)] <- "TP"
  forcsv$parameter[grepl(pattern = 'Ammonia',x = forcsv$parameter,ignore.case = T)] <- "NH4N"
  forcsv$parameter[grepl(pattern = 'NH4',x = forcsv$parameter,ignore.case = T)] <- "NH4N"
  # forcsv$parameter[grepl(pattern = 'nitrate',x = forcsv$parameter,ignore.case = T)] <- "NO3N" #Note, might be nitrate nitrogen
  forcsv$parameter[grepl(pattern = 'total nitrogen',x = forcsv$parameter,ignore.case = T)] <- "TN" #Note, might be nitrate nitrogen
  forcsv$parameter[agrepl(pattern = 'Nitrogen (Total)',x = forcsv$parameter,ignore.case = T)] <- "TN" #Note, might be nitrate nitrogen
  # forcsv$parameter[grepl(pattern = 'TN',x = forcsv$parameter,ignore.case = F)] <- "TN"
  forcsv$parameter[grepl(pattern = 'ph \\(field\\)',x = forcsv$parameter,ignore.case = T)] <- "pH"
  forcsv$parameter[grepl(pattern = 'ph \\(lab\\)',x = forcsv$parameter,ignore.case = T)] <- "pH"
  cat(length(unique(forcsv$parameter)),paste(unique(forcsv$parameter),collapse='\t'),'\n')
  cat(agency,'\t\t',lawalakenames[!lawalakenames%in%unique(forcsv$parameter)],'\n') #Missing parameters
  excess=unique(forcsv$parameter)[!unique(forcsv$parameter)%in%lawalakenames] #Surplus parameters
  if(length(excess)>0){
    forcsv=forcsv[-which(forcsv$parameter%in%excess),]
  }
  rm(excess)
  prenacount=sum(is.na(forcsv$Value))
  forcsv$Value=as.numeric(forcsv$Value)
  stopifnot(sum(is.na(forcsv$Value))==prenacount)
  rm(prenacount)
  write.csv(forcsv,
            file=paste0( 'H:/ericg/16666LAWA/2018/Lakes/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/',agency,'.csv'),
            row.names=F)
  suppressWarnings({rm(forcsv)})
}


# table(tolower(siteList)%in%tolower(siteTable$CouncilSiteID))
# tolower(siteList)[!tolower(siteList)%in%tolower(siteTable$CouncilSiteID)]

# table(tolower(siteTable$CouncilSiteID)%in%tolower(siteList))
# siteTable[which(!tolower(siteTable$CouncilSiteID)%in%tolower(siteList)),c(3,9)]


# #Find out from last time, which councils had which measurements
# ldl=readxl::read_xlsx("h:/ericg/16666LAWA/2018/Lakes/1.Imported/LakeDownloadData.xlsx",sheet = "Lakes Data")
# by(data = ldl[,7:14],INDICES = ldl$rcid,FUN = function(x)apply(x,2,FUN=function(x)any(!is.na(x))))





library(lubridate)
nms=data.frame(agency=NULL,xmlAge=NULL,var=NULL,earliest=NULL,latest=NULL,nMeas=NULL,nSite=NULL,meanMeas=NULL,maxMeas=NULL,minMeas=NULL,nNA=NULL)
for(agency in c("ac","boprc","ecan","es","gwrc","hbrc","hrc","nrc","orc","trc","wcrc","wrc")){
  forcsv=loadLatestCSVLake(agency)
  if(!is.null(forcsv)){
    newRows=data.frame(agency=rep(agency,length(unique(forcsv$parameter))),
                       xmlAge=checkXMLageLakes(agency = agency,maxHistory = 30),
                       var=sort(unique(forcsv$parameter)),
                       earliest=rep("",length(unique(forcsv$parameter))),
                       latest=rep("",length(unique(forcsv$parameter))),
                       nMeas=rep(0,length(unique(forcsv$parameter))),
                       nSite=rep(NA,length(unique(forcsv$parameter))),
                       meanMeas=rep(NA,length(unique(forcsv$parameter))),
                       maxMeas=rep(NA,length(unique(forcsv$parameter))),
                       minMeas=rep(NA,length(unique(forcsv$parameter))),
                       nNA=rep(NA,length(unique(forcsv$parameter))),
                       stringsAsFactors = F)
    for(v in 1:dim(newRows)[1]){
      newRows$earliest[v]=format(min(dmy(forcsv$Date[which(forcsv$parameter==newRows$var[v])])),"%d-%b-%Y")
      newRows$latest[v]=format(max(dmy(forcsv$Date[which(forcsv$parameter==newRows$var[v])])),"%d-%b-%Y")
      newRows$nMeas[v]=sum(forcsv$parameter==newRows$var[v])
      newRows$nSite[v]=length(unique(forcsv$SiteName[which(forcsv$parameter==newRows$var[v] & !is.na(forcsv$Value))]))
      newRows$meanMeas[v]=round(mean(forcsv$Value[forcsv$parameter==newRows$var[v]],na.rm=T),1)
      newRows$maxMeas[v]=round(max(forcsv$Value[forcsv$parameter==newRows$var[v]],na.rm=T),1)
      newRows$minMeas[v]=round(min(forcsv$Value[forcsv$parameter==newRows$var[v]],na.rm=T),1)
      newRows$nNA[v]=sum(is.na(forcsv$Value[forcsv$parameter==newRows$var[v]]))
    }
    nms <- rbind.data.frame(nms,newRows)
  }
}
by(INDICES = nms$var,data = nms$agency,FUN=function(x)unique(as.character(x)))
by(INDICES = nms$agency,data = nms$var,FUN=function(x)unique(as.character(x)))
sort(unique(as.character(nms$var)))

try(dir.create(path = paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",
                             format(Sys.Date(),"%Y-%m-%d"))))
write.csv(nms,paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",
                     format(Sys.Date(),"%Y-%m-%d"),"/lakeAudit.csv"))




#Per council audit
for(agency in c("ac","boprc","ecan","es","gwrc","hbrc","hrc","nrc","orc","trc","wcrc","wrc")){
  forcsv=loadLatestCSVLake(agency)
  nvar=length(uvars <- unique(forcsv$parameter))
  nsite=length(usites <- unique(forcsv$SiteName))
  councilDeets=as.data.frame(matrix(nrow=nvar*nsite,ncol=7))
  names(councilDeets)=c("Site","Var","StartDate","EndDate","nMeas","MinVal","MaxVal")
  r=1
  for(ns in 1:nsite){
    for(nv in 1:nvar){
      these=which(forcsv$SiteName==usites[ns]&forcsv$parameter==uvars[nv])
      councilDeets[r,]=c(usites[ns],uvars[nv],
                         as.character(min(dmy(forcsv$Date[these]))),
                         as.character(max(dmy(forcsv$Date[these]))),
                         length(these),
                         min(forcsv$Value[these],na.rm=T),max(forcsv$Value[these],na.rm=T))
      r=r+1
    }
  }
  suppressWarnings({try(dir.create(path = paste0("h:/ericg/16666LAWA/2018/Lakes/4.Analysis/",
                               format(Sys.Date(),"%Y-%m-%d"),'/',agency)))})
  write.csv(councilDeets,paste0( 'H:/ericg/16666LAWA/2018/Lakes/4.Analysis/',format(Sys.Date(),"%Y-%m-%d"),'/',agency,'/',agency,'Audit.csv'),row.names=F)
}


library(lubridate)


#Check variable presence
# lawalakenames=c("DRP",  "ECOLI",   "NH4N",   "NO3N",   "CHLA", "Secchi",     "TN",     "TP",   "TON")
lawalakenames=c("TN","NH4N","TP","CHLA","pH","Secchi","ECOLI")
for(agency in c("ac","boprc","ecan","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
  forcsv=loadLatestCSVLake(agency,quiet=T)
  if(!is.null(forcsv)){
    # cat(agency,":\t",paste(sort(unique(forcsv$parameter))),'\n')
    cat(agency,'\t\t',lawalakenames[!lawalakenames%in%unique(forcsv$parameter)],'\n')
  }
}

#Censoring check
for(agency in c("ac","boprc","ecan","es","gwrc","hbrc","hrc","nrc","orc","trc","wcrc","wrc")){
  suppressWarnings({rm(forcsv)})
  forcsv=xml2csvLake(agency=agency,maxHistory = 20,quiet=T)
  if(!any(forcsv$Censored)){
    cat(agency,'\t')
  }
}



for(agency in c("ac","boprc","ecan","es","gwrc","hbrc","hrc","nrc","orc","trc","wcrc","wrc")){
  suppressWarnings({rm(forcsv)})
  forcsv=xml2csvLake(agency=agency,maxHistory = 20,quiet=T)
  cat(length(unique(forcsv$parameter)),paste(unique(forcsv$parameter),collapse=', '),'\n')
}






#Combine and add metadata
combo=data.frame(council=NA,SiteName=NA,Date=NA,Value=NA,Method=NA,parameter=NA)
siteTable <- read.csv(file = "h:/ericg/16666LAWA/2018/Lakes/1.Imported/LAWA_Site_Table_Lakes.csv",stringsAsFactors = F)
for(council in c("ecan","ac","boprc","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
  forcsv=loadLatestCSVLake(council,quiet=T)
  if(!is.null(forcsv)){
    cat(council,'\n',paste(names(forcsv),collapse='\t'),'\n')
    forcsv$council=council

    if(council %in% c('ac','es','wrc')){ #es mg/L  ac mg/L g/m3             wanted in mg/m3
      forcsv$Value[forcsv$parameter=="CHLA"]=forcsv$Value[forcsv$parameter=="CHLA"]*1000
    }
    if(council=='boprc'){
      forcsv$Value[forcsv$parameter%in%c("NH4N")]=forcsv$Value[forcsv$parameter%in%c("NH4N")]/1000
      forcsv$Value[forcsv$parameter%in%c("TP")]=forcsv$Value[forcsv$parameter%in%c("TP")]/1000
      forcsv$Value[forcsv$parameter%in%c("TN")]=forcsv$Value[forcsv$parameter%in%c("TN")]/1000
    }
    
    combo=merge(combo,forcsv[,names(forcsv)%in%names(combo)],all=T)
  }  
}
combo=combo[-(dim(combo)[1]),]
combo=unique(combo)

#plot all councils next to each other to check unit consistency
upara=unique(combo$parameter)
ucounc=unique(combo$council)
# for(up in seq_along(upara)){
  pvals=combo[combo$parameter==upara[up],]
  p1=quantile(pvals$Value,p=0.01,na.rm=T)
  p5=quantile(pvals$Value,p=0.05,na.rm=T)
  p75=quantile(pvals$Value,p=0.75,na.rm=T)
  p95=quantile(pvals$Value,p=0.95,na.rm=T)
  p999=quantile(pvals$Value,p=0.999,na.rm=T)
  pvals=pvals[pvals$Value<p999,]
  par(mfrow=c(4,3),mar=c(3,1,2,1))
  for(cc in seq_along(ucounc)){
    cvals=pvals$Value[pvals$council==ucounc[cc]]
    if(length(cvals[!is.na(cvals)])>2){
      plot(density(cvals,na.rm=T,from = p1,to=p95),main=paste(ucounc[cc],upara[up]),xlab='',xlim=c(p5,p95))
    }else{
      plot(0,0)
    }
  # }
}
# combo$Value[combo$council=='boprc'&combo$parameter=='TN']=combo$Value[combo$council=='boprc'&combo$parameter=='TN']/1000

write.csv(combo,paste0('h:/ericg/16666LAWA/2018/Lakes/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/LakesCombined.csv'),row.names = F)


names(combo)

#These lines tell us that ac and boprc sites are not in siteTable$CouncilSiteID
table(unique(tolower(combo$SiteName))%in%tolower(siteTable$CouncilSiteID))
unique(combo$council[!tolower(combo$SiteName)%in%tolower(siteTable$CouncilSiteID)])
unique(combo$SiteName[!tolower(combo$SiteName)%in%tolower(siteTable$CouncilSiteID)])

missingSites <- unique(combo$SiteName[!tolower(combo$SiteName)%in%tolower(siteTable$CouncilSiteID)])
table(tolower(missingSites)%in%tolower(siteTable$SiteID))
missingSites[tolower(missingSites)%in%tolower(siteTable$SiteID)]->toSwitch

unique(combo$council[unlist(lapply(toSwitch[tolower(toSwitch)%in%tolower(siteTable$SiteID)],
                                   FUN = function(y){grep(pattern=y,x = combo$SiteName,ignore.case = T)}))])
#some SiteNames are found in siteTable$siteID
#So we'll flick them round in the siteTable
flick=which(tolower(siteTable$SiteID)%in%tolower(toSwitch))
store=siteTable$SiteID[flick]
siteTable$SiteID[flick]=siteTable$CouncilSiteID[flick]
siteTable$CouncilSiteID[flick]=store
rm(store,flick)
table(unique(tolower(combo$SiteName))%in%tolower(siteTable$CouncilSiteID))


#Drop the ac sites that we dont have metadata for
missingSites <- unique(combo$SiteName[!tolower(combo$SiteName)%in%tolower(siteTable$CouncilSiteID)])
combo=combo[-which(tolower(combo$SiteName)%in%tolower(missingSites)),]
rm(missingSites)

table(unique(tolower(combo$SiteName))%in%tolower(siteTable$CouncilSiteID))
#bingo
combo$SiteNamelc=tolower(combo$SiteName)
siteTable$CouncilSiteIDlc=tolower(siteTable$CouncilSiteID)



lakesWithMetadata=merge(combo,siteTable,by.x="SiteNamelc",by.y="CouncilSiteIDlc",all.x=T,all.y=F)
lakesWithMetadata <- lakesWithMetadata%>%select(-'SiteNamelc')
write.csv(lakesWithMetadata,paste0('h:/ericg/16666LAWA/2018/Lakes/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/LakesWithMetadata.csv'),row.names = F)
save(lakesWithMetadata,file = paste0('h:/ericg/16666LAWA/2018/Lakes/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/LakesWithMetadata.rData'))



