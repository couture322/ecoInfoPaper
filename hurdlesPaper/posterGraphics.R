### Visualzations for hurdles to synthesis poster
### Jessica Couture, Rachael Blake, Colette Ward
### Data: LTOP and FOCI zooplankton 

## GRAPH OF NUMBER OF SPECIES SAMPLED PER YEAR FOR EACH DATASET
library(httr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(rworldmap)


ltopURL <- "https://goa.nceas.ucsb.edu/goa/metacat?action=read&qformat=metacatui&sessionid=&docid=df35b.56.4"
ltopGet <- GET(ltopURL)
ltop1 <- content(ltopGet, as='text')
ltop <- read.csv(file=textConnection(ltop1),stringsAsFactors=FALSE,na.strings=c(
  '',' ','N/A','NA'))
head(ltop)

ltop2=ltop %>%
  mutate(taxInfo=paste(phylum,subphylum,class,subclass,infraclass,order,suborder,inraforder,family,genus,species)) %>%
  filter(stationID %in% c('GAK1','GAK2','GAK3','GAK4','GAK5','GAK6','GAK7','GAK8','GAK9','GAK10','GAK11','GAK12','GAK13')) %>%
  select(dateTime,lat,lon,taxInfo)


ltopLgURL <- "https://goa.nceas.ucsb.edu/goa/metacat?action=read&qformat=metacatui&sessionid=&docid=df35b.61.4"
ltopLgGet <- GET(ltopLgURL)
ltopLg1 <- content(ltopLgGet, as='text')
ltopLg <- read.csv(file=textConnection(ltopLg1),stringsAsFactors=FALSE,na.strings=c(
  '',' ','N/A','NA'))
head(ltopLg)

ltopLg2=ltopLg %>%
  mutate(taxInfo=paste(Phylum,Subphylum,Class,Subclass,Infraclass,Order,Suborder,Infraorder,Family,Genus,Species)) %>%
  rename(dateTime=startDateTime) %>%
  filter(stationID %in% c('GAK1','GAK2','GAK3','GAK4','GAK5','GAK6','GAK7','GAK8','GAK9','GAK10','GAK11','GAK12','GAK13')) %>%
  select(dateTime,lat,lon,taxInfo)

ltopDat=rbind(ltop2,ltopLg2)
datSpl=strsplit(ltopDat$dateTime,'-')
ltopDat2=ltopDat %>%
  mutate(year=as.numeric(sapply(datSpl,function(x) x[1]))) %>%
  mutate(yrTax=paste(year,taxInfo)) %>%
  filter(!duplicated(yrTax)) %>%
  filter(!is.na(lat)) %>%
  select(year,lat,lon,taxInfo)

ltopSum=data.frame(table(na.omit(ltopDat2$year)))
ltopSum2=ltopSum %>%
  mutate(year=as.numeric(as.character(Var1))) %>%
  rename(nTax=Freq) %>%    
  mutate(dataset='LTOP') %>%
  select(year,dataset,nTax)

fociTax=data.frame(year=1984:2015,
                   nTax=56,
                   dataset='FOCI')


nTax=rbind(ltopSum2,fociTax)

dsCols=c("LTOP"=rgb(155, 255, 255, maxColorValue=255),
         "FOCI"=rgb(255,153,255, maxColorValue=255))

nPlot2=ggplot(nTax)+
  geom_line(aes(x=year,y=nTax,colour=dataset),size=3)+
  scale_colour_manual(values=dsCols,
                      name="Dataset",
                      breaks=c("LTOP", "FOCI"),
                      labels=c("Seward Line LTOP", "EcoFOCI"))+
  theme(axis.line=element_line('black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.text.x = element_text(angle=90))+
  scale_x_continuous(breaks=c(seq(1984,2015,2)), labels=c(seq(1984,2015,2)))+
  xlab('\nYear')+
  ylab('Number of taxa sampled per year\n')+
  ylim(0,150)+
  theme(axis.text=element_text(size=14),
        title=element_text(size=16,face="bold"))



#########################################
######### MAP OF SAMPLING SITES #########
#########################################

fSites=read.csv('hurdlesPaper/line8coords.csv',header=T,stringsAsFactors=F)

splN=strsplit(fSites$N,' ')
splW=strsplit(fSites$W,' ')

fSites2=fSites %>%
  mutate(degN=sapply(splN,function(x) x[1])) %>%
  mutate(decN=sapply(splN,function(x) as.numeric(x[2])/60)) %>%
  mutate(lat=as.numeric(degN)+decN) %>%
  mutate(degW=sapply(splW,function(x) x[1])) %>%
  mutate(decW=sapply(splW,function(x) as.numeric(x[2])/60)) %>%
  mutate(lon=-(as.numeric(degW)+decW)) %>%
  mutate(dataset='FOCI') %>%
  select(dataset,lat,lon)

lSites=ltopDat2 %>%
  filter(year==2010) %>%
  mutate(lonFix=ifelse(lon>0,-lon,lon)) %>%
  mutate(lon2=round(lonFix,digits=1)) %>%
  mutate(lat2=round(lat,digits=1)) %>%
  filter(!duplicated(lat2)) %>%
  filter(!duplicated(lon2)) %>%
  mutate(dataset='LTOP') %>%
  select(dataset,lat2,lon2)
colnames(lSites)=c('dataset','lat','lon')

sites=rbind(lSites,fSites2)



### GIS map layer
state <- readOGR('hurdlesPaper/GIS','statep010')
stateDf=fortify(state)

sitesMap2=ggplot(data=stateDf, aes(y=lat, x=lon)) +
  geom_map(map=stateDf,aes(x=long,y=lat,map_id=id),fill=id)+
  coord_map(xlim = c(-157, -143),ylim = c(56, 62))+ 
  #scale_fill_manual(values=colMap)+
  geom_point(data=sites,mapping=aes(x=as.numeric(lon), y=as.numeric(lat),colour=dataset),size=5, shape=20) +
  scale_colour_manual(values=dsCols,
                      name="Dataset",
                      breaks=c("LTOP", "FOCI"),
                      labels=c("Seward Line LTOP", "EcoFOCI"))+
  theme(axis.line=element_line('black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab('lon')+
  ylab('lat')




## EXTENT MAP: get map layer from worldmap pkg
world=getMap('low',projection=NA)
worldB=world[!is.na(world$continent),]
world2=worldB[worldB$continent=='North America' & worldB$LON<0,]
fWorld=fortify(world2)

colMap=c('dimgrey','black')

extDf=data.frame(xmin=-157,xmax=-143,ymin=56,ymax=62)

extMap=ggplot(data=fWorld) +
  geom_map(map=fWorld,aes(x=long,y=lat,map_id=id),fill=id)+
  coord_map(xlim = c(-185, -110),ylim = c(30, 70))+ 
  scale_fill_manual(values=colMap)+
  #geom_point(data=sites,mapping=aes(x=as.numeric(lon), y=as.numeric(lat),colour=dataset),size=5, shape=20) +
  scale_colour_manual(values=dsCols,
                      name="Dataset",
                      breaks=c("LTOP", "FOCI"),
                      labels=c("Seward Line LTOP", "EcoFOCI"))+
  geom_rect(data=extDf,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),color='red',lwd=0.5,alpha=0)+
  #ggtitle('Marine mammal data\nNo. Pacific Ocean & Gulf of Alaska')+
  theme(axis.line=element_line('black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab('lon')+
  ylab('lat')
theme(axis.line=element_line(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      legend.position='right',
      axis.text=element_text(size=14),
      title=element_text(size=16,face="bold"))
