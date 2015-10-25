### Visualzations for hurdles to synthesis poster
### Jessica Couture, Rachael Blake, Colette Ward
### Data: LTOP and FOCI zooplankton 

## GRAPH OF NUMBER OF SPECIES SAMPLED PER YEAR FOR EACH DATASET
library(httr)
library(dplyr)
library(ggplot2)

ltopURL <- "https://goa.nceas.ucsb.edu/goa/metacat?action=read&qformat=metacatui&sessionid=&docid=df35b.56.4"
ltopGet <- GET(ltopURL)
ltop1 <- content(ltopGet, as='text')
ltop <- read.csv(file=textConnection(ltop1),stringsAsFactors=FALSE,na.strings=c(
  '',' ','N/A','NA'))
head(ltop)

ltop2=ltop %>%
  mutate(taxInfo=paste(phylum,subphylum,class,subclass,infraclass,order,suborder,inraforder,family,genus,species)) %>%
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
                   nTax=6,
                   dataset='FOCI')
  

nTax=rbind(ltopSum3,fociTax2)

nPlot2=ggplot(nTax)+
  geom_line(aes(x=year,y=nTax,colour=dataset),size=1.5)+
  scale_colour_discrete(name="Dataset",
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
  theme(axis.text=element_text(size=14),
        title=element_text(size=16,face="bold"))




## GRAPH OF SAMPLING SITES
# Need locations data from CW for the FOCI sites