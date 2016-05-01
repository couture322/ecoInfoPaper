## NCEAS GoA ecoinfo paper
## relabelling the 'categories' based on new ecosystem and dataType categories
##


library(dplyr)

dat=read.csv('data/archResNewCateg.csv',header=T,stringsAsFactors = F,na.strings=c('',' '))

datFill<-dat[is.na(dat$category.1),1:13]

rest<-dat %>%
  filter(!is.na(category.1)) %>%
  rename(dataType=category.1) %>%
  select(1:13)

barplot(table(datFill$category),
        las=3)

datFill <- datFill %>%
  mutate(dataType = ifelse(datFill$category=='oceanography','physical',
                           ifelse(datFill$category=='oil','chemical',
                                  ifelse(datFill$category=='fish','biological-fish',
                                         ifelse(datFill$category=='mammals','biological-mammals',
                                                ifelse(datFill$category=='marineBirds','biological-birds',
                                                       ifelse(datFill$category=='inverts','biological-benthicInverts',
                                                              ifelse(datFill$category=='plankton','biological-plankton','other'))))))))



## manually input terrestrial dataTypes and ecosystems into datFill
datFill[93,'dataType']<-'ecosystem/habitat' #Eric Meyers
datFill[93,'ecosystem']<-'terrestrial/freshWater'

datFill[120,'dataType']<-'ecosystem/habitat' #Geoffrey Coble
datFill[120,'ecosystem']<-'terrestrial/freshWater'

datFill[138,'dataType']<-'biological-mammals' #james Faro
datFill[138,'ecosystem']<-'terrestrial/freshWater'

datFill[148,'dataType']<-'biological-birds' #Jeffrey Hughes
datFill[148,'ecosystem']<-'terrestrial/freshWater'

datFill[167,'dataType']<-'biological-fish' #Jim Edmondson
datFill[167,'ecosystem']<-'terrestrial/freshWater'

datFill[186,'dataType']<-'biological-mammals' #Jonathan Lewis
datFill[186,'ecosystem']<-'terrestrial/freshWater'

datFill[201,'dataType']<-'ecosystem/habitat' #Ken Hodges
datFill[201,'ecosystem']<-'terrestrial/freshWater'

datFill[253,'dataType']<-'biological-mammals' #Merav Ben-David
datFill[253,'ecosystem']<-'terrestrial/freshWater' 

datFill[273,'dataType']<-'biological-mammals' #PI unavailable, black bear study
datFill[273,'ecosystem']<-'terrestrial/freshWater' 
datFill[274,'dataType']<-'ecosystem/habitat' #PI unavailable
datFill[274,'ecosystem']<-'terrestrial/freshWater'

datFill[297,'dataType']<-'biological-mammals' #Richard Sellers
datFill[297,'ecosystem']<-'terrestrial/freshWater' 
datFill[298,'dataType']<-'biological-mammals' #Richard Sellers
datFill[298,'ecosystem']<-'terrestrial/freshWater'

datFill[302,'dataType']<-'biological-mammals' #Robert White
datFill[302,'ecosystem']<-'terrestrial/freshWater' 

datFill[330,'dataType']<-'biological-mammals' #Steven Jewette
datFill[330,'ecosystem']<-'terrestrial/freshWater' 

datFill[340,'dataType']<-'physical' #Sue Mauger
datFill[340,'ecosystem']<-'terrestrial/freshWater' 

datFill[351,'dataType']<-'biological-birds' #Tim Bowman
datFill[351,'ecosystem']<-'terrestrial/freshWater' 

datFill[352,'dataType']<-'biological-birds' #Tim Bowman
datFill[352,'ecosystem']<-'terrestrial/freshWater' 

datFill[364,'dataType']<-'biological-birds' #Vernon Byrd
datFill[364,'ecosystem']<-'terrestrial/freshWater' 

barplot(table(factor(datFill$dataType,levels=c('physical',
                                               'chemical',
                                               'biological-plankton',
                                               'biological-benthicInverts',
                                               'biological-fish',
                                               'biological-birds',
                                               'biological-mammals',
                                               'other'
                                               ))),
        las=3)
        text(9,sum(datFill$dataType=='other')+7,as.character(sum(datFill$dataType=='other')))

datOthr=datFill[datFill$dataType=='other',c('PI','Status','Subject','category','parentTask','start','end','agency','secondaryCat','agency','dataType')]


#### adding the ecosystem/habitat data type:

datFill$dataType=ifelse(datFill$category %in% c('habitat','modeling','mapping'),'ecosystem/habitat',datFill$dataType)
datFill[datFill$dataType=='other','dataType']<-'social'

nonSoc<-datFill %>%
  filter(!dataType=='social') %>%
  select(c(1:12,14))
nonSoc$ecosystem<-sapply(nonSoc$ecosystem,function(x) ifelse(is.na(x),'marine',x))

rest$ecosystem<-sapply(rest$ecosystem,function(x) ifelse(x=='terrestrialFreshWater','terrestrial/freshWater','marine'))

soc<-datFill %>%
  filter(dataType=='social') %>%
  select(c(1:12,14))

datAll<-rbind(rest,nonSoc,soc)

write.csv(datAll,file='data/archResRevised.csv',row.names=F)
# ## digging into the left over "other" datasets
# fillIn<-datOthr2[,c('PI','category','parentTask','start','end','agency')]
# fillIn$subCat<-c('projMgmt','projMgmt','economics','projMgmt','economics','subsistence','communityMgmt','social','environmentalImpacts','samplingImpacts','projMgmt','meeting','economics','economics','economics','economics','economics','economics','subsistence','dataMgmt','dataMgmt','communityMgmt')
# fillIn$subCat2<-c('habitat','ecosystem','recreation','habitat','subsistence', 'film','waste','subsistence','infrastructure','metaAnalysis',NA,'10yrSymposium','resarchPrograms','subsistence','fisheries','resources','resources','fisheries','resources',NA,NA,'waste')