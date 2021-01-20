library(dplyr)
library(pracma)

dat_hostel<-read.csv(file = "https://raw.githubusercontent.com/william-heng/mds_pds_group_f/main/Hostel.csv",stringsAsFactors = F)


MS_TK<-read.csv(paste0(Folder2,"Tokyo Metro Station.csv"),stringsAsFactors = F)
MS_OS<-read.csv(paste0(Folder2,"Osaka Metro Station.csv"),stringsAsFactors = F)
MS_KT<-read.csv(paste0(Folder2,"Kyoto Metro Station.csv"),stringsAsFactors = F)
MS_KT<-MS_KT[apply(MS_KT,1,function(X) !any(is.na(X))),]
MS_FK<-read.csv(paste0(Folder2,"Fukuoka Metro Station.csv"),stringsAsFactors = F)
MS_FK<-MS_FK[apply(MS_FK,1,function(X) !any(is.na(X))),]
MS_HS<-read.csv(paste0(Folder2,"Hiroshima Metro Station.csv"),stringsAsFactors = F)
#MS_HS<-MS_HS[apply(MS_HS,1,function(X) !any(is.na(X))),]


dat_tk<-filter(dat_hostel,City=="Tokyo")
dat_os<-filter(dat_hostel,City=="Osaka")
dat_kt<-filter(dat_hostel,City=="Kyoto")
dat_fk<-filter(dat_hostel,City=="Fukuoka-City")
dat_hs<-filter(dat_hostel,City=="Hiroshima")

dat_temp<-data.frame()
for(i in 1:dim(dat_tk)[1]){
  for(j in 1:dim(MS_TK)[1]){
    l1<-c(dat_tk$lat[i],dat_tk$lon[i])
    l2<-c(MS_TK$lat[j],MS_TK$long[j])
    
    dat_temp<-rbind.data.frame(dat_temp
                               ,cbind.data.frame(
                                 HostelName=dat_tk$HostelName[i]
                                 ,Station=MS_TK$Station[j]
                                 ,dist=haversine(l1,l2)
                                 ,lat_station=MS_TK$lat[j]
                                 ,lng_station=MS_TK$long[j]
                               )
    )
    
  }
}

for(i in 1:dim(dat_os)[1]){
  for(j in 1:dim(MS_OS)[1]){
    l1<-c(dat_os$lat[i],dat_os$lon[i])
    l2<-c(MS_OS$lat[j],MS_OS$lon[j])
    
    dat_temp<-rbind.data.frame(dat_temp
                               ,cbind.data.frame(
                                 HostelName=dat_os$HostelName[i]
                                 ,Station=MS_OS$Station[j]
                                 ,dist=haversine(l1,l2)
                                 ,lat_station=MS_OS$lat[j]
                                 ,lng_station=MS_OS$lon[j]
                               )
    )
    
  }
}

for(i in 1:dim(dat_kt)[1]){
  for(j in 1:dim(MS_KT)[1]){
    l1<-c(dat_kt$lat[i],dat_kt$lon[i])
    l2<-c(MS_KT$lat[j],MS_KT$lon[j])
    
    dat_temp<-rbind.data.frame(dat_temp
                               ,cbind.data.frame(
                                 HostelName=dat_kt$HostelName[i]
                                 ,Station=MS_KT$Station[j]
                                 ,dist=haversine(l1,l2)
                                 ,lat_station=MS_KT$lat[j]
                                 ,lng_station=MS_KT$lon[j]
                               )
    )
    
  }
}

for(i in 1:dim(dat_fk)[1]){
  for(j in 1:dim(MS_FK)[1]){
    l1<-c(dat_fk$lat[i],dat_fk$lon[i])
    l2<-c(MS_FK$lat[j],MS_FK$lon[j])
    
    dat_temp<-rbind.data.frame(dat_temp
                               ,cbind.data.frame(
                                 HostelName=dat_fk$HostelName[i]
                                 ,Station=MS_FK$Station[j]
                                 ,dist=haversine(l1,l2)
                                 ,lat_station=MS_FK$lat[j]
                                 ,lng_station=MS_FK$lon[j]
                               )
    )
    
  }
}

for(i in 1:dim(dat_hs)[1]){
  for(j in 1:dim(MS_HS)[1]){
    l1<-c(dat_hs$lat[i],dat_hs$lon[i])
    l2<-c(MS_HS$lat[j],MS_HS$lon[j])
    
    dat_temp<-rbind.data.frame(dat_temp
                               ,cbind.data.frame(
                                 HostelName=dat_hs$HostelName[i]
                                 ,Station=MS_HS$Station[j]
                                 ,dist=haversine(l1,l2)
                                 ,lat_station=MS_HS$lat[j]
                                 ,lng_station=MS_HS$lon[j]
                               )
    )
    
  }
}

dat_temp<-unique(inner_join(dat_temp
                            ,summarise(group_by(dat_temp,HostelName),dist=min(dist))
                            ,by=c("HostelName","dist")
))

write.csv(dat_temp,paste0(Folder2,"mds_pds_group_f/Japan_Metro.csv"),row.names = F)


dat_metro<-read.csv("https://raw.githubusercontent.com/william-heng/mds_pds_group_f/main/Japan_Metro.csv")
dat_metro_ori<-read.csv("https://raw.githubusercontent.com/william-heng/mds_pds_group_f/main/Japan_Metro_Ori.csv")

dat_hostel<-dat_hostel[apply(dat_hostel,1,function(X) !any(is.na(X))),]
dat_hostel<-filter(dat_hostel,price.from!=1003200)

dat_hostel$lon[dat_hostel$hostel.name=="Hostel J Culture 168" & dat_hostel$City=="Osaka"]<-135.4756563455386
dat_hostel$lat[dat_hostel$hostel.name=="Hostel J Culture 168" & dat_hostel$City=="Osaka"]<-34.74883461136703

dat_hostel$lon[dat_hostel$hostel.name=="Sakura Guest House" & dat_hostel$City=="Osaka"]<-135.50494191623136
dat_hostel$lat[dat_hostel$hostel.name=="Sakura Guest House" & dat_hostel$City=="Osaka"]<-34.66842726841171


dat_hostel <- dat_hostel %>% dplyr::rename(
  HostelName = hostel.name,
  StartingPrice = price.from,
  RatingBand = rating.band,
  RatingScore = summary.score,
  Atmosphere = atmosphere,
  Cleanliness = cleanliness,
  Facilities = facilities,
  Location = location.y,
  Security = security,
  Staff = staff,
  ValueForMoney = valueformoney
)

names(dat_metro)[names(dat_metro)=="dist"]<-"Dist_Station"

dat_hostel2<-left_join(dat_hostel,dat_metro,by="HostelName")

write.csv(dat_hostel2,"C:/Users/Gigabyte/Google Drive/04 Master DS/WQD7001 PRINCIPLES OF DATA SCIENCE/Group Assignment/mds_pds_group_f/Hostel_cleaned.csv"
          ,row.names = F)
