library(readxl)
library(ggmap)

dat0<-read_excel(paste0("C:/Users/Gigabyte/Google Drive/04 Master DS/WQD7001 PRINCIPLES OF DATA SCIENCE/Group Assignment/Hiroshima Metro Station.xlsx"))
dat1<-read_excel(paste0("C:/Users/Gigabyte/Google Drive/04 Master DS/WQD7001 PRINCIPLES OF DATA SCIENCE/Group Assignment/Tokyo Metro Station.xlsx"))
dat2<-read_excel(paste0("C:/Users/Gigabyte/Google Drive/04 Master DS/WQD7001 PRINCIPLES OF DATA SCIENCE/Group Assignment/Kyoto Metro Station.xlsx"))
dat3<-read_excel(paste0("C:/Users/Gigabyte/Google Drive/04 Master DS/WQD7001 PRINCIPLES OF DATA SCIENCE/Group Assignment/Osaka Metro Station.xlsx"))
dat4<-read_excel(paste0("C:/Users/Gigabyte/Google Drive/04 Master DS/WQD7001 PRINCIPLES OF DATA SCIENCE/Group Assignment/Fukuoka Metro Station.xlsx"))

a<-geocode(dat0$Station2)
b<-geocode(dat1$Station2)
c<-geocode(dat2$Station2)
d<-geocode(dat3$Station2)
e<-geocode(dat4$Station2)


write.csv(cbind.data.frame(dat0,a)
          ,paste0("C:/Users/Gigabyte/Google Drive/04 Master DS/WQD7001 PRINCIPLES OF DATA SCIENCE/Group Assignment/Hiroshima Metro Station.csv")
          ,row.names = F
)

write.csv(cbind.data.frame(dat1,b)
          ,paste0("C:/Users/Gigabyte/Google Drive/04 Master DS/WQD7001 PRINCIPLES OF DATA SCIENCE/Group Assignment/Tokyo Metro Station.csv")
          ,row.names = F
)

write.csv(cbind.data.frame(dat2,c)
          ,paste0("C:/Users/Gigabyte/Google Drive/04 Master DS/WQD7001 PRINCIPLES OF DATA SCIENCE/Group Assignment/Kyoto Metro Station.csv")
          ,row.names = F
)

write.csv(cbind.data.frame(dat3,d)
          ,paste0("C:/Users/Gigabyte/Google Drive/04 Master DS/WQD7001 PRINCIPLES OF DATA SCIENCE/Group Assignment/Osaka Metro Station.csv")
          ,row.names = F
)

write.csv(cbind.data.frame(dat4,e)
          ,paste0("C:/Users/Gigabyte/Google Drive/04 Master DS/WQD7001 PRINCIPLES OF DATA SCIENCE/Group Assignment/Fukuoka Metro Station.csv")
          ,row.names = F
)



