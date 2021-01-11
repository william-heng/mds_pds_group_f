library(shiny)
library(leaflet)
library(dplyr)

dat_hostel<-read.csv(file = "Hostel.csv",stringsAsFactors = F)
dat_hostel<-dat_hostel[apply(dat_hostel,1,function(X) !any(is.na(X))),]
dat_hostel<-filter(dat_hostel,price.from!=1003200)

dat_hostel$lon[dat_hostel$hostel.name=="Hostel J Culture 168" & dat_hostel$City=="Osaka"]<-135.4756563455386
dat_hostel$lat[dat_hostel$hostel.name=="Hostel J Culture 168" & dat_hostel$City=="Osaka"]<-34.74883461136703

dat_hostel$lon[dat_hostel$hostel.name=="Sakura Guest House" & dat_hostel$City=="Osaka"]<-135.50494191623136
dat_hostel$lat[dat_hostel$hostel.name=="Sakura Guest House" & dat_hostel$City=="Osaka"]<-34.66842726841171


#dat_hostel <- dat_hostel %>% dplyr::rename(
#  hostel.name=HostelName ,
#  price.from=StartingPrice ,
#  rating.band=RatingBand,
#  summary.score=RatingScore ,
#  atmosphere=Atmosphere ,
#  cleanliness=Cleanliness,
#  facilities=Facilities ,
#  location.y=Location ,
#  security=Security ,
#  staff=Staff,
#  valueformoney=ValueForMoney 
#)

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


server <- shinyServer(function(input,output,session){
  # City selection
  dat_temp<-reactive({
    if (input$city=="All"){dat_hostel}
    else{filter(dat_hostel,City==input$city)}
    })
  
  # 1st layer filtering (Starting Price)
  dat_temp2<-reactive({filter(dat_temp()
                              ,StartingPrice<=input$SP[2]
                              ,StartingPrice>input$SP[1])})
  # 2nd layer filtering (Rating Score)
  dat_temp3<-reactive({filter(dat_temp2()
                              ,RatingScore<=input$RS[2]
                              ,RatingScore>input$RS[1])})
  # 3rd layer filtering (Value for Money)
  dat_temp4<-reactive({filter(dat_temp3()
                              ,ValueForMoney<=input$VM[2]
                              ,ValueForMoney>input$VM[1])})
  
  output$out<-renderTable(reactive({select(dat_temp4(),-lon,-lat,-Distance)})())
  output$mymap<-renderLeaflet(addAwesomeMarkers(addTiles(leaflet())
                      ,lng=dat_temp4()$lon
                      ,lat=dat_temp4()$lat
                      ,icon = awesomeIcons(icon = "ios-home", library = 'ion'
                                           ,iconColor = "#ffffff"
                                           ,markerColor = "blue")
                      ,popup=paste("Hostel Name : "
                                   ,dat_temp4()$HostelName,"<br>","Starting Price : "
                                   ,dat_temp4()$StartingPrice,"<br>","Rating Score : "
                                   ,dat_temp4()$RatingScore,"<br>","Value for Money : "
                                   ,dat_temp4()$ValueForMoney,"<br>","Cleanliness : "
                                   ,dat_temp4()$Cleanliness,"<br>","Security : "
                                   ,dat_temp4()$Security,"<br>","Atmosphere : "
                                   ,dat_temp4()$Atmosphere,"<br>","Staff : "
                                   ,dat_temp4()$Staff
                                   )
                      )
                      )
  output$table <- renderDataTable({dat_hostel})
  output$plot <- renderPlot(ggplot(dat_hostel, aes(x=StartingPrice, y=RatingScore,col=City)) + geom_point(size=5)+ labs(y="Rating Score", x = "Starting Price")+ggtitle("Distribution of Hostel based on Starting Price and Rating Score"))
}
)
