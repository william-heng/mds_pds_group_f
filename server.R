library(shiny)
library(leaflet)
library(dplyr)
library(ggbeeswarm)

dat_hostel<-read.csv(file = "https://raw.githubusercontent.com/william-heng/mds_pds_group_f/main/Hostel.csv",stringsAsFactors = F)
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
  
  dat_temp5<-reactive({mutate(dat_temp4()
                              ,col=if_else(StartingPrice>quantile(StartingPrice,0.75,na.rm = T)
                                           ,"Red"
                                           ,if_else(StartingPrice<quantile(StartingPrice,0.25,na.rm = T)
                                                    ,"green","blue"
                                                    )
                                           ))})

  
  output$out<-renderTable(reactive({select(dat_temp5(),-lon,-lat,-Distance)})())
  output$mymap<-renderLeaflet(addLegend(addAwesomeMarkers(addTiles(leaflet())
                      ,lng=dat_temp5()$lon
                      ,lat=dat_temp5()$lat
                      ,icon = awesomeIcons(icon = "ios-home", library = 'ion'
                                           ,iconColor = "#ffffff"
                                           ,markerColor = dat_temp5()$col)
                      ,popup=paste("Hostel Name : "
                                   ,dat_temp5()$HostelName,"<br>","Starting Price : "
                                   ,dat_temp5()$StartingPrice,"<br>","Rating Score : "
                                   ,dat_temp5()$RatingScore,"<br>","Value for Money : "
                                   ,dat_temp5()$ValueForMoney,"<br>","Cleanliness : "
                                   ,dat_temp5()$Cleanliness,"<br>","Security : "
                                   ,dat_temp5()$Security,"<br>","Atmosphere : "
                                   ,dat_temp5()$Atmosphere,"<br>","Staff : "
                                   ,dat_temp5()$Staff
                                   )
                      )
                      ,position = "bottomright",title="Prices within the selection",labFormat = labelFormat()
                      ,colors=c("red","blue","green")
                      ,labels=c("Top 20% most expensive"
                                ,"Normal Price Range"
                                ,"Bottom 20% cheapest")
                      )
                      )
  
  output$table <- renderDataTable({dat_hostel},options = list(pageLength = 10))
  output$plot <- renderPlot({
    if (input$Plot_Select=="Rating Score"){
    ggplot(dat_hostel, aes(y=RatingScore, x=City,col=City)) + 
        geom_beeswarm(size=5)+ labs(y="Rating Score")+
        ggtitle("Distribution of Hostel based on Starting Price and Rating Score")+
        theme(axis.text=element_text(size=20)
              ,axis.title=element_text(size=30,face="bold")
              ,legend.title = element_text(size = 20)
              ,legend.text = element_text(size = 20)
        )     
    }else if(input$Plot_Select=="Cleanliness"){
      ggplot(dat_hostel, aes(y=Cleanliness, x=City,col=City)) + 
        geom_beeswarm(size=5)+ labs(y="Cleanliness")+
        ggtitle("Distribution of Hostel based on Starting Price and Cleanliness Score")+
        theme(axis.text=element_text(size=20)
              ,axis.title=element_text(size=30,face="bold")
              ,legend.title = element_text(size = 20)
              ,legend.text = element_text(size = 20)
        )     
    }else if(input$Plot_Select=="Atmosphere"){
      ggplot(dat_hostel, aes(y=Atmosphere, x=City,col=City)) + 
        geom_beeswarm(size=5)+ labs(y="Atmosphere")+
        ggtitle("Distribution of Hostel based on Starting Price and Atmosphere Score")+
        theme(axis.text=element_text(size=20)
              ,axis.title=element_text(size=30,face="bold")
              ,legend.title = element_text(size = 20)
              ,legend.text = element_text(size = 20)
        )           
    }else if(input$Plot_Select=="Facilities"){
      ggplot(dat_hostel, aes(y=Facilities  , x=City,col=City)) + 
        geom_beeswarm(size=5)+ labs(y="Facilities")+
        ggtitle("Distribution of Hostel based on Starting Price and Facilities Score")+
        theme(axis.text=element_text(size=20)
              ,axis.title=element_text(size=30,face="bold")
              ,legend.title = element_text(size = 20)
              ,legend.text = element_text(size = 20)
        )           
    }else if(input$Plot_Select=="Security"){
      ggplot(dat_hostel, aes(y=Security, x=City,col=City)) + 
        geom_beeswarm(size=5)+ labs(y="Security")+
        ggtitle("Distribution of Hostel based on Starting Price and Security Score")+
        theme(axis.text=element_text(size=20)
              ,axis.title=element_text(size=30,face="bold")
              ,legend.title = element_text(size = 20)
              ,legend.text = element_text(size = 20)
        )         
    }else if(input$Plot_Select=="Location"){
      ggplot(dat_hostel, aes(y=Location, x=City,col=City)) + 
        geom_beeswarm(size=5)+ labs(y="Location")+
        ggtitle("Distribution of Hostel based on Starting Price and Location Score")+
        theme(axis.text=element_text(size=20)
              ,axis.title=element_text(size=30,face="bold")
              ,legend.title = element_text(size = 20)
              ,legend.text = element_text(size = 20)
        )      
    }else if(input$Plot_Select=="Staff"){
      ggplot(dat_hostel, aes(y=Staff, x=City,col=City)) + 
        geom_beeswarm(size=5)+ labs(y="Staff")+
        ggtitle("Distribution of Hostel based on Starting Price and Staff Score")+
        theme(axis.text=element_text(size=20)
              ,axis.title=element_text(size=30,face="bold")
              ,legend.title = element_text(size = 20)
              ,legend.text = element_text(size = 20)
        )           
    }else if(input$Plot_Select=="Starting Price"){
      ggplot(dat_hostel, aes(y=StartingPrice, x=City,col=City)) + 
        geom_beeswarm(size=5)+ labs(y="Starting Price")+
        ggtitle("Distribution of Hostel based on Starting Price and Staff Score")+
        theme(axis.text=element_text(size=20)
              ,axis.title=element_text(size=30,face="bold")
              ,legend.title = element_text(size = 20)
              ,legend.text = element_text(size = 20)
              )      
    }
    
  })
}
)
