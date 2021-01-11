library(shiny)
library(leaflet)
library(dplyr)

dat_hostel<-read.csv(file = "Hostel.csv",stringsAsFactors = F)
dat_hostel<-dat_hostel[apply(dat_hostel,1,function(X) !any(is.na(X))),]
dat_hostel<-filter(dat_hostel,price.from!=1003200)
dat_hostel <- dat_hostel %>% rename(
  "HostelName" = hostel.name,
  "StartingPrice" = price.from,
  "RatingBand" = rating.band,
  "RatingScore" = summary.score,
  "Atmosphere" = atmosphere,
  "Cleanliness" = cleanliness,
  "Facilities" = facilities,
  "Location" = location.y,
  "Security" = security,
  "Staff" = staff,
  "ValueForMoney" = valueformoney
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
                      ,popup=paste("Hostel Name : ",dat_temp4()$HostelName,"<br>","Starting Price : ",dat_temp4()$StartingPrice,"<br>","Rating Score : ",dat_temp4()$RatingScore,"<br>","Value for Money : ",dat_temp4()$ValueForMoney)))
  output$table <- renderDataTable({dat_hostel})
  output$plot <- renderPlot(ggplot(dat_hostel, aes(x=StartingPrice, y=RatingScore,col=City)) + geom_point(size=5)+ labs(y="Rating Score", x = "Starting Price")+ggtitle("Distribution of Hostel based on Starting Price and Rating Score"))
}
)