library(shiny)
library(leaflet)
library(dplyr)

Folder<-"https://raw.githubusercontent.com/william-heng/mds_pds_group_f/main/"
dat_hostel<-read.csv(paste0(Folder,"Hostel.csv"),stringsAsFactors = F)

dat_hostel$lon[dat_hostel$hostel.name=="Hostel J Culture 168" & dat_hostel$City=="Osaka"]<-135.4756563455386
dat_hostel$lat[dat_hostel$hostel.name=="Hostel J Culture 168" & dat_hostel$City=="Osaka"]<-34.74883461136703

dat_hostel$lon[dat_hostel$hostel.name=="Sakura Guest House" & dat_hostel$City=="Osaka"]<-135.50494191623136
dat_hostel$lat[dat_hostel$hostel.name=="Sakura Guest House" & dat_hostel$City=="Osaka"]<-34.66842726841171

dim(dat_hostel)
dat_hostel<-dat_hostel[apply(dat_hostel,1,function(X) !any(is.na(X))),]
dat_hostel

ui <- shinyUI(fluidPage(
  titlePanel("Japan Hostel")
  ,leafletOutput("mymap")
  ,fluidRow(verbatimTextOutput("map_marker_click"))
  ,sidebarLayout(
    sidebarPanel(
      selectInput("city","City : ", c("All",unique(dat_hostel$City)))
      ,sliderInput("CL","Cleaniness"
                  ,min=1,max=10,value = c(1,10),step=0.1))
    ,mainPanel(
      tableOutput("out")
      #,textOutput("out2")
      )
    )
  )
)

server<-shinyServer(function(input,output){
  
  dat_temp<-reactive({
    if (input$city=="All"){
      
      
      filter(dat_hostel
             ,cleanliness<=input$CL[2]
             ,cleanliness>input$CL[1]
      )
      
    }else{
      filter(dat_hostel,City==input$city             
             ,cleanliness<=input$CL[2]
             ,cleanliness>input$CL[1])
    }
  }
  
  
  )
  
  #dat_temp2<-reactive({select(dat_temp(),-lon:lat)})
  output$out<-renderTable(reactive({select(dat_temp()
                                           ,-lon,-lat
                                           ,-Distance)})())
  output$mymap<-renderLeaflet(
    addAwesomeMarkers(addTiles(leaflet())
                      ,lng=dat_temp()$lon
                      ,lat=dat_temp()$lat
                      ,popup="abc")
  )
  #output$out2<-renderText(input$CL[1])
  
  }
)

shinyApp(ui, server)
