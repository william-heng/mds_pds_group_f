library(shiny)
library(leaflet)
library(dplyr)

Folder<-"C:/Users/Gigabyte/Google Drive/04 Master DS/WQD7001 PRINCIPLES OF DATA SCIENCE/Group Assignment/"
dat_hostel<-read.csv(paste0(Folder,"Hostel.csv"),stringsAsFactors = F)
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
      dat_hostel
    }else{
      filter(dat_hostel,City==input$city)
    }
  }
  )
  
  dat_temp2<-reactive({filter(dat_temp()
                            ,cleanliness<=input$CL[2]
                            ,cleanliness>input$CL[1]
  )
    }
  )
  #dat_temp2<-reactive({select(dat_temp(),-lon:lat)})
  output$out<-renderTable(reactive({select(dat_temp2(),-lon,-lat,-Distance)})())
  output$mymap<-renderLeaflet(
    addAwesomeMarkers(addTiles(leaflet())
                      ,lng=dat_temp2()$lon
                      ,lat=dat_temp2()$lat)
  )
  #output$out2<-renderText(input$CL[1])
  
  }
)

shinyApp(ui, server)
