library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(ggplot2)
library(ggbeeswarm)

ui <- shinyUI(fluidPage(
  setBackgroundImage(src = "https://github.com/william-heng/mds_pds_group_f/blob/main/Cherry%20Blossom%20Japan.jpg?raw=true")
  
              ,wellPanel(tags$style(".well {background-color:rgba(255,255,255,0.7);}"
                                    ,type='text/css',".nav-tabs {font-size: 30px;font-weight: bold;}"
                                    ,'.nav-tabs {background-color:rgba(255,182,193,0.8);}'
                                    )
                                    
                         ,fluidRow(column(width = 12,titlePanel("Japan Hostel Finder"),
      tabsetPanel(
          tabPanel("About",
                   h3("Introduction", style = "font-size:30px;font-weight: bold;"),
                   p("Still thinking about where to stay when you are visiting Japan?"
                     , style = "font-size:20px;font-weight: bold;"),
                   p("Why not consider hostel? This app helps you finding your ideal place to stay"
                     , style = "font-size:20px;font-weight: bold;"),
                   br(),
                   h3("Why this app?", style = "font-size:30px;font-weight: bold;"),
                   p("1. You can make choice based on a handful of criteria", style = "font-size:20px;font-weight: bold;"),
                   p("2. You can visualise your selection on an interactive map which can help you plan your perfect itinerary"
                     , style = "font-size:20px;font-weight: bold;"),
                   p("3. We provide you an interactive table with more attributes to assist your decision making"
                     , style = "font-size:20px;font-weight: bold;"),
                   br(),
                   h3("Who are we?", style = "font-size:30px;font-weight: bold;"),
                   p("- Kumanan A/L N Govaichelvan (17096921)", style = "font-size:20px;font-weight: bold;"),
                   p("- Law Chuan Liang (S2003493)", style = "font-size:20px;font-weight: bold;"),
                   p("- Teh Lee Leng (S2012133)", style = "font-size:20px;font-weight: bold;"),
                   p("- William Heng Chun Meng (S2005592)", style = "font-size:20px;font-weight: bold;"),
                   br(),
                   br(),
                   br(),
                   p("Image Source:",a("Lonely Planet", href="https://lp-cms-production.imgix.net/image_browser/Cherry%20Blossom%20Japan.jpg?auto=format&fit=crop&sharp=10&vib=20&ixlib=react-8.6.4&w=850&q=75&dpr=1")
                     , style = "font-size:20px;font-weight: bold;")
                   ),
          tabPanel("Find Your Hostel",
                   sidebarLayout(sidebarPanel(
                                selectInput("city","City : ", c("All",unique(dat_hostel$City))),
                                sliderInput("SP","Starting Price :",min=1000,max=8000,value = c(0,8000),step=1000),
                                sliderInput("RS","Rating Score :",min=1,max=10,value = c(1,10),step=0.1),
                                sliderInput("VM","Value for Money :",min=1,max=10,value = c(1,10),step=0.1)
                                              ),
                                mainPanel(leafletOutput("mymap",width="100%",height=800),
                                          fluidRow(verbatimTextOutput("map_marker_click"))

                                          )
                                 ),
                   ),
          tabPanel("Directory",dataTableOutput("table")),
          tabPanel("Insights"
          , sidebarLayout(sidebarPanel(selectInput("Plot_Select:","Select Plot",c("Starting Price"
                                                                                  ,"Distance to City Centre"
                                                                                  ,"Rating Score"
                                                                                  ,"Cleanliness"
                                                                                  ,"Atmosphere"
                                                                                  ,"Facilities"
                                                                                  ,"Location"
                                                                                  ,"Security"
                                                                                  ,"Staff"
                                                                                  ,"Starting Price"
                                                                                  )
                                                   , selectize = F,size=11
                                                   )
                                       )
                          ,mainPanel(plotOutput("plot",width="100%",height=800))))
                  ))))))