library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)

ui <- shinyUI(fluidPage(
              wellPanel(tags$style(".well {background-color:#EBFBFF;}"),
                        fluidRow(column(width = 12,titlePanel("Japan Hostel"),
      tabsetPanel(
          tabPanel("About",
                   h3("Introduction"),
                   p("Still thinking about where to stay when you are visiting Japan?"),
                   p("Why not consider hostel? This app helps you finding your ideal place to stay"),
                   br(),
                   h3("Why this app?"),
                   p("1. You can make choice based on a handful of criteria"),
                   p("2. You can visualise your selection on an interactive map which can help you plan your perfect itinerary"),
                   p("3. We provide you an interactive table with more attributes to assist your decision making"),
                   br(),
                   h3("Who are we?"),
                   p("- Kumanan A/L N Govaichelvan (17096921)"),
                   p("- Law Chuan Liang (S2003493)"),
                   p("- Teh Lee Leng (S2012133)"),
                   p("- William Heng Chun Meng (S2005592)"),
                   ),
          tabPanel("Find Your Hostel",
                   sidebarLayout(sidebarPanel(
                                selectInput("city","City : ", c("All",unique(dat_hostel$City))),
                                sliderInput("SP","Starting Price :",min=0,max=8000,value = c(0,8000),step=1000),
                                sliderInput("RS","Rating Score :",min=1,max=10,value = c(1,10),step=0.1),
                                sliderInput("VM","Value for Money :",min=1,max=10,value = c(1,10),step=0.1)
                                              ),
                                mainPanel(leafletOutput("mymap",width="100%",height=600),
                                          fluidRow(verbatimTextOutput("map_marker_click"))

                                          )
                                 ),
                   ),
          tabPanel("Directory",dataTableOutput("table")),
          tabPanel("Plot",plotOutput("plot"))
                  ))))))