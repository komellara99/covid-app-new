library(xts)
library(shinydashboard)
library(shinyWidgets)
library(shiny)
library(ggplot2)
library(ggthemes)
library(shinycssloaders)
library(tidyverse)
library(lubridate)
library(gifski)
library(scales)
library(leaflet)
library(countrycode)
library(rgdal)
library(zoo)
library(plotly)
library(forecast)
library(summaryBox)
library(remotes)
library(bslib)
library(bsplus)

source("functions.R")
theme2 <- bslib::bs_theme(version = 4)
ui <- 
  navbarPage(
    
    icon("virus-covid"),
    header = tagList(
      useShinydashboard()
    ),
    
    id="selector",
    #homepage----
    tabPanel("Homepage",
             withSpinner(
               fluidPage( 
                 conditionalPanel(
                   condition = "!output.setupComplete",
                   style = "display: flex; justify-content: center; align-items: center;",
                   h1("Data is downloading... Please wait.")
                 ),
                 conditionalPanel(
                   condition = "output.setupComplete",
                   fluidRow(
                     valueBoxOutput("valuebox_cases"),
                     valueBoxOutput("valuebox_deaths"),
                     valueBoxOutput("valuebox_vacc"),
                   ),
                   br(),
                   fluidRow(
                     column(
                       width=4,class = "leaderboard",
                       box(width=12,
                           
                           #fluidRow("Leaderboard", width=12, class="lead"),
                           fluidRow(
                             infoBoxOutput("leader_1", width = 12)),
                           fluidRow(
                             infoBoxOutput("leader_2", width=12)),
                           fluidRow(
                             
                             infoBoxOutput("leader_3", width = 12)),
                           fluidRow(
                             
                             infoBoxOutput("leader_4", width = 12)),
                       )),
                     column(
                       width = 8, 
                       class = "plotout",
                       fluidRow(
                         tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; border-top: none;-moz-box-shadow: none;box-shadow: none;}'))),
                         box(
                           width = 12, 
                           class="mainplot",
                           div(class="firstplot", plotlyOutput("home_plot2")
                               #%>% withSpinner(color="#7DE2D1", type = 7)
                           )
                         )
                       )
                     )
                     
                   ),
                   br(),
                   
                   fluidRow(
                     column(
                       width = 12,
                       box(width=12, class="homeplot_d", plotOutput("home_hist"))
                     )
                   )
                 )),color = "#7DE2D1", type = 7)),
    #data by country----
    navbarMenu("Country data",
               tabPanel("Data by country",
                        fluidPage( 
                          conditionalPanel(
                            condition = "!output.setupComplete",
                            style = "display: flex; justify-content: center; align-items: center;",
                            h1("Data is downloading... Please wait.")
                          ),
                          conditionalPanel(
                            condition = "output.setupComplete",
                            
                            br(),
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                uiOutput("metric_country_card"), br(),
                                uiOutput("country_card"), br(),
                                uiOutput("daterange_card"),br(),
                                uiOutput("moving_av_country2"),
                                uiOutput("moving_av_days_country2")
                                
                                
                              ),
                              mainPanel(
                                width = 9,
                                div(class ="forecast_panel",plotlyOutput("single_country_plot")%>% withSpinner(color="#7DE2D1", type = 7)),
                                br(),
                                uiOutput("forecast_panel3")
                                
                                
                              )),
                            fluidRow(
                              
                              column(
                                width=12,
                                fluidRow( 
                                  uiOutput("countryboxes"),
                                  class="podatki"
                                  
                                )),
                              
                            ),
                            br(),
                            
                          ))),
               tabPanel("Compare Countries",
                        fluidPage(
                          conditionalPanel(
                            condition = "!output.setupComplete",
                            style = "display: flex; justify-content: center; align-items: center;",
                            h1("Data is downloading... Please wait.")
                          ),
                          conditionalPanel(
                            condition = "output.setupComplete",
                            br(),
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("metric_country"), br(),
                                uiOutput("country_country"), br(),
                                uiOutput("daterange_country"),br(),
                                uiOutput("moving_av_country"),br(),
                                uiOutput("moving_av_days_country"),br(),
                              ),
                              mainPanel(
                                div(class ="forecast_panel",plotlyOutput("country_plot")%>% withSpinner(color="#7DE2D1", type = 7)),
                                br(),
                                uiOutput("forecast_panel")
                              ))
                            
                            
                          )))
    ),
    navbarMenu("Continent data",
               tabPanel("Data by continent",
                        fluidPage( 
                          conditionalPanel(
                            condition = "!output.setupComplete",
                            style = "display: flex; justify-content: center; align-items: center;",
                            h1("Data is downloading... Please wait.")
                          ),
                          conditionalPanel(
                            condition = "output.setupComplete",
                            
                            br(),
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                uiOutput("metric_continent_card"), br(),
                                uiOutput("continent_card"), br(),
                                uiOutput("daterange_card_continent"),br(),
                                uiOutput("moving_av_country3"),
                                uiOutput("moving_av_days_country3")
                                
                              ),
                              mainPanel(
                                width = 9,
                                div(class ="forecast_panel",plotlyOutput("single_continent_plot")%>% withSpinner(color="#7DE2D1", type = 7)),
                                br(),
                                uiOutput("forecast_panel4")
                                
                              )),
                            fluidRow(
                              
                              column(
                                width=12,
                                fluidRow( 
                                  uiOutput("continentboxes"),
                                  class="podatki"
                                  
                                )),
                              
                            ),
                            br(),
                            
                          ))),
               tabPanel("Compare Continents",
                        fluidPage(
                          conditionalPanel(
                            condition = "!output.setupComplete",
                            h1("Data Downloading... Please wait.")
                          ),
                          conditionalPanel(
                            condition = "output.setupComplete",
                            br(),
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("metric_continent"), br(),
                                uiOutput("country_continent"), br(),
                                uiOutput("daterange_continent"),br(),
                                uiOutput("moving_av_continent"),br(),
                                uiOutput("moving_av_days_continent"),br(),
                              ),
                              mainPanel(
                                div(class ="forecast_panel",plotlyOutput("continent_plot")%>% withSpinner(color="#7DE2D1", type = 7)),
                                br(),
                                uiOutput("forecast_panel2")
                              ))
                            
                          )))
    ),
    #map----
    tabPanel("Map",
             fluidPage( 
               conditionalPanel(
                 condition = "!output.setupComplete",
                 style = "display: flex; justify-content: center; align-items: center;",
                 h1("Data is downloading... Please wait.")
               ),
               conditionalPanel(
                 condition = "output.setupComplete",
                 fluidRow(
                   class="input-row",
                   #checkboxInput(inputId = "cgf", label = "Metrics per million")),
                   column( 
                     width=4,
                     uiOutput("map_metrics")),
                   column( 
                     width=4,
                     uiOutput("map_date")
                   ),
                   column( 
                     width=4,
                     uiOutput("checkbox_map"))
                 ),
                 br(),
                 #fluidRow(class="plot4",plotOutput("plot4"))
                 fluidRow(class = "map1",leafletOutput("world_map")%>% withSpinner(color="#7DE2D1", type = 7))  
               ))),
    
    
    tabPanel("Data",
             conditionalPanel(
               condition = "!output.setupComplete",
               style = "display: flex; justify-content: center; align-items: center;",
               h1("Data is downloading... Please wait.")
             ),
             conditionalPanel(
               condition = "output.setupComplete",
               uiOutput("downloadButton"),
               br(),
               fluidRow(
                 column( width=12, class = "data",
                         box(title = "Data",
                             
                             status = "primary",
                             solidHeader = F,
                             collapsible = T,
                             width = 12,
                             #column(12, align="center", tableOutput('top5')))
                             dataTableOutput ("countrytable")), align="center"
                 )))),
    tabPanel("About",
             h3("About the project"),
             p("This application was made as a part of final thesis for a Bachelor's degree in Computer Science."),
             p("The web application is built using R and RShiny package, and it interactively displays data of the entire Covid-19 pandemic."),
             br(),
             p("Author: Lara Komel"), 
             p('Mentor: Assistent professor prof. Dr. Uroš Godnov'),br(),
             p("School: University of Primorska, The Faculty of Mathematics, Natural Sciences and Information Technologies (UP FAMNIT)"),br(),
             p("April 2023"),
             tags$img(src = "https://www.famnit.upr.si/sl/resources/images/studenti/alumni-famnit/logofamnit-02.png", width = "400px")
             
             
    ),
    #css----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
  )