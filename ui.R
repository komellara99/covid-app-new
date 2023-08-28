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
    
    #tags$head(
    #tags$link(rel = "stylesheet", type = "text/css", href = "appboxs.css")
    #),
    #css----
    
    tags$style(HTML(
      
      "body { background-color: #F3F3F3; }",
      ".firstplot {background-color: #ffffff; padding:20px;}",
      "#home_hist {margin-bottom: 70px; margin-top: -20px;}",
      ".forecast-bttn {padding-top: 65px;}",
      ".box {background-color: #F3F3F3;}",
      ".info-box-text {font-size: 17px}",
      ".forecast_panel {margin-left:80px, padding-left:80px}",
      ".well {background-color: #ffffff;}",
      ".item.active {color:#339989; background-color:#339989 !important;}",
      #".paginate-button.active > li > a {color:#339989; background-color:#339989 !important;}",
      "#moving_av_button_country {color:#FEFCFD; background-color:#1e5c52!important;}",
      ".btn.btn-default.action-button.btn.btn-sm.btn-primary.shiny-bound-input {color:#FEFCFD; background-color:#2B2C28!important;}",
      "#forecast_button > button {color:#FEFCFD; background-color:#1e5c52!important;}",
      "#forecast_button4 > button {color:#FEFCFD; background-color:#1e5c52!important;}",
      "#remove_forecast_button4 > button {color:#FEFCFD; background-color:#1e5c52!important;}",
      "#forecast_button3 > button {color:#FEFCFD; background-color:#1e5c52!important;}",
      "#remove_forecast_button3 > button {color:#FEFCFD; background-color:#1e5c52!important;}",
      "#world_map {height: calc(100vh - 160px) !important;}",
      #"body { background-color: #2B2C28; }",
      ".input-row { background-color: #FEFCFD; padding:3px; border-radius: 5px; }",
      ".lead {padding:5px; font-family: Arial; font-size: 18px; color:#FFFAFB}",
      ".info-box {border-radius: 5px; }",
      ".info-box-icon.bg-light-blue{border-radius: 5px 0 0 5px ;}",
      ".shiny-html-output col-sm-4 shiny-bound-output{width:100%;border-radius: 5px; }",
      #".box-body {background-color: #2B2C28 !important;}",
      #".leaderboard { border: solid 3px #339989;  }",
      ".data { background-color: #FFFAFB; }",
      ".small-box.bg-red { background-color: #1e5c52 !important; border-radius: 8px; }",
      ".small-box.bg-green { background-color: #339989 !important; border-radius: 8px; }",
      ".small-box.bg-light-blue { background-color: #7DE2D1 !important; border-radius: 8px; }",
      ".info-box-icon.bg-light-blue { background-color: #339989 !important;}",
      ".navbar { background-color: #FFFAFB;
      
                           font-family: Copperplate;
                           font-size: 18px;
                           color: #2B2C28;
                           }",
      
      ".plot4 {width: 800px; margin:auto;}",
      ".paginate_button .active >a {background-color:black;}",
      ".dropdown-menu > .active > a {background-color:#7DE2D1 !important;}",
      ".dropdown-menu > .active > a:hover {background-color:#7DE2D1;}",
      ".navbar-default .navbar-nav > li > a {color:#2B2C28;}",
      ".navbar-default .navbar-nav > li > a:hover {color:#7DE2D1; }",
      ".navbar-default .navbar-nav > .active > a:hover {color:#7DE2D1;}",    
      ".navbar-default .navbar-nav > .active > a {color:#339989; background-color: #FFFAFB;}",
      ".navbar-default .navbar-nav > .active > a:focus {color:#339989; background-color: #FFFAFB;}",
      ".navbar-header {text-align: right;}",
      ".active > a:hover {color:red; background-color:red}",
      ".navbar-default .navbar-brand {color: #339989; font-family: Copperplate; font-size: 35px; text-align: center; padding: 8px; margin-left: 20px;}",
      ".homeplot_d { border-radius: 8px;  }",
      ".mainplot {border-radius: 8px; }",
      ".btn3 {background-color: #1e5c52 !important; width:100%; color:white; border-radius: 12px;}",
      ".countbpx {background-color: #1e5c52 !important;  color:white; border-radius: 12px; font-size: 20px}",
      ".box11 {border: 3px solid #1e5c52 !important; border-radius: 12px; display: flex; justify-content: center; align-items: center;}",
      ".countryinput {background-color: white; }",
      ".hi {background-color: white!important; border-radius: 0 0 8px 8px;  height: 400px !important;}",
      ".box.box-solid.bg-black {background-color: #339989 !important;border-radius: 8px; }",
      ".blabla {background-color: #339989 !important; margin:10px; padding:10px; height:40px;border-radius: 5px; justify-content: center; align-items: center; text-align:center; }",
      ".blabla2 {background-color: #339989 !important; margin:10px; padding:10px; height:70px;border-radius: 5px; justify-content: center; align-items: center; text-align:center; height:120px; color: white;}",
      ".stolpec {text-align:left; } ",
      
      
      
      ".podatki {margin:10px; padding:10px; height:70px;border-radius: 5px; justify-content: center; align-items: center; text-align:center;}",
      "#italy > .col-sm-4 > .small-box.bg-yellow > .inner > h3 {font-size: 18px;}",
      "#italy > .col-sm-4 > .small-box.bg-blue > .inner > h3 {font-size: 18px;}",
      "#italy > .col-sm-4 > .small-box.bg-navy > .inner > h3 {font-size: 18px;}",
      
      "#italy > .col-sm-4 > .small-box.bg-yellow > .icon-large > .fas.fa-virus-covid {font-size: 40px; color: #1e5c52 !important; opacity: 0.3;}",
      "#italy > .col-sm-4 > .small-box.bg-blue > .icon-large > .fas.fa-virus-covid {font-size: 40px; color: #339989 !important; opacity: 0.3;}",
      "#italy > .col-sm-4 > .small-box.bg-navy > .icon-large > .fas.fa-virus-covid {font-size: 40px; color: #7DE2D1 !important; opacity: 0.3;}",
      ".box-body.hi.countbox {color: #2B2C28; font-size: 16px; font-weight: bold;}",
      ".box-title {color: white; font-size: 20px; font-weight: bold;}",
      ".small-box.bg-yellow { height: 72px; color: #1e5c52 !important; border-radius: 8px; background-color: #ffffff !important; font-weight: bold !important;}",
      ".small-box.bg-blue {height: 72px; color: #339989 !important; border-radius: 8px; background-color: #ffffff !important; font-weight: bold !important;}",
      ".small-box.bg-navy { height: 72px; color: #7DE2D1 !important; border-radius: 8px; background-color: #ffffff !important; font-weight: bold !important;}",
      ".legend {margin: 50px;}"
      
      
      
      
      
    )),
    
  )