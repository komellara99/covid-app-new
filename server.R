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

server <- function(input, output) {
  rv <- reactiveValues()
  rv$setupComplete <- FALSE
  initial_data <- NULL
  
  
  tryCatch({
    initial_data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
    observe({ 
      rv$setupComplete <- TRUE
    })   
  }, error = function(e) {
    message("Error while downloading the data")
    message(e$message)
  })
  output$setupComplete <- reactive({
    return(rv$setupComplete)
  })
  outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
  
  
  basic_data <- initial_data[, c(2:6,8,9,11,12,14,15,37,39,41,42)] #extract basics
  basic_data2 <- initial_data[, c(2:6,8,9,11,12,14,15,37,39,41,42, 63, 34, 50)]
  non_countries <- c("Asia", "Europe", "North America", "South America", "Oceania", "Africa", "World", "High income", "European Union", "Low income","Upper middle income","Lower middle income")
  `%!in%` = Negate(`%in%`)
  download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")
  unzip("world_shape_file.zip")
  # dat <- iso3166[,c("a3", "ISOname")] 
  # dat2 <- dat
  # dat <- rename(dat, "location" = ISOname)
  # dat <- rename(dat, "ISO3" = a3)
  # dat2 <- rename(dat2, "country" = ISOname)
  # dat2 <- rename(dat2, "ISO3" = a3)
  
  drzave <- unique(  basic_data[, 2])
  
  non_countries2 <- c("Asia", "Europe", "North America", "South America", "Oceania", "Africa", "World", "High income", "European Union", "Low income","Upper middle income","Lower middle income",
                      "England", "International", "Kosovo","Micronesia (country)", "Northern Ireland", "Scotland", "Timor", "Wales")
  drzave <- drzave%>%filter(location %!in% non_countries2)
  drzave$iso <- countrycode(drzave$location, "country.name","iso3c")
  
  continents <- c("Asia", "Europe", "North America", "South America", "Oceania", "Africa")
  dat <- drzave
  dat2 <- drzave
  dat <- rename(dat, "ISO3" = iso)
  dat2 <- rename(dat2, "country" = location)
  dat2 <- rename(dat2, "ISO3" = iso)
  
  #group by day and continent, new cases and deaths only
  continent_day <-basic_data %>% group_by(continent,date)%>% summarise(
    new_cases = sum(new_cases),
    new_deaths = sum(new_deaths)
  )
  #basic data for every day and country
  #countryday_data <- initial_data[, c(3,4,6,9,12,15)]
  countryday_data <- initial_data[, c(3,4,6,9,12,15)]
  
  metric_choices <- colnames(basic_data)[c(5,7,9,11,13)]
  metric_names <- gsub("_", " ", metric_choices)
  metric_names <- paste0(toupper(substr(metric_names,1,1)), substr(metric_names, 2, nchar(metric_names)))
  
  metric_list <- as.list(metric_choices)
  names(metric_list) <- metric_names
  
  today <- Sys.Date()%>%format("%Y-%m-%d")

  #data clean-up ----
  #homepage plot data----
  data_home_plot0 <- reactive({x1<-basic_data %>%
    filter(location %!in% non_countries) %>% 
    select(location, date, new_cases )%>%
    setNames(c("location","date", "cases"))})
 
  data_home_plot <- reactive({x2<- data_home_plot_cleanup(data_home_plot0()) %>% drop_na(cases)%>%
    group_by(Month_Yr) %>% 
    summarise(num = n(),
              total_c = sum(cases)) })

  output$home_hist <- renderPlot({
    #homepage_plot2()
    homepage_hist(basic_data, non_countries)
  })
  
  output$home_plot2 <- renderPlotly({
    #homepage_plot2()
    homepage_plot_merge(data_home_plot_vacc(), data_home_plot_deaths(), data_home_plot())
  })
  output$home_plot <- renderImage({
    homepage_plot(data_home_plot())
    filename <- "animated_plot.gif"
    list(src = filename,
         contentType = "image/gif",
         width = "80%",
         height = "100%"
    )
  }, deleteFile = FALSE)
  
  
  #homepage small plots----
  data_home_plot0_deaths <- reactive({x1<-basic_data %>%
    filter(location %!in% non_countries) %>% 
    select(location, date, new_deaths )%>%
    setNames(c("location","date", "deaths"))})

  data_home_plot_deaths <- reactive({x2<- data_home_small_plot_cleanup(data_home_plot0_deaths()) %>% drop_na(deaths)%>%
    group_by(Month_Yr) %>% 
    summarise(num = n(),
              total_d = sum(deaths)) })
  
  output$home_plot2_deaths <- renderPlot({
    homepage_deaths_plot(data_home_plot_deaths())
  })
  ####
  data_home_plot0_vaccine <- reactive({x1<-basic_data %>%
    filter(location %!in% non_countries) %>% 
    select(location, date, new_vaccinations )%>%
    setNames(c("location","date", "vaccines"))})
  
  data_home_plot_vacc <- reactive({x2<- data_home_small_plot_cleanup(data_home_plot0_vaccine()) %>% drop_na(vaccines)%>%
    group_by(Month_Yr) %>% 
    summarise(num = n(),
              total_v = sum(vaccines)) })
  
  output$home_plot2_vacc <- renderPlot({
    homepage_vaccine_plot(data_home_plot_vacc())
  })
  
  #homepage boxes----

  output$valuebox_cases <- renderValueBox({
    valueBox(
      format(home_boxes_data(basic_data, non_countries)[1], big.mark=","), "Total cases worldwide ", icon = icon("virus-covid"),
      color = "red"
    )
  })
  output$valuebox_deaths <- renderValueBox({
    valueBox(
      format(home_boxes_data(basic_data, non_countries)[2], big.mark=","), "Total deaths worldwide ", icon = icon("virus-covid"),
      color = "green"
    )
  })
  output$valuebox_vacc <- renderValueBox({
    valueBox(
      format(home_boxes_data(basic_data, non_countries)[3], big.mark=","), "Total vaccinations worldwide ", icon = icon("virus-covid"),
      color = "light-blue"
    )
  })
  
  #leaderboard----
  

  output$leader_1 <- renderInfoBox({
    infoBox(
      paste0(" ", (leaderboard_data(basic_data, non_countries)[1,])[1]), paste0("Total Cases: ", format(as.numeric((leaderboard_data(basic_data, non_countries)[1,])[3]), big.mark=",")), paste0("Total Deaths: ", format(as.numeric((leaderboard_data(basic_data, non_countries)[1,])[4]), big.mark=",")), icon = icon("1"),
      color = "light-blue",width = 12
    )
  })
  output$leader_2 <- renderInfoBox({
    infoBox(
      paste0(" ", (leaderboard_data(basic_data, non_countries)[2,])[1]), paste0("Total Cases: ", format(as.numeric((leaderboard_data(basic_data, non_countries)[2,])[3]), big.mark=",")), paste0("Total Deaths: ", format(as.numeric((leaderboard_data(basic_data, non_countries)[2,])[4]), big.mark=",")), icon = icon("2"),
      color = "light-blue",width = 12
    )
  })
  output$leader_3 <- renderInfoBox({
    infoBox(
      paste0(" ", (leaderboard_data(basic_data, non_countries)[3,])[1]), paste0("Total Cases: ", format(as.numeric((leaderboard_data(basic_data, non_countries)[3,])[3]), big.mark=",")), paste0("Total Deaths: ", format(as.numeric((leaderboard_data(basic_data, non_countries)[3,])[4]), big.mark=",")), icon = icon("3"),
      color = "light-blue",width = 12
    )
  })
  output$leader_4 <- renderInfoBox({
    infoBox(
      paste0(" ", (leaderboard_data(basic_data, non_countries)[4,])[1]), paste0("Total Cases: ", format(as.numeric((leaderboard_data(basic_data, non_countries)[4,])[3]), big.mark=",")), paste0("Total Deaths: ", format(as.numeric((leaderboard_data(basic_data, non_countries)[4,])[4]), big.mark=",")), icon = icon("4"),
      color = "light-blue",width = 12
    )
  }) 
  #data table ----
  clean_data_country <- reactive({cases_new <- basic_data %>% 
    filter(location %!in% non_countries) %>% 
    select(location, date, new_cases, new_deaths )%>%
    set_names(c("location", "date", "cases", "deaths"))%>%
    arrange(date)})
  clean_data_country_t <- reactive({cases_new <- basic_data %>% 
    filter(location %!in% non_countries) %>% 
    select(location, date, new_cases, new_deaths )%>%
    set_names(c("location", "date", "cases", "deaths"))%>%
    arrange(date)})
  
  output$countrytable <- renderDataTable(clean_data_country_t(), options = list(pageLength = 20))
  
  
  #map inputs----
  output$map_metrics <- renderUI({
    selectInput(
      inputId = "map_metric_i",
      label = ("Select metric"),
      choices = metric_list,
      selected = metric_list[1]
      
    )
  })
  output$map_date <- renderUI({
    dateInput(
      inputId = "map_date_i",
      label = "Select date",
      value ="2021-12-31"
    )
  })
  output$map_dates <- renderUI({
    dateRangeInput(
      inputId = "map_dates_i",
      label = "Select date range",
      start = "2020-01-01",
      end = today
    )
    
  })
  output$checkbox_map <- renderUI({ 
    checkboxInput(
      inputId = "checkbox_map",
      label = div("Date range", style = "font-size: 10pt"),
      value = FALSE
    )
  })
  #worldmap----
  clean_data_map<- reactive({xxx <- basic_data %>% 
    filter(date == input$map_date_i)%>%
    select(location, date, input$map_metric_i )%>%
    setNames(c("location", "date", "metric"))})
  
  clean_data_map_range<- reactive({xxx <- basic_data %>% 
    filter(date >= input$map_dates_i[1])%>%filter(date <= input$map_dates_i[2])%>%
    select(location, date, input$map_metric_i )%>%
    setNames(c("location", "date", "metric"))%>%
    drop_na(metric)%>%
    group_by(location) %>%
    summarize(metric = round(mean(metric)), digits=0)
  })
  
  observeEvent(input$checkbox_map, {
    if (!input$checkbox_map){
      output$map_date <- renderUI({
        dateInput(
          inputId = "map_date_i",
          label = "Select date",
          value ="2021-12-31"
        )
      })}
    else {
      output$map_date <- renderUI({
        dateRangeInput(
          inputId = "map_dates_i",
          label = "Select date range",
          start = "2020-01-01",
          end = today
        )
      })
    }
  })
  
  output$world_map <- renderLeaflet({ 
    req(input$map_date_i)
    
    if (!input$checkbox_map){ 
      worldmap(clean_data_map(), input, dat)
    }else{
      req(input$map_dates_i)
      worldmap_range(clean_data_map_range(), dat)
    }
  })
  
  
  #data by country----
  output$country_country <- renderUI({ 
    selectInput(
      inputId = "country_country",
      multiple = TRUE,
      label = "Select one or multiple countries",
      choices = sort(unique((basic_data%>%filter(location %!in% non_countries2))$location)),
      selected = c("United States", "Italy", "Croatia")
    )})
  
  output$metric_country <- renderUI({
    radioButtons(
      inputId = "metric_country",
      label = "Select the metric",
      choices = metric_list)
  })
  output$daterange_country <- renderUI({
    dateRangeInput(inputId = "daterange_country",
                   label = "Select date range",
                   start = "2021-01-01",
                   end = today)
  })
  output$moving_av_country <- renderUI({ 
    checkboxInput(
      inputId = "moving_av_country",
      label = div("Moving avarage", style = "font-size: 10pt"),
      value = FALSE
    )
  })
  output$moving_av_days_country <- renderUI({
    div( 
      numericInput(
        inputId = "moving_av_days_country",
        label = "Number of days for moving avarage",
        value= 5,
        min= 0,
        step = 1
      ),
      actionButton(
        inputId = "moving_av_button_country",
        label = "Update",
        type = "default",
        class = "btn-sm"
      ))
  })
  output$moving_av_country2 <- renderUI({ 
    checkboxInput(
      inputId = "moving_av_country2",
      label = div("Moving avarage", style = "font-size: 10pt"),
      value = FALSE
    )
  })
  output$moving_av_days_country2 <- renderUI({
    div( 
      numericInput(
        inputId = "moving_av_days_country2",
        label = "Number of days for moving avarage",
        value= 5,
        min= 0,
        step = 1
      ),
      actionButton(
        inputId = "moving_av_button_country2",
        label = "Update",
        type = "default",
        class = "btn-sm"
      ))
  })
  output$moving_av_country3 <- renderUI({ 
    checkboxInput(
      inputId = "moving_av_country3",
      label = div("Moving avarage", style = "font-size: 10pt"),
      value = FALSE
    )
  })
  output$moving_av_days_country3 <- renderUI({
    div( 
      numericInput(
        inputId = "moving_av_days_country3",
        label = "Number of days for moving avarage",
        value= 5,
        min= 0,
        step = 1
      ),
      actionButton(
        inputId = "moving_av_button_country3",
        label = "Update",
        type = "default",
        class = "btn-sm"
      ))
  })
  output$moving_av_country4 <- renderUI({ 
    checkboxInput(
      inputId = "moving_av_country4",
      label = div("Moving avarage", style = "font-size: 10pt"),
      value = FALSE
    )
  })
  output$moving_av_days_country4 <- renderUI({
    div( 
      numericInput(
        inputId = "moving_av_days_country4",
        label = "Number of days for moving avarage",
        value= 5,
        min= 0,
        step = 1
      ),
      actionButton(
        inputId = "moving_av_button_country4",
        label = "Update",
        type = "default",
        class = "btn-sm"
      ))
  })
  output$forecast_panel <- renderUI({
    #class= "jumbotron",
    div( 
      #class = "container bg-secondary",
      fluidRow( 
        h4("Forecast"),
        
        uiOutput("forecast"),
        div(
          div(style="display: inline-block; width: 95px ;", uiOutput("forecast_button")),
          div(style="display: inline-block; width: 25px ;"),
          div(style="display: inline-block; width: 95px ;", uiOutput("remove_forecast_button"))))
    )
  })
  output$forecast_panel2 <- renderUI({
    #class= "jumbotron",
    div( 
      #class = "container bg-secondary",
      fluidRow( 
        h4("Forecast"),
        
        uiOutput("forecast2"),
        div(
          div(style="display: inline-block; width: 95px ;", uiOutput("forecast_button2")),
          div(style="display: inline-block; width: 25px ;"),
          div(style="display: inline-block; width: 95px ;", uiOutput("remove_forecast_button2"))))
    )
  })
  output$forecast <- renderUI({
    numericInput(
      inputId = "forecast",
      label = "Number of days to forecast",
      value = 20,
      step = 1,
      min = 0, 
      max = 100
      
    )
  })
  output$forecast_button <- renderUI({
    actionButton(inputId = "forecast_button",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Make a forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  output$remove_forecast_button <- renderUI({
    actionButton(inputId = "remove_forecast_button",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Remove forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  #country data card----
  output$countrybox <- renderUI({
    req(input$country_card)
    data <- func_card_info(basic_data2,input$country_card, input$daterange_card[1], input$daterange_card[2])
    data2 <- func_card_info2(basic_data2,input$country_card, input$daterange_card[1], input$daterange_card[2])
    box(
      class="hi",
      title = input$country_card, 
      background = "black",
      
      fluidRow(
        div(id="italy",
            valueBox(
              5846843, "Total cases ", icon = icon("virus-covid"),
              color = "yellow"
            )),
        div(id="italy",
            valueBox(
              5846, "Total deaths ", icon = icon("virus-covid"),
              color = "blue"
            )),
        div(id="italy",
            valueBox(
              5846843, "Total vaccinations ", icon = icon("virus-covid",),
              color = "navy"
            ))
      ),
      fluidRow(
        column(width=12,
               fluidRow( 
                 div(paste0( "Date range: ",input$daterange_card[1], " to ", input$daterange_card[2]), br(), br(),
                     column(width=6, paste0("Cases: ", data2[2]), br(), paste0("Deaths: ", data2[3]), class="stolpec" ),
                     column(width = 6, paste0("Vaccinations: ", data2[4]), br(), "Deaths: 534", class="stolpec"),
                     br(),
                     class="blabla2"),
               )),
      ),
      paste0( "Population: ", data[1]), br(), 
      paste0( "Mortality rate: ",data[5]), br(),
      paste0("First case: ", data2$date), br(),
      paste0("Population vaccinated: ", data[3]), br(),
      paste0( "People vaccinated: ",data[2]), br(),
      paste0("Avarage age: ", data[4]), br(),
      br(),
      br(),
      br(),
      width=12, class="countbox"
    )
  })
  
  #country data boxes top format(home_boxes_data()[1], big.mark=",")
  output$countryboxes <- renderUI({
    req(input$country_card)
    data2 <- func_card_info2(basic_data2,input$country_card, input$daterange_card[1], input$daterange_card[2])
    data <- leaderboard_data(basic_data, non_countries)%>%filter(location == input$country_card)
    
    
    fluidRow(
      div(id="italy",
          valueBox(
            paste0("Total cases: ", format(as.integer(data[3]), big.mark=",")), 
            paste0("In date-range: ", format(data2[2],big.mark=",")), 
            icon = icon("virus-covid"),
            color = "yellow"
          )),
      div(id="italy",
          valueBox(
            paste0("Total deaths: ", format(as.integer(data[4]), big.mark=",")), 
            paste0("In date-range: ", format(data2[3],big.mark=",")), 
            icon = icon("virus-covid"),
            color = "blue"
          )),
      div(id="italy",
          valueBox(
            paste0("Total vaccinations: ", format(as.integer(data[5]), big.mark=",")), paste0("In date-range: ", format(data2[4],big.mark=",")), icon = icon("virus-covid"),
            color = "navy"
          ))
    )
  })

  
  card_data <- reactive({data <- func_card_info(basic_data2,input$country_card, input$date1_card, input$date2_card)})
  output$country_card <- renderUI({ 
    selectInput(
      inputId = "country_card",
      multiple = FALSE,
      label = "Select one country",
      choices = sort(unique((basic_data%>%filter(location %!in% non_countries2))$location)),
      selected = "Italy"
    )})
  
  output$metric_country_card <- renderUI({
    radioButtons(
      inputId = "metric_country_card",
      label = "Select the metric",
      choices = metric_list)
  })
  output$daterange_card <- renderUI({
    dateRangeInput(inputId = "daterange_card",
                   label = "Select date range",
                   start = "2021-01-01",
                   end = today)
  })
  #plot country----
  clean_data_country_card <- reactive({cases_new <- basic_data %>% 
    filter(location == input$country_card) %>% 
    filter(date >= input$daterange_card[1])%>%filter(date <= input$daterange_card[2])%>%
    select(location, date,input$metric_country_card )%>%
    set_names(c("location", "date", "metric"))%>%
    arrange(date)}) 
  make_forecast_c <- reactiveValues(value=0)
  
  observeEvent(input$forecast_button3, {
    make_forecast_c$value <- 1
  })
  observeEvent(input$remove_forecast_button3, {
    make_forecast_c$value <- 0
  })
  make_forecast_c2 <- reactiveValues(value=0)
  
  observeEvent(input$forecast_button4, {
    make_forecast_c2$value <- 1
  })
  observeEvent(input$remove_forecast_button4, {
    make_forecast_c2$value <- 0
  })
  
  output$single_country_plot <- renderPlotly({
    req(input$country_card)
    #return(plot_by_country_card())
    ifelse(make_forecast_c$value==0, return (plot_by_country_card(clean_data_country_card(), input, ma_days3())), return(plot_by_country_card_forecast(clean_data_country_card(), input, ma_days3())))
    
  })
  
  #plot country compare----
  make_forecast <- reactiveValues(value=0)
  
  observeEvent(input$forecast_button, {
    make_forecast$value <- 1
  })
  observeEvent(input$remove_forecast_button, {
    make_forecast$value <- 0
  })
  
  
  output$country_plot <- renderPlotly({
    req(input$country_country)
    ifelse(make_forecast$value==0, return (plot_by_country(input, clean_data_country(), ma_days())), return(plot_data_country_forecast(clean_data_country(), input, ma_days())))
    
  })
  clean_data_country <- reactive({cases_new <- basic_data %>% 
    filter(location %in% input$country_country) %>% 
    filter(date >= input$daterange_country[1])%>%filter(date <= input$daterange_country[2])%>%
    select(location, date,input$metric_country )%>%
    set_names(c("location", "date", "metric"))%>%
    arrange(date)})
  clean_data_country_c <- reactive({cases_new <- basic_data %>% 
    filter(location %in% input$country_card) %>% 
    filter(date >= input$daterange_card[1])%>%filter(date <= input$daterange_card[2])%>%
    select(location, date,input$metric_card )%>%
    set_names(c("location", "date", "metric"))%>%
    arrange(date)})
  
  
  ma_days <- eventReactive(input$moving_av_button_country,
                           {
                             req(input$moving_av_days_country)
                             input$moving_av_days_country
                           }, ignoreNULL = FALSE)
  ma_days3 <- eventReactive(input$moving_av_button_country2, #single country
                            {
                              req(input$moving_av_days_country2)
                              input$moving_av_days_country2
                            }, ignoreNULL = FALSE)
  ma_days4 <- eventReactive(input$moving_av_button_country3, #single country
                            {
                              req(input$moving_av_days_country3)
                              input$moving_av_days_country3
                            }, ignoreNULL = FALSE)
  


  #data by continent----
  output$country_continent <- renderUI({ 
    selectInput(
      inputId = "country_continent",
      multiple = TRUE,
      label = "Select one or multiple continent",
      choices = continents,
      selected = c("North America", "Asia")
    )})
  
  output$metric_continent <- renderUI({
    radioButtons(
      inputId = "metric_continent",
      label = "Select the metric",
      choices = metric_list)
  })
  output$daterange_continent <- renderUI({
    dateRangeInput(inputId = "daterange_continent",
                   label = "Select date range",
                   start = "2021-01-01",
                   end = today)
  })
  output$moving_av_continent <- renderUI({ 
    checkboxInput(
      inputId = "moving_av_continent",
      label = div("Moving avarage", style = "font-size: 10pt"),
      value = FALSE
    )
  })
  output$moving_av_days_continent <- renderUI({
    div( 
      numericInput(
        inputId = "moving_av_days_continent",
        label = "Number of days for moving avarage",
        value= 5,
        min= 0,
        step = 1
      ),
      actionButton(
        inputId = "moving_av_button_continent",
        label = "Update",
        type = "default",
        class = "btn-sm"
      ))
  })
  output$forecast_panel3 <- renderUI({
    #class= "jumbotron",
    div( 
      #class = "container bg-secondary",
      fluidRow( 
        column(
          width = 4,
          h4("Forecast"),
          
          uiOutput("forecast3")),
        column(
          width = 4,
          div( class="forecast-bttn",
               div(style="display: inline-block; width: 95px ;", uiOutput("forecast_button3")),
               div(style="display: inline-block; width: 25px ;"),
               div(style="display: inline-block; width: 95px ;", uiOutput("remove_forecast_button3"))))
      ))
  })
  output$forecast_panel4 <- renderUI({
    #class= "jumbotron",
    div( 
      #class = "container bg-secondary",
      fluidRow( 
        column(
          width = 4,
          h4("Forecast"),
          
          uiOutput("forecast4")),
        column(
          width = 4,
          div( class="forecast-bttn",
               div(style="display: inline-block; width: 95px ;", uiOutput("forecast_button4")),
               div(style="display: inline-block; width: 25px ;"),
               div(style="display: inline-block; width: 95px ;", uiOutput("remove_forecast_button4"))))
      ))
  })
  output$forecast2 <- renderUI({
    numericInput(
      inputId = "forecast2",
      label = "Number of days to forecast",
      value = 20,
      step = 1,
      min = 0, 
      max = 100
      
    )
  })
  output$forecast3 <- renderUI({
    numericInput(
      inputId = "forecast3",
      label = "Number of days to forecast",
      value = 20,
      step = 1,
      min = 0, 
      max = 100
      
    )
  })
  output$forecast4 <- renderUI({
    numericInput(
      inputId = "forecast4",
      label = "Number of days to forecast",
      value = 20,
      step = 1,
      min = 0, 
      max = 100
      
    )
  })
  output$forecast_button2 <- renderUI({
    actionButton(inputId = "forecast_button2",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Make a forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  output$remove_forecast_button2 <- renderUI({
    actionButton(inputId = "remove_forecast_button2",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Remove forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  output$forecast_button3 <- renderUI({
    actionButton(inputId = "forecast_button3",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Make a forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  output$remove_forecast_button3 <- renderUI({
    actionButton(inputId = "remove_forecast_button3",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Remove forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  output$forecast_button4 <- renderUI({
    actionButton(inputId = "forecast_button4",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Make a forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  output$remove_forecast_button4 <- renderUI({
    actionButton(inputId = "remove_forecast_button4",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Remove forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  #continent data card----
  output$continentbox <- renderUI({
    req(input$continent_card)
    data <- func_card_info_c(basic_data2,input$continent_card, input$daterange_card_continent[1], input$daterange_card_continent[2])
    data2 <- func_card_info2_c(basic_data2,input$continent_card, input$daterange_card_continent[1], input$daterange_card_continent[2])
    box(
      class="hi",
      title = input$continent_card, 
      background = "black",
      
      fluidRow(
        div(id="italy",
            valueBox(
              5846843, "Total cases ", icon = icon("virus-covid"),
              color = "yellow"
            )),
        div(id="italy",
            valueBox(
              5846, "Total deaths ", icon = icon("virus-covid"),
              color = "blue"
            )),
        div(id="italy",
            valueBox(
              5846843, "Total vaccinations ", icon = icon("virus-covid",),
              color = "navy"
            ))
      ),
      fluidRow(
        column(width=12,
               fluidRow( 
                 div(paste0( "Date range: ",input$daterange_card_continent[1], " to ", input$daterange_card_continent[2]), br(), br(),
                     column(width=6, paste0("Cases: ", data2[2]), br(), paste0("Deaths: ", data2[3]), class="stolpec" ),
                     column(width = 6, paste0("Vaccinations: ", data2[4]), br(), "Deaths: 534", class="stolpec"),
                     br(),
                     class="blabla2"),
               )),
      ),
      paste0( "Population: ", data[1]), br(), 
      paste0( "Mortality rate: ",data[5]), br(),
      paste0("First case: ", data2$date), br(),
      paste0("Population vaccinated: ", data[3]), br(),
      paste0( "People vaccinated: ",data[2]), br(),
      paste0("Avarage age: ", data[4]), br(),
      br(),
      br(),
      br(),
      width=12, class="countbox"
    )
  })
  output$continentboxes <- renderUI({
    req(input$continent_card)
    data <-basic_data2 %>%filter(location == input$continent_card) %>% 
      select( location, new_cases, new_deaths, new_vaccinations )%>%
      setNames(c( "location","cases", "deaths", "vacc"))%>%group_by(location) %>% drop_na(c(cases, deaths, vacc))%>%
      summarise(num = n(),
                cases = sum(cases), deaths = sum(deaths), vacc = sum(vacc))
    #leaderboard <- leaderboard[order(leaderboard$cases, decreasing = TRUE),]
    data2 <-basic_data2 %>%filter(location == input$continent_card) %>% filter(date >= as.Date(input$daterange_card_continent[1]))%>%filter(date <= as.Date(input$daterange_card_continent[2]))%>%
      select( location, new_cases, new_deaths, new_vaccinations )%>%
      setNames(c( "location","cases", "deaths", "vacc"))%>%group_by(location) %>% drop_na(c(cases, deaths, vacc))%>%
      summarise(num = n(),
                cases = sum(cases), deaths = sum(deaths), vacc = sum(vacc))
    data23 <- func_card_info2_c(basic_data2,input$continent_card, input$daterange_card_continent[1], input$daterange_card_continent[2])
    #data <- data%>%filter(location == input$continent_card)
    
    
    fluidRow(
      div(id="italy",
          valueBox(
            paste0("Total cases: ", format((as.integer(data[3])), big.mark=",")), paste0("In date-range: ", format((as.integer(data2[3])), big.mark=",")), icon = icon("virus-covid"),
            color = "yellow"
          )),
      div(id="italy",
          valueBox(
            paste0("Total deaths: ", format((as.integer(data[4])), big.mark=",")), paste0("In date-range: ", format((as.integer(data2[4])), big.mark=",")), icon = icon("virus-covid"),
            color = "blue"
          )),
      div(id="italy",
          valueBox(
            paste0("Total vaccinations: ", format((as.integer(data[5])), big.mark=",")), paste0("In date-range: ", format((as.integer(data2[5])), big.mark=",")), icon = icon("virus-covid"),
            color = "navy"
          ))
    )
  })
  
 
  card_data_continent <- reactive({data <- func_card_info_c(basic_data2,input$continent_card, input$date1_card, input$date2_card)})
  output$continent_card <- renderUI({ 
    selectInput(
      inputId = "continent_card",
      multiple = FALSE,
      label = "Select one continent",
      choices = continents,
      selected = "Asia"
    )})
  
  output$metric_continent_card <- renderUI({
    radioButtons(
      inputId = "metric_continent_card",
      label = "Select the metric",
      choices = metric_list)
  })
  output$daterange_card_continent <- renderUI({
    dateRangeInput(inputId = "daterange_card_continent",
                   label = "Select date range",
                   start = "2021-01-01",
                   end = today)
  })
  #plot continent----
  clean_data_continent_card <- reactive({cases_new <- basic_data %>% 
    filter(location == input$continent_card) %>% 
    filter(date >= input$daterange_card_continent[1])%>%filter(date <= input$daterange_card_continent[2])%>%
    select(location, date,input$metric_continent_card )%>%
    set_names(c("location", "date", "metric"))%>%
    arrange(date)}) 
  

 
  output$single_continent_plot <- renderPlotly({
    req(input$continent_card)
    ifelse(make_forecast_c2$value==0, return (plot_by_continent_card(clean_data_continent_card(),input, ma_days4())), return(plot_by_continent_card_forecast(clean_data_continent_card(),clean_data_country_card(),input, ma_days4())))
    #return(plot_by_continent_card())
    
  })
  
  #plot continent compare----
  make_forecast2 <- reactiveValues(value=0)
  
  observeEvent(input$forecast_button2, {
    make_forecast2$value <- 1
  })
  observeEvent(input$remove_forecast_button2, {
    make_forecast2$value <- 0
  })
  
  
  output$continent_plot <- renderPlotly({
    req(input$country_continent)
    ifelse(make_forecast2$value==0, return (plot_by_continent(clean_data_continent(), input, ma_days2())), return(plot_data_continent_forecast(clean_data_continent(), input, ma_days2())))
    
  })
  clean_data_continent <- reactive({cases_new <- basic_data %>% 
    filter(location %in% input$country_continent) %>% 
    filter(date >= input$daterange_continent[1])%>%filter(date <= input$daterange_continent[2])%>%
    select(location, date,input$metric_continent )%>%
    set_names(c("location", "date", "metric"))%>%
    arrange(date)})

  ma_days2 <- eventReactive(input$moving_av_button_continent,
                            {
                              req(input$moving_av_days_continent)
                              input$moving_av_days_continent
                            }, ignoreNULL = FALSE)
  
  
  #data buttons----
  output$download_csv2 <- renderUI({
    fluidRow( class="btns-dwnl",
              column( width=2,
                      downloadButton(
                        "download_csv",
                        inputId = "remove_forecast_buttonx",
                        icon = icon("glyphicon glyphicon-download", lib = "glyphicon"),
                        
                        label = "Download as CSV",
                        class = "btn3"
                      )),
    )
    
  })
  output$downloadButton<- renderUI({
    
    downloadButton(
      "download_csv",
      label = "Download as CSV",
      class = "btn3"
      
    )
    
  })
  output$download_csv <- downloadHandler(
    filename = function() {
      "data.csv"
    },
    content = function(file) {
      write.csv(clean_data_country_t(), file, row.names = FALSE)
    }
  )
  #country data boxes----
  output$country_boxes <- renderUI({
    fluidRow( class="country_box_row",
              column(width=4, box(class="countbpx","TOTAL CASES",br(), "328765376", icon = icon("virus-covid")) ),
              column( width=4,box(class="countbpx",title = "drzava", "328765376")
              ),
              column( width=4,box(class="countbpx",title = "drzava", "328765376")
              ))
  })
  output$play <- renderUI({
    fluidRow(
      summaryBox2("Italy total cases", "402,450", width = 3, icon = "fa-solid fa-virus", style = "primary"),
      summaryBox2("Italy total deaths", "29,332", width = 3, icon = "fa-solid fa-viruses", style = "success"),
      summaryBox2("Italy total vaccinations", "346,283", width = 3, icon = "fa-solid fa-syringe", style = "danger"),
      summaryBox2("Italy mortality rate", "1.46", width = 3, icon = "fa-solid fa-percent", style = "primary")
    )#spodaj- v date rangu, people vaccinated, poglej og dataframe
  })
  
}