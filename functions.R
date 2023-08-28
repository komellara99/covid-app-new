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

name_fix <- function(x){
  s1 <- gsub("_", " ", x)
  s2 <- paste0(toupper(substr(s1,1,1)), substr(s1, 2, nchar(s1)))
  return (s2)
}

data_home_plot_cleanup <- function(dat1){
  dat1$date <- as.Date( dat1$date)
  dat1$Month_Yr <- format(as.Date(dat1$date), "%Y-%m")
  dat1$location<- NULL
  dat1$date<- NULL
  
  return(dat1)
}

homeplot_fixdate <- function(dat2){
  dat2$neki <- as.Date(as.yearmon(dat2$Month_Yr))
  return(dat2)
}

homepage_plot <- function(data_home_plot){
  graph3 = homeplot_fixdate(data_home_plot) %>%
    ggplot(aes(x=neki, y=total_c)) +
    geom_line(size = 2, alpha = 0.75, col = "#339989")+
    labs(title = "Total world cases by month",
         x= NULL,
         y = NULL)+ 
    scale_y_continuous(labels = label_comma())+
    transition_reveal(neki)
  gif<- animate(graph3, renderer = gifski_renderer())
  #return(animate(graph3, renderer = gifski_renderer()))
  gif_file <- "animated_plot.gif"
  anim_save(gif_file, gif)
}

homepage_plot_merge <- function(data_home_plot_vacc, data_home_plot_deaths, data_home_plot){
  data3 <- homeplot_deaths_fixdate(data_home_plot_vacc)
  data2 <- homeplot_deaths_fixdate(data_home_plot_deaths)
  data1 <- homeplot_fixdate(data_home_plot)
  
  p <- plot_ly() %>%
    add_trace(x = ~data1$neki, y = ~data1$total_c, type = 'scatter', mode = 'lines', name = "World cases", line = list(color = '#131515', width = 2), yaxis = "y1") %>%
    add_trace(x = ~data2$neki, y = ~data2$total_d, type = 'scatter', mode = 'lines', name = "World deaths", line = list(color = '#339989', width = 2), yaxis = "y2") %>%
    add_trace(x = ~data3$neki, y = ~data3$total_v, type = 'scatter', mode = 'lines', name = "World vaccinations", line = list(color = '#2B2C28', width = 2), yaxis = "y1") %>%
    layout(
      #title = "Total world cases by month",
      xaxis = list(title = ""),
      yaxis = list(title = "Total cases and vaccinations"),
      yaxis2 = list(title = "Total deaths", overlaying = "y", side = "right"),
      legend = list(title = "Legend", orientation = "h",
                    xanchor = "center",  
                    x = 0.5),
      margin = list(l = 100, r = 100)
      #yaxis = list(tickformat = ",")
    )
  
  return(p)
}

homepage_hist <- function(basic_data, non_countries){
  data <- leaderboard_data(basic_data, non_countries)%>%head(30)
  #as.numeric((leaderboard_data()[1:20,])[3])
  options(scipen = 999)
  bar <- barplot(
    data$cases, 
    #names.arg = data$location, 
    col = "#339989", 
    border = "black",
    ylab = "Cases", 
    main = "Bar-chart by Country")
  
  modified_labels <- sapply(strsplit(data$location, " "), function(x) {
    if (length(x) > 1) {
      paste0(substr(x[1], 1, 1), ". ", paste(x[-1], collapse = " "))
    } else {
      x
    }
  })
  
  text(x = bar, 
       y = par("usr")[3] - 0.02 * (par("usr")[4] - par("usr")[3]),
       labels = modified_labels, 
       srt = 90, 
       adj = c(1, 0.5), 
       xpd = TRUE)
  
  return(bar)
}

homepage_plot_merge2 <- function(home_data_plot_vacc, home_data_plot_deaths, home_data_plot){
  data3 <- homeplot_deaths_fixdate(data_home_plot_vacc);
  data2 <- homeplot_deaths_fixdate(data_home_plot_deaths);
  
  
  
  homeplot_fixdate(data_home_plot) %>%
    ggplot() +
    geom_line(aes(x = neki, y = total_c), size = 2, alpha = 0.75, col = "#339989") +
    geom_line(data = data2, aes(x = neki, y = total_d), size = 2, alpha = 0.75, col = "#FF0000") +
    geom_line(data = data3, aes(x = neki, y = total_v), size = 2, alpha = 0.75, col = "#0000FF") +
    labs(title = "Total world cases by month",
         x= NULL,
         y = "Total cases")+ 
    scale_y_continuous(labels = label_comma())+
    scale_color_manual(values = c("#339989", "#FF0000", "#0000FF"),
                       labels = c("World cases", "World deaths", "World vaccinations")) +
    guides(color = guide_legend(title = "Legend"))
}
homepage_plot2 <- function(data_home_plot){
  homeplot_fixdate(data_home_plot) %>%
    ggplot(aes(x=neki, y=total_c)) +
    geom_line(size = 2, alpha = 0.75, col = "#339989")+
    labs(title = "Total world cases by month",
         x= NULL,
         y = "Total cases")+ 
    scale_y_continuous(labels = label_comma())
}

data_home_small_plot_cleanup <- function(dat1){ #zbrisi
  dat1$date <- as.Date( dat1$date)
  dat1$Month_Yr <- format(as.Date(dat1$date), "%Y-%m")
  dat1$location<- NULL
  dat1$date<- NULL
  
  return(dat1)
}

homeplot_deaths_fixdate <- function(dat2){
  dat2$neki <- as.Date(as.yearmon(dat2$Month_Yr))
  return(dat2)
}


homepage_deaths_plot <- function(data_home_plot_deaths){
  homeplot_deaths_fixdate(data_home_plot_deaths) %>%
    ggplot(aes(x=neki, y=total_d)) +
    geom_line(size = 2, alpha = 0.75, col = "#7DE2D1")+
    labs(title = "Total world deaths by month",
         x= NULL,
         y = "Total deaths")+ 
    scale_y_continuous(labels = label_comma())
}

homepage_vaccine_plot <- function(data_home_plot_vacc){
  homeplot_deaths_fixdate(data_home_plot_vacc) %>%
    ggplot(aes(x=neki, y=total_v)) +
    geom_line(size = 2, alpha = 0.75, col = "#7DE2D1")+
    labs(title = "Total world vaccines given by month",
         x= NULL,
         y = "Total vaccinations")+ 
    scale_y_continuous(labels = label_comma())
}

home_boxes_data <- function(basic_data, non_countries){
  total <-basic_data %>%filter(location %!in% non_countries) %>% 
    select( new_cases, new_deaths, new_vaccinations )%>%
    setNames(c( "cases", "deaths", "vaccinations"))
  
  total_cases <- sum((total%>%drop_na(cases))$cases)
  total_deaths <- sum((total%>%drop_na(deaths))$deaths)
  total_vacc <- sum((total%>%drop_na(vaccinations))$vaccinations)
  
  return(c(total_cases, total_deaths, total_vacc))
}

leaderboard_data <- function(basic_data, non_countries){
  leaderboard <-basic_data %>%filter(location %!in% non_countries) %>% 
    select( location, new_cases, new_deaths, new_vaccinations )%>%
    setNames(c( "location","cases", "deaths", "vacc"))%>%group_by(location) %>% drop_na(c(cases, deaths, vacc))%>%
    summarise(num = n(),
              cases = sum(cases), deaths = sum(deaths), vacc=sum(vacc))
  leaderboard <- leaderboard[order(leaderboard$cases, decreasing = TRUE),]
  return(leaderboard)
}

worldmap <- function(clean_data_map, input, dat){
  dataf = merge(x = clean_data_map, y = dat, by = "location")
  world_spdf = readOGR(dsn=getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")
  wrldx <- merge(world_spdf, dataf,  duplicateGeoms = T)
  #colnames(wrldx@data)
  wrldx$metric = as.numeric(as.character(wrldx$metric))
  
  bins = c(0, 0.5, 5, 15, 50, 100, 400, 1000, 2000, Inf)
  #"YlOrBr"
  pallmy = c("#a0ebdf","#75d1c3","#55b5a6","#339989","#237d6f","#16695c", "#0b423a", "#06332c","#011a16" )
  pal = colorBin(palette = pallmy, domain=wrldx$metric, na.color = "transparent", bins=bins)
  
  customLabel = paste("Country: ", wrldx$NAME, "<br/>", input$map_metric_i, ": ", wrldx$metric, sep = "") %>% 
    lapply(htmltools::HTML)
  
  wrldmap <- leaflet(wrldx) %>%
    addProviderTiles(providers$OpenStreetMap, options = tileOptions(minZoom=2, maxZoom=8)) %>%
    addPolygons(fillColor = ~pal(metric),
                fillOpacity = 0.9,
                stroke = TRUE,
                color="white",
                highlight=highlightOptions(
                  weight = 5,
                  fillOpacity = 0.3
                ),
                label = customLabel,
                weight = 0.3,
                smoothFactor = 0.2)%>%
    addLegend(
      pal=pal,
      values = ~metric,
      position = "bottomright",
      title ="Covid-19 cases"
    )
  
  
  return(wrldmap)
}

worldmap_range <- function(clean_data_map_range, dat){
  dataf = merge(x = clean_data_map_range, y = dat, by = "location")
  world_spdf = readOGR(dsn=getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")
  wrldx <- merge(world_spdf, dataf,  duplicateGeoms = T)
  #colnames(wrldx@data)
  wrldx$metric = as.numeric(as.character(wrldx$metric))
  
  bins = c(0, 0.5, 5, 15, 50, 100, 400, 1000, 2000, Inf)
  #"YlOrBr"
  pallmy = c("#a0ebdf","#75d1c3","#55b5a6","#339989","#237d6f","#16695c", "#0b423a", "#06332c","#011a16" )
  pal = colorBin(palette = pallmy, domain=wrldx$metric, na.color = "transparent", bins=bins)
  
  customLabel = paste("Country: ", wrldx$NAME, "<br/>", "Cases: ", wrldx$metric, sep = "") %>% 
    lapply(htmltools::HTML)
  
  wrldmap <- leaflet(wrldx) %>%
    addProviderTiles(providers$OpenStreetMap, options = tileOptions(minZoom=2, maxZoom=8)) %>%
    addPolygons(fillColor = ~pal(metric),
                fillOpacity = 0.9,
                stroke = TRUE,
                color="white",
                highlight=highlightOptions(
                  weight = 5,
                  fillOpacity = 0.3
                ),
                label = customLabel,
                weight = 0.3,
                smoothFactor = 0.2)%>%
    addLegend(
      pal=pal,
      values = ~metric,
      position = "bottomright",
      title ="Covid-19 cases"
    )
  
  
  return(wrldmap)
}

func_card_info <- function(basic_data2, country, date1, date2){
  count <-basic_data2 %>%filter(location == country) %>% 
    filter(date >= as.Date(date1))%>%filter(date <= as.Date(date2))
  
  pop <- count%>%filter(date==as.Date(date2))
  pop <- (pop[1,])[16]
  
  people_vacc <- count%>%filter(date==as.Date(date2))
  people_vacc <- (people_vacc[1,])[12]
  perc_vacc <- ((100*people_vacc)/pop)
  med_age <- count%>%filter(date==as.Date(date2))
  med_age <- (med_age[1,])[18]
  
  
  total_c <- count%>%filter(date==as.Date(date2))
  total_c <- (total_c[1,])[4]
  total_d <- count%>%filter(date==as.Date(date2))
  total_d <- ((total_d[1,])[6])  
  
  fatality <- ((total_d/total_c)*100)
  
  rez <- c(pop, people_vacc, perc_vacc, med_age , fatality)
  return(rez)
}

func_card_info2 <- function(basic_data2, country, date1, date2){
  count <- (basic_data2 %>%filter(location == country)%>%drop_na(new_cases)%>%filter(new_cases != 0))
  first_case <- (count[1,])[3]
  data <- basic_data2 %>%filter(location == country)%>%filter(date >= as.Date(date1))%>%filter(date <= as.Date(date2))
  
  cases <- data%>%drop_na(new_cases)%>%summarise(all_cases = sum(new_cases))
  deaths <- data%>%drop_na(new_deaths)%>%summarise(all_deaths = sum(new_deaths))
  vaccine <- data%>%drop_na(new_vaccinations)%>%summarise(all_vacc = sum(new_vaccinations))
  
  
  return(c(first_case, cases, deaths, vaccine))
}

plot_by_country_card <- function(clean_data_country_card, input, ma_days3){
  #dat_ma <- clean_data_country_card()
  ifelse(input$moving_av_country2 ==T & !is.null(input$moving_av_days_country2),
         dat_ma <- clean_data_country_card%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days3,mean,align='right',fill=NA)),
         dat_ma <- clean_data_country_card
  )
  
  
  plt <- plot_ly (data = dat_ma, x=~date, color=~location, text = ~location)
  plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines',
                           line = list(color = 'black'),
                           hovertemplate = paste(
                             paste0('<extra></extra>Country Name: %{text}\n','Metric: ',name_fix(input$metric_country_card),': %{y}\nDate: %{x} ')
                           ))
  if(input$moving_av_country2==T & !is.null(input$moving_av_days_country2)){ 
    plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot", color='#7DE2D1'), showlegend=F,
                             hovertemplate = paste(
                               paste0("<extra>Moving Average</extra>Country Name: %{text}\n",'Metric: ',name_fix(input$metric_country_card),': %{y}\nDate: %{x}')) )
  }
  highlight(plt)
  
  
}

plot_by_country_card_forecast <- function(clean_data_country_card, input, ma_days3){
  #dat_ma <- clean_data_country_card()
  ifelse(input$moving_av_country2 ==T & !is.null(input$moving_av_days_country2),
         dat_ma <- clean_data_country_card%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days3,mean,align='right',fill=NA)),
         dat_ma <- clean_data_country_card
  )
  ifelse(input$moving_av_country2 ==T & !is.null(input$moving_av_days_country2),
         forecastdata <- forecast_data_c(clean_data_country_card, input)%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days3,mean,align='right',fill=NA)),
         forecastdata <- forecast_data_c(clean_data_country_card, input)
  )
  
  forecasted_data2 <- rbind(forecastdata%>%filter(forecast==0)%>%filter(date==max(date)),
                            forecastdata%>%filter(forecast==1))
  
  plt <- plot_ly (data = forecastdata%>%filter(forecast==0), x=~date, color=~location, text = ~location)
  #svetla #7DE2D1
  #temna #339989
  
  #plt <- plot_ly (data = dat_ma, x=~date, color=~location, text = ~location)
  plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines',
                           line = list(color = 'black'),
                           hovertemplate = paste(
                             paste0('<extra></extra>Country Name: %{text}\n','Metric: ',name_fix(input$metric_country_card),': %{y}\nDate: %{x} ')
                           ))
  if(input$moving_av_country2==T & !is.null(input$moving_av_days_country2)){ 
    plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot", color="#7DE2D1"), showlegend=F,
                             hovertemplate = paste(
                               paste0("<extra>Moving Average</extra>Country Name: %{text}\n",'Metric: ',name_fix(input$metric_country_card),': %{y}\nDate: %{x}')) )
  }
  plt <- plt%>% add_trace(data = forecasted_data2 , y=~metric, x=~date, color="#339989", showlegend = F,
                          type="scatter", mode="lines", line=list(color=~location, dash="dot"),
                          hovertemplate = paste(
                            paste0('<extra>Forecast</extra>Country Name: %{text}\n',name_fix(input$metric_country),': %{y}\nDate: %{x} ')
                          ))
  highlight(plt)
  
  
}

forecast_data <- function(clean_data_country, input){
  unforecasted_data <- clean_data_country
  unforecasted_data$forecast <- 0 #forecast je 0 če je to datum s podatki in 1 če imamo forecast podatkov
  forecasted_data <- predictions_by_country(clean_data_country, input)
  forecasted_data <- do.call(rbind,forecasted_data)
  rbind(unforecasted_data,forecasted_data) 
}
forecast_data_c <- function(clean_data_country_card, input){
  unforecasted_data <- clean_data_country_card
  unforecasted_data$forecast <- 0 #forecast je 0 če je to datum s podatki in 1 če imamo forecast podatkov
  forecasted_data <- predictions_by_country_c(clean_data_country_card, input)
  forecasted_data <- do.call(rbind,forecasted_data)
  rbind(unforecasted_data,forecasted_data) 
}
forecast_data_c2 <- function(clean_data_continent_card, input){
  unforecasted_data <- clean_data_continent_card
  unforecasted_data$forecast <- 0 #forecast je 0 če je to datum s podatki in 1 če imamo forecast podatkov
  forecasted_data <- predictions_by_continent_c(clean_data_continent_card, input)
  forecasted_data <- do.call(rbind,forecasted_data)
  rbind(unforecasted_data,forecasted_data) 
}
create_forecast <- function(dat, num_forecasts){
  name_country <- unique(dat$location)
  auto_forecast <-  forecast(auto.arima(dat$metric),num_forecasts)$mean
  max_date <- max(dat$date)
  new_dates <- max_date + c(1:num_forecasts)
  new_forecast <- tibble( location = name_country, date = new_dates , metric = as.vector(auto_forecast), forecast = 1 )
  return(new_forecast)
}

predictions_by_country <- function(clean_data_country, input){
  clean_data_country %>%
    group_by(location) %>%
    group_map(~ create_forecast(.x, num_forecasts=input$forecast), .keep=T )
}
predictions_by_country_c <- function(clean_data_country_card, input){
  clean_data_country_card %>%
    group_by(location) %>%
    group_map(~ create_forecast(.x, num_forecasts=input$forecast3), .keep=T )
}

plot_by_country <- function(input, clean_data_country, ma_days){
  ifelse(input$moving_av_country ==T & !is.null(input$moving_av_days_country),
         dat_ma <- clean_data_country%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days,mean,align='right',fill=NA)),
         dat_ma <- clean_data_country
  )
  
  plt <- plot_ly (data = dat_ma, x=~date, color=~location, text = ~location)
  plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines',
                           hovertemplate = paste(
                             paste0('<extra></extra>Country Name: %{text}\n','Metric: ',name_fix(input$metric_country),': %{y}\nDate: %{x} ')
                           ))
  if(input$moving_av_country==T & !is.null(input$moving_av_days_country)){ 
    plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot"), showlegend=F,
                             hovertemplate = paste(
                               paste0("<extra>Moving Average</extra>Country Name: %{text}\n",'Metric: ',name_fix(input$metric_country),': %{y}\nDate: %{x}')) )
  }
  highlight(plt)
  
  
}
plot_data_country_forecast <- function(clean_data_country, input, ma_days){
  
  ifelse(input$moving_av_country ==T & !is.null(input$moving_av_days_country),
         forecastdata <- forecast_data(clean_data_country, input)%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days,mean,align='right',fill=NA)),
         forecastdata <- forecast_data(clean_data_country, input)
  )
  
  forecasted_data2 <- rbind(forecastdata%>%filter(forecast==0)%>%filter(date==max(date)),
                            forecastdata%>%filter(forecast==1))
  
  plt <- plot_ly (data = forecastdata%>%filter(forecast==0), x=~date, color=~location, text = ~location)
  plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines+markers',
                           hovertemplate = paste(
                             paste0('<extra></extra>Country Name: %{text}\n',name_fix(input$metric_country),': %{y}\nDate: %{x} ')
                           ))
  if(input$moving_av_country==T & !is.null(input$moving_av_days_country)){ 
    plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot"), showlegend=F,
                             hovertemplate = paste(
                               paste0("<extra>Moving Average</extra>Country Name: %{text}\n",name_fix(input$metric_country),': %{y}\nDate: %{x}')) )
  }
  plt <- plt%>% add_trace(data = forecasted_data2 , y=~metric, x=~date, color=~location, showlegend = F,
                          type="scatter", mode="lines", line=list(color=~location, dash="dot"),
                          hovertemplate = paste(
                            paste0('<extra>Forecast</extra>Country Name: %{text}\n',name_fix(input$metric_country),': %{y}\nDate: %{x} ')
                          ))
  highlight(plt)
}

func_card_info_c <- function(basic_data2, continent, date1, date2){
  count <-basic_data2 %>%filter(location == continent) %>% 
    filter(date >= as.Date(date1))%>%filter(date <= as.Date(date2))
  
  pop <- count%>%filter(date==as.Date(date2))
  pop <- (pop[1,])[16]
  
  people_vacc <- count%>%filter(date==as.Date(date2))
  people_vacc <- (people_vacc[1,])[12]
  perc_vacc <- ((100*people_vacc)/pop)
  med_age <- count%>%filter(date==as.Date(date2))
  med_age <- (med_age[1,])[18]
  
  
  total_c <- count%>%filter(date==as.Date(date2))
  total_c <- (total_c[1,])[4]
  total_d <- count%>%filter(date==as.Date(date2))
  total_d <- ((total_d[1,])[6])  
  
  fatality <- ((total_d/total_c)*100)
  
  rez <- c(pop, people_vacc, perc_vacc, med_age , fatality)
  return(rez)
}

func_card_info2_c <- function(basic_data2 ,continent, date1, date2){
  count <- (basic_data2 %>%filter(location == continent)%>%drop_na(new_cases)%>%filter(new_cases != 0))
  first_case <- (count[1,])[3]
  data <- basic_data2 %>%filter(location == continent)%>%filter(date >= as.Date(date1))%>%filter(date <= as.Date(date2))
  
  cases <- data%>%drop_na(new_cases)%>%summarise(all_cases = sum(new_cases))
  deaths <- data%>%drop_na(new_deaths)%>%summarise(all_deaths = sum(new_deaths))
  vaccine <- data%>%drop_na(new_vaccinations)%>%summarise(all_vacc = sum(new_vaccinations))
  
  
  return(c(first_case, cases, deaths, vaccine))
}

plot_by_continent_card <- function(clean_data_continent_card, input, ma_days4){
  ifelse(input$moving_av_country3 ==T & !is.null(input$moving_av_days_country3),
         dat_ma <- clean_data_continent_card%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days4,mean,align='right',fill=NA)),
         dat_ma <- clean_data_continent_card
  )
  
  
  plt <- plot_ly (data = dat_ma, x=~date, color=~location, text = ~location)
  plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines', line = list(color = 'black'),
                           hovertemplate = paste(
                             paste0('<extra></extra>Continent Name: %{text}\n','Metric: ',name_fix(input$metric_continent_card),': %{y}\nDate: %{x} ')
                           ))
  if(input$moving_av_country3==T & !is.null(input$moving_av_days_country3)){ 
    plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot", color='#7DE2D1'), showlegend=F,
                             hovertemplate = paste(
                               paste0("<extra>Moving Average</extra>Continent Name: %{text}\n",'Metric: ',name_fix(input$metric_continent_card),': %{y}\nDate: %{x}')) )
  }
  highlight(plt)
  
  
}

plot_by_continent_card_forecast <- function(clean_data_continent_card, clean_data_country_card,input, ma_days4){
  ifelse(input$moving_av_country3 ==T & !is.null(input$moving_av_days_country3),
         dat_ma <- clean_data_continent_card%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days4,mean,align='right',fill=NA)),
         dat_ma <- clean_data_continent_card
  )
  ifelse(input$moving_av_country3 ==T & !is.null(input$moving_av_days_country3),
         forecastdata <- forecast_data_c2(clean_data_continent_card, input)%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days4,mean,align='right',fill=NA)),
         forecastdata <- forecast_data_c2(clean_data_continent_card, input)
  )
  
  forecasted_data2 <- rbind(forecastdata%>%filter(forecast==0)%>%filter(date==max(date)),
                            forecastdata%>%filter(forecast==1))
  #dat_ma <- clean_data_continent_card()
  plt <- plot_ly (data = forecastdata%>%filter(forecast==0), x=~date, color=~location, text = ~location)
  #svetla #7DE2D1
  #temna #339989
  
  #plt <- plot_ly (data = dat_ma, x=~date, color=~location, text = ~location)
  plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines',
                           line = list(color = 'black'),
                           hovertemplate = paste(
                             paste0('<extra></extra>Continent Name: %{text}\n','Metric: ',name_fix(input$metric_continent_card),': %{y}\nDate: %{x} ')
                           ))
  if(input$moving_av_country3==T & !is.null(input$moving_av_days_country3)){ 
    plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot",color='#7DE2D1'), showlegend=F,
                             hovertemplate = paste(
                               paste0("<extra>Moving Average</extra>Continent Name: %{text}\n",'Metric: ',name_fix(input$metric_continent_card),': %{y}\nDate: %{x}')) )
  }
  plt <- plt%>% add_trace(data = forecasted_data2 , y=~metric, x=~date, color='#339989', showlegend = F,
                          type="scatter", mode="lines", line=list(color='#339989', dash="dot"),
                          hovertemplate = paste(
                            paste0('<extra>Forecast</extra>Continent Name: %{text}\n',name_fix(input$metric_continent_card),': %{y}\nDate: %{x} ')
                          ))
  highlight(plt)
  ###########
  
}
forecast_data2 <- function(clean_data_continent, input){
  unforecasted_data <- clean_data_continent
  unforecasted_data$forecast <- 0 #forecast je 0 če je to datum s podatki in 1 če imamo forecast podatkov
  forecasted_data <- predictions_by_continent(clean_data_continent, input)
  forecasted_data <- do.call(rbind,forecasted_data)
  rbind(unforecasted_data,forecasted_data) 
}
create_forecast2 <- function(dat, num_forecasts){
  name_country <- unique(dat$location)
  auto_forecast <-  forecast(auto.arima(dat$metric),num_forecasts)$mean
  max_date <- max(dat$date)
  new_dates <- max_date + c(1:num_forecasts)
  new_forecast <- tibble( location = name_country, date = new_dates , metric = as.vector(auto_forecast), forecast = 1 )
  return(new_forecast)
}

predictions_by_continent <- function(clean_data_continent, input){
  clean_data_continent %>%
    group_by(location) %>%
    group_map(~ create_forecast2(.x, num_forecasts=input$forecast2), .keep=T )
}
predictions_by_continent_c <- function(clean_data_continent_card, input){
  clean_data_continent_card %>%
    group_by(location) %>%
    group_map(~ create_forecast2(.x, num_forecasts=input$forecast4), .keep=T )
}
plot_by_continent <- function(clean_data_continent, input, ma_days2){
  ifelse(input$moving_av_continent ==T & !is.null(input$moving_av_days_continent),
         dat_ma <- clean_data_continent%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days2,mean,align='right',fill=NA)),
         dat_ma <- clean_data_continent
  )
  
  plt <- plot_ly (data = dat_ma, x=~date, color=~location, text = ~location)
  plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines',
                           hovertemplate = paste(
                             paste0('<extra></extra>Country Name: %{text}\n','Metric: ',name_fix(input$metric_continent),': %{y}\nDate: %{x} ')
                           ))
  if(input$moving_av_continent==T & !is.null(input$moving_av_days_continent)){ 
    plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot"), showlegend=F,
                             hovertemplate = paste(
                               paste0("<extra>Moving Average</extra>Country Name: %{text}\n",'Metric: ',name_fix(input$metric_continent),': %{y}\nDate: %{x}')) )
  }
  highlight(plt)
  
  
}
plot_data_continent_forecast <- function(clean_data_continent, input, ma_days2){
  
  ifelse(input$moving_av_continent ==T & !is.null(input$moving_av_days_continent),
         forecastdata <- forecast_data2(clean_data_continent, input)%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days2,mean,align='right',fill=NA)),
         forecastdata <- forecast_data2(clean_data_continent, input)
  )
  
  forecasted_data2 <- rbind(forecastdata%>%filter(forecast==0)%>%filter(date==max(date)),
                            forecastdata%>%filter(forecast==1))
  
  plt <- plot_ly (data = forecastdata%>%filter(forecast==0), x=~date, color=~location, text = ~location)
  plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines+markers',
                           hovertemplate = paste(
                             paste0('<extra></extra>Continent Name: %{text}\n',name_fix(input$metric_continent),': %{y}\nDate: %{x} ')
                           ))
  if(input$moving_av_continent==T & !is.null(input$moving_av_days_country)){ 
    plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot"), showlegend=F,
                             hovertemplate = paste(
                               paste0("<extra>Moving Average</extra>Country Name: %{text}\n",name_fix(input$metric_continent),': %{y}\nDate: %{x}')) )
  }
  plt <- plt%>% add_trace(data = forecasted_data2 , y=~metric, x=~date, color=~location, showlegend = F,
                          type="scatter", mode="lines", line=list(color=~location, dash="dot"),
                          hovertemplate = paste(
                            paste0('<extra>Forecast</extra>Country Name: %{text}\n',name_fix(input$metric_continent),': %{y}\nDate: %{x} ')
                          ))
  highlight(plt)
}