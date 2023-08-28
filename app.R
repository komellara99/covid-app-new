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
#crna #131515
#bela #FFFAFB
#svetla #7DE2D1
#temna #339989
#skori crna #2B2C28
# #1e5c52


theme2 <- bslib::bs_theme(version = 4)

source("ui.R")
source("server.R")


shinyApp(ui = ui, server = server)