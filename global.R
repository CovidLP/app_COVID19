## Load packages
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)
library(knitr)
library(shiny)
library(shinyjs)
library(plotly)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(shinyBS)
library(markdown)

## Load local functions
source("utils.R")

## Define font to be used later
f1 <- list(family = "Arial", size = 10, color = "rgb(30, 30, 30)")

## colors for observed data
blu <- 'rgb(100, 140, 240)'
dblu <- 'rgb(0, 0, 102)'
red <- 'rgb(200, 30, 30)'
dred <- 'rgb(100, 30, 30)'

##-- DATA SOURCES ----
## setup data source (Johns Hopkins)- GLOBAL
baseURL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

allData <- loadData("time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
  inner_join(
    loadData("time_series_covid19_deaths_global.csv", "CumDeaths"),
    by = c("Province/State", "Country/Region", "date")
  ) 

countries <- sort(unique(allData$`Country/Region`))

## Setup data source (MSaude/BR)- BRAZIL
# baseURL.BR = "https://raw.githubusercontent.com/belisards/coronabr/master/dados"
# baseURL.BR = "https://covid.saude.gov.br/assets/files/COVID19_"
baseURL.BR <- "https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/dados"
# baseURL.BR <- "https://raw.githubusercontent.com/thaispaiva/app_COVID19/master/R/STAN"
brData <- loadData.BR("EstadosCov19.csv")

##-- LOAD PREDICTION RESULTS ----
files <- readfiles.repo()
aux <- sub(pattern = '(\\_n.rds$)|(\\_d.rds$)', replacement = '', x = files)

## List of countries for SHORT TERM prediction
countries_STpred <- sort(unique(
  unlist(lapply(strsplit(aux, "_"), function(x) x[1]))))
countries_STpred_orig <- gsub("-", " ", countries_STpred)

## List of Brazil's states
statesBR_STpred <- unique(unlist(lapply(strsplit(aux, "_"), function(x) if(x[1] == "Brazil") return(x[2]))))
statesBR_STpred[is.na(statesBR_STpred)] <- "<all>"
statesBR_STpred <- sort(statesBR_STpred)

## List of countries for LONG TERM prediction
countries_LTpred_orig <- countries_STpred_orig

## List of Brazil's states - LONG TERM
statesBR_LTpred <- statesBR_STpred

## Read RDS files from github repository
githubURL <- "https://github.com/thaispaiva/app_COVID19/raw/master/STpredictions"