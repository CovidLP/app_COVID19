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
library(markdown)

##############################################################################

## Define font to be used later
f1 = list(family = "Arial", size = 10, color = "rgb(30,30,30)")

## Function to measure how old a file is
minutesSinceLastUpdate = function(fileName) {
  (as.numeric(as.POSIXlt(Sys.time())) - as.numeric(file.info(fileName)$ctime)) / 60
}

## Function to format the dates for better plotting
printDate = function(date){
  # paste0(day(date),"/",month(date, lab=T, locale="us"))
  monthsEn=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  paste0(day(date),"/",monthsEn[month(date)])
}

## colors for observed data
blu = 'rgb(100,140,240)'
dblu = 'rgb(0,0,102)'
red = 'rgb(200,30,30)'
dred = 'rgb(100,30,30)'

##############################################################################
## DATA SOURCES

## setup data source (Johns Hopkins)- GLOBAL
baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

## Main function to load the data - GLOBAL
loadData = function(fileName, columnName) {
  if(!file.exists(fileName) || 
     minutesSinceLastUpdate(fileName) > 10) {
    data = read.csv(file.path(baseURL, fileName), 
                    check.names=FALSE, stringsAsFactors=FALSE) %>%
      select(-Lat, -Long) %>% 
      pivot_longer(-(1:2), names_to="date", values_to=columnName)%>%
      mutate(
        date=as.Date(date, format="%m/%d/%y"),
        `Province/State`=
          if_else(`Province/State` == "", "<all>", `Province/State`)
      )
    save(data, file=fileName)  
  } else {
    load(file=fileName)
  }
  return(data)
}

allData = loadData("time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
  inner_join(
    loadData("time_series_covid19_deaths_global.csv", "CumDeaths"),
    by = c("Province/State", "Country/Region", "date")
  ) # %>%
# inner_join(
# loadData("time_series_covid19_recovered_global.csv","CumRecovered"))

##############################################################################
## Setup data source (MSaude/BR)- BRAZIL
# baseURL.BR = "https://raw.githubusercontent.com/belisards/coronabr/master/dados"
# baseURL.BR = "https://covid.saude.gov.br/assets/files/COVID19_"
baseURL.BR = "https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/dados"

## Function to load data - BRAZIL
loadData.BR = function(fileName) {
  if(!file.exists(fileName) || minutesSinceLastUpdate(fileName) > 10) {
    data = read.csv(file.path(baseURL.BR, fileName), 
                    check.names=FALSE, stringsAsFactors=FALSE) %>%
      # select(-c(1,3:8,10:11,14)) %>% # remove unwanted columns
      select(-c(1,4,6)) %>%
      as_tibble() %>%
      mutate(date = as.Date(data)
             # date=as.Date(date), # format dates
             # `CumRecovered` = NA
      ) %>% 
      rename(`Province/State` = estado, # rename some variables
             `CumConfirmed` = casos.acumulados,
             `CumDeaths` = obitos.acumulados
      ) %>%
      select(-2)
    # browser()
    save(data, file=fileName)  
  } else {
    load(file=fileName)
  }
  
  return(data)
}

# brData = loadData.BR("corona_brasil.csv")
brData = loadData.BR("EstadosCov19.csv")

##############################################################################
## LOAD PREDICTION RESULTS

## Function to return list of countries with available data on github
readfiles.repo <- function() {
  req <- GET("https://api.github.com/repos/thaispaiva/app_COVID19/git/trees/master?recursive=1")
  stop_for_status(req)
  # extract list of files on github
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
  files <- grep("STpredictions/", filelist, value = TRUE, fixed = TRUE)
  files <- unlist(lapply(strsplit(files,"/"),"[",2))
  files <- grep(".rds",files, value=TRUE)
  
  return(files)
}

files = readfiles.repo() # get available results
aux = sub('\\_n.rds$', '', sub('\\_d.rds$', '', files))

##############################################
## List of countries for SHORT TERM prediction
countries_STpred = sort(unique(  # country names without space 
  unlist(lapply(strsplit(aux,"_"), function(x) x[1]))))
countries_STpred_orig = gsub("-"," ", countries_STpred) # country names with space (original)

## List of Brazil's states
statesBR_STpred = unique(
  unlist(lapply(strsplit(aux,"_"), function(x) if(x[1]=="Brazil") return(x[2]) )))
statesBR_STpred[is.na(statesBR_STpred)] = "<all>"
statesBR_STpred = sort(statesBR_STpred)

#############################################
## List of countries for LONG TERM prediction
countries_LTpred_orig = countries_STpred_orig

## List of Brazil's states - LONG TERM
statesBR_LTpred = statesBR_STpred

## Read RDS files from github repository
githubURL = "https://github.com/thaispaiva/app_COVID19/raw/master/STpredictions"
