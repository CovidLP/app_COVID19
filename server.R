## Aplicativo Shiny - Modelo de Predição de Casos de Coronavírus
## DEST/UFMG - mar/2020

## Código do servidor

## load packages
library(dplyr)
library(tidyr)

##############################################################################

## define font to be used later
f1 = list(#family="Courier New, monospace",
          family="Arial", 
          size=10,color="rgb(30,30,30)")

## function to measure how old a file is
minutesSinceLastUpdate = function(fileName) {
  (as.numeric(as.POSIXlt(Sys.time())) -  
     as.numeric(file.info(fileName)$ctime)) / 60
}

##############################################################################

## DATA SOURCES

## setup data source (Johns Hopkins)- GLOBAL
baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

## setup data source (IVIS/MSaude/BR)- BRAZIL
baseURL.BR = "https://raw.githubusercontent.com/belisards/coronabr/master/dados"


## main function to load the data - GLOBAL
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

allData = 
  loadData("time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
  inner_join(
    loadData("time_series_covid19_deaths_global.csv", "CumDeaths")) %>%
  inner_join(
    loadData("time_series_covid19_recovered_global.csv","CumRecovered"))


## function to load data - BRAZIL
loadData.BR = function(fileName) {
  if(!file.exists(fileName) || 
     minutesSinceLastUpdate(fileName) > 10) {
    data = read.csv(file.path(baseURL.BR, fileName), 
                    check.names=FALSE, stringsAsFactors=FALSE) %>%
      # select(-c(1,3:8,10:11,14)) %>% # remove unwanted columns
      as_tibble() %>%
      mutate(date=as.Date(date), # format dates
             `CumRecovered` = NA
             ) %>% 
      rename(`Province/State` = uf, # rename some variables
             `CumConfirmed` = cases,
             `CumDeaths` = deaths
             ) 
    save(data, file=fileName)  
  } else {
    load(file=fileName)
  }
  return(data)
}

brData = loadData.BR("corona_brasil.csv")

##############################################################################

## REACTIVE SERVER CODE

server = function(input, output, session) {
  
  ## load data depending on the country/region selected
  
  data = reactive({
    
    if(input$country == "Brazil"){ # if selected Brazil
      d = brData %>%
        arrange(`Province/State`, date)
    } else{ # otherwise, filter by selected country
      d = allData %>%
        filter(`Country/Region` == input$country)
    }
    
    if(input$state != "<all>") {
      d = d %>% 
        filter(`Province/State` == input$state) # filter by selected state
    } else {
      d = d %>% 
        group_by(date) %>% 
        summarise_at(c("CumConfirmed","CumDeaths","CumRecovered"), sum)
    }
    d %>%
      mutate(
        dateStr = format(date, format="%d/%b"),
        NewConfirmed=CumConfirmed - lag(CumConfirmed, default=0),
        NewRecovered=CumRecovered - lag(CumRecovered, default=0),
        NewDeaths=CumDeaths - lag(CumDeaths, default=0)
      )
  })
  
  ## observe if there is any change on the dropdown menu
  
  observeEvent(input$country, {
    # states = allData %>%
    #   filter(`Country/Region` == input$country) %>% 
    #   pull(`Province/State`)
    # states = c("<all>", sort(unique(states)))
    # updateSelectInput(session, "state", choices=states, 
    #                   selected=states[1])
    if(input$country == "Brazil"){ # if country is Brazil
      states = brData %>% pull(`Province/State`)
      # updateCheckboxGroupInput(session, "metrics", label=h5("Casos/Cases"),
      #                          choices=c("Confirmed","Deaths","Recovered"),
      #                          selected=c("Confirmed","Deaths","Recovered"))
    } else{ # if other countries
      states = allData %>%
        filter(`Country/Region` == input$country) %>% 
        pull(`Province/State`)
    }
    states = c("<all>", sort(unique(states)))
    updateSelectInput(session, "state", choices=states, selected=states[1])
  })
  
  ## setup the list of countries
  countries = sort(unique(allData$`Country/Region`))
  
  ## start session with Brazil selected
  updateSelectInput(session, "country", choices=countries, 
                    selected="Brazil")
                    # selected="China")
  
  ## graphs
  renderPlot = function(varPrefix, legendPrefix, yaxisTitle) {
    ## plotly function for interactive plot
    renderPlotly({
      data = data()
      plt = data %>%
        plot_ly() %>%  # first, make empty plot and setup axis and legend
        config(displayModeBar=FALSE) %>%
        layout(
          #barmode='group',
          xaxis=list(
            title="", tickangle=-90, type='category',
            ticktext=as.list(data$dateStr),
            tickvals=as.list(data$date)),
          yaxis=list(title=yaxisTitle),
          legend=list(x=0.1, y=0.9,bgcolor='rgba(240,240,240,0.5)'),
          font=f1
        )
      # after, add lines for each metric from input
      metrics_pt = c("Confirmados", "Mortes", "Recuperados")
      for(metric in input$metrics)
      # for(i in 1:length(input$metrics))
      #   metric = input$metrics[i]
        # metric_pt = ifelse(metric=="Confirmed", "Confirmados",
        #                    ifelse(metric=="Deaths", "Mortes", "Recuperados"))
        plt = plt %>%
        add_trace(
          x=~date, y=data[[paste0(varPrefix, metric)]],
          type='scatter', mode='lines+markers', #type='bar',
          name=paste(legendPrefix, metric, "Cases"),
          # name=paste("Casos",metric_pt),
          marker=list(
            color=switch(metric,
                         Deaths='rgb(200,30,30)',
                         Recovered='rgb(30,200,30)',
                         Confirmed='rgb(100,140,240)'),
            # color=switch(metric_pt,
            #              Mortes='rgb(200,30,30)',
            #              Recuperados='rgb(30,200,30)',
            #              Confirmados='rgb(100,140,240)'),
            line=list(color='rgb(8,48,107)', width=1.0)
          )
        )
      plt
    })
  }
  output$dailyMetrics = renderPlot(
    "New", # legendPrefix="New", yaxisTitle="New Cases per Day")
    legendPrefix="", yaxisTitle="Novos Casos por Dia")
  output$cumulatedMetrics = renderPlot(
    "Cum", # legendPrefix="Cumulated", yaxisTitle="Cumulated Cases")
    legendPrefix="", yaxisTitle="Casos Acumulados")
  
  
}
