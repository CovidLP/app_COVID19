## Aplicativo Shiny - Modelo de Predição de Casos de Coronavírus
## DEST/UFMG - mar/2020

## Código do servidor

## load packages
library(dplyr)
library(tidyr)

## setup data source (Johns Hopkins) 
baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

## define font to be used later
f1 = list(#family="Courier New, monospace",
          family="Arial", 
          size=10,color="rgb(30,30,30)")

## function to measure how old a file is
minutesSinceLastUpdate = function(fileName) {
  (as.numeric(as.POSIXlt(Sys.time())) -  
     as.numeric(file.info(fileName)$ctime)) / 60
}

## main function to load the data
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
    loadData("time_series_covid19_deaths_global.csv", "CumDeaths")) 
# %>%
  # inner_join(
  #   loadData("time_series_covid19_recovered_global.csv","CumRecovered"))


## REACTIVE SERVER CODE

server = function(input, output, session) {
  
  ## load data depending on the country/region selected
  
  data = reactive({
    d = allData %>%
      filter(`Country/Region` == input$country)
    if(input$state != "<all>") {
      d = d %>% 
        filter(`Province/State` == input$state) 
    } else {
      d = d %>% 
        group_by(date) %>% 
        summarise_if(is.numeric, sum, na.rm=TRUE)
    }
    d %>%
      mutate(
        # dateStr = format(date, format="%b %d, %Y"),
        dateStr = format(date, format="%d/%b"),
        NewConfirmed=CumConfirmed - lag(CumConfirmed, default=0),
        # NewRecovered=CumRecovered - lag(CumRecovered, default=0),
        NewDeaths=CumDeaths - lag(CumDeaths, default=0)
      )
  })
  
  ## observe if there is any change on the dropdown menu
  
  observeEvent(input$country, {
    states = allData %>%
      filter(`Country/Region` == input$country) %>% 
      pull(`Province/State`)
    states = c("<all>", sort(unique(states)))
    updateSelectInput(session, "state", choices=states, 
                      selected=states[1])
  })
  
  ## setup the list of countries
  countries = sort(unique(allData$`Country/Region`))
  
  ## start session with Italy selected
  updateSelectInput(session, "country", choices=countries, 
                    selected="Brazil")
  
  ## graphs
  renderPlot = function(varPrefix, legendPrefix, yaxisTitle) {
    ## função do pacote 'plotly' para criar o gráfico interativo
    renderPlotly({
      data = data()
      plt = data %>%
        plot_ly() %>%  # primeiro, cria o gráfico vazio para configurar eixos e legenda
        config(displayModeBar=FALSE) %>%
        layout(
          #barmode='group',
          xaxis=list(
            title="", tickangle=-90, type='category',
            ticktext=as.list(data$dateStr),
            tickvals=as.list(data$date)),
          yaxis=list(title=yaxisTitle),
          legend=list(x=0.1, y=0.9,bgcolor='rgba(240,240,240,0.5)')
          ,          font=f1
        )
      # depois, adiciona as linhas de cada uma das medidas fornecidas no input
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
                         # Recovered='rgb(30,200,30)',
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
