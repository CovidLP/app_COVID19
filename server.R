server = function(input, output, session) {
  ##-- Send an alert when the app runs ----
  # shinyWidgets::sendSweetAlert(
  #   session = session, 
  #   title = "Você está no celular?", 
  #   text = "Caso esteja acessando pelo celular, o aplicativo é melhor visualizado com o aparelho na horizontal <br> If you are on mobile, the application is best viewed with the device on horizontal.", 
  #   type = "info", 
  #   closeOnClickOutside = FALSE, 
  #   html = TRUE
  # )
  
  ##-- Add message on startup ----
  showModal(modalDialog(
    div(
      p("Caso esteja acessando pelo celular, o aplicativo é melhor visualizado com o aparelho na horizontal"),
      p("If you are on mobile, the application is best viewed with the device on horizontal.")
    ),
    easyClose = FALSE,
    footer = tagList(
      actionButton(inputId = "ok_popup", label = "Ok")
    )
  ))
  
  ##-- Data reactives ----
  ##-- + Load data depending on the country/region selected
  data <- reactive({
    country_name <- input$country
    state_name <- input$state
    
    # if(country_name == "Brazil") {
    #   d <- brData %>%
    #     arrange(`Province/State`, date)
    # } else { 
    #   d <- allData %>%
    #     filter(`Country/Region` == country_name)
    # }
    
    if(country_name == "Brazil") {
      d <- brData %>% arrange(`Province/State`, date)
    } else {
      if (country_name == "US") { 
        d <- usData %>% arrange(`Province/State`, date)
      } else { 
        d <- allData %>% filter(`Country/Region` == country_name)
      }
    }
    
    if(state_name != "<all>") {
      d <- d %>% 
        filter(`Province/State` == state_name) 
    } else {
      if(country_name == "Brazil") {
        d <- d %>% 
          group_by(date) %>% 
          # summarise_at(c("NewConfirmed", "NewDeaths"), sum)
          summarise_at(c("CumConfirmed", "CumDeaths"), sum)
      } else {
        d <- d %>% 
          group_by(date) %>% 
          summarise_at(c("CumConfirmed", "CumDeaths"), sum)
      }
      
    }
    
    if(country_name == "Brazil") {
      out <- d %>% 
        mutate(
          dateStr = format(date, format = "%d/%b"),
          NewConfirmed = CumConfirmed - lag(CumConfirmed, default = 0),
          NewDeaths = CumDeaths - lag(CumDeaths, default = 0)
          # CumConfirmed = cumsum(NewConfirmed),
          # CumDeaths = cumsum(NewDeaths)
        )
    } else {
      out <- d %>% 
        mutate(
          dateStr = format(date, format = "%d/%b"),
          NewConfirmed = CumConfirmed - lag(CumConfirmed, default = 0),
          NewDeaths = CumDeaths - lag(CumDeaths, default = 0)
          # CumConfirmed = cumsum(NewConfirmed),
          # CumDeaths = cumsum(NewDeaths)
        )
    }
    
    return(out)
  })
  
  ##-- + Load short term prediction results depending on the country/region selected
  pred_n <- reactive({
    country_name <- input$country
    country_name <- gsub(" ", "-", country_name)
    state_name <- input$state
    
    metric = ifelse(input$metrics_ST == "Confirmed", "n", "d")
    
    if(country_name == "Brazil") {
      if(state_name != "<all>"){
        if(exists(paste0(country_name, "_", state_name, "_", metric))) {
          pred_n <- get(paste0(country_name, "_", state_name, "_", metric))
        } else {
          pred_n <- readRDS(url(paste0(githubURL, "/", country_name, "_", state_name, "_", metric, "e.rds")))
          assign(paste0(country_name, "_", state_name, "_", metric), pred_n)
        }
      } else {
        if(exists(paste0(country_name, "_", metric))) {
          pred_n <- get(paste0(country_name, "_", metric))
        } else {
          pred_n <- readRDS(url(paste0(githubURL, "/", country_name, "_", metric, ".rds")))
          assign(paste0(country_name, "_", metric), pred_n)
        }
      }
    } else {
      if(exists(paste0(country_name, "_", metric))) {
        pred_n <- get(paste0(country_name, "_", metric))
      } else {
        pred_n <- readRDS(url(paste0(githubURL, "/", country_name, "_", metric, ".rds")))
        assign(paste0(country_name, "_", metric), pred_n)
      }
    }
    
    closeAllConnections()
    
    ##-- Select only object 'df_predict' from the list
    pred_n <- pred_n$df_predict
    pred_n$date <- as.Date(pred_n$date)
    
    return(pred_n)
  })
  
  ##-- + Load long term prediction results depending on the country/region selected
  predLT_n <- reactive({
    country_name <- input$country
    country_name <- gsub(" ", "-", country_name)
    
    state_name <- input$state
    
    metric <- ifelse(input$metrics_LT == "Confirmed", "n", "d")
    
    if(country_name == "Brazil") {
      if(state_name != "<all>") {
        if(exists(paste0(country_name, "_", state_name, "_", metric))) {
          pred_n <- get(paste0(country_name, "_", state_name, "_", metric))
        } else {
          pred_n <- readRDS(url(paste0(githubURL, "/", country_name, "_", state_name, "_", metric, "e.rds")))
          assign(paste0(country_name, "_", state_name, "_", metric), pred_n)
        }
      } else {
        if(exists(paste0(country_name, "_", metric))) {
          pred_n <- get(paste0(country_name, "_", metric))
        } else {
          pred_n <- readRDS(url(paste0(githubURL, "/", country_name, "_", metric, ".rds")))
          assign(paste0(country_name, "_", metric), pred_n) 
        }
      }
    } else {
      if(exists(paste0(country_name, "_", metric))) {
        pred_n <- get(paste0(country_name, "_", metric))
      } else {
        pred_n <- readRDS(url(paste0(githubURL, "/", country_name, "_", metric, ".rds")))
        assign(paste0(country_name, "_", metric), pred_n)
      }
    }
    
    closeAllConnections()
    
    pred_n$lt_predict$date <- as.Date(pred_n$lt_predict$date)
    pred_n$df_predict = NULL
    
    return(pred_n)
  })
  
  ##-- Updating inputs buttons ----
  ##-- + Country
  observeEvent(input$tabset_covid, {
    if(input$tabset_covid == "tab_dados") {
      countries <- countries
    }
    
    if(input$tabset_covid == "tab_curto") {
      countries <- countries_STpred_orig
    }
    
    if(input$tabset_covid == "tab_longo") {
      countries <- countries_LTpred_orig
    }
    
    sel <- ifelse(input$country %in% countries, input$country, "Brazil")
    updateSelectInput(session, "country", choices = countries, selected = sel)
  })
  
  ##-- + State
  observeEvent(c(input$tabset_covid, input$country), {
    if(input$country == "Brazil") {
      states <- brData %>% pull(`Province/State`)
    } else { 
      if(input$tabset_covid == "tab_dados") {
        if(input$country == "US") {
          states <- usData %>% pull(`Province/State`)
        } else {
          states <- allData %>%
            filter(`Country/Region` == input$country) %>% 
            pull(`Province/State`)
        }
      } else {
        states <- NULL
      }
    }
    
    states <- c("<all>", sort(unique(states)))
    sel <- ifelse(input$state %in% states, input$state, "<all>")
    
    updateSelectInput(session, "state", choices = states, selected = sel)
  })
  
  ##-- Plots ----
  ##-- + Observed data
  observe({
    if(!is.null(input$ok_popup) && input$ok_popup > 0) {
      removeModal()
      
      output$dailyMetrics = renderPlotly({
        plt <- plot_obs(
          data = data(),
          country_name = input$country, 
          state_name = input$state, 
          metrics = input$metrics_data,
          scale = input$scale_data,
          varPrefix = "New", 
          legendPrefix = "", 
          yaxisTitle = "Novos Casos por Dia/New Cases per Day"
        )
        
        return(plt)
      })
      
      output$cumulatedMetrics = renderPlotly({
        plt <- plot_obs(
          data = data(),
          country_name = input$country, 
          state_name = input$state, 
          metrics = input$metrics_data,
          scale = input$scale_data,
          varPrefix = "Cum",  
          legendPrefix = "", 
          yaxisTitle = "Novas Mortes por Dia/New deaths per Day"
        ) 
        
        return(plt)
      })
    }
  })
  
  ##-- + Download
  output$downloadData <- downloadHandler(
    filename = function() {
      ## Get last date from observed data
      data = data()
      last_date_n = max(data$date)
      
      file <- paste0("Data_", isolate(input$country), 
                     ifelse(isolate(input$state) == "<all>", "_", paste0("_", isolate(input$state), "_")),
                     format(last_date_n, format = "%d%m%Y"), ".csv")
      
      return(file)
    },
    content = function(con) {
      data <- data() %>% select(-dateStr)
      
      write.csv(data, con, row.names = FALSE)
    }
  )
  
  ##-- + Data message
  output$msg_data1 = renderUI({
    if(input$country == "Brazil"){
      tags$i("Os dados e previsões para o Brasil estão temporariamente suspensos devido à instabilidade nas fontes dos dados.",br(),"Data and forecasts for Brazil are temporarily suspended due to instability in data sources.", style = "text-align:left; color:red")
    } else{
      tags$h5("", style = "text-align:left")
    }
  })
  output$msg_data2 = renderUI({
    if(input$country == "Brazil"){
      tags$i("Os dados e previsões para o Brasil estão temporariamente suspensos devido à instabilidade nas fontes dos dados.",br(),"Data and forecasts for Brazil are temporarily suspended due to instability in data sources.", style = "text-align:left; color:red")
    } else{
      tags$h5("", style = "text-align:left")
    }
  })
  
  ##-- + Data message
  output$msg_ST = renderUI({
    if(input$country == "Brazil"){
      tags$i("Os dados e previsões para o Brasil estão temporariamente suspensos devido à instabilidade nas fontes dos dados.",br(),"Data and forecasts for Brazil are temporarily suspended due to instability in data sources.", style = "text-align:left; color:red")
    } else{
      tags$h5("", style = "text-align:left")
    }
  })
  
  ##-- + Short term prediction title
  output$title_ST = renderUI({
    if(input$metrics_ST == "Confirmed"){
      tags$h3("Previsão de casos acumulados/Prediction of cumulated cases:", style = "text-align:left")
    } else{
      tags$h3("Previsão de mortes acumuladas/Prediction of cumulated deaths:", style = "text-align:left")
    }
  })
  
  ##-- + Short term prediction graph
  output$STpred = renderPlotly({
    yaxisTitle = switch(
      input$metrics_ST,
      Confirmed = "Casos acumulados/Cumulated cases",
      Deaths = "Mortes acumuladas/Cumulated deaths"
    )
    
    if((input$country %in% hide_countries_nc & input$metrics_ST == "Confirmed") | (input$country %in% hide_countries_d & input$metrics_ST == "Deaths")) {
      return(NULL) 
    }
    
    plt <- plot_STpred(
      data = data(),
      predST_n = pred_n(),
      country_name = input$country, 
      state_name = input$state, 
      metrics = input$metrics_ST,
      scale = input$scale_ST,
      pred_time = input$pred_time,
      varPrefix = "Cum",  
      legendPrefix = "", 
      yaxisTitle = yaxisTitle
    ) 
    
    return(plt)  
  })
  
  ##-- + Download
  output$downloadData_ST <- downloadHandler(
    filename = function() {
      ## Get last date from prediction data
      pred_n <- pred_n()
      last_date_n <- min(pred_n$date) - 1
      
      file <- paste0("STpred_", isolate(input$country),
                     ifelse(isolate(input$state) == "<all>", "", paste0("_", isolate(input$state))),
                     switch(isolate(input$metrics_ST), Deaths = "_d_", Confirmed = "_n_"),
                     format(last_date_n, format = "%d%m%Y"), ".csv")
      
      return(file)
    },
    content = function(con) {
      ## load observed & prediction data
      data <- data() %>% 
        select(date, paste0("Cum", isolate(input$metrics_ST)))
      
      pred_n <- pred_n() %>% 
        select(-m) %>% 
        mutate_at(2:4, round, 0) %>% 
        rename_at(2:4, function(x) paste0("Pred_", x))
      
      data.out <- bind_rows(data, pred_n)
      
      write.csv(data.out, con, row.names = FALSE)
    }
  )
  
  ##-- + Long term prediction title
  output$title_LT = renderUI({
    if(input$metrics_LT == "Confirmed"){
      tags$h3("Previsão de novos casos/Prediction of new cases:", style = "text-align:left")
    } else{
      tags$h3("Previsão de novas mortes/Prediction of new deaths:", style = "text-align:left")
    }
  })
  
  ##-- + Long term prediction message
  output$msg_LT = renderUI({
    if(input$country == "Brazil"){
      tags$i("Os dados e previsões para o Brasil estão temporariamente suspensos devido à instabilidade nas fontes dos dados.",br(),"Data and forecasts for Brazil are temporarily suspended due to instability in data sources.", style = "text-align:left; color:red")
    } else{
      tags$h5("", style = "text-align:left")
    }
  })
  
  ##-- + Long term prediction graph
  output$LTpred = renderPlotly({
    yaxisTitle = switch(
      input$metrics_LT,
      Confirmed = "Novos casos por dia/New cases per day",
      Deaths = "Novas mortes por dia/New deaths per day"
    )
    
    if((input$country %in% hide_countries_nc & input$metrics_LT == "Confirmed") | (input$country %in% hide_countries_d & input$metrics_LT == "Deaths")) {
      return(NULL) 
    }
    
    plt <- plot_LTpred(
      data = data(),
      predLT_n = predLT_n(),
      country_name = input$country, 
      state_name = input$state, 
      metrics = input$metrics_LT,
      scale = input$scale_LT,
      varPrefix = "New",  
      legendPrefix = "", 
      yaxisTitle = yaxisTitle
    ) 
    
    return(plt) 
  })
  
  ##-- + Downloads
  output$downloadData_LT <- downloadHandler(
    filename = function() {
      ## Get last date from prediction data
      pred_n <- predLT_n()
      last_date_n <- min(pred_n$lt_predict$date) - 1
      
      file <- paste0("LTpred_", isolate(input$country),
                    ifelse(isolate(input$state) == "<all>", "", paste0("_", isolate(input$state))),
                    switch(isolate(input$metrics_LT), Deaths = "_d_", Confirmed = "_n_"),
                    format(last_date_n, format = "%d%m%Y"), ".csv")
      
      return(file)
    },
    content = function(con) {
      ## Load observed & prediction data
      data <- data() %>% 
        select(date, paste0("New", isolate(input$metrics_LT)))
      
      aux <- predLT_n()
      pred_n <- aux$lt_predict %>% 
        select(-m) %>% 
        mutate_at(2:4, round, 0) %>%
        rename_at(2:4 ,function(x) paste0("Pred_", x))
      
      if(aux$flag == 1) pred_n <- pred_n %>% select(-c("Pred_q25", "Pred_q975"))
      
      data.out <- bind_rows(data, pred_n)
      data.out <- inner_join(data.out, aux$mu_plot)
      
      write.csv(data.out, con, row.names = FALSE)
    }
  )
  
  output$downloadData_LTsummary <- downloadHandler(
    filename = function() {
      ## Get last date from prediction data
      pred_n <- predLT_n()
      last_date_n <- min(pred_n$lt_predict$date) - 1
      
      file <- paste0("LTpredSummary_", isolate(input$country),
                     ifelse(isolate(input$state) == "<all>", "", paste0("_", isolate(input$state))),
                     switch(isolate(input$metrics_LT), Deaths = "_d_", Confirmed = "_n_"),
                     format(last_date_n, format = "%d%m%Y"), ".csv")
      
      return(file)
    },
    content = function(con) {
      ## Load observed & prediction data
      aux <- predLT_n()
      summary <- aux$lt_summary
      summary$NTC500 <- round(summary$NTC500,0)
      
      if(aux$flag != 1){
        summary$NTC25 <- round(summary$NTC25, 0)
        summary$NTC975 <- round(summary$NTC975, 0)
      }
      
      summary <- lapply(summary, function(x) ifelse(is.null(x), "NA", x))
      
      write.csv(summary, con, row.names = FALSE)
    }
  )
  
  ##-- Hiding countries
  ##-- + Hiding countries for new cases
  output$msg_hide_ST = renderUI({
    if((input$country %in% hide_countries_nc & input$metrics_ST == "Confirmed") | (input$country %in% hide_countries_d & input$metrics_ST == "Deaths")) {
      tags$i("Resultados não disponíveis.", br(), "Results not available.", style = "text-align:left; color:red")
    } else{
      tags$h5("", style = "text-align:left")
    }
  })
  
  ##-- + Hiding countries for deaths
  output$msg_hide_LT = renderUI({
    if((input$country %in% hide_countries_nc & input$metrics_LT == "Confirmed") | (input$country %in% hide_countries_d & input$metrics_LT == "Deaths")) {
      tags$i("Resultados não disponíveis.", br(), "Results not available.", style = "text-align:left; color:red")
    } else{
      tags$h5("", style = "text-align:left")
    }
  })
  
  ##-- + Prediction evolution
  data_pred_hist <- eventReactive(c(input$country, input$state, input$metrics_LT), {
    url <- switch(input$country,
                  "Brazil" = url("https://github.com/thaispaiva/app_COVID19/blob/master/STvideos/Brazil_n.rds?raw=true"),
                  #"China" = url("https://github.com/thaispaiva/app_COVID19/blob/master/STvideos/China_n.rds?raw=true"),
                  "Italy" = url("https://github.com/thaispaiva/app_COVID19/blob/master/STvideos/Italy_n.rds?raw=true"),
                  "Spain" = url("https://github.com/thaispaiva/app_COVID19/blob/master/STvideos/Spain_n.rds?raw=true")#,
                  #"US" = url("https://github.com/thaispaiva/app_COVID19/blob/master/STvideos/US_n.rds?raw=true")
    )
    
    if(is.null(url)) return(NULL)
    if(input$state != "<all>") return(NULL)
    if(input$metrics_LT != "Confirmed") return(NULL)
    
    data_slider <- readRDS(file = url)
    
    names(data_slider) <- sapply(data_slider, function(x) as.character(x$last_date))
    
    return(data_slider)
  }, ignoreNULL = TRUE)
  
  observeEvent(c(input$country, input$state, input$metrics_LT), {
    data_slider <- data_pred_hist()
    
    if(!is.null(data_slider)) {
      dt_ini <- min(names(data_slider))
      dt_fim <- max(names(data_slider))
      
      # updateSliderTextInput(session = session, inputId = "slider", label = "", choices = names(data_slider), selected = dt_fim)
      updateSliderInput(session = session, inputId = "slider", label = "", min = as.Date(dt_ini), max = as.Date(dt_fim), value = as.Date(dt_fim))
      #updateNumericInput(session = session, inputId = "show_video", label = NULL, value = 1L)
    } else{
      #updateNumericInput(session = session, inputId = "show_video", label = NULL, value = 0L)
    }
  })
  
  observeEvent(c(input$country, input$state, input$metrics_LT, input$scale, input$fixed_scale, input$slider, input$tabset_covid), {
    if(input$tabset_covid == "tab_longo") {
      
      output$LTpred_video <- renderPlotly({
        plot_video(data = data(), 
                   data_pred_hist = data_pred_hist(),
                   country_name = input$country, 
                   state_name = input$state, 
                   metrics = input$metrics_LT, 
                   scale = input$scale,
                   fixed_scale = input$fixed_scale,
                   varPrefix = "New", 
                   legendPrefix = "", 
                   yaxisTitle = "", 
                   frame = input$slider) 
      })
    }
  })
}