## Aplicativo Shiny - Modelo de Predição de Casos de Coronavírus
## DEST/UFMG - mar/2020

## Código do servidor
# Sys.setlocale("LC_TIME", "pt-br")

##############################################################################
#########################
## REACTIVE SERVER CODE

server = function(input, output, session) {
  
  ## Add message on startup
  showModal(modalDialog(
    div(p("Caso esteja acessando pelo celular, o aplicativo é melhor visualizado com o aparelho na horizontal"),#tags$br,
        p("If you are on mobile, the application is best viewed with the device on horizontal.")),
    easyClose = T,
    footer = tagList(
      modalButton("Ok")
    )
  ))
  
  ###################
  ## Download
  ## Downloading methodology
  
  output$material_covid <- downloadHandler(
    filename = function() {
      "theory.pdf"
    },
    content = function(con) {
      file.copy(from = "www/Covid19UFMG.pdf", con)
    }
  )
  
  ###################
  ## Markdown
  ## Load markdown just if it is necessary
  
  observeEvent(input$covid_navbar, {
    if(input$covid_navbar == "teoria"){
      output$markdown <- renderUI({
        withMathJax(includeMarkdown("www/CoronaUFMG_MD.Rmd"))
      })
    }
  })
  
  ###################
  ## OBSERVED DATA
  ## Load data depending on the country/region selected
  
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
    } else { # otherwise, group by all states
      d = d %>% 
        group_by(date) %>% 
        summarise_at(c("CumConfirmed","CumDeaths"), sum) #,"CumRecovered"), sum)
    }
    d %>% # create new variables
      mutate(
        dateStr = format(date, format="%d/%b"),
        NewConfirmed=CumConfirmed - lag(CumConfirmed, default=0),
        # NewRecovered=CumRecovered - lag(CumRecovered, default=0),
        NewDeaths=CumDeaths - lag(CumDeaths, default=0)
      )
  })
  
  ## OBSERVED DATA
  ## Observe if there is any change on the dropdown menu ( -> show states)
  
  observeEvent(input$country, {
    if(input$country == "Brazil"){ # if country is Brazil
      states = brData %>% pull(`Province/State`)
    } else{ # if other countries
      states = allData %>%
        filter(`Country/Region` == input$country) %>% 
        pull(`Province/State`)
    }
    states = c("<all>", sort(unique(states)))
    updateSelectInput(session, "state", choices=states, selected=states[1])
  })
  
  ## Setup the list of countries - OBSERVED DATA
  countries = sort(unique(allData$`Country/Region`))
  
  ## Start session with Brazil selected
  updateSelectInput(session, "country", choices=countries, 
                    selected="Brazil")
  
  ################
  ## ST prediction
  ## Load (observed) data depending on the country/region selected
  
  data_pred = reactive({
    if(input$country_STpred == "Brazil"){ # if selected Brazil
      d = brData %>%
        arrange(`Province/State`, date)
    } else{ # otherwise, filter by selected country
      d = allData %>%
        filter(`Country/Region` == input$country_STpred)
    }
    if(input$state_STpred != "<all>") {
      d = d %>% 
        filter(`Province/State` == input$state_STpred) # filter by selected state
    } else { # otherwise, group by all states
      d = d %>% 
        group_by(date) %>% 
        summarise_at(c("CumConfirmed","CumDeaths"), sum) #,"CumRecovered"), sum)
    }
    d %>% # create new variables
      mutate(
        dateStr = format(date, format="%d/%b"),
        NewConfirmed=CumConfirmed - lag(CumConfirmed, default=0),
        # NewRecovered=CumRecovered - lag(CumRecovered, default=0),
        NewDeaths=CumDeaths - lag(CumDeaths, default=0)
      )
  })
  
  ## Load ST prediction results depending on the country/region selected
  pred_n = reactive({
    country_name = input$country_STpred
    country_name = gsub(" ","-", country_name) # remove space from country name
    
    metric = ifelse(input$metrics_ST=="Confirmed","n","d") # which results to load
    
    if(country_name=="Brazil"){
      state = input$state_STpred
      if(state != "<all>"){
        if(exists(paste0(country_name,"_",state,"_",metric))){
          pred_n = get(paste0(country_name,"_",state,"_",metric))
        }else{
          pred_n = readRDS(url(paste0(githubURL,"/",country_name,"_",state,"_",metric,"e.rds")))
          assign(paste0(country_name,"_",state,"_",metric), pred_n)  # create country_name object
        }
      }else{
        if(exists(paste0(country_name,"_",metric))){
          pred_n = get(paste0(country_name,"_",metric))
        }else{
          pred_n = readRDS(url(paste0(githubURL,"/",country_name,"_",metric,".rds")))
          assign(paste0(country_name,"_",metric), pred_n)  # create country_name object
        }
      }
    }else{
      if(exists(paste0(country_name,"_",metric))){
        pred_n = get(paste0(country_name,"_",metric))
      }else{
        pred_n = readRDS(url(paste0(githubURL,"/",country_name,"_",metric,".rds")))
        assign(paste0(country_name,"_",metric), pred_n)  # create country_name object
      }
    }
    
    closeAllConnections()
    
    ## select only object 'df_predict' from the list
    pred_n = pred_n$df_predict
    pred_n$date = as.Date(pred_n$date)  # format date
    return(pred_n)
  })
  
  ## ST prediction
  ## Observe if there is any change on the dropdown menu ( -> show states)
  observeEvent(input$country_STpred, {
    if(input$country_STpred == "Brazil"){ # if country is Brazil
      states = statesBR_STpred
    } else{ # if other countries
      states = "<all>"
    }
    # states = c("<all>", sort(unique(states)))
    updateSelectInput(session, "state_STpred", choices=states, selected=states[1])
  })
  
  ## Setup the list of countries - ST prediction
  updateSelectInput(session, "country_STpred",
                    choices=countries_STpred_orig, selected="Brazil")
  updateSelectInput(session, "state_STpred", 
                    choices=NULL, selected=NULL)
  
  ################
  ## LT prediction
  ## Load (observed) data depending on the country/region selected
  data_predLT = reactive({
    if(input$country_LTpred == "Brazil"){ # if selected Brazil
      d = brData %>%
        arrange(`Province/State`, date)
    } else{ # otherwise, filter by selected country
      d = allData %>%
        filter(`Country/Region` == input$country_LTpred)
    }
    if(input$state_LTpred != "<all>") {
      d = d %>% 
        filter(`Province/State` == input$state_LTpred) # filter by selected state
    } else { # otherwise, group by all states
      d = d %>% 
        group_by(date) %>% 
        summarise_at(c("CumConfirmed","CumDeaths"), sum) #,"CumRecovered"), sum)
    }
    d %>% # create new variables
      mutate(
        dateStr = format(date, format="%d/%b"),
        NewConfirmed=CumConfirmed - lag(CumConfirmed, default=0),
        # NewRecovered=CumRecovered - lag(CumRecovered, default=0),
        NewDeaths=CumDeaths - lag(CumDeaths, default=0)
      )
  })
  
  ## Load LT prediction results depending on the country/region selected
  predLT_n = reactive({
    country_name = input$country_LTpred
    country_name = gsub(" ","-", country_name) # remove space from country name
    
    if(country_name == "") return(NULL)
    
    metric = ifelse(input$metrics_LT=="Confirmed","n","d") # which results to load
    
    if(country_name=="Brazil"){
      state = input$state_LTpred
      if(state != "<all>"){
        if(exists(paste0(country_name,"_",state,"_",metric))){
          pred_n = get(paste0(country_name,"_",state,"_",metric))
        }else{
          pred_n = readRDS(url(paste0(githubURL,"/",country_name,"_",state,"_",metric,"e.rds")))
          assign(paste0(country_name,"_",state,"_",metric), pred_n)  # create country_name object
        }
      }else{
        if(exists(paste0(country_name,"_",metric))){
          pred_n = get(paste0(country_name,"_",metric))
        }else{
          pred_n = readRDS(url(paste0(githubURL,"/",country_name,"_",metric,".rds")))
          assign(paste0(country_name,"_",metric), pred_n)  # create country_name object
        }
      }
    }else{
      if(exists(paste0(country_name,"_",metric))){
        pred_n = get(paste0(country_name,"_",metric))
      }else{
        pred_n = readRDS(url(paste0(githubURL,"/",country_name,"_",metric,".rds")))
        assign(paste0(country_name,"_",metric), pred_n)  # create country_name object
      }
    }
    
    closeAllConnections()
    
    pred_n$lt_predict$date = as.Date(pred_n$lt_predict$date)
    pred_n$df_predict = NULL  # remove df_predict object
    return(pred_n)
  })
  
  ## Observe if there is any change on the dropdown menu (show states) - LTpred
  observeEvent(input$country_LTpred, {
    if(input$country_LTpred == "Brazil"){ # if country is Brazil
      states = statesBR_LTpred
    } else{ # if other countries
      states = "<all>"
    }
    # states = c("<all>", sort(unique(states)))
    updateSelectInput(session, "state_LTpred", choices=states, selected=states[1])
  })
  
  ## Setup the list of countries - LT prediction
  updateSelectInput(session, "country_LTpred",
                    choices=countries_LTpred_orig, selected="Brazil")
  updateSelectInput(session, "state_LTpred", 
                    choices=NULL, selected=NULL)
  
  #####################################################################
  ## GRAPHS
  
  ################
  ## OBSERVED DATA
  ## Plot for observed data (confirmed, deaths, recovered)
  renderPlot_obs = function(varPrefix, legendPrefix, yaxisTitle) {
    ## plotly function for interactive plot
    renderPlotly({
      ## load observed data based on input
      data = data()
      if(nrow(data) == 0) return(NULL) ## DOUGLAS
      ## create last date
      last_date_n = max(data$date)
      
      ## create title with country/state name
      title = paste0(input$country,
                     ifelse(input$state == "<all>","",paste0(" / ",input$state)))
      
      png_title = paste0("Data_",input$country,
                         ifelse(input$state == "<all>","",paste0("_",input$state,"_")),
                         format(last_date_n, format="%d%m%Y")
      )
      
      plt = data %>%
        plot_ly() %>%  # first, make empty plot and setup axis and legend
        plotly::config(displayModeBar=TRUE,  # include ModeBar
                       displaylogo=FALSE,    # remove some buttons to simplify
                       modeBarButtonsToRemove = c("lasso2d","select2d","toggleSpikelines","hoverCompareCartesian",
                                                  "hoverClosestCartesian", "autoScale2d"),
                       toImageButtonOptions = list(format = "png",
                                                   filename = png_title )) %>% #, locale='pt-br') %>%
        layout(
          ## add country and state to title
          title = list(text = paste0("<b>",title,"<b>")),
          ## add pred dates to the x axis
          xaxis=list(
            title="", tickangle=-90, 
            tick0=data$dateStr[1],dtick=7*86400000, # add ticks every week
            tickformat="%d/%b"
          ),
          yaxis=list(title=yaxisTitle, 
                     type=if_else(input$scale==1,"log","linear"), # add option for log scale
                     hoverformat='.0f', hoverinfo="x+y"), # show only date/value on hover
          legend=list(x=0.03,y=0.97, # legend position
                      bgcolor='rgba(240,240,240,0.5)'),
          font=list(family="Arial",size=10),
          dragmode = FALSE, # to avoid zoom when the graph starts
          modebar = list(orientation = "v") # modebar orientation
        ) %>%
        ## add update date
        add_annotations(text=paste0("Atualizado em/Updated on ",printDate(last_date_n)),
                        x = 0.99, y = 0, xref="paper", yref="paper",
                        font=list(family="Arial",size=10), align="right",
                        showarrow=FALSE)
      
      ## after, add lines for each metric from input
      for(metric in input$metrics){
        ## portuguese labels for legend
        metric_pt = switch(metric, Deaths="Mortes/Deaths",
                           Recovered="Recuperados/Recovered Cases",
                           Confirmed="Casos confirmados/Confirmed cases")
        plt = plt %>%
          add_trace(
            x=~date, y=data[[paste0(varPrefix, metric)]],
            type='scatter', mode='lines+markers', hoverinfo="x+y",
            name= metric_pt,
            marker=list(
              color = switch(metric, Deaths=red, Confirmed=blu),
              line=list(color = switch(metric, Deaths=dred, Confirmed=dblu),width=1)),
            line=list(
              color = switch(metric, Deaths=dred, Confirmed=blu),
              width=1.5)
          )
      }
      plt
    })
  }
  
  # ## add title bar with country name - New Cases
  # output$plotTitle_daily <- renderUI({
  #   title = paste0(input$country,ifelse(input$state == "<all>","",paste0(" / ",input$state)))
  #   div(style='text-align:center; font-size:15px; 
  #       font-family:"Open Sans",arial,sans-serif; font-weight:bold', 
  #       "Novos Casos/New Cases - ",title)
  # })
  # 
  # ## add title bar with country name - Cumulated Cases
  # output$plotTitle_cum <- renderUI({
  #   title = paste0(input$country,ifelse(input$state == "<all>","",paste0(" / ",input$state)))
  #   div(style= 'text-align:center; font-size:15px;
  #       font-family:"Open Sans",arial,sans-serif; font-weight:bold', 
  #       "Casos Acumulados/Cumulated Cases - ",title)
  # })
  
  #########################
  ## Plot for ST prediction
  renderPlot_STpred = function(varPrefix, legendPrefix, yaxisTitle, pred_time) {
    ## plotly function for interactive plot
    renderPlotly({
      ## load observed data based on input
      data = data_pred()
      if(nrow(data) == 0) return(NULL) ## DOUGLAS
      ## load prediction data
      pred_n = pred_n()
      
      ## create last date and format dates for prediction
      last_date_n = min(pred_n$date)-1
      pred_dateStr = format(pred_n$date, format="%d/%b")
      
      metric = input$metrics_ST
      metric_leg = switch(metric, Deaths="Deaths", Confirmed="Cases") 
      ## portuguese labels for legend
      metric_pt = switch(metric, Deaths="Mortes/Deaths",
                         Confirmed="Casos Confirmados/Confirmed Cases")
      
      title = paste0(input$country_STpred,
                     ifelse(input$state_STpred == "<all>","",paste0(" / ",input$state_STpred)))
      metric_tit = ifelse(input$metrics_ST == "Confirmed", "Casos confirmados/Confirmed cases",
                          "Mortes/Deaths")
      
      png_title = paste0("STpred_",input$country_STpred,
                         ifelse(input$state_STpred == "<all>","",paste0("_",input$state_STpred)),
                         switch(metric, Deaths="_d_", Confirmed="_n_"),
                         format(last_date_n, format="%d%m%Y")
      )
      
      plt = data %>%
        plot_ly() %>%  # first, make empty plot and setup axis and legend
        plotly::config(displayModeBar=TRUE,  # include ModeBar
                       displaylogo=FALSE,    # remove some buttons to simplify
                       modeBarButtonsToRemove = c("lasso2d","select2d","toggleSpikelines","hoverCompareCartesian",
                                                  "hoverClosestCartesian", "autoScale2d"),
                       toImageButtonOptions = list(format = "png",
                                                   filename = png_title )) %>% #, locale='pt-br') %>%
        layout(
          ## title of ST pred graph
          title = list(text = paste0("<b>",title," - ",metric_tit,"<b>")),
          xaxis=list(
            title="",
            tickangle=-90, #type='category',
            ## add pred dates to the x axis
            tick0=data$dateStr[31],dtick=7*86400000, # add ticks every week
            tickformat="%d/%b"
          ),
          yaxis=list(title=yaxisTitle, type=if_else(input$scale_STpred==1,"log","linear"),
                     hoverformat='.0f', hoverinfo="x+y"), 
          legend=list(x=0.03, y=0.97,bgcolor='rgba(240,240,240,0.5)',
                      font=f1),
          # title=list(text=paste0("<b>",metric_leg,"</b>"))),
          font=f1,
          # add vertical line on last observed day
          shapes=list(type="line", opacity=0.7, line=list(color="black", width=1),
                      y0=0, y1=1, yref="paper",
                      x0=last_date_n, x1=last_date_n),
          dragmode = FALSE,
          modebar = list(orientation = "v")
        ) %>%
        add_annotations(text=paste0("Atualizado em/Updated on ",printDate(last_date_n)),
                        #format(last_date_n, format="%d/%b")), # format="%d/%m/%Y")),
                        x = 0.99, y = 0, xref="paper", yref="paper",
                        font=list(family="Arial",size=10), align="right",
                        showarrow=FALSE)
      
      plt = plt %>%
        ## add lines for observed data
        add_trace(
          x=data$date[which(data$date<=last_date_n)][-c(1:30)], # removing 1st 30 days
          y=data[[paste0(varPrefix, metric)]][which(data$date<=last_date_n)][-c(1:30)], 
          type='scatter', mode='lines+markers', hoverinfo="x+y", 
          # name=metric_pt,
          name="Observed Data",
          marker=list(
            color = switch(metric, Deaths=red, Confirmed=blu),
            line=list(color = switch(metric, Deaths=dred, Confirmed=dblu),width=1)),
          line=list(
            color = switch(metric, Deaths=dred, Confirmed=blu),
            width=1.5)
        ) %>% 
        ## add 2,5% and 97,5% quantiles
        add_trace(
          x=c(data$date[which(data$date==last_date_n)], # to connect with last observed point
              pred_n$date[1:input$pred_time]),
          y=c(data[[paste0(varPrefix, metric)]][which(data$date==last_date_n)],
              pred_n$q25[1:input$pred_time]),
          showlegend=F,
          # name=paste("95% IC - ",metric_pt,"CI"),
          name = "95% CI",
          type='scatter', #mode = 'none',
          mode='lines', hoverinfo="x+y", 
          fillcolor='rgba(150,150,150,0.5)',
          line=list(color='rgba(0,0,0,1)', width=0) #, dash='dot')
        ) %>%
        add_trace(
          x=c(data$date[which(data$date==last_date_n)], # to connect with last observed point
              pred_n$date[1:input$pred_time]),
          y=c(data[[paste0(varPrefix, metric)]][which(data$date==last_date_n)],
              pred_n$q975[1:input$pred_time]),
          type='scatter', #mode = 'none',
          mode='lines', hoverinfo="x+y", 
          fill='tonexty',
          # showlegend=F,
          # name=paste("95% IC - ",metric_pt,"CI"),
          name = "95% CI",
          fillcolor='rgba(150,150,150,0.5)',
          line=list(color='rgba(0,0,0,1)', width=0)# , dash='dot')
        ) %>%
        ## add median of prediction
        add_trace(
          x=c(data$date[which(data$date==last_date_n)], # to connect with last observed point
              pred_n$date[1:input$pred_time]),
          y=c(data[[paste0(varPrefix, metric)]][which(data$date==last_date_n)],
              pred_n$med[1:input$pred_time]),
          type='scatter', mode='lines+markers', hoverinfo="x+y", 
          # name=paste("Previsão", metric_pt, "Prediction"),
          name="Prediction",
          marker=list(color='rgb(0,0,0)', size=4), line=list(color='rgb(0,0,0)', dash='dot')
        )
      plt
    })
  }
  
  # ## add title bar with country name - ST prediction
  # output$plotTitle <- renderUI({
  #   title = paste0(input$country_STpred,
  #                  ifelse(input$state_STpred == "<all>","",paste0(" / ",input$state_STpred)))
  #   metric = ifelse(input$metrics_ST == "Confirmed", "Casos confirmados/Confirmed Cases",
  #                   "Mortes/Deaths")
  #   div(style= 'text-align:center; font-size:15px;
  #       font-family:"Open Sans",arial,sans-serif; font-weight:bold',
  #       metric, " - ",title
  #   )
  # })
  
  #########################
  ## Plot for LT prediction
  renderPlot_LTpred = function(varPrefix, legendPrefix, yaxisTitle) {
    ## plotly function for interactive plot
    renderPlotly({
      ## load observed data based on input
      data = data_predLT()
      if(nrow(data) == 0) return(NULL) ## DOUGLAS
      ## load prediction data
      aux = predLT_n()
      pred_n = aux$lt_predict
      pred_summary = aux$lt_summary
      mu_plot = aux$mu_plot
      flag = aux$flag
      
      ## create last date, and format dates for prediction
      last_date_n = min(pred_n$date)-1
      pred_dateStr = format(pred_n$date, format="%d/%b")
      
      # varPrefix = "New"; legendPrefix = ""
      metric = input$metrics_LT
      # metric_leg = switch(metric, Deaths="Deaths", Confirmed="Cases")
      metric_leg = switch(metric, Deaths="deaths", Confirmed="cases") 
      ## portuguese labels for legend
      metric_pt = switch(metric, Deaths="Deaths", #Deaths="Mortes/Deaths",
                         Confirmed="Confirmed Cases") # Confirmed="Casos Confirmados/Confirmed Cases")
      
      title = paste0(input$country_LTpred,
                     ifelse(input$state_LTpred == "<all>","",paste0(" / ",input$state_LTpred)))
      metric_tit = ifelse(input$metrics_LT == "Confirmed", "Casos confirmados/Confirmed cases",
                          "Mortes/Deaths")
      png_title = paste0("LTpred_",input$country_LTpred,
                         ifelse(input$state_LTpred == "<all>","",paste0("_",input$state_LTpred)),
                         switch(metric, Deaths="_d_", Confirmed="_n_"),
                         format(last_date_n, format="%d%m%Y")
      )
      
      plt = data %>%
        plot_ly() %>%  # first, make empty plot and setup axis and legend
        plotly::config(displayModeBar=TRUE,
                       displaylogo=FALSE,
                       modeBarButtonsToRemove = c("lasso2d","select2d","toggleSpikelines","hoverCompareCartesian",
                                                  "hoverClosestCartesian", "autoScale2d"),
                       toImageButtonOptions = list(format = "png",
                                                   filename = png_title )) %>% #, locale='pt-br') %>%
        layout(
          title = list(text = paste0("<b>",title," - ",metric_tit,"<b>")),
          annotations = list(text = paste0("<span style=\"line-height: 40px;\">",
                                           "<b>Peak 95% CI:</b> ",
                                           ifelse(is.null(pred_summary$high.dat.low)|is.null(pred_summary$high.dat.upper),
                                                  "NA",
                                                  paste0("(", printDate(pred_summary$high.dat.low),", ",
                                                         printDate(pred_summary$high.dat.upper), ")")),
                                           "<br><b>Total number of ",metric_leg,":</b> ",
                                           round(pred_summary$NTC500,0), "<br>",
                                           "<b>95% CI:</b> ",
                                           ifelse(flag==1, "NA",
                                                  paste0("(", round(pred_summary$NTC25,0),
                                                         ", ", round(pred_summary$NTC975,0),")")),
                                           "<br><b>End (99%) of ",metric_leg,":</b> ",
                                           printDate(pred_summary$end.dat.med),
                                           "<br><b>95% CI:</b> ",
                                           ifelse(is.null(pred_summary$end.dat.low)|is.null(pred_summary$end.dat.upper),
                                                  "NA",
                                                  paste0("(", printDate(pred_summary$end.dat.low),", ",
                                                         printDate(pred_summary$end.dat.upper),")" ) ),
                                           "</span>"
          ),
          x = 0.97, y = 0.97, xref="paper", yref="paper",
          font=list(family="Arial",size=10), align="right",
          showarrow=FALSE),
          xaxis=list(
            title="",
            tickangle=-90, #type='category',
            ## add pred dates to the x axis
            tick0=data$dateStr[1],dtick=14*86400000, # add ticks every week
            tickformat="%d/%b"
          ),
          yaxis=list(title=yaxisTitle, type=if_else(input$scale_LTpred==1,"log","linear"),
                     hoverformat='.0f', hoverinfo="x+y"),
          legend=list(x=0.03, y=0.97,bgcolor='rgba(240,240,240,0.5)',
                      font=list(family="Arial",size=10) #,
                      # title=list(text=paste0("<b>",metric_pt,"</b>"))
          ),
          font=f1,
          # add vertical line on last observed day
          shapes=list(type="line", opacity=0.7, line=list(color="black", width=1),
                      y0=0, y1=1, yref="paper",
                      x0=last_date_n, x1=last_date_n),
          margin = list(t=20),
          dragmode = FALSE,
          modebar = list(orientation = "v")
        ) %>%
        add_annotations(text=paste0("Atualizado em/Updated on ",printDate(last_date_n)),
                        #format(last_date_n, format="%d/%b")), # format="%d/%m/%Y")),
                        x = 0.99, y = 0, xref="paper", yref="paper",
                        font=list(family="Arial",size=10), align="right",
                        showarrow=FALSE)
      
      
      # browser()
      plt = plt %>%
        ## add mu line
        add_trace(
          # x=c(data$date[-c(1:(length(data$date)+length(pred_n$date)-length(mu_plot)))],
          #     pred_n$date),
          x = mu_plot$date, y = mu_plot$mu,
          # y=c(mu_plot),
          type='scatter', mode='lines', hoverinfo="none", # "x+y",
          # name=paste(metric_pt, "Estimated Mean"),
          name=("Estimated Mean"),
          line=list(color='rgb(230,115,0)', dash='solid', width=2.5)
        ) %>%
        ## add lines for observed data
        add_trace(
          x=data$date[which(data$date<=last_date_n)],#[-c(1:30)], # removing 1st 30 days
          y=data[[paste0(varPrefix, metric)]][which(data$date<=last_date_n)],#[-c(1:30)], 
          type='scatter', mode='lines+markers', hoverinfo="x+y",
          name="Observed Data",
          marker=list(
            color = switch(metric, Deaths=red, Confirmed=blu),
            line=list(color = switch(metric, Deaths=dred, Confirmed=dblu),width=1)),
          line=list(
            color = switch(metric, Deaths=dred, Confirmed=blu),
            width=1.5)
        )
      if(flag!=1){
        plt = plt %>%
          ## add 2,5% and 97,5% quantiles
          add_trace(
            x=c(data$date[which(data$date==last_date_n)], # to connect with last observed point
                pred_n$date),
            y=c(data[[paste0(varPrefix, metric)]][which(data$date==last_date_n)],
                pred_n$q25),
            showlegend=F,
            # name=paste("95% CI - ",metric_pt),
            name="95% CI",
            type='scatter', #mode = 'none',
            mode='lines', hoverinfo="x+y",
            fillcolor='rgba(150,150,150,0.5)',
            line=list(color='rgba(0,0,0,1)', width=0) #, dash='dot')
          ) %>%
          add_trace(
            x=c(data$date[which(data$date==last_date_n)], # to connect with last observed point
                # pred_n$date[1:input$pred_time]),
                pred_n$date),
            y=c(data[[paste0(varPrefix, metric)]][which(data$date==last_date_n)],
                # pred_n$q975[1:input$pred_time]),
                pred_n$q975),
            type='scatter', #mode = 'none',
            mode='lines', hoverinfo="x+y",
            fill='tonexty',
            # name=paste("95% CI - ",metric_pt),
            name="95% CI",
            fillcolor='rgba(150,150,150,0.5)',
            line=list(color='rgba(0,0,0,1)', width=0)# , dash='dot')
          )
      }
      plt = plt %>%
        ## add median of prediction
        add_trace(
          x=c(data$date[which(data$date==last_date_n)], # to connect with last observed point
              pred_n$date),
          y=c(data[[paste0(varPrefix, metric)]][which(data$date==last_date_n)],
              pred_n$med),
          type='scatter', mode='lines+markers', hoverinfo="x+y",
          # name=paste(metric_pt, "Prediction"),
          name="Prediction",
          marker=list(color='rgb(0,0,0)', size=4), line=list(color='rgb(0,0,0)', dash='dot')
        ) 
      plt
    })
  }
  
  ## Add title bar with country name - LT prediction
  output$plotTitle_LT <- renderUI({
    title = paste0(input$country_LTpred,
                   ifelse(input$state_LTpred == "<all>","",paste0(" / ",input$state_LTpred)))
    metric = ifelse(input$metrics_LT == "Confirmed", "Casos confirmados/Confirmed Cases",
                    "Mortes/Deaths")
    div(style= 'text-align:center; font-size:15px;
        font-family:"Open Sans",arial,sans-serif; font-weight:bold',
        metric, " - ",title
    )
  })
  
  ## hide plot -> if lt_summary$flag=2
  ## observeEvent(c(input$country_LTpred, input$state_LTpred), {
  observe({
    ## add code to read results and get flag
    ## get flag from confirmed or deaths!
    flag = predLT_n()$flag
    ## then change if condition to if(flag==2)
    # if(input$country_LTpred == "Brazil" & input$state_LTpred %in% statesBR2rem) {
    if(flag == 2){
      updateSelectInput(session, "show_plotLT", choices = c(TRUE, FALSE), selected = FALSE)
    } else{
      updateSelectInput(session, "show_plotLT", choices = c(TRUE, FALSE), selected = TRUE)
    }
  })
  
  ###################################################################
  
  ## OBSERVED DATA GRAPHS
  output$dailyMetrics = renderPlot_obs(
    varPrefix = "New", # legendPrefix="New", yaxisTitle="New Cases per Day")
    legendPrefix="", yaxisTitle="Novos Casos por Dia/New Cases per Day")
  output$cumulatedMetrics = renderPlot_obs(
    varPrefix = "Cum", # legendPrefix="Cumulated", yaxisTitle="Cumulated Cases")
    legendPrefix="", yaxisTitle="Casos Acumulados/Cumulated Cases")
  
  ## SHORT TERM PREDICTION GRAPH
  output$STpred = renderPlot_STpred(
    varPrefix = "Cum", legendPrefix = "", 
    yaxisTitle = "Casos Acumulados/Cumulated Cases",
    input$pred_time
  )
  
  ## LONG TERM PREDICTION GRAPH
  output$LTpred = renderPlot_LTpred(
    varPrefix = "New", legendPrefix = "", 
    yaxisTitle = "Novos Casos por Dia/New Cases per Day"
  )
}
