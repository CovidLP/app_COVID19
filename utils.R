##-- Plot for observed data (confirmed, deaths, recovered)
plot_obs <- function(data,
                     country_name, state_name, metrics, scale, 
                     varPrefix, legendPrefix, yaxisTitle) {
  ## Last date
  last_date_n <- max(data$date)
  
  ## Title with country/state name
  title = paste0(country_name, ifelse(state_name == "<all>", "", paste0(" / ", state_name)))
  
  png_title = paste0(
    "Data_", 
    country_name,
    ifelse(state_name == "<all>", "", paste0("_", state_name, "_")),
    format(last_date_n, format = "%d%m%Y")
  )
  
  plt <- data %>%
    plot_ly() %>%
    plotly::config(displayModeBar = TRUE,  # Include ModeBar
                   displaylogo = FALSE,    # Remove some buttons to simplify
                   modeBarButtonsToRemove = c("lasso2d", "select2d", 
                                              "toggleSpikelines", "hoverCompareCartesian",
                                              "hoverClosestCartesian", "autoScale2d"),
                   toImageButtonOptions = list(format = "png", filename = png_title)) %>%
    layout(
      ## Add country and state to title
      title = list(text = paste0("<b>", title, "<b>")),
      ## Add pred dates to the x axis
      xaxis = list(
        title = "", tickangle = -90, 
        tick0 = data$dateStr[1], dtick = 7*86400000, # Add ticks every week
        tickformat = "%d/%b"
      ),
      yaxis = list(
        title = yaxisTitle, 
        type = if_else(scale == 1, "log", "linear"), # Add option for log scale
        hoverformat = '.0f', hoverinfo = "x+y"
      ),           # Show only date/value on hover
      legend = list(
        x = 0.03, 
        y = 0.97,                             # Legend position
        bgcolor = 'rgba(240, 240, 240, 0.5)'
      ),
      font = list(family = "Arial", size = 10),
      dragmode = FALSE,                                             # To avoid zoom when the graph starts
      modebar = list(orientation = "v")                             # Modebar orientation
    ) %>%
    ## Add update date
    add_annotations(text = paste0("Atualizado em/Updated on ", printDate(last_date_n)),
                    x = 0.99, y = 0, xref = "paper", yref = "paper",
                    font = list(family = "Arial", size = 10), align = "right",
                    showarrow = FALSE) %>%
    add_annotations(text = "Source: www.est.ufmg.br/covidlp",
                    x = 0, y = 0, xref = "paper", yref = "paper",
                    font = list(family = "Arial", size = 10, color =' rgba(128, 128, 128, 0.5)'), align = "left",
                    showarrow = FALSE)
  
  ## After, add lines for each metric from input
  for(metric in metrics) {
    ## Portuguese labels for legend
    metric_pt <- switch(
      metric, 
      Deaths = "Mortes/Deaths",
      Recovered = "Recuperados/Recovered Cases",
      Confirmed = "Casos confirmados/Confirmed cases"
    )
    
    plt <- plt %>%
      add_trace(
        x = ~date, y = data[[paste0(varPrefix, metric)]],
        type = 'scatter', mode = 'lines+markers', hoverinfo = "x+y",
        name = metric_pt,
        marker = list(
          color = switch(metric, Deaths = red, Confirmed = blu),
          line = list(color = switch(metric, Deaths = dred, Confirmed = dblu), width = 1),
          size = 5
        ),
        line = list(
          color = switch(metric, Deaths = dred, Confirmed = blu),
          width = 1.5
        )
      )
  }
  
  return(plt)
}

##-- Plot for ST prediction
plot_STpred <- function(data, predST_n,
                        country_name, state_name, metrics, scale, pred_time,
                        varPrefix, legendPrefix, yaxisTitle) {
  ## Create last date and format dates for prediction
  last_date_n <- min(predST_n$date) - 1
  pred_dateStr <- format(predST_n$date, format = "%d/%b")
  
  metric <- metrics
  metric_leg <- switch(
    metric, 
    Deaths = "Deaths", 
    Confirmed = "Cases"
  ) 
  
  ## Portuguese labels for legend
  metric_pt <- switch(
    metric, 
    Deaths = "Mortes/Deaths", 
    Confirmed = "Casos Confirmados/Confirmed Cases"
  )
  
  title <- paste0(country_name,
                  ifelse(state_name == "<all>", "", paste0(" / ", state_name)))
  metric_tit <- ifelse(metric == "Confirmed", "Casos confirmados/Confirmed cases", "Mortes/Deaths")
  
  png_title <- paste0("STpred_", country_name,
                      ifelse(state_name == "<all>", "", paste0("_", state_name)),
                      switch(metric, Deaths = "_d_", Confirmed = "_n_"),
                      format(last_date_n, format = "%d%m%Y")
  )
  
  plt <- data %>%
    plot_ly() %>%  
    plotly::config(displayModeBar = TRUE,  # include ModeBar
                   displaylogo = FALSE,    # remove some buttons to simplify
                   modeBarButtonsToRemove = c("lasso2d","select2d",
                                              "toggleSpikelines","hoverCompareCartesian",
                                              "hoverClosestCartesian", "autoScale2d"),
                   toImageButtonOptions = list(format = "png",
                                               filename = png_title)) %>%
    layout(
      ## Title of ST pred graph
      title = list(text = paste0("<b>", title, " - ", metric_tit, "<b>")),
      xaxis = list(
        title = "",
        tickangle = -90,
        ## Add pred dates to the x axis
        tick0 = data$dateStr[31], dtick = 7*86400000, # add ticks every week
        tickformat = "%d/%b"
      ),
      yaxis = list(title = yaxisTitle, type = if_else(scale == 1, "log", "linear"),
                   hoverformat = '.0f', hoverinfo = "x+y"), 
      legend = list(x = 0.03, y = 0.97, bgcolor = 'rgba(240, 240, 240, 0.5)',
                    font = f1),
      font = f1,
      ## Add vertical line on last observed day
      shapes = list(type = "line", opacity = 0.7, line = list(color = "black", width = 1),
                    y0 = 0, y1 = 1, yref = "paper",
                    x0 = last_date_n, x1 = last_date_n),
      dragmode = FALSE,
      modebar = list(orientation = "v")
    ) %>%
    add_annotations(text = paste0("Atualizado em/Updated on ", printDate(last_date_n)),
                    x = 0.99, y = 0, xref = "paper", yref = "paper",
                    font = list(family = "Arial", size = 10), align = "right",
                    showarrow = FALSE) %>%
    add_annotations(text = "Source: www.est.ufmg.br/covidlp",
                    x = 0, y = 0, xref = "paper", yref = "paper",
                    font = list(family = "Arial", size = 10, color = 'rgba(128, 128, 128, 0.5)'), align = "left",
                    showarrow = FALSE)
  
  plt <- plt %>%
    ## Add lines for observed data
    add_trace(
      x = data$date[which(data$date <= last_date_n)][-c(1:30)], # removing 1st 30 days
      y = data[[paste0(varPrefix, metric)]][which(data$date <= last_date_n)][-c(1:30)], 
      type = 'scatter', mode = 'lines+markers', hoverinfo = "x+y", 
      name = "Observed Data",
      marker = list(
        color = switch(metric, Deaths = red, Confirmed = blu),
        line = list(color = switch(metric, Deaths = dred, Confirmed = dblu), width = 1),
        size = 5),
      line = list(
        color = switch(metric, Deaths = dred, Confirmed = blu),
        width = 1.5)
    ) %>% 
    ## Add 2,5% and 97,5% quantiles
    add_trace(
      x = c(data$date[which(data$date == last_date_n)],
            predST_n$date[1:pred_time]),
      y = c(data[[paste0(varPrefix, metric)]][which(data$date == last_date_n)],
            predST_n$q25[1:pred_time]),
      showlegend = F,
      name = "95% CI",
      type = 'scatter', 
      mode = 'lines', hoverinfo = "x+y", 
      fillcolor = 'rgba(150, 150, 150, 0.5)',
      line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
    ) %>%
    add_trace(
      x = c(data$date[which(data$date == last_date_n)], 
            predST_n$date[1:pred_time]),
      y = c(data[[paste0(varPrefix, metric)]][which(data$date == last_date_n)],
            predST_n$q975[1:pred_time]),
      type = 'scatter', 
      mode = 'lines', hoverinfo = "x+y", 
      fill = 'tonexty',
      name = "95% CI",
      # fillcolor = 'rgba(150, 150, 150, 0.5)',
      fillcolor = 'rgba(100, 100, 100, 0.5)',
      line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
    ) %>%
    ## Add median of prediction
    add_trace(
      x = c(data$date[which(data$date == last_date_n)], 
            predST_n$date[1:pred_time]),
      y = c(data[[paste0(varPrefix, metric)]][which(data$date == last_date_n)],
            predST_n$med[1:pred_time]),
      type = 'scatter', mode = 'lines+markers', hoverinfo = "x+y", 
      name = "Prediction",
      # marker = list(color = 'rgb(0, 0, 0)', size = 4),
      marker = list(color = 'rgb(0, 0, 0)', size = 2),
      # line = list(color = 'rgb(0, 0, 0)', dash = 'dot')
      line = list(color = 'rgb(0, 0, 0)', dash = 'dot', width=0.5)
    )
  plt
}

##-- Plot for ST prediction
plot_LTpred <- function(data, predLT_n,
                        country_name, state_name, metrics, scale,
                        varPrefix, legendPrefix, yaxisTitle) {
  
    ## Load prediction data
    pred_n <- predLT_n$lt_predict
    pred_summary <- predLT_n$lt_summary
    mu_plot <- predLT_n$mu_plot
    flag <- predLT_n$flag
    
    ## Create last date, and format dates for prediction
    last_date_n <- min(pred_n$date) - 1
    pred_dateStr <- format(pred_n$date, format = "%d/%b")
    
    metric <- metrics
    metric_leg <- switch(metric, Deaths = "deaths", Confirmed = "cases") 
    
    ## Portuguese labels for legend
    metric_pt <- switch(metric, 
                        Deaths = "Deaths", 
                        Confirmed = "Confirmed Cases")
    
    title <- paste0(country_name, ifelse(state_name == "<all>", "", paste0(" / ", state_name)))
    metric_tit <- ifelse(metrics == "Confirmed", "Casos confirmados/Confirmed cases", "Mortes/Deaths")
    
    png_title <- paste0("LTpred_", country_name,
                        ifelse(state_name == "<all>", "", paste0("_", state_name)),
                        switch(metric, Deaths = "_d_", Confirmed = "_n_"),
                        format(last_date_n, format = "%d%m%Y")
    )
    
    plt <- data %>%
      plot_ly() %>%
      plotly::config(displayModeBar = TRUE,
                     displaylogo = FALSE,
                     modeBarButtonsToRemove = c("lasso2d", "select2d",
                                                "toggleSpikelines", "hoverCompareCartesian",
                                                "hoverClosestCartesian", "autoScale2d"),
                     toImageButtonOptions = list(format = "png",
                                                 filename = png_title )) %>%
      layout(
        title = list(text = paste0("<b>", title, " - ", metric_tit, "<b>")),
        annotations = list(text = paste0("<span style=\"line-height: 40px;\">",
                                         "<b>Peak:</b> ", printDate(pred_summary$high.dat.med),
                                         "<br><b>Peak 95% CI:</b> ",
                                         ifelse(is.null(pred_summary$high.dat.low)|is.null(pred_summary$high.dat.upper),
                                                "NA",
                                                paste0("(", printDate(pred_summary$high.dat.low),", ",
                                                       printDate(pred_summary$high.dat.upper), ")")),
                                         "<br><b>Total number of ", metric_leg, ":</b> ",
                                         round(pred_summary$NTC500,0), "<br>",
                                         "<b>95% CI:</b> ",
                                         ifelse(flag == 1, "NA",
                                                paste0("(", round(pred_summary$NTC25, 0),
                                                       ", ", round(pred_summary$NTC975, 0), ")")),
                                         "<br><b>End (99%) of ", metric_leg, ":</b> ",
                                         printDate(pred_summary$end.dat.med),
                                         "<br><b>95% CI:</b> ",
                                         ifelse(is.null(pred_summary$end.dat.low) | is.null(pred_summary$end.dat.upper),
                                                "NA",
                                                paste0("(", printDate(pred_summary$end.dat.low), ", ",
                                                       printDate(pred_summary$end.dat.upper), ")" )),
                                         "</span>"
        ),
        x = 0.97, y = 0.97, xref = "paper", yref = "paper",
        font = list(family = "Arial", size = 10), align = "right",
        showarrow = FALSE),
        xaxis = list(
          title = "",
          tickangle = -90,
          ## Add pred dates to the x axis
          tick0 = data$dateStr[1], dtick = 14*86400000,
          tickformat = "%d/%b"
        ),
        yaxis = list(title = yaxisTitle, type = if_else(scale == 1, "log", "linear"),
                     hoverformat = '.0f', hoverinfo = "x+y"),
        legend = list(x = 0.03, y = 0.97, bgcolor = 'rgba(240, 240, 240, 0.5)',
                      font = list(family = "Arial", size = 10)
        ),
        font = f1,
        ## Add vertical line on last observed day
        shapes = list(type = "line", opacity = 0.7, line = list(color = "black", width = 1),
                      y0 = 0, y1 = 1, yref = "paper",
                      x0 = last_date_n, x1 = last_date_n),
        margin = list(t = 20),
        dragmode = FALSE,
        modebar = list(orientation = "v")
      ) %>%
      add_annotations(text = paste0("Atualizado em/Updated on ", printDate(last_date_n)),
                      x = 0.99, y = 0, xref = "paper", yref = "paper",
                      font = list(family = "Arial", size = 10), align = "right",
                      showarrow = FALSE) %>%
      add_annotations(text = "Source: www.est.ufmg.br/covidlp",
                      x = 0, y = 0, xref = "paper", yref = "paper",
                      font = list(family = "Arial", size = 10, color = 'rgba(128, 128, 128, 0.5)'), align = "left",
                      showarrow = FALSE)
    
    plt <- plt %>%
      ## Add mu line
      add_trace(
        x = mu_plot$date, y = mu_plot$mu,
        type = 'scatter', mode = 'lines', hoverinfo = "none",
        name = ("Estimated Mean"),
        line = list(color = 'rgb(230, 115, 0)', dash = 'solid',
                    width=1.5) ## width = 2.5)
      ) %>%
      ## Add lines for observed data
      add_trace(
        x = data$date[which(data$date <= last_date_n)],
        y = data[[paste0(varPrefix, metric)]][which(data$date <= last_date_n)], 
        type = 'scatter', mode = 'lines+markers', hoverinfo = "x+y",
        name = "Observed Data",
        marker = list(
          color = switch(metric, Deaths = red, Confirmed = blu),
          line = list(color = switch(metric, Deaths = dred, Confirmed = dblu), width = 1),
          size = 5),
        line = list(
          color = switch(metric, Deaths = dred, Confirmed = blu),
          width = 1.5)
      )
    if(flag != 1){
      plt <- plt %>%
        ## Add 2,5% and 97,5% quantiles
        add_trace(
          x = c(data$date[which(data$date == last_date_n)], pred_n$date),
          y = c(data[[paste0(varPrefix, metric)]][which(data$date == last_date_n)], pred_n$q25),
          showlegend = F,
          name = "95% CI",
          type = 'scatter', 
          mode = 'lines', hoverinfo = "x+y",
          fillcolor = 'rgba(150, 150, 150, 0.5)',
          line = list(color = 'rgba(0, 0, 0, 1)', width = 0) 
        ) %>%
        add_trace(
          x = c(data$date[which(data$date == last_date_n)], pred_n$date),
          y = c(data[[paste0(varPrefix, metric)]][which(data$date == last_date_n)], pred_n$q975),
          type = 'scatter',
          mode = 'lines', hoverinfo = "x+y",
          fill = 'tonexty',
          name = "95% CI",
          # fillcolor = 'rgba(150, 150, 150, 0.5)',
          fillcolor = 'rgba(100, 100, 100, 0.5)',
          line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
        )
    }
    plt <- plt %>%
      ## Add median of prediction
      add_trace(
        x = c(data$date[which(data$date == last_date_n)], pred_n$date),
        y = c(data[[paste0(varPrefix, metric)]][which(data$date == last_date_n)], pred_n$med),
        type = 'scatter', mode = 'lines+markers', hoverinfo = "x+y",
        name = "Prediction",
        # marker = list(color = 'rgb(0,0,0)', size = 4),
        marker = list(color = 'rgb(0,0,0)', size = 2),
        # line = list(color = 'rgb(0, 0, 0)', dash = 'dot')
        line = list(color = 'rgb(0, 0, 0)', dash = 'dot', width=0.5)
      ) 
    
    return(plt)
}

##-- Plot video
plot_video <- function(data, data_pred_hist,
                       country_name, state_name, metrics, scale, fixed_scale,
                       varPrefix, legendPrefix, yaxisTitle, 
                       frame) {
  max_y1 <- lapply(data_pred_hist, "[[", "lt_predict") %>% bind_rows() %>% .$med %>% max()
  max_y2 <- max(data$NewConfirmed)
  
  max_y <- 1.1*max(max_y1, max_y2)
  
  aux <- data_pred_hist[[as.character(frame)]]
  if(is.null(aux)) return(NULL)
  
  pred_n <- aux$lt_predict
  pred_summary <- aux$lt_summary
  mu_plot <- aux$mu_plot
  flag <- aux$flag
  
  last_date_n <- min(pred_n$date) - 1
  pred_dateStr <- format(pred_n$date, format = "%d/%b")
  
  metric <- metrics
  metric_leg <- switch(metric, Deaths = "Deaths", Confirmed = "Confirmed") 
  metric_pt <- switch(metric, Deaths = "Deaths", Confirmed = "Confirmed")
  
  title <- paste0(country_name, ifelse(state_name == "<all>", "", paste0(" / ", state_name)))
  metric_tit <- ifelse(metric == "Confirmed", "Casos confirmados/Confirmed cases", "Mortes/Deaths")
  png_title = paste0("LTpred_", country_name, ifelse(state_name == "<all>", "", paste0("_", state_name)),
                     switch(metric, Deaths = "_d_", Confirmed = "_n_"),
                     format(last_date_n, format = "%d%m%Y")
  )
  
  tot <- data %>% filter(date <= frame) %>% .$CumConfirmed %>% max
  
  plt <- data %>%
    plot_ly() %>%
    plotly::config(displayModeBar = TRUE,
                   displaylogo = FALSE,
                   modeBarButtonsToRemove = c("lasso2d", "select2d", "toggleSpikelines", "hoverCompareCartesian",
                                              "hoverClosestCartesian", "autoScale2d"),
                   toImageButtonOptions = list(format = "png",
                                               filename = png_title )) %>%
    layout(
      title = list(text = paste0("<b>", title, " - ", metric_tit, "<b>")),
      annotations = list(text = formatC(tot, format = "f", big.mark = ".", decimal.mark = ",", digits = 0),
                         x = 0.97, y = 0.97, xref = "paper", yref = "paper",
                         font = list(family = "Arial", size = 50, color = 'rgba(100, 100, 100, 0.5)'), align = "right",
                         showarrow = FALSE),
      xaxis = list(
        title = "",
        tickangle = -90,
        tick0 = data$dateStr[1], dtick  =14*86400000,
        tickformat = "%d/%b"
      ),
      yaxis = list(title = yaxisTitle, type = if_else(scale == 1, "log", "linear"), hoverformat = '.0f', hoverinfo = "x+y"),
      legend = list(x = 0.03, y = 0.97, bgcolor = 'rgba(240, 240, 240, 0.5)', font = list(family = "Arial", size = 10)),
      font = f1,
      shapes = list(type = "line", opacity = 0.7, line = list(color = "black", width = 1),
                    y0 = 0, y1 = 1, yref = "paper",
                    x0 = last_date_n, x1 = last_date_n),
      margin = list(t = 20),
      dragmode = FALSE,
      modebar = list(orientation = "v")
    ) 
  
  plt <- plt %>%
    add_trace(
      x = mu_plot$date, y = mu_plot$mu,
      type = 'scatter', mode = 'lines', hoverinfo = "none", 
      name = "Estimated Mean",
      line = list(color = 'rgb(230, 115, 0)', dash = 'solid', width = 2.5)
    ) %>%
    add_trace(
      x = data$date[which(data$date <= last_date_n)],
      y = data[[paste0(varPrefix, metric)]][which(data$date <= last_date_n)], 
      type = 'scatter', mode = 'lines+markers', hoverinfo = "x+y",
      name = "Observed Data",
      marker = list(
        color = switch(metrics, Deaths = red, Confirmed = blu),
        line = list(color = switch(metrics, Deaths = dred, Confirmed = dblu), width = 1)),
      line = list(
        color = switch(metrics, Deaths = dred, Confirmed = blu),
        width = 1.5)
    )
  
  if(flag != 1){
    plt <- plt %>%
      add_trace(
        x = c(data$date[which(data$date == last_date_n)], pred_n$date),
        y = c(data[[paste0(varPrefix, metrics)]][which(data$date == last_date_n)], pred_n$q25),
        showlegend = F,
        name = "95% CI",
        type = 'scatter', 
        mode = 'lines', 
        hoverinfo = "x+y",
        fillcolor = 'rgba(150, 150, 150, 0.5)',
        line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
      ) %>%
      add_trace(
        x = c(data$date[which(data$date == last_date_n)], pred_n$date),
        y = c(data[[paste0(varPrefix, metrics)]][which(data$date == last_date_n)], pred_n$q975),
        type = 'scatter',
        mode = 'lines', hoverinfo = "x+y",
        fill = 'tonexty',
        name = "95% CI",
        fillcolor = 'rgba(150, 150, 150, 0.5)',
        line = list(color = 'rgba(0, 0, 0, 1)', width = 0)
      )
  }
  
  plt <- plt %>%
    add_trace(
      x = c(data$date[which(data$date == last_date_n)], pred_n$date),
      y = c(data[[paste0(varPrefix, metrics)]][which(data$date == last_date_n)], pred_n$med),
      type = 'scatter', mode = 'lines+markers', hoverinfo = "x+y",
      name = "Prediction",
      marker = list(color = 'rgb(0, 0, 0)', size = 4), line = list(color = 'rgb(0, 0, 0)', dash = 'dot')
    )  
  
  if(fixed_scale) {
    plt <- plt %>%
      layout(yaxis = list(range = c(0, max_y)))
  }
  
  ## Hachurando regiões
  x1 <- pred_summary$high.dat.low
  x2 <- pred_summary$high.dat.upper
  
  x3 <- pred_summary$end.dat.low
  x4 <- pred_summary$end.dat.upper
  
  if(x1 < min(pred_n$date)) {
    data_sub1 <- subset(mu_plot, date >= x1 & date <= x2)
    data_sub2 <- subset(mu_plot, date >= x3 & date <= x4)
    data_sub1$y <- data_sub1$mu
    data_sub2$y <- data_sub2$mu
  } else {
    data_sub1 <- subset(pred_n, date >= x1 & date <= x2)
    data_sub2 <- subset(pred_n, date >= x3 & date <= x4)
    data_sub1$y <- data_sub1$med
    data_sub2$y <- data_sub2$med
  }
  
  plt %>%
    add_trace(x = ~data_sub1$date, y = ~data_sub1$y, name = NULL, fill = 'tozeroy', mode = 'none',
              fillcolor = 'rgba(200, 50, 50, 0.5)', showlegend = FALSE) %>%
    add_trace(x = ~data_sub2$date, y = ~data_sub2$y, name = NULL, fill = 'tozeroy', mode = 'none',
              fillcolor = 'rgba(50, 200, 50, 0.5)', showlegend = FALSE) 
}

##-- Function to measure how old a file is
minutesSinceLastUpdate <- function(fileName) {
  out <- (as.numeric(as.POSIXlt(Sys.time())) - as.numeric(file.info(fileName)$ctime)) / 60
  
  return(out)
}

##-- Function to format the dates for better plotting
printDate <- function(date){
  monthsEn <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  out <- paste0(day(date), "/", monthsEn[month(date)])
  
  return(out)
}

##-- Main function to load the data - GLOBAL
loadData <- function(fileName, columnName) {
  if(!file.exists(fileName) || minutesSinceLastUpdate(fileName) > 10) {
    data <- read.csv(file.path(baseURL, fileName), check.names = FALSE, stringsAsFactors = FALSE) %>%
      select(-Lat, -Long) %>% 
      pivot_longer(-(1:2), names_to = "date", values_to = columnName)%>%
      mutate(
        date = as.Date(date, format = "%m/%d/%y"),
        `Province/State`= if_else(`Province/State` == "", "<all>", `Province/State`)
      )
    
    save(data, file = paste0("cache/", fileName))  
  } else {
    load(file = paste0("cache/", fileName))
  }
  
  return(data)
}

##-- Function to load data - BRAZIL
loadData.BR <- function(fileName) {
  if(!file.exists(fileName) || minutesSinceLastUpdate(fileName) > 10) {
    data <- read.csv(file.path(baseURL.BR, fileName), check.names = FALSE, stringsAsFactors = FALSE, sep=",") %>%
      select(-c(1, 4, 6)) %>%
      as_tibble() %>%
      mutate(date = as.Date(data)) %>%
      rename(
        `Province/State` = estado,
        `CumConfirmed` = casos.acumulados,
        `CumDeaths` = obitos.acumulados
      ) %>%
      select(-2)
      # select(-c(1,5,7)) %>%
      # as_tibble() %>%
      # mutate(date = as.Date(data, "%d/%m/%y")) %>%
      # rename(
      #   'Province/State' = estado,
      #   'NewConfirmed' = novos.casos,
      #   'NewDeaths' = obitos.novos
      # ) %>%
      # select(-2) 
    
    save(data, file = paste0("cache/", fileName))  
  } else {
    load(file = paste0("cache/", fileName))
  }
  
  return(data)
}

##-- Function to return list of countries with available data on github
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