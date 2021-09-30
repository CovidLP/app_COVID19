## Código da Interface
shinyUI(
  fluidPage(
    ## Add css style file
    theme = "styles.css",
    
    ## Include Google Analytics
    tags$head(includeHTML(("google-analytics.html"))),
    
    ## Habilitando o shinyjs
    useShinyjs(),
    
    ## add favicon
    tags$head(tags$link(rel = "icon", href = "favicon.ico")),
    
    ## Título
    tagList(
      tags$head(
        tags$title(
          "Previsão COVID-19 - DEST/UFMG"
        )
      ),
      
      fluidRow(
        ## Logo
        column(
          width = 2,
          tags$a(
            href = "http://est.ufmg.br/covidlp/home/pt/", 
            tags$img(src = "logo_covid.png", title = "DEST/UFMG", height = "40px", style = "margin-top: 10px; margin-left: 20px")
          )
        ),
        ## Título
        column(
          width = 8,
          h4(
            strong("Previsão de curto e longo prazo para COVID-19"),
            style = "text-align: center"
          ),
          h5("Short and long term prediction for COVID-19", style = "text-align: center")
        ),
        ## github, webpage, email
        column(
          width = 1, offset = 1,
          br(),
          tags$a(icon("github"), href = "https://github.com/thaispaiva/app_COVID19"),
          tags$a(icon("globe"),  href = "http://est.ufmg.br/covidlp/home/pt/"),
          tags$a(icon("envelope"),  href = "mailto:covidlp.team@gmail.com")
        ),
        style = "background-color: white; margin: 0; padding: 0;",
      )
    ),
    
    ## Informações
    fluidRow(
      column(width = 10, offset = 1,
             wellPanel(
               h4("Selecione o país/estado que deseja investigar.", style = "text-align: center;"),
               h4("Select a country/state to analyse.", style = "text-align: center;"),
               HTML("<center><a href = 'http://est.ufmg.br/covidlp/home/pt/' style = 'color: #00b1d8; text-decoration: underline;'>Sobre/About</a> | <a href = 'http://est.ufmg.br/covidlp/home/pt/metodologia' style = 'color: #00b1d8; text-decoration: underline;'>Metodologia/Methodology</a></center>"),
               # h5(sprintf("Última atualização/last update: %s", Sys.Date()), style = "text-align: center;"),
               # HTML("<center><a href = 'http://est.ufmg.br/covidlp/home/pt/' style = 'color: #00b1d8; text-decoration: underline;'>CovidLP website</a> | <a href = 'https://github.com/thaispaiva/app_COVID19' style = 'color: #00b1d8; text-decoration: underline;'>Source code</a> <br> <a href = 'http://est.ufmg.br/covidlp/home/pt/' style = 'color: #00b1d8; text-decoration: underline;'>Sobre/About</a> | <a href = 'http://est.ufmg.br/covidlp/home/pt/metodologia' style = 'color: #00b1d8; text-decoration: underline;'>Metodologia/Methodology</a></center>"),
               # HTML("<center><a href = 'http://est.ufmg.br/covidlp/home/pt/' style = 'color: #00b1d8; text-decoration: underline;'>CovidLP website</a></center>"),
               style = "background-color: #ffffff; border-radius: 4px; box-shadow: 0 0 4px 0 rgba(69,69,69,.2);"
             )
      ),
      style = "padding: 20px"
    ),
    
    ## Informações
    fluidRow(
      HTML("<center>"),
      div(
        class = "btn_div",
        shinyWidgets::pickerInput(inputId = "country", label = NULL, choices = countries, selected = "Brazil")
      ),
      div(
        class = "btn_div",
        shinyWidgets::pickerInput(inputId = "state", label = NULL, choices = "<all>", selected = "<all>"),
      ),
      div(
        class = "btn_div",
        tipify(bsButton("pB2", "?", style = "inverse", size = "extra-small"), HTML("Selecione o país e o estado de interesse! <br> Select a country and a state!"))
      ),
      
      HTML("</center>"),
      style = "background-color: #00b1d8;
               border-radius: 4px;
               box-shadow: 0 0 4px 0 rgba(69, 69, 69, 0.2);
               margin: 0px 10px 30px 10px; padding: 10px;"
    ),
    
    ## Dados
    fluidRow(
      tabsetPanel(
        id = "tabset_covid",
        tabPanel(
          title = HTML("<center>
                          <i class='fa fa-envelope'></i>
                          <div>
                            Dados<br>Data
                          </div>
                        </center>"), 
          value = "tab_dados", 
          # icon = icon("database"),
          
          ## Inputs
          HTML("<center>"),
          HTML("<h4 style = 'text-align: center;'>Selecione o tipo de caso que deseja investigar.<br>Select the kind of data to analyze.</h4>"),
          div(
            class = "btn_div",
            shinyWidgets::awesomeCheckboxGroup(
              inputId = "metrics_data", 
              label = NULL,
              choices = c("Confirmados/Confirmed" = "Confirmed", "Mortes/Deaths" = "Deaths" ),
              selected = c("Confirmed", "Deaths"), 
              inline = TRUE
            ),
            bsTooltip(id = "metrics_data",
                      title = HTML("Selecione as métricas a serem apresentadas. <br> Select the metrics to display."),
                      placement = "right", 
                      options = list(container = "body")
            ),
            
            shinyWidgets::awesomeCheckbox(
              inputId = "scale_data",
              label = "Escala logarítmica/Log-scaled vertical axis",
              value = FALSE
            ),
          ),
          HTML("</center>"),
          
          ## Valores diários
          h3("Novos casos/New cases:", style = "text-align:left"),
          # uiOutput("msg_data1"),
          withSpinner(
            plotlyOutput("dailyMetrics")
          ),
          
          ## Valores acumulados
          h3("Casos acumulados/Cumulated cases:", style = "text-align:left"),
          # uiOutput("msg_data2"),
          withSpinner(
            plotlyOutput("cumulatedMetrics")
          ),
          downloadBttn("downloadData", label = "Download data", size = "xs", style = "material-flat"),
          hr(),
          
          style = "background-color: #fff; 
                   border-radius: 4px; 
                   box-shadow: 0 0 4px 0 rgba(69, 69, 69, 0.2); 
                   margin: 10px; padding: 10px;"
        ),
        tabPanel(
          title = HTML("<center>
                          <i class='fa fa-chart-line'></i>
                          <div>
                            Curto prazo<br>Short term
                          </div>
                        </center>"),
          value = "tab_curto", 
          # icon = icon("chart-line"),
          
          ## Inputs
          HTML("<center>"),
          HTML("<h4 style = 'text-align: center;'>Selecione o tipo de caso que deseja investigar.<br>Select the kind of data to analyze.</h4>"),
          div(
            class = "btn_div",
            radioGroupButtons(
              inputId = "metrics_ST",
              label = NULL,
              choices = c("Confirmados/Confirmed" = "Confirmed", "Mortes/Deaths" = "Deaths" ), 
              status = "primary",
              individual = TRUE,
              checkIcon = list(
                yes = icon("ok", lib = "glyphicon"),
                no = icon("remove",lib = "glyphicon")
              )
            ),
            bsTooltip(id = "metrics_ST",
                      title = HTML("Selecione uma das métricas para ser apresentada. <br> Select one metric to display."),
                      placement = "right", 
                      options = list(container = "body")
            ),
            
            shinyWidgets::awesomeCheckbox(
              inputId = "scale_ST",
              label = "Escala logarítmica/Log-scaled vertical axis",
              value = FALSE
            )
          ),
          HTML("</center>"),
          
          shinyjs::hidden(
            shinyWidgets::sliderTextInput(
              inputId = "pred_time",
              label = "Previsão (em dias)/Prediction (in days)",
              choices = seq(from = 1, to = 14, by = 1),
              selected = 14,
              grid = TRUE
            )
          ),
          
          ## Short term prediction
          uiOutput("title_ST"),
          # uiOutput("msg_ST"),
          HTML("<div class = 'row'>
                  <div class = 'column'>
                      <h5>Em desenvolvimento/Under development</h5>
                  </div>
                  <div class = 'column'>
                      <a href = 'http://est.ufmg.br/covidlp/home/pt/blog/atualizacoes' style = 'float: right' target = '_blank''>Atualizações/Updates</a>
                  </div>
               </div>"),
          uiOutput("msg_hide_ST"),
          withSpinner(
            plotlyOutput("STpred")
          ),
          downloadBttn("downloadData_ST", label = "Download data", size = "xs", style = "material-flat"),
          hr(),
          
          style = "background-color: #fff; 
                   border-radius: 4px; 
                   box-shadow: 0 0 4px 0 rgba(69, 69, 69, 0.2); 
                   margin: 10px; padding: 10px;"
        ),
        tabPanel(
          title = HTML("<center>
                          <i class='fa fa-chart-line'></i>
                          <div>
                            Longo prazo<br>Long term
                          </div>
                        </center>"),
          value = "tab_longo", 
          # icon = icon("chart-line"),
          
          ## Inputs
          HTML("<center>"),
          HTML("<h4 style = 'text-align: center;'>Selecione o tipo de caso que deseja investigar.<br>Select the kind of data to analyze.</h4>"),
          div(
            class = "btn_div",
            radioGroupButtons(
              inputId = "metrics_LT",
              label = NULL,
              choices = c("Confirmados/Confirmed" = "Confirmed", "Mortes/Deaths" = "Deaths" ),
              status = "primary", 
              individual = TRUE,
              checkIcon = list(
                yes = icon("ok", lib = "glyphicon"),
                no = icon("remove",lib = "glyphicon")
              )
            ),
            bsTooltip(id = "metrics_LT",
                      title = HTML("Selecione uma das métricas para ser apresentada. <br> Select one metric to display."),
                      placement = "right", 
                      options = list(container = "body")
            ),
            
            
            shinyWidgets::awesomeCheckbox(
              inputId = "scale_LT",
              label = "Escala logarítmica/Log-scaled vertical axis",
              value = FALSE
            )
          ),
          HTML("</center>"),
          
          ## Short term prediction
          uiOutput("title_LT"),
          # uiOutput("msg_LT"),
          HTML("<div class = 'row'>
                  <div class = 'column'>
                      <h5>Em desenvolvimento/Under development</h5>
                  </div>
                  <div class = 'column'>
                      <a href = 'http://est.ufmg.br/covidlp/home/pt/blog/atualizacoes' style = 'float: right' target = '_blank''>Atualizações/Updates</a>
                  </div>
               </div>"),
          uiOutput("msg_hide_LT"),
          withSpinner(
            plotlyOutput("LTpred")
          ),
          downloadBttn("downloadData_LT", label = "Download data", size = "xs", style = "material-flat"),
          downloadBttn("downloadData_LTsummary", label = "Download summary", size = "xs", style = "material-flat"),
          hr(),
          
          shinyjs::hidden(numericInput(inputId = 'show_video', label = NULL, value = 0)),
          conditionalPanel(
            condition = "input.show_video!=0",
            h3("Evolução da previsão/Forecast evolution", style = "text-align:left; padding-top: 2%"),
            HTML("<div class = 'row'>
                  <div class = 'column'>
                      <h5>Em desenvolvimento/Under development</h5>
                  </div>
                  <div class = 'column'>
                      <a href = 'http://est.ufmg.br/covidlp/home/pt/blog/atualizacoes' style = 'float: right' target = '_blank''>Atualizações/Updates</a>
                  </div>
               </div>"),
            plotlyOutput("LTpred_video"),
            HTML("<center>"),
            # sliderTextInput(inputId = "slider", label = "", grid = TRUE, choices = Sys.Date(), selected = Sys.Date(), animate = TRUE),
            sliderInput(
              inputId = "slider",
              label = "", 
              min = Sys.Date()-1,
              max = Sys.Date(),
              value = Sys.Date(),
              animate = TRUE
            ),
            shinyWidgets::awesomeCheckbox(
              inputId = "fixed_scale",
              label = "Escala fixa/Fixed scale",
              value = TRUE
            ),
            HTML("</center>")
          ),
          
          style = "background-color: #fff; 
                   border-radius: 4px; 
                   box-shadow: 0 0 4px 0 rgba(69, 69, 69, 0.2); 
                   margin: 10px; padding: 10px;"
        )
      )
    ),
    
    ## Disqus
    # HTML("<center>"),
    # h3("Comentários/Comments:"),
    # div(id = "disqus_thread",
    #     HTML(
    #       "<script>
    #          (function() { 
    #             var d = document, s = d.createElement('script');
    #             s.src = 'https://covidlp.disqus.com/embed.js';
    #             s.setAttribute('data-timestamp', +new Date());
    #             (d.head || d.body).appendChild(s);
    #           })();
    #        </script>
    #        <noscript>Please enable JavaScript to view the <a href='https://disqus.com/?ref_noscript'>comments powered by Disqus.</a></noscript>"
    #     ),
    #     style = "max-width: 80%;"
    # ),
    # HTML("</center>"),
    
    ##-- Footer ----
    includeHTML("footer.html")
  )
)