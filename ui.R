## Aplicativo Shiny - Modelo de Predição de Casos de Coronavírus
## DEST/UFMG - mar/2020

## load packages
library(shiny)
library(plotly)
library(shinythemes)
library(shinycssloaders)

## Código da Interface

shinyUI(
  
  fluidPage(
    
    ## título
    titlePanel(
      div(column(width = 9, h2(strong("Previsão de curto e longo prazo para COVID-19")), #, style = "font-family:'Verdana';"),
                 h4("Short and long term prediction for COVID-19")),
          column(width = 3, tags$a(href="https://est.ufmg.br",
                                   tags$img(src="logoestatistica.png",
                                            title="DEST/UFMG", height="45")),
                 tags$a(href="https://github.com/thaispaiva/app_COVID19",
                        div(h5("Código Fonte/Source Code"))),
                            # tags$img(src="www/GitHub-Mark-32px.png",
                            #          title="GitHub", height="5"))), 
                 style = "transform: translate(0%, 22%);"
                 )
                                   
          ), windowTitle = "Previsão COVID-19 - DEST/UFMG"
    ),
    
    fluidRow( ),
    
    navbarPage(
      
      # ## tema do layout
      theme = shinytheme("flatly"),
      
      ## Nome do app
      # "Previsão de curto e longo prazo para COVID-19", # sigla?
      " ",
      
      ########################################

      ## 1a aba
      tabPanel(
        title = HTML("<b>Dados</b>/<br>Data"),
        
        sidebarLayout(
          ## painel lateral - input
          sidebarPanel(
            selectizeInput("country", label=h5("País/Country"), choices=NULL, width="100%"),
            selectizeInput("state", label=h5("Estado/State"), choices=NULL, width="100%"),
            checkboxGroupInput("metrics", label=h5("Casos/Cases"),
                               choices=c("Confirmados/Confirmed" = "Confirmed",
                                         "Mortes/Deaths" = "Deaths" ), #,
                                         # "Recuperados/Recovered" = "Recovered"),
                               selected=c("Confirmed","Deaths"), #,"Recovered"),
                               # choices=c("Confirmed","Deaths","Recovered"),
                               # selected=c("Confirmed","Deaths","Recovered"),
                               width="100%"),
            checkboxInput("scale", value=FALSE, width="100%",
                          label="Escala logarítmica/Log-scaled vertical axis"),
            width=3
          ),
          
          ## painel principal - output (gráficos)
          mainPanel(
            uiOutput("plotTitle_daily"),
            withSpinner( # add spinner while loading
              plotlyOutput("dailyMetrics")),
            uiOutput("plotTitle_cum"),
            withSpinner(
              plotlyOutput("cumulatedMetrics"))
          )
          
        )
      ),
      
      ## 2a aba
      tabPanel(
        title = HTML("<b>Previsão Curto Prazo</b>/<br>Short term Prediction"),
        
        sidebarLayout(
          ## painel lateral - input
          sidebarPanel(
            selectizeInput("country_STpred", label=h5("País/Country"), choices=NULL, width="100%"),
            selectizeInput("state_STpred", label=h5("Estado/State"), choices=NULL, width="100%"),
            sliderInput("pred_time",
                        label="Janela de previsão (em dias)/Prediction window (in days)",
                        # min=1, max=14, value=7 ),
                        min=1, max=7, value=7 ),
            checkboxInput("scale_STpred", value=FALSE, width="100%",
                          label="Escala logarítmica/Log-scaled vertical axis"),
            width=3
          ),
          
          ## painel principal - output (gráficos)
          mainPanel(
            # h5("Em desenvolvimento/Under development"),
            # h5("\n"),
            uiOutput("plotTitle"),
            withSpinner( # add spinner while loading
              plotlyOutput("STpred"))  # gráfico previsão curto prazo
          )
          
        )
      ),
      
      ## 3a aba
      tabPanel(
        title = HTML("<b>Previsão Longo Prazo</b>/<br>Long Term Prediction"),
        
        sidebarLayout(
          ## painel lateral - input
          sidebarPanel(
            selectizeInput("country_LTpred", label=h5("País/Country"), choices=NULL, width="100%"),
            selectizeInput("state_LTpred", label=h5("Estado/State"), choices=NULL, width="100%"),
            checkboxInput("scale_LTpred", value=FALSE, width="100%",
                          label="Escala logarítmica/Log-scaled vertical axis"),
            width=3
          ),
          
          ## painel principal - output (gráficos)
          mainPanel(
              h5("Em desenvolvimento/Under development") # ,
          #   # h5("\n"),
          #   uiOutput("plotTitle_LT"),
          #   withSpinner( # add spinner while loading
          #     plotlyOutput("LTpred"))  # gráfico previsão curto prazo
          )
          
        )
      ),
      
      
      ## 4a aba
      tabPanel(
        title = HTML("<b>Fundamentação Teórica</b>/<br>Theoretical Foundation"),
        
          ## painel principal - output (pdf)
          mainPanel(
            tags$iframe(style="width:100%; height:500px; scrolling=auto; align:middle", 
                        src="Covid19UFMG.pdf"))
      )
      
    )
  )
)

                              
                            

