## Aplicativo Shiny - Modelo de Predição de Casos de Coronavírus
## DEST/UFMG - mar/2020

## load packages
library(shiny)
library(plotly)
library(shinythemes)

## Código da Interface

shinyUI(
  
  fluidPage(
    
    ## título
    titlePanel(
      div(column(width = 9, h3("Previsão de curto e longo prazo para COVID-19"),
                 h5("Short and long term prediction for COVID-19")),
          column(width = 3, tags$a(href="https://est.ufmg.br",
                                   tags$img(src="logoestatistica.png",
                                            title="DEST/UFMG", height="45")))
                                   #width="420", height="80")))
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
        "Dados/Data",
        
        sidebarLayout(
          ## painel lateral - input
          sidebarPanel(
            selectizeInput("country", label=h5("País/Country"), choices=NULL, width="100%"),
            selectizeInput("state", label=h5("Estado/State"), choices=NULL, width="100%"),
            checkboxGroupInput("metrics", label=h5("Casos/Cases"),
                               choices=c("Confirmed","Deaths","Recovered"),
                               selected=c("Confirmed","Deaths","Recovered"),
                               # "metrics", label=h5("Medidas selecionadas"), 
                               # choices=c("Confirmados", "Mortes", "Recuperados"), 
                               # selected=c("Confirmados", "Mortes", "Recuperados"), 
                               width="100%"),
            width=3
          ),
          
          ## painel principal - output (gráficos)
          mainPanel(
            plotlyOutput("dailyMetrics"),
            plotlyOutput("cumulatedMetrics")
          )
          
        )
      ),
      
      ## 2a aba
      tabPanel(
        "Previsão Curto Prazo/Short term",
        
        sidebarLayout(
          ## painel lateral - input
          sidebarPanel(
            selectizeInput("country", label=h5("País/Country"), choices=NULL, width="100%"),
            selectizeInput("state", label=h5("Estado/State"), choices=NULL, width="100%"),
            sliderInput("pred_time",
                        label="Janela de previsão (em dias)/Prediction window (in days)",
                        min=1, max=14, value=7 ),
            width=3
          ),
          
          ## painel principal - output (gráficos)
          mainPanel(
            
          )
          
        )
      ),
      
      ## 3a aba
      tabPanel(
        "Previsão Longo Prazo/Long Term",
        
        sidebarLayout(
          ## painel lateral - input
          sidebarPanel(
            selectizeInput("country", label=h5("País/Country"), choices=NULL, width="100%"),
            selectizeInput("state", label=h5("Estado/State"), choices=NULL, width="100%"),
            width=3
          ),
          
          ## painel principal - output (gráficos)
          mainPanel(
            
          )
          
        )
      )
    )
  )
)

                              
                            

  # # bootstrap theme
  # theme = "bootstrap.min.css",
  
  # # título
  # titlePanel(
  #   div(column(width = 8, h2("Modelo de Predição de Casos de Coronavírus")), 
  #       column(width = 4, tags$a(href="https://est.ufmg.br",
  #                                tags$img(src="logoestatistica.png", 
  #                                         title="DEST/UFMG", width="420", height="80")))
  #   ), windowTitle = "Predição Coronavírus - DEST/UFMG"
  # ),
  
#   # definindo os comandos de input
#   fluidRow(
#     column(
#       width=4, 
#       selectizeInput(
#         # "country", label=h5("Country"), choices=NULL, width="100%")
#         "country", label=h5("País"), choices=NULL, width="75%")
#     ),
#     column(
#       width=4, 
#       selectizeInput(
#         # "state", label=h5("State"), choices=NULL, width="100%")
#         "state", label=h5("Estado"), choices=NULL, width="75%")
#     ),
#     column(
#       width=4, 
#       checkboxGroupInput(
#         "metrics", label=h5("Selected Metrics"),
#         choices=c("Confirmed", "Deaths", "Recovered"),
#         selected=c("Confirmed", "Deaths", "Recovered"),
#         # "metrics", label=h5("Medidas selecionadas"), 
#         # choices=c("Confirmados", "Mortes", "Recuperados"), 
#         # selected=c("Confirmados", "Mortes", "Recuperados"), 
#         width="100%")
#     )
#   ),
#   
#   # acrescentar slider para selecionar no. dias de estimativa
#   fluidRow(
#     column(
#       width=4,
#       sliderInput(
#         "est_time", label="Tempo de estimativa (em dias)",
#         min=1, max=10, value=5  # ALTERAR!
#       )
#     )
#   ),
#   
#   # gráficos de output
#   fluidRow(
#     plotlyOutput("dailyMetrics")
#   ),
#   fluidRow(
#     plotlyOutput("cumulatedMetrics")
#   )
#   
# ))