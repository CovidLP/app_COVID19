## Aplicativo Shiny - Modelo de Predição de Casos de Coronavírus
## DEST/UFMG - mar/2020

## load packages
library(shiny)
library(shinyjs)
library(plotly)
library(shinythemes)
library(shinycssloaders)

## Código da Interface

shinyUI(
  
  fluidPage(
    
    ## Include Google Analytics
    tags$head(includeHTML(("google-analytics.html"))),
    
    useShinyjs(), ## DOUGLAS
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
            radioButtons("metrics_ST", label=h5("Casos/Cases"),
                         choices=c("Confirmados/Confirmed" = "Confirmed",
                                   "Mortes/Deaths" = "Deaths" ),
                         selected="Confirmed",
                         width="100%"),
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
            selectizeInput("country_LTpred", label=h5("País/Country"), choices="Brazil", selected="Brazil", width="100%"),
            selectizeInput("state_LTpred", label=h5("Estado/State"), choices="<all>", selected="<all>", width="100%"),
            radioButtons("metrics_LT", label=h5("Casos/Cases"),
                         choices=c("Confirmados/Confirmed" = "Confirmed",
                                   "Mortes/Deaths" = "Deaths" ),
                         selected="Confirmed",
                         width="100%"),
            checkboxInput("scale_LTpred", value=FALSE, width="100%",
                          label="Escala logarítmica/Log-scaled vertical axis"),
            width=3
          ),
          
          ## painel principal - output (gráficos)
          mainPanel(
            uiOutput("plotTitle_LT"),
            ## condition to hide/show plot depending on the flag
            shinyjs::hidden(selectInput(inputId = "show_plotLT", label = "",               
                                        choices = c(TRUE, FALSE), selected = TRUE)          
                            ),
            conditionalPanel("input.show_plotLT == 'TRUE'",       
                             withSpinner( # add spinner while loading
                               plotlyOutput("LTpred")  # gráfico previsão longo prazo
                             )
            ),
            conditionalPanel("input.show_plotLT == 'FALSE'",     
                             div(style= 'text-align:left; font-size:15px;
                                  font-family:"Open Sans",arial,sans-serif', # font-weight:bold',
                                 "Resultados não disponíveis/Results not available")
            )
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
      ),
      
      
      ## 5a aba
      tabPanel(
        title = HTML("<b>Sobre</b>/<br>About"),
        
        sidebarLayout(
          ## painel lateral 
          sidebarPanel(
            h3("Contato/Contact:"),
            tags$a(href="mailto:danig@ufmg.br", "danig@ufmg.br"),
            h3("Equipe/Team:"),
            p("Prof. Dani Gamerman - coordenador",br(),
              "Prof. Marcos Prates",br(),
              "Profa. Thaís Paiva",br(),
              "Prof. Vinícius Mayrink"),
            div(tags$b("Alunos do Programa de PG:"),br(),
              "Ana Julia Alves Camara",br(),
              "Danna Lesley Cruz Reyes",br(),
              "Debora de Freitas Magalhaes",br(),
              "Gabriel Oliveira Assunção",br(),
              "Guilherme Aguiar",br(),
              "Jonathan de Souza Matias",br(),
              "Leonardo Brandão Freitas do Nascimento",br(),
              "Marcio Augusto",br(),
              "Marta Cristina Colozza Bianchi",br(),
              "Otavio O. A. S. Lima",br(),
              "Ricardo Cunha Pedroso",br(),
              "Thais Pacheco Menezes",br(),
              "Vitor Faria de Carvalho Oliveira"),
            div(tags$b("Doutora:"),br(),
              "Juliana Freitas De Mello E Silva")
          ),
          
          ## painel principal 
          mainPanel(
            h3("Sobre o projeto/About the project:"),
            p("Esse aplicativo é o resultado de um trabalho conjunto de professores e alunos de pós-graduação em Estatística da UFMG. Ele teve origem como um desafio em uma disciplina de pós-graduação após a suspensão das aulas devido à Covid19."),
            p("Na configuração atual, o aplicativo tem 2 principais tipos de resultado:",tags$b("previsões de curto prazo e de longo prazo.")," O primeiro se refere a previsões de mortes e número de casos confirmados para o futuro imediato (até 1 a 2 semanas). O segundo tipo de previsões é mais abrangente e visa traçar um panorama mais completo da pandemia: quando o número de casos deixará de crescer e começará a decair? quantas pessoas essa pandemia irá adoecer? quando podemos esperar que a pandemia seja encerrada?"),
            p(tags$b("Nossas previsões são atualizadas diariamente"),", e podem se alterar com base nos novos dados que chegam todo dia. As previsões são acompanhadas dos respectivos intervalos de probabilidade (ou credibilidade, no jargão estatístico) para que o usuário tenha sempre noção da verdadeira incerteza associada a cada previsão fornecida. Outro ponto importante é que essas previsões são sempre baseadas na manutenção das condições no dia em que a previsão foi feita, incluindo as condições de isolamento. Alterações podem causar mudanças substanciais nas previsões."),
            p("Para mais informações, veja a aba de Fundamentação Teórica e acesse ",a(href="http://www.statpop.com.br/2020/04/previsao-de-curto-e-longo-prazos-da_30.html", "www.statpop.com.br.")),
            h3("Na mídia/In the news:"),
            p("01/05/2020 - ",a(href="https://www.itatiaia.com.br/noticia/especialistas-revelam-que-minas-gerais-ja-pod", "Entrevista na Rádio Itatiaia"))
          )
        )
      )
    )
  )
)

                              
                            

