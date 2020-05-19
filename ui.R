## Aplicativo Shiny - Modelo de Predição de Casos de Coronavírus
## DEST/UFMG - mar/2020

## Código da Interface

shinyUI(
  
  fluidPage(
    
    ## Add css style file
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")), ## ATUALIZAR
    
    ## Include Google Analytics
    tags$head(includeHTML(("google-analytics.html"))),
    useShinyjs(),
    
    ## Título
    titlePanel(
      div(
        column(
          width = 9,
          h2(strong("Previsão de curto e longo prazo para COVID-19")), #, style = "font-family:'Verdana';"),
          h4("Short and long term prediction for COVID-19")
        ),
        column(
          width = 3, 
          tags$a(href="https://est.ufmg.br",
                 tags$img(src="logoestatistica.png", title="DEST/UFMG", height="45")),
          # tags$a(                                                     
          #   href = "https://github.com/thaispaiva/app_COVID19",       
          #   icon("github")                                            ## ATUALIZAR
          # ),                                                          
          # tags$a(href="https://github.com/thaispaiva/app_COVID19",
          #        div(h5("Código Fonte/Source Code"))),
          # tags$img(src="www/GitHub-Mark-32px.png",
          #          title="GitHub", height="5"))),
          style = "transform: translate(0%, 22%);"
        )
      ), windowTitle = "Previsão COVID-19 - DEST/UFMG"
    ),
    
    fluidRow(),
    
    tagList(
      
      ## js file to add github logo to navbar
      tags$head(tags$script(type="text/javascript", src = "code.js", ".tab-content")),
      #       tags$script(HTML("var header = $('.navbar > .container-fluid');
      # header.append('<div style=\"float:right\"><a href=\"https://github.com/thaispaiva/app_COVID19\"><img src=\"www/GitHub-Mark-Light-32px.png\" alt=\"alt\" style=\"float:right;width:33px;height:41px;padding-top:10px;\"> </a>`</div>');
      #                        console.log(header)")
      #       ),
      
      navbarPage(
        
        ## Tema do layout
        theme = shinytheme("flatly"),
        
        ## ID 
        id = "covid_navbar",
        selected = "dados",
        
        ## Nome do app
        # "Previsão de curto e longo prazo para COVID-19", # sigla?
        HTML("<b>CovidLP</b>"),
        
        ########################################
        ## 1a aba
        tabPanel(
          title = HTML("<b>Dados</b>/<br>Data"),
          value = "dados",
          
          sidebarLayout(
            ## painel lateral - input
            sidebarPanel(
              selectizeInput("country", label=h5("País/Country"), choices="Brazil",selected="Brazil",width="100%"),
              # choices=NULL, width="100%"),
              selectizeInput("state", label=h5("Estado/State"), choices="<all>", selected="<all>",width="100%"),
              # choices=NULL, width="100%"),
              checkboxGroupInput("metrics", label=h5("Casos/Cases"),
                                 choices=c("Confirmados/Confirmed" = "Confirmed",
                                           "Mortes/Deaths" = "Deaths" ), #,
                                 # "Recuperados/Recovered" = "Recovered"),
                                 selected=c("Confirmed","Deaths"), #,"Recovered"),
                                 width="100%"),
              checkboxInput("scale", value=FALSE, width="100%",
                            label="Escala logarítmica/Log-scaled vertical axis"),
              width=3
            ),
            
            ## Painel principal - output (gráficos)
            mainPanel(
              p("Novos casos/New cases:", style="text-align:left"),
              withSpinner( # add spinner while loading
                plotlyOutput("dailyMetrics")),
              p("Casos acumulados/Cumulated cases:", style="text-align:left"),
              withSpinner(
                plotlyOutput("cumulatedMetrics")),
              # downloadButton("downloadData", label = "Download data", style = 'float: right; margin-bottom: 5%;')
              div(style= 'float: right; margin-bottom: 5%;',
                  downloadBttn("downloadData", label = "Download data", size="xs")
              )
            )
            
          )
        ),
        
        ## 2a aba
        tabPanel(
          title = HTML("<b>Previsão Curto Prazo</b>/<br>Short term Prediction"),
          value = "curt_prazo",
          
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
                          min=1, max=14, value=7 ),
              # min=1, max=7, value=7 ),
              checkboxInput("scale_STpred", value=FALSE, width="100%",
                            label="Escala logarítmica/Log-scaled vertical axis"),
              width=3
            ),
            
            ## painel principal - output (gráficos)
            mainPanel(
              p("Em desenvolvimento/Under development", style="font-size:80%;"),
              p("Previsão de casos acumulados/Prediction of cumulated cases:", style="text-align:left"),
              withSpinner( # add spinner while loading
                plotlyOutput("STpred")),  # gráfico previsão curto prazo
              # downloadButton("downloadData_ST", label = "Download data", style = 'float: right; margin-bottom: 5%;')
              div(style= 'float: right; margin-bottom: 5%;',
                  downloadBttn("downloadData_ST", label = "Download data", size="xs")
              )
            )  
            
          )
        ),
        
        ## 3a aba
        tabPanel(
          title = HTML("<b>Previsão Longo Prazo</b>/<br>Long Term Prediction"),
          value = "longo_prazo",
          
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
              p("Em desenvolvimento/Under development", style="font-size:80%;"), # font-family:arial"),
              p("Previsão de novos casos/Prediction of new cases:", style="text-align:left"),
              ## condition to hide/show plot depending on the flag
              shinyjs::hidden(selectInput(inputId = "show_plotLT", label = "",               
                                          choices = c(TRUE, FALSE), selected = TRUE)          
              ),
              conditionalPanel("input.show_plotLT == 'TRUE'",       
                               withSpinner( # add spinner while loading
                                 plotlyOutput("LTpred")  # gráfico previsão longo prazo
                               ),
                               # downloadButton("downloadData_LT", label = "Download data", style = 'float: right; margin-bottom: 5%; margin-right: 5%'),
                               div(style= 'float: right; margin-bottom: 5%;',
                                   downloadBttn("downloadData_LT", label = "Download data", size="xs"),
                                   downloadBttn("downloadData_LTsummary", label = "Download summary", size="xs")
                               )
                               # downloadButton("downloadData_LTsummary", label = "Download summary", style = 'float: right; margin-bottom: 5%;')
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
          value = "teoria",
          
          ## Painel principal - output (pdf)
          mainPanel(
            #withMathJax(includeMarkdown("www/CoronaUFMG_MD.Rmd"))
            column(
              width = 12,
              wellPanel(
                fluidRow(
                  downloadButton("material_covid", label = "Download", style = 'float: right; margin-right: 5%;')
                ),
                uiOutput("markdown")    
              )
            ) #,
            # tags$iframe(style="width:100%; height:500px; scrolling=auto; align:middle", 
            # src="Covid19UFMG.pdf"))
          )
        ),
        
        ## 5a aba
        tabPanel(
          title = HTML("<b>Sobre</b>/<br>About"),
          value = "sobre",
          
          sidebarLayout(
            ## painel lateral 
            sidebarPanel(
              h3("Contato/Contact:"),
              tags$a(href="mailto:danig@ufmg.br", "danig@ufmg.br"),
              h3("Equipe/Team:"),
              h4("Professores/Faculty:"),
              p("Prof. Dani Gamerman - coordenador",br(),
                "Prof. Marcos Prates",br(),
                "Profa. Thaís Paiva",br(),
                "Prof. Vinícius Mayrink"),
              h4("Alunas e alunos de Pós-Graduação/Graduate Students:"),
              div(
                # tags$b("Discentes do Programa de PG:"),br(),
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
            h4("Pesquisadores/Researchers:"),
              div(
                # tags$b("Doutora:"),br(),
                  "Juliana Freitas De Mello E Silva",
              # div(tags$b("Doutor:"),br(),
                  "Douglas R. Mesquita Azevedo")
            ),
            
            ## painel principal 
            mainPanel(
              h3("Sobre o projeto:"),
              p("Esse aplicativo é o resultado de um trabalho conjunto de professores e alunas e alunos de pós-graduação em Estatística da UFMG. Ele teve origem como um desafio em uma disciplina de pós-graduação após a suspensão das aulas devido à Covid19."),
              p("Na configuração atual, o aplicativo tem dois principais tipos de resultado:",tags$b("previsões de curto prazo e de longo prazo.")," O primeiro se refere a previsões de mortes e número de casos confirmados para o futuro imediato (até 1 a 2 semanas). O segundo tipo de previsões é mais abrangente e visa traçar um panorama mais completo da pandemia: quando o número de casos deixará de crescer e começará a decair? Quantas pessoas essa pandemia irá adoecer? Quando podemos esperar que a pandemia seja encerrada?"),
              p(tags$b("Nossas previsões são atualizadas diariamente,"),"e podem se alterar com base nos novos dados que chegam todo dia. As previsões são acompanhadas dos respectivos intervalos de probabilidade (ou credibilidade, no jargão estatístico) para que o usuário tenha sempre noção da verdadeira incerteza associada a cada previsão fornecida. Outro ponto importante é que essas previsões são sempre baseadas na manutenção das condições no dia em que a previsão foi feita, incluindo as condições de isolamento. Alterações podem causar mudanças substanciais nas previsões."),
              p("Para mais informações, veja a aba de Fundamentação Teórica e acesse ",a(href="http://www.statpop.com.br/2020/04/previsao-de-curto-e-longo-prazos-da_30.html", "www.statpop.com.br.")),
              # br(), # ENGLISH VERSION
              h3("About the project:"),
              p("This application is the result of a joint work by professors and graduate students from the Statistics Department at UFMG. It originated as a challenge in a graduate course after the suspension of classes due to Covid19."),
              p("In the current configuration, the application has two main types of results:",tags$b("short-term and long-term predictions.")," The first refers to prediction of number of deaths and confirmed cases for the immediate future (up to 1 to 2 weeks). The second type of forecast is more comprehensive and aims to provide a full picture of the pandemic: when will the number of cases stop growing and start to decline? How many people will get sick? When can we expect the pandemic to end?"),
              p(tags$b("Our forecasts are updated daily,"),"and may change based on new data that arrives every day. The predictions are accompanied by the respective probability intervals (or credibility, in statistical jargon) so that the user is always aware of the true uncertainty associated with each forecast provided. Another important point is that the predictions are always based on the maintenance of conditions on the day the forecast was made, including isolation conditions. Changes on these conditions can cause substantial alterations on the predictions."),
              p("For more information, check the Theoretical Foundation tab and access ",a(href="http://www.statpop.com.br/2020/04/previsao-de-curto-e-longo-prazos-da_30.html", "www.statpop.com.br.")),
              
              ###########
              hr(),
              h3("Na mídia/In the news:"),
              p("08/05/2020 - ",a(href="https://www.jetro.go.jp/biznews/2020/05/c9c4655a1215521d.html", "Notícia no site Jetro Japão")),
              p("05/05/2020 - ",a(href="https://ufmg.br/comunicacao/noticias/aplicativos-projetam-infeccoes-e-mortes-pelo-coronavirus-em-longo-prazo", "Notícia no site da UFMG")),
              p("04/05/2020 - ",a(href="https://www.hojeemdia.com.br/primeiro-plano/estudo-da-ufmg-projeta-pico-de-casos-da-covid-19-no-brasil-para-o-dia-18-deste-m%C3%AAs-1.785365", "Matéria no Jornal Hoje em Dia")),
              p("01/05/2020 - ",a(href="https://www.itatiaia.com.br/noticia/especialistas-revelam-que-minas-gerais-ja-pod", "Entrevista na Rádio Itatiaia")),
              
              ###########
              hr(),
              h3("Próximos eventos/Upcoming events:"),
              fluidRow(
                div(img(src="event_2105.jpg", align="left", width="300px"))
              ),
              br(),
              
              ########
              ## add disqus for comments
              hr(),
              h3("Comentários/Comments:"),
              div(id="disqus_thread",
                  HTML(
                    "<script>
                    (function() { // DONT EDIT BELOW THIS LINE
                      var d = document, s = d.createElement('script');
                      s.src = 'https://covidlp.disqus.com/embed.js';
                      s.setAttribute('data-timestamp', +new Date());
                      (d.head || d.body).appendChild(s);
                    })();
                  </script>
                  <noscript>Please enable JavaScript to view the <a href='https://disqus.com/?ref_noscript'>comments powered by Disqus.</a></noscript>"
                  )
              )
              
            )
          )
        )
      )
    )
  )
)