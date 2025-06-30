ui <- navbarPage(
  title = "Focos de Queimada e Internações no Brasil 2022–2024",
  theme = shinytheme("flatly"),
  
  # Aba Início
  tabPanel("Início",
           fluidPage(
             h1("Bem-vindo ao Painel Interativo!"),
             p("Este aplicativo permite explorar dados espaciais sobre queimadas e internações hospitalares no Brasil
        entre os anos de 2022 e 2024 em decorrência de problemas respiratórios, filtrados pelos CIDS:
        J40, J41, J42, J43, J44, J45, J46, J18, J00, J01, J02, J03, J04, J05, J06, J20, J21 e J22.
        Foi criado como parte da disciplina de Estatística Espacial do Programa de Pós-Graduação em
        Estatística Aplicada da Universidade Federal Rural do Rio de Janeiro, sob orientação do Prof. Wagner Tassinari."),
             br(),
             h4("Repositórios relacionados:"),
             tags$ul(
               tags$li(tags$a(href = "https://github.com/evymonteiro", "GitHub Évelyn Monteiro", target = "_blank")),
               tags$li(tags$a(href = "https://github.com/wtassinari", "GitHub Wagner Tassinari", target = "_blank"))
             ),
             br(),
             h3("O que você encontrará aqui?"),
             tags$ul(
               tags$li("📊 Análises exploratórias"),
               tags$li("🗺️ Visualizações espaciais interativas por estado e bioma"),
               tags$li("📥 Integração de dados de diferentes fontes públicas")
             ),
             br(),
             h3("Fontes dos dados"),
             tags$p("Os dados utilizados foram obtidos nas seguintes fontes:"),
             tags$ul(
               tags$li(tags$a(href = "https://terrabrasilis.dpi.inpe.br/queimadas/bdqueimadas/#apresentacao", "TerraBrasilis – INPE", target = "_blank")),
               tags$li(tags$a(href = "https://github.com/rfsaldanha/microdatasus", "MicroDataSUS – Rafael Saldanha", target = "_blank"))
             ),
             br(),
             p("Desenvolvido por: Évelyn Muniz | Última atualização: Junho de 2025")
           )
  ),
  
  # Aba Análise Exploratória
  tabPanel("Análise Exploratória",
           sidebarLayout(
             sidebarPanel(
               selectInput("dataset_escolhido", "Selecione o banco de dados:", choices = names(lista_datasets)),
               uiOutput("coluna_numerica_ui")
             ),
             mainPanel(
               h3("Resumo Estatístico"),
               verbatimTextOutput("summary_text"),
               fluidRow(
                 column(width = 6, h3("Boxplot"), plotOutput("boxplot")),
                 column(width = 6, h3("Histograma"), plotOutput("histograma"))
               ),
               h3("Banco de Dados"),
               DTOutput("tabela_dados")
             )
           )
  ),
  
  # Aba Mapas Temáticos
  tabPanel("Mapas Temáticos",
           sidebarLayout(
             sidebarPanel(
               selectInput("mapa_tematico_escolhido", "Selecione o mapa:",
                           choices = c(
                             "Focos por Bioma",
                             "Focos por Estado",
                             "Biomas",
                             "Internações por Estado",
                             "Internações e Focos por Estado"
                           )
               )
             ),
             mainPanel(
               plotOutput("mapa_tematico", height = "700px", width = "100%")
             )
           )
  ),
  
  # Aba Estatística Espacial
  tabPanel("Estatística Espacial",
  tabsetPanel(type = "tabs",
                       
# Subaba Mapas
 tabPanel("Mapas",
  sidebarLayout(
        sidebarPanel(
         width = 4,
        selectInput("mapa_espacial_escolhido", "Selecione o mapa:",
            choices = c(
                 "Vizinhança por Contiguidade Estadual",
                     "Vizinhança por Contiguidade Biomas",
                   "Biomas - Moran Local",
                             "LISAMAP Focos Estadual",
                                                  "LISAMAP Focos Biomas",
                                                  "LISAMAP Taxa de Internação /100mil hab",
                                                  "LISAMAP Internação por Estado"
                                                )
                                    ),
                                    br(),
                                    DTOutput("tabela_atributos")
                                  ),
                                  mainPanel(
                                    width = 8,
                                    plotOutput("mapa_espacial", height = "700px", width = "100%")
                                  )
                                )
                       ),
                       
                       # Subaba Modelos
                       tabPanel("Modelos",
                                tabsetPanel(type = "tabs",
                                            
                                            # GWR
                                            tabPanel("GWR",
                                                     tabsetPanel(type = "tabs",
                                                                 tabPanel("Estatísticas",
                                                                          fluidRow(
                                                                            column(6, h4("Summary"), verbatimTextOutput("summary_gwr")),
                                                                            column(6, h4("Head"), tableOutput("head_gwr"))
                                                                          )
                                                                 ),
                                                                 tabPanel("Mapa Predito",
                                                                          plotOutput("mapa_predito_gwr", height = "600px")
                                                                 ),
                                                                 tabPanel("Comparação Observado x GWR",
                                                                          plotOutput("comparacao_gwr", height = "600px")
                                                                 )
                                                     )
                                            ),
                                            
                                            # CAR
                                            tabPanel("CAR",
                                                     tabsetPanel(type = "tabs",
                                                                 tabPanel("Estatísticas",
                                                                          fluidRow(
                                                                            column(6, h4("Summary"), verbatimTextOutput("summary_car")),
                                                                            column(6, h4("Head"), tableOutput("head_car"))
                                                                          )
                                                                 ),
                                                                 tabPanel("Mapa Predito",
                                                                          plotOutput("mapa_predito_car", height = "600px")
                                                                 ),
                                                                 tabPanel("Comparação Observado x CAR",
                                                                          plotOutput("comparacao_car", height = "600px")
                                                                 )
                                                     )
                                            ),
                                            
    # RESÍDUOS
      tabPanel("Resíduos",
    plotOutput("plot_residuos", height = "600px")
     )
    )
     )
                       
    )
  ),
  
  #Testes Estatísticos 
tabPanel("Testes",
         sidebarLayout(
           sidebarPanel(
             width = 4,
             selectInput("variavel_teste", "Selecione a variável",
                         choices = c("Focos por Estado", 
                                     "Focos por Bioma", 
                                     "Internações por Estado", 
                                     "Taxa de Internações por Estado")),
             selectInput("teste_escolhido", "Selecione o teste",
                         choices = c("Moran Global", "Moran Local")),
             br(),
             DTOutput("matriz_vizinhanca")
           ),
           mainPanel(
             width = 8,
             conditionalPanel(
               condition = "input.teste_escolhido == 'Moran Global'",
               verbatimTextOutput("resultado_teste")
             ),
             conditionalPanel(
               condition = "input.teste_escolhido == 'Moran Local'",
               DTOutput("resultado_local")
             )
           )
         )
), 
# Discussões
tabPanel("Discussões",
         fluidPage(
           h2("Dificuldades e Limitações:"),
           p("Os resultados aqui obtidos possuem recorte temporal entre janeiro de 2022
             a dezembro de 2024. Esta escolha deu-se devido ao custo computacional
             para baixar os dados disponiveis na biblioteca do Microdatasus, entre 
             os problemas encontrados, é a falta de padronização nos dados de todos os Estados
             sendo necessário algumas análises manuais em Estados com padronização diferente,
             além disso, não são todos os Estados que possuem dados desde a mesma época."),
           br(),
           h2("A respeito dos resultados aqui encontrados:"),
           p("Os resultados obtidos nos modelos propostos não foram satisfatórios, como pode ser visto
           de acordo com os valores obtidos na aba modelo, em que o Modelo Linear possui melhor critério
           AIC, além disso, o R-squared de ambos modelos propostos indicam que o modelo
           explicou mal as variáveis, em que a QtdFocos não foi significativa em nenhum
           modelo proposto. Uma vez que estados como São Paulo são muito populosos e estão distantes
           de biomas com maior incidência de focos.
           
             Compreende-se que seja melhor analisar individualmente cada bioma pois possuem características
             físicas distintas, para este caso, é necessário determinar também os biomas
             associados a variável de internações. Quando analisamos a variável de focos
             por estado, é possível identificar que a região do bioma Amazônia possui
             forte correlação, entretanto, ao analisar os Biomas individualmente, nenhum 
             apresentou autocorrelação espacial significativa.
             O gráfico de resíduos são um reflexo da inadequação dos modelos propostos, porém, o modelo GWR
             apresentou resultados superiores ao CAR. 
             "),
           br(),
           h2("Sugestões:"),
           p("Para próximas etapas: 
           Conectar dados de internação ao bioma;
           Analisar individualmente algum bioma específico num intervalo de tempo maior;
           Testar outra matriz de vizinhança;
           Realizar análise espaço-temporal.
             "),
           br(),
           h3("Repositórios para reprodutibilidade:"),
           tags$ul(
             tags$li(tags$a(href = "https://github.com/evymonteiro/RShiny-Queimadas-Internacoes", "Pasta do GitHub para App Local - Reprodutível", target = "_blank")),
             tags$li(tags$a(href = "https://github.com/evymonteiro/RShiny-Queimadas-Internacoes/tree/main/script", "Código utilizado para manipular os dados", target = "_blank"))
           ),
           br(),
           p("Desenvolvido por: Évelyn Muniz | Última atualização: Junho de 2025")
         )
)
)
