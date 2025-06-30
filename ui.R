
ui <- navbarPage(
  title = "Focos de Queimada e Internações no Brasil 2022–2024",
  theme = shinytheme("flatly"),
  tabPanel("Início",
           fluidPage(
             h1("Bem-vindo ao Painel Interativo :)"),
             p("Este aplicativo permite explorar dados espaciais sobre queimadas e internações hospitalares no Brasil
               entre os anos de 2022 e 2024.
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
                 column(width = 6,
                        h3("Boxplot"),
                        plotOutput("boxplot")
                 ),
                 column(width = 6,
                        h3("Histograma"),
                        plotOutput("histograma")
                 )
               ),
               h3("Banco de Dados"),
               DTOutput("tabela_dados")
             )
           )
  ),
  
  tabPanel("Mapas Temáticos",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 "mapa_tematico_escolhido", "Selecione o mapa:",
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
  
  tabPanel("Estatística Espacial",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 "mapa_espacial_escolhido", "Selecione o mapa:",
                 choices = c(
                   #"Focos Pontuais",
                   "Vizinhança por Contiguidade Estadual",
                   "Vizinhança por Contiguidade Biomas",
                   "Biomas - Moran Local",
                   "LISAMAP Focos Estadual",
                   "LISAMAP Focos Biomas",
                   "LISAMAP Taxa de Internação /100mil hab"
                 )
               ),
               selectInput(
                 "modelo_espacial", "Selecione o modelo espacial:",
                 choices = c("Modelo CAR", "Modelo GWR")
               )
             ),
             mainPanel(
               plotOutput("mapa_espacial", height = "700px", width = "100%"),
               br(),
               DTOutput("tabela_atributos")  # Tabela abaixo do mapa
             )
           )
  )
)
