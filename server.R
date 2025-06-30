# SERVER
server <- function(input, output, session) {
  dados_selecionados <- reactive({
    lista_datasets[[input$dataset_escolhido]]
  })
  
  output$coluna_numerica_ui <- renderUI({
    dados <- dados_selecionados()
    num_cols <- names(dados)[sapply(dados, is.numeric)]
    
    if (length(num_cols) == 0) return(p("Não há colunas numéricas disponíveis."))
    
    selectInput("coluna_numerica", "Coluna numérica para gráficos:", choices = num_cols)
  })
  
  output$summary_text <- renderPrint({
    summary(dados_selecionados())
  })
  
  output$tabela_dados <- renderDT({
    datatable(dados_selecionados(), options = list(pageLength = 10))
  })
  
  output$boxplot <- renderPlot({
    req(input$coluna_numerica)
    
    ggplot(dados_selecionados(), aes_string(x = "1", y = input$coluna_numerica)) +
      geom_boxplot(fill = "skyblue") +
      labs(
        x = "", y = input$coluna_numerica,
        title = paste("Boxplot de", input$coluna_numerica)
      ) +
      theme_classic() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  output$histograma <- renderPlot({
    req(input$coluna_numerica)
    
    ggplot(dados_selecionados(), aes_string(x = input$coluna_numerica)) +
      geom_histogram(aes(y = ..density..), fill = "salmon", bins = 30, alpha = 0.7) +
      labs(
        title = paste("Histograma de", input$coluna_numerica),
        x = input$coluna_numerica,
        y = "Densidade"
      ) +
      theme_classic()
  })
  ########## MAPAS TEMÁTICOS
  output$mapa_tematico <- renderPlot({
    req(input$mapa_tematico_escolhido)
    
    if (input$mapa_tematico_escolhido == "Focos por Bioma") {
      ggplot() +
        geom_sf(data = biomas, aes(fill = name_biome), color = NA, alpha = 0.5) +
        geom_sf(data = estados, fill = NA, color = "black", size = 0.4) +
        geom_text(data = estados_centroides,
                  aes(x = X, y = Y, label = abbrev_state),
                  size = 3, fontface = "bold", color = "black") +
        geom_point(data = biomas_centroides,
                   aes(x = X, y = Y, size = QtdFocos),
                   color = "red", alpha = 0.7) +
        scale_fill_brewer(palette = "Dark2", name = "Biomas") +
        scale_size_continuous(
          name = "Qtd. de Focos",
          range = c(6, 40),
          breaks = c(2000, 25000, 50000, 100000, 200000, 350000),
          labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
        ) +
        labs(title = "Focos de Calor por Bioma no Brasil",
             caption = "Fonte: INPE") +
        annotation_north_arrow(location = "tr", which_north = "true",
                               style = north_arrow_fancy_orienteering,
                               height = unit(1, "cm"), width = unit(1, "cm"),
                               pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
        annotation_scale(location = "bl", style = "ticks", text_cex = 1) +
        coord_sf() +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          legend.position = "right",
          plot.caption = element_text(hjust = 0.5),
          axis.text = element_blank(),
          axis.ticks = element_blank()
        ) +
        annotation_custom(
          grob = grid::textGrob("CRS: SIRGAS 2000",
                                x = unit(0.95, "npc"),
                                y = unit(0.1, "npc"),
                                just = c("right", "bottom"),
                                gp = grid::gpar(fontsize = 9, fontface = "italic"))
        )
      
    } else if (input$mapa_tematico_escolhido == "Focos por Estado") {
      ggplot() +
        geom_sf(data = focos_por_estado, aes(fill = QtdFocos), color = NA) +
        geom_text(data = estados_centroides,
                  aes(x = X, y = Y, label = abbrev_state),
                  size = 4, fontface = "bold", color = "black") +
        scale_fill_gradient(
          low = "#fde0dd",
          high = "#c51b8a",
          name = "Qtd. de Focos\n(2022–2024)",
          labels = scales::comma_format()
        ) +
        labs(
          title = "Total de Focos por Estado (2022–2024)",
          caption = "Fonte: INPE"
        ) +
        annotation_north_arrow(location = "tr", which_north = "true",
                               style = north_arrow_fancy_orienteering,
                               height = unit(1, "cm"), width = unit(1, "cm"),
                               pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
        annotation_scale(location = "bl", style = "ticks", text_cex = 1) +
        coord_sf() +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          legend.position = "right",
          plot.caption = element_text(hjust = 0.5),
          axis.text = element_blank(),
          axis.ticks = element_blank()
        ) +
        annotation_custom(
          grob = grid::textGrob("CRS: SIRGAS 2000",
                                x = unit(0.95, "npc"),
                                y = unit(0.1, "npc"),
                                just = c("right", "bottom"),
                                gp = grid::gpar(fontsize = 9, fontface = "italic"))
        )
      
    } else if (input$mapa_tematico_escolhido == "Biomas") {
      ggplot() +
        geom_sf(data = biomas, aes(fill = name_biome), color = NA, alpha = 0.6) +
        geom_sf(data = estados, fill = NA, color = "black", size = 0.4) +
        geom_text(data = estados_centroides,
                  aes(x = X, y = Y, label = abbrev_state),
                  size = 3, fontface = "bold", color = "black") +
        scale_fill_brewer(palette = "Set3", name = "Biomas") +
        labs(title = "Biomas em Estados Brasileiros",
             caption = "Fonte: IBGE") +
        annotation_north_arrow(location = "tr", which_north = "true",
                               style = north_arrow_fancy_orienteering,
                               height = unit(1, "cm"), width = unit(1, "cm"),
                               pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
        annotation_scale(location = "bl", style = "ticks", text_cex = 1) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          legend.position = "right",
          plot.caption = element_text(hjust = 0.5),
          axis.text = element_blank(),
          axis.ticks = element_blank()
        ) +
        coord_sf() +
        annotation_custom(
          grob = grid::textGrob("CRS: SIRGAS 2000",
                                x = unit(0.95, "npc"),
                                y = unit(0.1, "npc"),
                                just = c("right", "bottom"),
                                gp = grid::gpar(fontsize = 9, fontface = "italic"))
        )
      
    } else if (input$mapa_tematico_escolhido == "Internações por Estado") {
      ggplot() +
        geom_sf(data = internacoes_por_estado, aes(fill = Internações), color = NA) +
        geom_text(data = estados_centroides,
                  aes(x = X, y = Y, label = abbrev_state),
                  size = 4, fontface = "bold", color = "black") +
        scale_fill_gradient(
          low = "lightblue",
          high = "blue",
          name = "Internações por CID Respiratória \n(2022–2024)",
          labels = scales::comma_format()
        ) +
        labs(
          title = "Internações por CID Respiratória (2022–2024)",
          caption = "Fonte: Datasus"
        ) +
        annotation_north_arrow(location = "tr", which_north = "true",
                               style = north_arrow_fancy_orienteering,
                               height = unit(1, "cm"), width = unit(1, "cm"),
                               pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
        annotation_scale(location = "bl", style = "ticks", text_cex = 1) +
        coord_sf() +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          legend.position = "right",
          plot.caption = element_text(hjust = 0.5),
          axis.text = element_blank(),
          axis.ticks = element_blank()
        ) +
        annotation_custom(
          grob = grid::textGrob("CRS: SIRGAS 2000",
                                x = unit(0.95, "npc"),
                                y = unit(0.1, "npc"),
                                just = c("right", "bottom"),
                                gp = grid::gpar(fontsize = 9, fontface = "italic"))
        )
      
    } else if (input$mapa_tematico_escolhido == "Internações e Focos por Estado") {
      ggplot() +
        geom_sf(data = internacoes_por_estado, aes(fill = Internações), color = "gray30", size = 0.3) +
        geom_sf(data = estados, fill = NA, color = "black", size = 0.3) +
        geom_text(data = estados_centroides,
                  aes(x = X, y = Y, label = name_state),
                  size = 3.5, fontface = "bold", color = "black") +
        geom_point(data = focos_centroides,
                   aes(x = X, y = Y, size = QtdFocos),
                   color = "red", alpha = 0.7) +
        scale_fill_gradient(low = "lightyellow", high = "darkred", name = "Internações") +
        scale_size_continuous(
          name = "Qtd. de Focos",
          range = c(6, 40),
          breaks = c(500, 5000, 15000, 50000, 100000, 150000),
          labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
        ) +
        labs(title = "Internações por Estado e Focos de Calor no Brasil",
             caption = "Fonte: DataSUS e INPE") +
        annotation_north_arrow(location = "tr", which_north = "true",
                               style = north_arrow_fancy_orienteering,
                               height = unit(1, "cm"), width = unit(1, "cm"),
                               pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
        annotation_scale(location = "bl", style = "ticks", text_cex = 1) +
        coord_sf() +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          legend.position = "right",
          plot.caption = element_text(hjust = 0.5),
          axis.text = element_blank(),
          axis.ticks = element_blank()
        ) +
        annotation_custom(
          grob = grid::textGrob("CRS: SIRGAS 2000",
                                x = unit(0.95, "npc"),
                                y = unit(0.1, "npc"),
                                just = c("right", "bottom"),
                                gp = grid::gpar(fontsize = 9, fontface = "italic"))
        )
    }
  })
  
  ####### REACTIVE PARA RETORNAR OS DADOS ESPACIAIS DO MAPA ESCOLHIDO
  dados_espaciais_selecionados <- reactive({
    req(input$mapa_espacial_escolhido)
    switch(input$mapa_espacial_escolhido,
           #"Focos Pontuais" = pontos_sf,
           "Vizinhança por Contiguidade Estadual" = nb_focos_estados_sf,
           "Vizinhança por Contiguidade Biomas" = nb_bioma_sf,
           "Biomas - Moran Local" = focos_bioma_proj,
           "LISAMAP Focos Estadual" = focos_proj,
           "LISAMAP Taxa de Internação /100mil hab" = internacoes_proj,
           "LISAMAP Internação por Estado" = internacoes_proj,
           "LISAMAP Focos Biomas" = focos_bioma_proj,
           NULL)
  })
  
  ####### RENDERIZAR TABELA DOS ATRIBUTOS
  output$tabela_atributos <- DT::renderDT({
    dados <- dados_espaciais_selecionados()
    if (is.null(dados)) return(NULL)
    
    df <- sf::st_drop_geometry(dados)  # remove geometria para tabela
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  ########## MAPAS ESTATÍSTICA ESPACIAL
  output$mapa_espacial <- renderPlot({
    req(input$mapa_espacial_escolhido)
    
    #if (input$mapa_espacial_escolhido == "Focos Pontuais") {
    #ggplot() +
    # geom_sf(data = biomas, aes(fill = name_biome), color = NA, alpha = 0.6) +
    #geom_sf(data = estados, fill = NA, color = "black", size = 0.4) +
    #geom_sf(data = pontos_sf, color = "red", size = 0.5, alpha = 0.6) +
    #geom_text(data = estados_centroides,
    #aes(x = X, y = Y, label = abbrev_state),
    #size = 3, fontface = "bold", color = "black") +
    #scale_fill_brewer(palette = "Set3", name = "Biomas") +
    #labs(title = "Biomas, Estados e Focos de Calor (Pontos)",
    #caption = "Fonte: IBGE / Dados de Satélite") +
    #annotation_north_arrow(location = "tr", which_north = "true",
    #style = north_arrow_fancy_orienteering,
    #height = unit(1, "cm"), width = unit(1, "cm"),
    #pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm")) +
    #annotation_scale(location = "bl", style = "ticks", text_cex = 1) +
    #theme_minimal() +
    #theme(
    #plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  
          #legend.position = "right", 
    #plot.caption = element_text(hjust = 0.5)
    #) + 
    #coord_sf() + 
    #annotation_custom(
    #grob = grid::textGrob("CRS: SIRGAS 2000",
    # x = unit(0.95, "npc"),  
    #y = unit(0.1, "npc"), 
    #just = c("right", "bottom"),
    # gp = grid::gpar(fontsize = 9, fontface = "italic"))
    # )
    if (input$mapa_espacial_escolhido == "Vizinhança por Contiguidade Estadual") {
      ggplot() +
        geom_sf(data = focos_proj, fill = "lightpink", color = "white") +
        geom_sf(data = nb_focos_estados_sf, color = "red", size = 1.2) +
        labs(title = "Vizinhança por Contiguidade - Estados") +
        theme_classic()
    } else if (input$mapa_espacial_escolhido == "Vizinhança por Contiguidade Biomas") {
      ggplot() +
        geom_sf(data = focos_bioma_proj, fill = "lightpink", color = "white") +
        geom_sf(data = nb_bioma_sf, color = "blue", size = 1.2) +
        labs(
          title = "Vizinhança por Contiguidade - Biomas",
          x = "Longitude", y = "Latitude"
        ) +
        theme_minimal()
    } else if (input$mapa_espacial_escolhido == "Biomas - Moran Local") {
      tm1 <- tm_shape(focos_bioma_proj) +
        tm_polygons(
          fill = "pvalue",
          fill.scale = tm_scale_intervals(
            breaks = c(0, 0.01, 0.05, 0.1, 1),
            values = RColorBrewer::brewer.pal(5, "Blues")
          ),
          fill.legend = tm_legend(title = "p-valores"),
          col = "grey"
        ) +
        tm_layout(
          title = "Significância do Moran Local (Focos - Biomas)",
          title.position = c("center", "top"),
          legend.outside = TRUE
        ) +
        tm_compass(position = c("left", "top"), type = "arrow", size = 2) +
        tm_scale_bar(position = c("left", "bottom")) +
        tm_credits(
          text = "CRS: SIRGAS 2000 UTM ZONE 23 EPSG 31893",
          position = c("right", "bottom"),
          size = 0.7, fontface = "italic"
        )
      print(tm1)
    } else if (input$mapa_espacial_escolhido == "LISAMAP Focos Estadual") {
      tm2 <- tm_shape(focos_proj) +
        tm_fill(
          col = "cluster_type",
          palette = c(
            "Alto-Alto"         = "#E60000",
            "Baixo-Baixo"       = "#0033CC",
            "Baixo-Alto"        = "#9999FF",
            "Alto-Baixo"        = "#FF9999",
            "Não significativo" = "#FFFFFF"
          ),
          title = "Cluster Local (LISA)",
          legend.is.portrait = TRUE
        ) +
        tm_borders(col = "gray40", lwd = 0.4) +
        tm_layout(
          main.title = "Mapa LISA - Clusters Locais de Focos por Estado",
          main.title.size = 1.2,
          main.title.position = "center",
          legend.outside = TRUE,
          frame = FALSE,
          bg.color = "white"
        ) +
        tm_compass(type = "8star", position = c("right", "top"), size = 2) +
        tm_scale_bar(position = c("left", "bottom")) +
        tm_credits("CRS: SIRGAS 2000 / UTM Zone 23S", position = c("RIGHT", "BOTTOM"), size = 0.6)
      print(tm2)
      
    } else if (input$mapa_espacial_escolhido == "LISAMAP Taxa de Internação /100mil hab") {
      tmap_mode("plot")
      tmap_style("white")
      tm4 <- tm_shape(internacoes_proj) +
        tm_fill(
          col = "cluster_type",
          palette = c(
            "Alto-Alto"         = "#E60000",
            "Baixo-Baixo"       = "#0033CC",
            "Baixo-Alto"        = "#9999FF",
            "Alto-Baixo"        = "#FF9999",
            "Não significativo" = "#FFFFFF"
          ),
          title = "Cluster Local (LISA)",
          legend.is.portrait = TRUE
        ) +
        tm_borders(col = "gray40", lwd = 0.4) +
        tm_layout(
          main.title = "Mapa LISA - Internações por Estado (Taxa/100 mil hab.)",
          main.title.size = 1.2,
          main.title.position = "center",
          legend.outside = TRUE,
          frame = FALSE,
          bg.color = "white"
        ) +
        tm_compass(type = "8star", position = c("right", "top"), size = 2) +
        tm_scale_bar(position = c("left", "bottom")) +
        tm_credits("CRS: SIRGAS 2000 / UTM Zone 23S", position = c("RIGHT", "BOTTOM"), size = 0.6)
      print(tm4)
    } else if (input$mapa_espacial_escolhido == "LISAMAP Focos Biomas") {
      tm3 <- tm_shape(focos_bioma_proj) +
        tm_fill(
          col = "cluster_type",
          palette = c(
            "Alto-Alto"         = "#E60000",
            "Baixo-Baixo"       = "#0033CC",
            "Baixo-Alto"        = "#9999FF",
            "Alto-Baixo"        = "#FF9999",
            "Não significativo" = "#FFFFFF"
          ),
          title = "Cluster Local (LISA)",
          legend.is.portrait = TRUE
        ) +
        tm_borders(col = "gray40", lwd = 0.4) +
        tm_layout(
          main.title = "Mapa LISA - Clusters Locais de Focos por Bioma",
          main.title.size = 1.2,
          main.title.position = "center",
          legend.outside = TRUE,
          frame = FALSE,
          bg.color = "white"
        ) +
        tm_compass(type = "8star", position = c("right", "top"), size = 2) +
        tm_scale_bar(position = c("left", "bottom")) +
        tm_credits("CRS: SIRGAS 2000 / UTM Zone 23S", position = c("RIGHT", "BOTTOM"), size = 0.6)
      print(tm3)
    } else if (input$mapa_espacial_escolhido == "LISAMAP Internação por Estado") {
      tmap_mode("plot")
      tmap_style("white")
      tm5 <- tm_shape(internacoes_proj) +
        tm_fill(
          col = "cluster_type_2",
          palette = c(
            "Alto-Alto"         = "#E60000",
            "Baixo-Baixo"       = "#0033CC",
            "Baixo-Alto"        = "#9999FF",
            "Alto-Baixo"        = "#FF9999",
            "Não significativo" = "#FFFFFF"
          ),
          title = "Cluster Local (LISA)",
          legend.is.portrait = TRUE
        ) +
        tm_borders(col = "gray40", lwd = 0.4) +
        tm_layout(
          main.title = "Mapa LISA - Internações por Estado",
          main.title.size = 1.2,
          main.title.position = "center",
          legend.outside = TRUE,
          frame = FALSE,
          bg.color = "white"
        ) +
        tm_compass(type = "8star", position = c("right", "top"), size = 2) +
        tm_scale_bar(position = c("left", "bottom")) +
        tm_credits("CRS: SIRGAS 2000 / UTM Zone 23S", position = c("RIGHT", "BOTTOM"), size = 0.6)
      print(tm5)
    } else {
      plot(1, main = "Selecione uma opção")  # fallback
    }
    
})
  
  ######## Outputs modelos: 
    
    # Estatísticas GWR
output$summary_gwr <- renderPrint({
  summary(gwr_modelo)
})

output$head_gwr <- renderTable({
  head(as.data.frame(gwr_modelo$SDF))
})



# Mapa Predito GWR
output$mapa_predito_gwr <- renderPlot({
  ggplot(internacoes_proj) +
    geom_sf(aes(fill = brks.gwr), color = "black", size = 0.1) +
    scale_fill_brewer(palette = "YlOrRd", name = "Taxa") +
    ggtitle("Taxa Predita - GWR") +
    theme_void()
})

# Comparação GWR
output$comparacao_gwr <- renderPlot({
  gridExtra::grid.arrange(
    ggplot(internacoes_proj) +
      geom_sf(aes(fill = brks), color = "black", size = 0.1) +
      scale_fill_brewer(palette = "YlOrRd") +
      ggtitle("Taxa Bruta Observada") +
      theme_void(),
    ggplot(internacoes_proj) +
      geom_sf(aes(fill = brks.gwr), color = "black", size = 0.1) +
      scale_fill_brewer(palette = "YlOrRd") +
      ggtitle("Taxa Predita - GWR") +
      theme_void(),
    ncol = 2
  )
})

# Estatísticas CAR
output$summary_car <- renderPrint({
  summary(modelo_car)
})

output$head_car <- renderTable({
(modelo_car$fitted.values)
})


# Mapa Predito CAR
output$mapa_predito_car <- renderPlot({
  ggplot(internacoes_proj) +
    geom_sf(aes(fill = brks.car), color = "black", size = 0.1) +
    scale_fill_brewer(palette = "YlOrRd", name = "Taxa") +
    ggtitle("Taxa Predita - CAR") +
    theme_void()
})

# Comparação CAR
output$comparacao_car <- renderPlot({
  gridExtra::grid.arrange(
    ggplot(internacoes_proj) +
      geom_sf(aes(fill = brks), color = "black", size = 0.1) +
      scale_fill_brewer(palette = "YlOrRd") +
      ggtitle("Taxa Bruta Observada") +
      theme_void(),
    ggplot(internacoes_proj) +
      geom_sf(aes(fill = brks.car), color = "black", size = 0.1) +
      scale_fill_brewer(palette = "YlOrRd") +
      ggtitle("Taxa Predita - CAR") +
      theme_void(),
    ncol = 2
  )
})

output$plot_residuos <- renderPlot({
  cores <- c("#66C2A5", "#FC8D62", "#8DA0CB")
  results <- as.data.frame(gwr_modelo$SDF)
  r1<- car_modelo$residuals
  r2 <- results$gwr.e
  
  # Gráfico de violino
  vioplot(r1, r2,
          names = c("CAR", "GWR"),
          col = cores, border = "black", drawRect = TRUE)
  
  title(main = "Distribuição dos Resíduos por Modelo", cex.main = 1.2)
  mtext("Resíduos", side = 2, line = 2.5, cex = 1.1)
  abline(h = 0, lty = 2, col = "grey40")
})

####### TESTES
dados_teste <- reactive({
  switch(input$variavel_teste,
         "Focos por Estado" = list(lw = lw_focos_estados, var = internacoes_proj$QtdFocos),
         "Focos por Bioma" = list(lw = lw_bioma, var = focos_bioma$QtdFocos),
         "Internações por Estado" = list(lw = lw_internacoes, var = internacoes_proj$Internações),
         "Taxa de Internações por Estado" = list(lw = lw_internacoes, var = internacoes_proj$taxa_incidencia)
  )
})

output$matriz_vizinhanca <- renderDT({
  lw <- dados_teste()$lw
  mat <- as.matrix(listw2mat(lw))
  datatable(round(mat, 3), options = list(scrollX = TRUE, pageLength = 5))
})

output$resultado_teste <- renderPrint({
  lw <- dados_teste()$lw
  var <- dados_teste()$var
  moran.test(var, lw)
})

output$resultado_local <- renderDT({
  lw <- dados_teste()$lw
  var <- dados_teste()$var
  local <- localmoran(var, lw)
  colnames(local) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "p-valor")
  local_df <- as.data.frame(local) %>%
    dplyr::mutate(ID = row_number()) %>%
    dplyr::select(ID, everything()) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), round, 4))
  datatable(local_df, options = list(pageLength = 10), rownames = FALSE)
})
  }
  