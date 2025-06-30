library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(tidyr)
library(microdatasus)
library(tidyverse)
library(gridExtra)
library(purrr)
library(geobr)
library(RColorBrewer)
library(colorspace)
library(tmap)
library(leaflet)
library(leaflet.extras2)
library(leafem)
library(ggspatial)
library(vioplot)
library(sf)
library(sp)
library(raster)
library(lattice)
library(spatstat)
library(spdep)
library(spatialreg)
library(spgwr)
library(gstat)
library(ggplot2)
library(sf)
library(ggspatial)
library(grid)
library(vioplot)


# Caminho da pasta de dados
caminho_dados <- "dados"

#load(file.path(caminho_dados, "DadosBR.RData"))
load(file.path(caminho_dados, "biomas_centroides.RData"))
load(file.path(caminho_dados, "dados_biomas.RData"))
load(file.path(caminho_dados, "dados_sih.RData"))
load(file.path(caminho_dados, "dadosBR_mes.RData"))
load(file.path(caminho_dados, "estados_centroides.RData"))
load(file.path(caminho_dados, "focos_centroides.RData"))
load(file.path(caminho_dados, "focos_por_estado.RData"))
load(file.path(caminho_dados, "internacoes_por_estado.RData"))
load(file.path(caminho_dados, "shape_biomas.RData"))
load(file.path(caminho_dados, "shape_estados.RData"))
load(file.path(caminho_dados, "sih_mes.RData"))
load(file.path(caminho_dados, "focos_bioma.RData"))
load(file.path(caminho_dados, "focos_bioma_proj.RData"))
load(file.path(caminho_dados, "focos_proj.RData"))
load(file.path(caminho_dados, "internacoes_proj.RData"))
load(file.path(caminho_dados, "lw_bioma.RData"))
load(file.path(caminho_dados, "lw_focos_estados.RData"))
load(file.path(caminho_dados, "lw_internacoes.RData"))
load(file.path(caminho_dados, "nb_bioma.RData"))
load(file.path(caminho_dados, "nb_bioma_sf.RData"))
load(file.path(caminho_dados, "nb_focos_estados.RData"))
load(file.path(caminho_dados, "nb_focos_estados_sf.RData"))
load(file.path(caminho_dados, "nb_internacoes.RData"))
#load(file.path(caminho_dados, "pontos_sf.RData"))
load(file.path(caminho_dados, "POPULACAO.RData"))

car_modelo <- readRDS(file.path(caminho_dados, "modelo_car.rds"))
gwr_modelo <- readRDS(file.path(caminho_dados, "gwr_modelo.rds"))

#datasets
lista_datasets <- list(
  "População Estimada" = POPULACAO,
  "Queimadas Mensal" = dadosBR_mes,
  #"Queimadas Total" = dadosBR,
  "Biomas" = dados_biomas,
  "SIH Total" = dados_sih,
  "SIH Mensal" = sih_mes
)

