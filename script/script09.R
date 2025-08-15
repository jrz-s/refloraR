##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' Neste script vamos a fazer o webscraping de toda a base de dados do Reflora Brasil
#' Vamos a gerar dados binários para cada gênero e respectivas espécies
#' É bom quantificar quantas gêneros temos, depois verificar quantas espécies por gênero 
#' temos para poder fazer pouco a pouco a extração e ir armazenando-a 
#' em pastas específicas, Ex.: Myrtaceas -> part1, part2, etc.
# -------------------------------------------------------------------------
# Load packages

if(!require("pacman")){
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(tidyverse, here, janitor)

# -------------------------------------------------------------------------
# 

