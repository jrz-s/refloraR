##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' Função para obter informações via Webscraping 
#' dados da página Reflora Brasil
#' Importante: este script monta uma função para montar um base de dados
#' numa tabela data.frame ou tibble para poder exportar como excel.
#' Este é um script que 

# -------------------------------------------------------------------------
# Load packages

if(!require("pacman")){
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(tidyverse, here, rvest, glue, chromote)

# -------------------------------------------------------------------------
# Load 'get_reflora_info()' Function  

source(file = here::here("function","get_refloraR_info.R"))

# -------------------------------------------------------------------------
# Function testing

# get information on one species
get_reflora_info(genus = "Cattleya"
                 ,species = "elongata")

# get information on several species 
aaa <- get_reflora_info(genus = c("Cattleya", "Adamantinia", "Lippia")
                 ,species = c("elongata", "miltonioides", "grata"))

View(aaa)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
