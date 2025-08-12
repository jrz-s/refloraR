##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' Processamento de bases de dados das orquídeas da Vivian para banco de 
#' sinonimos

# -------------------------------------------------------------------------
# Load packages

if(!require("pacman")){
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(tidyverse, here,visdat)

# -------------------------------------------------------------------------
# Load database

db <- load(file = here::here("database"
                            ,"orquidea"
                            ,"tidy_data"
                            ,"orquidea_data.rda")) %>% get()
rm('orquidea_list')

# -------------------------------------------------------------------------
# Database manipulation

db <- tibble::enframe(
  x = db
  ,name = "name1"
  ,value = "name2") %>% 
  tidyr::unnest(name2) %>% 
  dplyr::select(!name1)

# -------------------------------------------------------------------------
# EAD

visdat::vis_dat(db)

# -------------------------------------------------------------------------
# Get data description to synonyms

db$ctrl_descrp
db$free_descrp_pt
db$free_descrp_en
db$public_comm

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
