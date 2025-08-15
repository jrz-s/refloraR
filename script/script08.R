##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' Script para carregar dados binários no formato (rda) e transformar em excel.

# -------------------------------------------------------------------------
# Load packages

if(!require("pacman")){
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(tidyverse, here)

# -------------------------------------------------------------------------
# Load database

# base de dados obtidas por webscraping
wdb <- load(file = here::here("database"
                       ,"orquidea"
                       ,"tidy_data"
                       ,"db_webscrp_id_synm.rda")) %>% get()

# remover a base de dados com nome extenso.
rm("db_webscrp_id")

# exportar como excel
writexl::write_xlsx(x = wdb
                    ,path = here::here("database"
                                       ,"orquidea"
                                       ,"tidy_data"
                                       ,"wdb.xlsx"))

# -------------------------------------------------------------------------

# base de dados totais

db_total <- load(file = here::here("database"
                       ,"orquidea"
                       ,"tidy_data"
                       ,"db_completa_com_ID_synm.rda")) %>% get()


# remover a base de dados com nome extenso.
rm("db_caat_principal_id")


# Exportar dados em excel
writexl::write_xlsx(x = db_total
                   ,path = here::here("database"
                                      ,"orquidea"
                                      ,"tidy_data"
                                      ,"db_total.xlsx"))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------