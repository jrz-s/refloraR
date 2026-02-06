##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' Iniciar o processo de extração dos traits da base de dados de 
#' ReFloraBrasil
#' ESTRATÉGIA Nº1

# -------------------------------------------------------------------------
# Load packages
# if(!require("pacman")){
#   install.packages("pacman")
#   library(pacman)
# }

pacman::p_load(here, tidyverse, janitor)

# -------------------------------------------------------------------------
# Load Functions

source(here::here('script','traits','blocoA.R'))

# -------------------------------------------------------------------------
# Load database

db <- load(file = here::here('database'
                             ,'orquidea'
                             ,'tidy_data'
                             ,'db_webscrp_id_synm.rda')) %>% 
  get() %>% 
  dplyr::select(!synonym) %>% 
  unique() %>% 
  dplyr::select(id, ctrl_descrp, free_descrp_pt)

rm("db_webscrp_id")

# -------------------------------------------------------------------------
# Database manipulation

## EXEMPLO: AMOSTRA DE DADOS

db <- db[c(1,9,10,14,19,20,42,48,57),c(1,2)]

# writexl::write_xlsx(x = db,path = here::here("teste.xlsx"))

# -------------------------------------------------------------------------

db_clean <- db %>%
  dplyr::mutate(
    status = map_chr(ctrl_descrp, validar_descricao)
  )

# -------------------------------------------------------------------------

# dados_quant <- db_clean |> filter(status == "ok")
# dados_qual  <- db_clean |> filter(status %in% c("ok", "morfologia_sem_medidas"))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------