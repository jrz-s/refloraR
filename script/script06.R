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

#visdat::vis_dat(db)

# base de dados principal
pdb <- readRDS(file = here::here("database"
                                 ,"393.417"
                                 ,"CompleteBrazilianFlora.rds"))

# -------------------------------------------------------------------------
# Selecionar as variaveis do Webscraping

db <- db %>% 
  dplyr::select(sc_name
                ,genus
                ,"specificEpithet" = species
                ,wfo
                ,cities
                ,ctrl_descrp
                ,free_descrp_pt
                ,public_comm
                ,citation)

db_synm <- florabr::get_synonym(data = pdb
                     ,species = db$sc_name) %>% 
  dplyr::select("sc_name" =  acceptedName
                ,synonym) %>% tibble::as_tibble()

## unir os sinomicos da base geral para a nossa base webscraping

db_webscrp_id <- db %>% 
  dplyr::inner_join(db_caat_principal %>% 
                      dplyr::mutate(sc_name = paste0(genus
                                                     ," "
                                                     ,specificEpithet)) %>%
                      dplyr::select(id,sc_name) %>% tibble::as_tibble()
                    ,by = "sc_name") %>% 
  dplyr::select(id, everything()) %>% 
  dplyr::inner_join(db_synm,by = "sc_name",relationship = "many-to-many") %>% 
  dplyr::select(id,genus
                ,specificEpithet
                ,sc_name
                ,synonym
                , everything())

# -------------------------------------------------------------------------
load(file = here::here("database"
                       ,"orquidea"
                       ,"tidy_data"
                       ,"db_completa.rda"))

db_caat_principal_id <- db_caat_principal %>% 
  dplyr::mutate(sc_name = paste0(genus," ",specificEpithet)) %>% 
  dplyr::inner_join(db_synm,by = "sc_name")


# Save tidy data

save(db_caat_principal_id,file = here::here("database"
                                         ,"orquidea"
                                         ,"tidy_data"
                                         ,"db_completa_com_ID_synm.rda")) 

# Save tidy data

save(db_webscrp_id,file = here::here("database"
                                            ,"orquidea"
                                            ,"tidy_data"
                                            ,"db_webscrp_id_synm.rda")) 

# -------------------------------------------------------------------------
# Get data description to synonyms

db$ctrl_descrp
db$free_descrp_pt
db$free_descrp_en
db$public_comm





































# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
