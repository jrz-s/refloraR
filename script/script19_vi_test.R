##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' vamos a carregar a base de dados 'orquidea_data.rda' e exportar em excel.

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

db <- tibble::enframe(
  x = db
  ,name = "name1"
  ,value = "name2") %>% 
  tidyr::unnest(name2) %>% 
  dplyr::select(!name1) 

# identificamos os nomes repetidos

mm <- db %>% 
  dplyr::select(sc_name) %>% 
  dplyr::group_by(sc_name) %>% 
  dplyr::count() %>% 
  dplyr::filter(n>1)

nn <- db %>% 
  dplyr::filter(sc_name %in% mm$sc_name) 

# retiramos os nomes repetidos na base de dados

db <- db %>% 
  dplyr::filter(!sc_name %in% mm$sc_name) %>% 
  tibble::add_row(nn[c(1,3),]) %>% 
  dplyr::select(sc_name, ctrl_descrp, free_descrp_pt, free_descrp_en, public_comm)

# -------------------------------------------------------------------------
# Export database ---------------------------------------------------------

writexl::write_xlsx(x = db
                    ,path = here::here("database"
                                       ,"orquidea"
                                       ,'tidy_data'
                                       ,'webscraping_final_06022026.xlsx'))

save(db
     ,file = here::here("database"
                        ,"orquidea"
                        ,'tidy_data'
                        ,'webscraping_final_06022026.rda'))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------






























































