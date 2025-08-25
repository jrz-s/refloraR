##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' Neste script adaptei o scritp 09 para obter apenas as famílias numa lista só.
#' consulting link to arrange for first letter: https://r-coder.com/arrange-dplyr-r/

# -------------------------------------------------------------------------
# Load packages
if(!require("pacman")){
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(tidyverse, here, janitor)

# -------------------------------------------------------------------------
# Load 'get_reflora_info()' Function  

source(file = here::here("function","get_refloraR_info.R"))

# -------------------------------------------------------------------------
# Load database Vivi

# base de dados principal
db1 <- load(file = here::here('database'
                              ,'orquidea'
                              ,'tidy_data'
                              ,'db_caat.rda')) %>% get()

rm('db_caat')

# -------------------------------------------------------------------------
db2 <- load(file = here::here('database'
                              ,'orquidea'
                              ,'tidy_data'
                              ,'db_completa_com_ID_synm.rda')) %>% get() 

rm('db_caat_principal_id')

db2 <- db2 %>% dplyr::select(!synonym) %>% 
  dplyr::distinct()


db2$sc_name %>% unique %>% length()
db1$sci_name %>% unique %>% length()

# -------------------------------------------------------------------------
db3 <- load(file = here::here('database'
                              ,'orquidea'
                              ,'tidy_data'
                              ,'db_completa.rda')) %>% get() 

rm('db_caat_principal')

sp_unique <- db1$sci_name %>% unique

# -------------------------------------------------------------------------
db4 <- db3 %>% dplyr::select(all_of(colnames(db2)[-length(colnames(db2))])) %>% 
  dplyr::mutate(sci_name = paste0(genus," ",specificEpithet)) %>% 
  #dplyr::filter(sci_name %in% sp_unique) %>% 
  dplyr::select(sci_name, habitat)

# -------------------------------------------------------------------------
db_caat_habitat <- db1 %>% 
  dplyr::left_join(db4,by = "sci_name")


mm <- db5 %>% dplyr::filter(is.na(habitat))

db4 %>% dplyr::filter(sci_name %in% mm$sci_name %>% unique)

# save in rda and excel

writexl::write_xlsx(x = db_caat_habitat
                    ,path = here::here("database"
                                       ,"orquidea"
                                       ,"tidy_data"
                                       ,"db_caat_habitat.xlsx"))

save(db_caat_habitat
     ,file = here::here("database"
                        ,"orquidea"
                        ,"tidy_data"
                        ,"db_caat_habitat.rda"))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------












































































