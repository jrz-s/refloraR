##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' Neste script vamos a aleatorizar 10 espécies para as 10 famílias botânicas
#' mais relevantes, para poder criar um dicionário de informações para 
#' fazermos o webscraping.

# -------------------------------------------------------------------------
# Load packages

if(!require("pacman")){
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(tidyverse, here)

# -------------------------------------------------------------------------
# Load 'get_reflora_info()' Function  

source(file = here::here("function","get_refloraR_info.R"))

# -------------------------------------------------------------------------
# Load database

db <- readRDS(file = here::here("database","393.417","CompleteBrazilianFlora.rds"))

# -------------------------------------------------------------------------
# Database manipulation

colnames(db)

familyp <- c("Orchidaceae","Fabaceae","Asteraceae"
             ,"Myrtaceae","Melastomataceae","Rubiaceae"
             ,"Bromeliaceae","Euphorbiaceae","Euphobiaceae"
             ,"Lauraceae","Solanaceae","Poaceae")

# fix randomization
set.seed(123)

# get species data 
mm <- db %>% dplyr::select(family
                     #,scientificName
                     #,genus
                     ,species) %>% 
  dplyr::filter(family %in% familyp) %>% 
  tidyr::drop_na() %>% tibble::as_tibble() %>%
  dplyr::group_by(family) %>% 
  dplyr::sample_n(size = 10, replace = FALSE) %>% 
  dplyr::mutate(aaa = species %>% 
                  stringr::str_detect(pattern = 'var.')) %>% 
  dplyr::filter(aaa == FALSE) %>% dplyr::select(!aaa) %>% 
  tidyr::separate(col = "species"
                  ,into = c("genus","species")
                  ,sep = " "
                  ,remove = TRUE) %>% 
  dplyr::ungroup()

# -------------------------------------------------------------------------
# Get random species data by family

{
  
  sp_list <- list()
  
  for(i in 1:length(familyp)){
    
    db_i <- mm %>% 
      dplyr::filter(family %in% familyp[i]) %>% 
      dplyr::select(!family)
    
    sp_list[i] <- get_reflora_info(genus = db_i$genus
                                   ,species = db_i$species
    ) %>% list()
    
  }
  
  names(sp_list) <- familyp
  
  save(sp_list,file = here::here('database','species_list.rda'))
  
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Getting data ("Orchidaceae")

# db_i <- mm %>% 
#   dplyr::filter(family %in% familyp[1]) %>% 
#   dplyr::select(!family)
# 
# qq <- get_reflora_info(genus = db_i$genus
#                  ,species = db_i$species)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
