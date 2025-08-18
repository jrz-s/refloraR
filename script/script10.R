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
# Load database

# base de dados principal
pdb <- readRDS(file = here::here("database","393.417","CompleteBrazilianFlora.rds"))

# database
db <- pdb %>% 
  dplyr::select(id
                ,family
                ,genus
                ,"species" = specificEpithet
                ,taxonomicStatus 
                ,group
  ) %>% dplyr::filter(taxonomicStatus == "Accepted"
                      ,group == "Angiosperms"
                      ,!is.na(genus)
                      ,!is.na(species)
  ) %>% 
  dplyr::select(!c(id,taxonomicStatus,group)) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(substr(family, 1, 1)) %>% 
  dplyr::mutate(scientificName = paste0(genus," ",species)
                ,scientificName = scientificName %>% stringr::str_squish())

# -------------------------------------------------------------------------
# Get random species data by family

# order matrix with alphabetic

familyp <- db %>% 
  dplyr::select(family) %>% 
  dplyr::distinct() %>% 
  tibble::as_tibble() %>% 
  dplyr::arrange(substr(family, 1, 1)) %>% 
  unlist %>% as.vector

# i <- 1 # family

# Loops
{
  
  cat("Start Run", "\n")
  start.time <- Sys.time()
  
  # database folder
  if(!dir.exists(paths = here::here("database"
                                    ,"reflora_database"))){
    dir.create(path =  here::here("database"
                                  ,"reflora_database"))
  }
  
  # list created
  sp_list <- list()
  
  # family loops
  for(i in 1:length(familyp)){
    
    cat("Botanical family |", familyp[i], "\n")
    
    # database manipulation
    db_i <- db %>% 
      dplyr::filter(family %in% familyp[i]) %>% 
      dplyr::select(!family)
    
    # fill list
    sp_list[i] <- get_reflora_info(scientificName = db_i$scientificName) %>% 
      dplyr::select(sc_name
                    ,wfo
                    ,cities
                    ,ctrl_descrp
                    ,free_descrp_pt
                    ,free_descrp_en
                    ,public_comm
                    ,taxon_link
                    ,reference) %>% list()
    
  }
  
  # rename data
  names(sp_list) <- genusp
  
  # save data
  save(sp_list,file = here::here('database'
                                 ,"reflora_database"
                                 ,"total_reflora.rda"))
  
  # final database
  end.time <- Sys.time()
  duration <- end.time - start.time
  write.table(x = paste0("Time difference of "
                         ,lubridate::as.duration(duration %>% round(2))) %>% 
                unlist %>% as.vector
              ,file = here::here("database"
                                 ,"reflora_database"
                                 ,"reflora_time_spent.txt"))
  cat("### End Run ###", "\n")
  cat("Webscraping - Total Run duration:", "\n")
  print(duration)
  
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
