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
  dplyr::distinct()

# -------------------------------------------------------------------------
# Get random species data by family

# order matrix with alphabetic
# https://r-coder.com/arrange-dplyr-r/

familyp <- db %>% 
  dplyr::select(family) %>% 
  dplyr::distinct() %>% 
  tibble::as_tibble() %>% 
  dplyr::arrange(substr(family, 1, 1)) %>% 
  unlist %>% as.vector

# i <- 1 # family
# j <- 1 # genus

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
  
  # family loops
  for(i in 1:length(familyp)){
    
    start.time1 <- Sys.time()
    
    # database folder
    if(!dir.exists(paths = here::here("database"
                                      ,"reflora_database"
                                      ,familyp[i]))){
      dir.create(path =  here::here("database"
                                    ,"reflora_database"
                                    ,familyp[i]))
    }
    
    cat("Botanical family |", familyp[i], "\n")
    
    # list created
    sp_list <- list()
    
    # database manipulation
    db_i <- db %>% 
      dplyr::filter(family %in% familyp[i]) %>% 
      dplyr::select(!family)
    
    # get genus from family
    genusp <- db_i$genus %>% unique
    
    # Genus loops
    for(j in 1:length(genusp)){
      
      db_j <- db_i %>% 
        dplyr::filter(genus %in% genusp[j])
      
      sp_list[j] <- get_reflora_info(genus = db_j$genus
                                     ,species = db_j$species) %>% 
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
                                   ,familyp[i]
                                   ,paste0("list_",familyp[i])
    ))
    
    # save spent time - botanical family
    end.time1 <- Sys.time()
    duration1 <- end.time1 - start.time1
    write.table(x = paste0("Time difference of "
                           ,lubridate::as.duration(duration1 %>% round(2))) %>% 
                  unlist %>% as.vector
                ,file = here::here("database"
                                   ,"reflora_database"
                                   ,familyp[i]
                                   ,paste0(familyp[i],"_time",".txt")
                ))
    
    cat("Run duration by botanical family:", "\n")
    print(duration1)
    
  }
  
  end.time <- Sys.time()
  duration <- end.time - start.time
  cat("### End Run ###", "\n")
  cat("Webscraping - Total Run duration:", "\n")
  print(duration)
  
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
