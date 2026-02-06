#################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

#' Extrair as ocorrencias do fogo

# -------------------------------------------------------------------------
# Load packages -----------------------------------------------------------

library(pacman)
pacman::p_load(here,tidyverse,geobr,raster,terra)

# -------------------------------------------------------------------------
# Load raster lists

r <- list.files(path = here::here('rasters')
                ,pattern = "*.tif"
                ,full.names = TRUE)

# -------------------------------------------------------------------------
# Input

yearp <- seq(1985,2022,1)
r.extent <- c(-42,-40,-14,-10)

# -------------------------------------------------------------------------
# General loop

# i <- 1 # year

{
  
  # time start
  start.time <- Sys.time()
  
  burned_list <- list()
  
  for(i in 1:length(yearp)){
    
    burned_list[[i]] <- r[i] %>% 
      stringr::str_subset(pattern = as.character(yearp[i])) %>% 
      terra::rast() %>% 
      terra::crop(extent(r.extent)) %>% 
      as.data.frame(xy = TRUE) %>% 
      tidyr::drop_na() %>% 
      dplyr::rename("burned" = starts_with("burned")) %>% 
      dplyr::mutate(burned = ifelse(burned == 0, "no","yes")) %>% 
      dplyr::group_by(burned) %>% 
      dplyr::count() %>%
      tibble::add_column("year" = yearp[i]) %>% 
      dplyr::select(year, everything()) %>% 
      dplyr::ungroup(burned)
    
  }
  
  # save list
  save(burned_list,file = here::here('database','burned_list.rda'))
  
  # time end
  end.time <- Sys.time()
  duration <- end.time - start.time
  cat("Run duration:","\n")
  print(duration)
  
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
