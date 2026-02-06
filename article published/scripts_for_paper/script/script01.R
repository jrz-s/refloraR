#################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

#' Renomear os rasters (nomes curtos)

# -------------------------------------------------------------------------
# Load packages -----------------------------------------------------------

library(pacman)
pacman::p_load(here,tidyverse)

# -------------------------------------------------------------------------
# Load raster lists

r <- list.files(path = here::here('rasters')
                      ,pattern = "*.tif"
                      ,full.names = TRUE)

# -------------------------------------------------------------------------
# Rename rasters

file.rename(from = r,to = r %>%
              stringr::str_remove(pattern = "-0000000000-0000000000") %>% 
              stringr::str_remove(pattern = "-brazil") %>% 
              stringr::str_remove(pattern = "20-"))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
