##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' ECOSYS - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description -------------------------------------------------------------
#> Transformar o número de registros em rasters

# -------------------------------------------------------------------------
# Load packages -----------------------------------------------------------

pacman::p_load(here,tidyverse,MASS,ggtext,terra,sf)

# -------------------------------------------------------------------------
# Load shapefile

# Caatinga
caatinga <- sf::st_read("shp/Municipios-Caatinga/Caatinga-Municipios.shp")
caatinga <- sf::st_transform(caatinga, crs = 4674)

plot(caatinga)

# Municipios (base de dados, desde o ministerio de turismo)
municipios <- readxl::read_excel(path = here::here("database"
                                                   ,"orquidea"
                                                   ,"raw_data"
                                                   ,"turismo_leitos.xlsx"))

colnames(municipios) <- c('SIGLA_UF',"NM_MUN","HOSPEDAGEM","LEITOS")

plot(municipios)

# -------------------------------------------------------------------------
# Data manipulation

# Unir dados do município com o shape file da caatinga
caatinga2 <- caatinga %>% 
  as.data.frame() %>% 
  tibble::as_tibble() %>% 
  dplyr::inner_join(municipios) %>% 
  dplyr::arrange(SIGLA_UF) 

# sf dos leitos
leitos <- sf::st_as_sf(caatinga2) %>% 
  dplyr::select(LEITOS)

# sf dos hospedagens
hospedagem <- sf::st_as_sf(caatinga2) %>% 
  dplyr::select(HOSPEDAGEM)

# plotamos
plot(leitos)
plot(hospedagem)

# -------------------------------------------------------------------------
# Rasterizamos

# Leito

# raster base, definimos a resolução
r_base <- terra::rast(leitos, resolution = 0.5)

plot(r_base)

# raster do leito
r_leitos <- terra::rasterize(leitos, r_base, field = "LEITOS")

# plotamos o raster de leitos
plot(r_leitos)

# exportamos o raster
terra::writeRaster(
  r_leitos
  ,filename = here::here(
    "rasters"
    ,"raster_leitos_exemplo.tiff")
  ,overwrite = TRUE)

# hospedagem


# registros




# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

