
# vamos a adicionar puxar as categorias do turismo e adicionar na matriz de presença-ausencia
# que está na pasta 'database', 'orquidea', 'raw_data'

municipios <- readxl::read_excel(path = here::here("database"
                                                    ,"orquidea"
                                                    ,"raw_data"
                                                    ,"cat_turismo.xlsx"))


colnames(municipios) <- c("NM_MUN",'SIGLA_UF','CATEGORIA')

caatinga <- st_read("shp/Municipios-Caatinga/Caatinga-Municipios.shp")
caatinga <- st_transform(caatinga, crs = 4674)  # garantir CRS em graus
plot(caatinga)


bb <- caatinga %>% as.data.frame() %>% tibble::as_tibble() %>% 
  dplyr::inner_join(municipios)

caatinga2 <- sf::st_as_sf(bb) %>% 
  dplyr::select(CATEGORIA)

plot(caatinga2)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# export raster
terra::writeRaster(
  rt
  ,filename = here::here(
    result_join_file
    ,regionp[i]
    ,scenariop[j]
    ,file.save
    ,variablep[p]
    ,r.name)
  ,overwrite = TRUE)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

r30 <- terra::rast(x = choose.files())
plot(r30)

r05 <- terra::aggregate(r30, fact = 60, fun = mean)

plot(r05)

# Cria um raster vazio com a extensão e resolução do shapefile
r_template <- rast(grid)

# Reamostra o raster original para coincidir com o template
r05 <- resample(r30, r_template, method = "average")

plot(r05)

# Cortar pela extensão
r_crop <- terra::crop(r05, grid)

# Mascarar (ajustar ao formato exato do shapefile)
r_mask <- raster::mask(r_crop, grid)

plot(r_crop)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

library(terra)

# 1. Ler o shapefile (pode ser .shp, .gpkg, etc.)
grid <- grid_filtrada

# 2. Criar um raster base (define resolução e extensão)
# Exemplo: resolução de 0.01 graus
r_base <- rast(grid, resolution = 0.5)

# 3. Converter (rasterizar) com base em um atributo do shapefile
# Suponha que o shapefile tenha uma coluna chamada "valor"
r_grid <- rasterize(grid, r_base, field = "n_especies")

# 4. Salvar o resultado
# writeRaster(r_grid, "caminho/para/saida_grid_raster.tif", overwrite = TRUE)

plot(r_grid)

# export raster
terra::writeRaster(
  r_grid
  ,filename = here::here(
    "rasters"
    ,"gride_05_example.tiff")
  ,overwrite = TRUE)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------























