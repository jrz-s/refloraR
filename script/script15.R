
# vamos a adicionar puxar as categorias do turismo e adicionar na matriz de presença-ausencia
# que está na pasta 'database', 'orquidea', 'raw_data'

municipios <- readxl::read_excel(path = here::here("database"
                                                    ,"orquidea"
                                                    ,"raw_data"
                                                    ,"turismo_leitos.xlsx"))


colnames(municipios) <- c('SIGLA_UF',"NM_MUN","HOSPEDAGEM","LEITOS")

caatinga <- st_read("shp/Municipios-Caatinga/Caatinga-Municipios.shp")
caatinga <- st_transform(caatinga, crs = 4674)  # garantir CRS em graus
plot(caatinga)


caatinga2 <- caatinga %>% as.data.frame() %>% tibble::as_tibble() %>% 
  dplyr::inner_join(municipios) %>% 
  dplyr::arrange(SIGLA_UF) 

leitos <- sf::st_as_sf(caatinga2) %>% 
  dplyr::select(LEITOS)

hospedagem <- sf::st_as_sf(caatinga2) %>% 
  dplyr::select(HOSPEDAGEM)

plot(leitos)
plot(hospedagem)

# Transformar o shapefile em raster: LEITOS

r_base <- rast(leitos, resolution = 0.5)
r_grid_leitos <- rasterize(leitos, r_base, field = "LEITOS")
plot(r_grid_leitos)
terra::writeRaster(
  r_grid_leitos
  ,filename = here::here(
    "rasters"
    ,"raster_leitos.tiff")
  ,overwrite = TRUE)

# Transformar o shapefile em raster: HOSPEDAGENS

r_base <- rast(hospedagem, resolution = 0.5)
r_grid_hospedagem <- rasterize(hospedagem, r_base, field = "HOSPEDAGEM")
plot(r_grid_hospedagem)
terra::writeRaster(
  r_grid_leitos
  ,filename = here::here(
    "rasters"
    ,"raster_hospedagem.tiff")
  ,overwrite = TRUE)

# -------------------------------------------------------------------------

rrr <- r %>% 
  terra::crop(r_grid_hospedagem) %>% 
  terra::mask(hospedagem) %>% 
  terra::trim(hospedagem)

plot(rrr)
plot(r_grid_hospedagem)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

xx <- as.data.frame(r,xy = TRUE) 
yy <- as.data.frame(r_grid_hospedagem,xy = TRUE)
zz <- as.data.frame(r_grid_leitos,xy = TRUE)

db <- xx %>% 
  dplyr::inner_join(yy) %>% 
  dplyr::inner_join(zz) 

colnames(db) <- c('x','y','nespecies','hospedagem','leitos')

db <- db %>% 
  dplyr::filter(leitos>0
                ,hospedagem>0
                ,nespecies>0)


writexl::write_xlsx(db
                    ,path = here::here("database"
                                       ,"orquidea"
                                       ,"tidy_data"
                                       ,"caatinga_orquidea_turismo.xlsx"))


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

mdl <- lm(nespecies~leitos,data = db)

res <- stats::residuals(mdl)

stats::shapiro.test(res) 
vi <- db$leitos
grupos <- cut(vi, breaks = quantile(vi, probs = seq(0, 1, 0.25)), include.lowest = TRUE)
bartlett.test(res ~ grupos)

# Definimos o painel 2x2
par(mfrow = c(2, 2))

# Plotamos os gráficos de diagnóstico
plot(mdl)

par(mfrow = c(1, 1))
boxplot(res ~ grupos, data = db)

anova(mdl)
summary(mdl)

performance::r2(mdl)

ggplot2::ggplot(
  data = db
  ,mapping = aes(x = leitos
                 ,y = nespecies)) +
  
  geom_point() +
  
  geom_smooth(
    method = "lm"
    ,se = TRUE
    ,formula = y ~ x) +
  
  ggpmisc::stat_poly_eq(
    formula = y ~ x
    ,parse = TRUE
    ,aes(label = paste(after_stat(eq.label)
                       ,after_stat(rr.label)
                       ,after_stat(p.value.label)
                       ,sep = "~~~")) 
    ,label.x = 0.10
    ,label.y = 0.95) + 
  
  labs(y = "Nº de espécies"
       ,x = "Nº de leitos") +
  
  theme_bw()

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

mdl <- glm(nespecies ~ leitos,data = db,family = 'poisson')

res <- stats::residuals(mdl)

stats::shapiro.test(res) 
vi <- db$leitos
grupos <- cut(vi, breaks = quantile(vi, probs = seq(0, 1, 0.25)), include.lowest = TRUE)
bartlett.test(res ~ grupos)

# Definimos o painel 2x2
par(mfrow = c(2, 2))

# Plotamos os gráficos de diagnóstico
plot(mdl)

par(mfrow = c(1, 1))
boxplot(res ~ grupos, data = db)

anova(mdl)
summary(mdl)

performance::check_overdispersion(mdl)

par(mfrow = c(1,1))
DHARMa::simulateResiduals(fittedModel = mdl, plot = TRUE)

performance::r2(mdl)

ggplot2::ggplot(
  data = db
  ,mapping = aes(x = leitos
                 ,y = nespecies)) +
  
  geom_point() +
  
  geom_smooth(
    method = "glm"
    ,se = TRUE
    ,formula = y ~ x) +
  
  ggpmisc::stat_poly_eq(
    formula = y ~ x
    ,parse = TRUE
    ,aes(label = paste(after_stat(eq.label)
                       ,after_stat(rr.label)
                       ,after_stat(p.value.label)
                       ,sep = "~~~")) 
    ,label.x = 0.10
    ,label.y = 0.95) + 
  
  labs(y = "Nº de espécies"
       ,x = "Nº de leitos") +
  
  theme_bw()

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

library(MASS)

mdl2 <- MASS::glm.nb(nespecies ~ leitos,data = db,link = log)

# Definimos o painel 2x2
par(mfrow = c(2, 2))

# Plotamos os gráficos de diagnóstico
plot(mdl2)

anova(mdl2)
summary(mdl2)
deviance(mdl2) / df.residual(mdl2)
performance::check_overdispersion(mdl2)

par(mfrow = c(1,1))
DHARMa::simulateResiduals(fittedModel = mdl2, plot = TRUE)

performance::r2(mdl2)

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

r <- terra::rast(x = here::here("rasters","gride_05_example.tiff"))

plot(r)

as.data.frame(r,xy = TRUE)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
