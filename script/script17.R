##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description -------------------------------------------------------------
#> Neste script vamos tentar realizar outros modelos para ver o comportamento real da relação entre o número de espécies e leitos

# -------------------------------------------------------------------------
# Load packages -----------------------------------------------------------
pacman::p_load(here,tidyverse,MASS,ggtext,terra)

# -------------------------------------------------------------------------
# Load database -----------------------------------------------------------

r_especies <- terra::rast(x = here::here("rasters","raster_nespecies.tiff"))
r_registros <- terra::rast(x = here::here("rasters","raster_ocorrencias.tiff"))
r_hospedagem <- terra::rast(x = here::here("rasters","raster_hospedagem.tiff"))
r_leitos <- terra::rast(x = here::here("rasters","raster_leitos.tiff"))

plot(r_especies)
plot(r_registros)
plot(r_hospedagem)
plot(r_leitos)

# -------------------------------------------------------------------------
# Data manipulation -------------------------------------------------------

df_especies <- as.data.frame(r_especies, xy = TRUE) 
df_registros <- as.data.frame(r_registros, xy = TRUE) 
df_hospedagem <- as.data.frame(r_hospedagem, xy = TRUE)
df_leitos <- as.data.frame(r_leitos, xy = TRUE)

# Inner join
db <- df_especies %>% 
  dplyr::inner_join(df_registros) %>% 
  dplyr::inner_join(df_hospedagem) %>% 
  dplyr::inner_join(df_leitos) 

# rename variables
colnames(db) <- c('x','y','nespecies','nregistros','hospedagem','leitos')

# Remove 0
db <- db %>% 
  dplyr::filter(nespecies>0
                ,nregistros>0
                ,hospedagem>0
                ,leitos>0)

# -------------------------------------------------------------------------
# Modelagem ---------------------------------------------------------------

# Modelo log–linear (GLM com log de X)
mdl_log <- MASS::glm.nb(nregistros ~ log(leitos + 1), data = db)
summary(mdl_log)
performance::r2(mdl_log)
par(mfrow = c(2, 2))
plot(mdl_log)
par(mfrow = c(1,1))
DHARMa::simulateResiduals(fittedModel = mdl_log, plot = TRUE)

# Modelo exponencial simples (Y = a * b^X)
mdl_exp <- lm(log(nregistros) ~ leitos, data = db)
summary(mdl_exp)
performance::r2(mdl_exp)
par(mfrow = c(2, 2))
plot(mdl_exp)
par(mfrow = c(1,1))
DHARMa::simulateResiduals(fittedModel = mdl_exp, plot = TRUE)

# Modelo potencial (Y = a * X^b)
mdl_pow <- lm(log(nregistros) ~ log(leitos + 1), data = db)
summary(mdl_pow)
performance::r2(mdl_pow)
par(mfrow = c(2, 2))
plot(mdl_pow)
par(mfrow = c(1,1))
DHARMa::simulateResiduals(fittedModel = mdl_pow, plot = TRUE)

# comparar modelos
performance::compare_performance(mdlbn, mdl_log, mdl_pow)

# -------------------------------------------------------------------------
# Gráfico log-log (forma linearizada)

ggplot(db, aes(x = log(leitos + 1)
               , y = log(nregistros))) +
  
  geom_point(size = 2) +
  
  geom_smooth(method = "lm"
              , se = TRUE
              , color = "blue") +
  
  labs(x = "log(Leitos + 1)"
       , y = "log(Número de registros)"
       , title = "Relação potencial entre leitos e número de espécies") +
  
  theme_minimal()

# -------------------------------------------------------------------------
# Gráfico na escala original (forma potencial)
# gerar predições no espaço original

newdata <- data.frame(leitos = seq(min(db$leitos)
                                   , max(db$leitos)
                                   , length.out = 100))

newdata$pred <- exp(0.9896) * (newdata$leitos + 1)^0.1942

ggplot(db, aes(x = leitos
               , y = nregistros)) +
  
  geom_point(size = 2) +
  
  geom_line(data = newdata
            , aes(x = leitos
                  , y = pred)
            , color = "blue"
            , linewidth = 1.2) +
  
  labs(x = "Número de leitos", y = "Número de registros",
       title = "Ajuste do modelo potencial: Y = 2.69 * (X)^0.194") +
  
  theme_minimal()

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
