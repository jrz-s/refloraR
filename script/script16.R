##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' ECOSYS - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description -------------------------------------------------------------
#> Neste script vamos a gerar uma base de dados com o número de espécies de orquídeas ocorrentes,
#> número de hospedagens e número de leitos na Caatinga.
#> Na sequência, verificaremos uma regressão linear.

# -------------------------------------------------------------------------
# Load packages -----------------------------------------------------------

pacman::p_load(here,tidyverse,MASS,ggtext,terra)

# -------------------------------------------------------------------------
# Load database -----------------------------------------------------------

r_especies <- terra::rast(x = here::here("rasters","raster_nespecies.tiff"))
r_ocorrencias <- terra::rast(x = here::here("rasters","raster_ocorrencias.tiff"))
r_hospedagem <- terra::rast(x = here::here("rasters","raster_hospedagem.tiff"))
r_leitos <- terra::rast(x = here::here("rasters","raster_leitos.tiff"))

plot(r_especies)
plot(r_hospedagem)
plot(r_leitos)

# -------------------------------------------------------------------------
## padronizar a extensao para melhor representacao
r1 <- terra::rast(x = here::here("rasters","raster_nespecies.tiff"))
r2 <- terra::rast(x = here::here("rasters","raster_hospedagem.tiff"))
r3 <- terra::rast(x = here::here("rasters","raster_leitos.tiff"))

ref <- r1  # referência
r2_res <- terra::resample(r2, ref, method = "bilinear")  # ou "near" se for dados categóricos
r3_res <- terra::resample(r3, ref, method = "bilinear")  # provavelmente já alinhado, mas garante

r2_res <- terra::crop(r2_res, ext(ref))
r3_res <- terra::crop(r3_res, ext(ref))

stacked <- c(r1, r2_res, r3_res)
names(stacked) <- c("n_especies", "hospedagem", "leitos")

# plotar juntos
plot(
  stacked
  ,nc = 3          
  ,nr = 1          
  ,mar = c(3, 3, 2, 5)
  ,main = c("Número de espécies", "Hospedagem", "Leitos"))

# -------------------------------------------------------------------------
# Data manipulation -------------------------------------------------------

df_especies <- as.data.frame(r_especies, xy = TRUE) 
df_hospedagem <- as.data.frame(r_hospedagem, xy = TRUE)
df_leitos <- as.data.frame(r_leitos, xy = TRUE)

# Inner join
db <- df_especies %>% 
  dplyr::inner_join(df_hospedagem) %>% 
  dplyr::inner_join(df_leitos) 

# rename variables
colnames(db) <- c('x','y','nespecies','hospedagem','leitos')

# Remove 0
db <- db %>% 
  dplyr::filter(nespecies>0
                ,hospedagem>0
                ,leitos>0)

# export database
# writexl::write_xlsx(db
#                     ,path = here::here("database"
#                                        ,"orquidea"
#                                        ,"tidy_data"
#                                        ,"caatinga_orquidea_turismo.xlsx"))

### EAD

# Estrutura dos dados
dplyr::glimpse(db)

# Estrutura visual dos dados
visdat::vis_dat(db)

# Resumo descritivo
skimr::skim(db)

# -------------------------------------------------------------------------
# Linear regression analysis

summary(db)

# -------------------------------------------------------------------------
## Modelo 1 # linear

mdl <- lm(nespecies ~ leitos, data = db) 
mdl

# get residuals
res <- stats::residuals(mdl)

# Normalidade
stats::ks.test(x = res, y = "pnorm", mean = mean(res), sd = sd(res)) 
stats::ks.test(x = res, y = "pnorm", mean = 0, sd = 1) 
stats::shapiro.test(res) 

# Homocedasticidade

vi <- db$leitos
grupos <- cut(vi, breaks = quantile(vi, probs = seq(0, 1, 0.25)), include.lowest = TRUE)
bartlett.test(res ~ grupos)

# Plotamos os gráficos de diagnóstico
par(mfrow = c(2, 2))
plot(mdl)

# Complementarmente podemos utilizar um boxplot
par(mfrow = c(1, 1))
boxplot(res ~ grupos, data = db)

# Anova
anova(mdl)

# Resumo estatístico (prestem atenção aqui!!!)
summary(mdl)

# Obter coeficientes
coeficientes <- data.frame('parameters' = c("a","b")
                           ,'values' = c(coef(mdl)[1],coef(mdl)[2])) %>% 
  rownames_to_column("row") %>% dplyr::mutate(row = 1:2) %>% 
  column_to_rownames('row')

# Obter parâmetros
parametros <- broom::glance(mdl) %>% 
  dplyr::rename("r2" = r.squared) %>% 
  dplyr::mutate(r = sqrt(r2)) %>% 
  dplyr::select(r2, r, p.value) %>%
  tidyr::pivot_longer(cols = 1:3, names_to = 'parameters',values_to = 'values') 

# Unir coeficientes e parâmetros
reg.coef <- coeficientes %>% tibble::add_row(parametros) %>% 
  dplyr::mutate(values = values %>% round(4) %>% sprintf("%.4f",.))

# Verificar os coeficientes e parâmetros
print(reg.coef)

# Elaboramos o gráfico

ggplot(
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
  
  labs(y = "Nº Leitos"
       ,x = "Nº Espécies") +
  
  theme_bw()

# -------------------------------------------------------------------------
## Modelo 2 (GLM Poisson) 

mdlp <- glm(nespecies ~ leitos,data = db,family = 'poisson')

# get residuals
res <- stats::residuals(mdlp)

# Plotamos os gráficos de diagnóstico
par(mfrow = c(2, 2))
plot(mdlp)

# boxplot
vi <- db$leitos
grupos <- cut(vi, breaks = quantile(vi, probs = seq(0, 1, 0.25)), include.lowest = TRUE)
par(mfrow = c(1, 1))
boxplot(res ~ grupos, data = db)

# Anova
anova(mdlp)

# Resumo estatístico (prestem atenção aqui!!!)
summary(mdlp)

# Check overdispersion
performance::check_overdispersion(mdlp)

# DHARMa verification
par(mfrow = c(1,1))
DHARMa::simulateResiduals(fittedModel = mdlp, plot = TRUE)

# Get R²
performance::r2(mdlp)

# -------------------------------------------------------------------------
## Modelo 3 (GLM Binomial negativa) 

mdlbn <- MASS::glm.nb(nespecies ~ leitos,data = db,link = log)

# get residuals
res <- stats::residuals(mdlbn)

# Plotamos os gráficos de diagnóstico
par(mfrow = c(2, 2))
plot(mdlbn)

# boxplot
vi <- db$leitos
grupos <- cut(vi, breaks = quantile(vi, probs = seq(0, 1, 0.25)), include.lowest = TRUE)
par(mfrow = c(1, 1))
boxplot(res ~ grupos, data = db)

# Anova
anova(mdlbn)

# Resumo estatístico (prestem atenção aqui!!!)
summary(mdlbn)

# Check overdispersion
performance::check_overdispersion(mdlbn)

# DHARMa verification
par(mfrow = c(1,1))
DHARMa::simulateResiduals(fittedModel = mdlbn, plot = TRUE)

# Get R²
performance::r2(mdlbn)

performance::check_overdispersion(mdlp)
performance::r2(mdlp)
performance::r2(mdlbn)

performance::r2(mdl)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Sequência de valores para predição
newdata <- data.frame(leitos = seq(min(db$leitos), max(db$leitos), length.out = 100))

# Predições Poisson
pred_pois <- predict(mdlp, newdata, type = "link", se.fit = TRUE)
newdata$pred_pois <- exp(pred_pois$fit)
newdata$lower_pois <- exp(pred_pois$fit - 1.96 * pred_pois$se.fit)
newdata$upper_pois <- exp(pred_pois$fit + 1.96 * pred_pois$se.fit)

# Predições Binomial Negativa
pred_nb <- predict(mdlbn, newdata, type = "link", se.fit = TRUE)
newdata$pred_nb <- exp(pred_nb$fit)
newdata$lower_nb <- exp(pred_nb$fit - 1.96 * pred_nb$se.fit)
newdata$upper_nb <- exp(pred_nb$fit + 1.96 * pred_nb$se.fit)

# -------------------------------------------------------------------------
# Gráfico
ggplot(db, aes(x = leitos, y = nespecies)) +
  
  geom_point(alpha = 0.6) +
  
  geom_line(data = newdata
            , aes(x = leitos
                  , y = pred_pois
                  , color = "Poisson")
            , size = 1, inherit.aes = FALSE) +
  
  geom_ribbon(data = newdata
              , aes(x = leitos
                    , ymin = lower_pois
                    , ymax = upper_pois
                    , fill = "Poisson")
              , alpha = 0.15, inherit.aes = FALSE) +
  
  geom_line(data = newdata
            , aes(x = leitos
                  , y = pred_nb
                  , color = "Neg. Binomial")
            , size = 1.1
            , linetype = "dashed"
            , inherit.aes = FALSE) +
  
  geom_ribbon(data = newdata
              , aes(x = leitos
                    , ymin = lower_nb
                    , ymax = upper_nb
                    , fill = "Neg. Binomial")
              , alpha = 0.15
              , inherit.aes = FALSE) +
  
  scale_color_manual(values = c("Poisson" = "red"
                                , "Neg. Binomial" = "blue")) +
  
  scale_fill_manual(values = c("Poisson" = "red"
                               , "Neg. Binomial" = "blue")) +
  
  labs(x = "Número de leitos"
       ,y = "Número de espécies de orquídeas"
       ,color = "Modelo", fill = "Modelo"
       ,title = "Modelos GLM: Poisson vs Binomial Negativa") +
  
  theme_minimal(base_size = 13)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
