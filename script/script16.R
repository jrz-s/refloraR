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

pacman::p_load(here,tidyverse,MASS,ggtext)

# -------------------------------------------------------------------------
# Load database -----------------------------------------------------------

r_especies <- terra::rast(x = here::here("rasters","raster_nespecies.tiff"))
r_hospedagem <- terra::rast(x = here::here("rasters","raster_hospedagem.tiff"))
r_leitos <- terra::rast(x = here::here("rasters","raster_leitos.tiff"))

plot(r_especies)
plot(r_hospedagem)
plot(r_leitos)

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
# Linear regression analisys

# -------------------------------------------------------------------------
## Modelo 1 

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
  
  labs(y = "Altura de planta (m)"
       ,x = "Diâmetro do caule (cm)") +
  
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

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Tarefas: o que falta agora é fazermos os gráficos
# verificar se vale a pena retirar os outilers
# Verificar a melhor representação gráfica da famíla Poisson.



















