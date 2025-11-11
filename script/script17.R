##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' ECOSYS - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description -------------------------------------------------------------
#> Neste script vamos tentar realizar outros modelos para ver o comportamento real da relação entre o número de espécies e leitos

library(mgcv)

# Modelo aditivo generalizado (GAM)
mdl_gam <- gam(nespecies ~ s(leitos, k = 5), family = nb(), data = db)
summary(mdl_gam)
plot(mdl_gam, shade = TRUE, rug = TRUE)
par(mfrow = c(1, 1))
plot(mdl_gam)

# Modelo log–linear (GLM com log de X)
mdl_log <- MASS::glm.nb(nespecies ~ log(leitos + 1), data = db)
summary(mdl_log)
performance::r2(mdl_log)
par(mfrow = c(2, 2))
plot(mdl_log)
par(mfrow = c(1,1))
DHARMa::simulateResiduals(fittedModel = mdl_log, plot = TRUE)

# Modelo exponencial simples (Y = a * b^X)
mdl_exp <- lm(log(nespecies) ~ leitos, data = db)
summary(mdl_exp)
performance::r2(mdl_exp)
par(mfrow = c(2, 2))
plot(mdl_exp)
par(mfrow = c(1,1))
DHARMa::simulateResiduals(fittedModel = mdl_exp, plot = TRUE)

# Modelo potencial (Y = a * X^b)
mdl_pow <- lm(log(nespecies) ~ log(leitos + 1), data = db)
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
               , y = log(nespecies))) +
  
  geom_point(size = 2) +
  
  geom_smooth(method = "lm"
              , se = TRUE
              , color = "blue") +
  
  labs(x = "log(Leitos + 1)"
       , y = "log(Número de espécies)"
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
               , y = nespecies)) +
  
  geom_point(size = 2) +
  
  geom_line(data = newdata
            , aes(x = leitos
                  , y = pred)
            , color = "blue"
            , linewidth = 1.2) +
  
  labs(x = "Número de leitos", y = "Número de espécies",
       title = "Ajuste do modelo potencial: Y = 2.69 * (X)^0.194") +
  
  theme_minimal()

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
