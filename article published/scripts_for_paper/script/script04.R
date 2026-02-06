
#################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

#' Obter uma regressão linear entre burned e species

#-------------------------------------------------------------------------
# Load packages -----------------------------------------------------------

library(pacman)
pacman::p_load(here,tidyverse,broom,ggpmisc)

#-------------------------------------------------------------------------
# GRAFICAMOS

ggplot2::ggplot(
  data = db
  ,mapping = aes(x = species
                 ,y = burned)) +
  
  geom_point() +
  
  geom_smooth(method = "lm"
              ,formula = y ~ x) +
  
  stat_poly_eq(formula = y ~ x
               ,parse = TRUE,
               aes(label = paste(after_stat(eq.label)
                                 ,after_stat(rr.label)
                                 ,after_stat(p.value.label)
                                 ,sep = "~~~")), 
               label.x = 0.1, label.y = 0.95) + 
  
  labs(y = 'Species records'
       ,x = 'Fire occurrence') +
  
  theme_minimal()

#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
