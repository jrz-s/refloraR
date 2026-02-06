#################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

#' Unir ocorrência de especies e do fogo por ano

# -------------------------------------------------------------------------
# Load packages -----------------------------------------------------------

library(pacman)
pacman::p_load(here,tidyverse)

# -------------------------------------------------------------------------
# Load database

load(file = here::here('database','burned_list.rda'))

# -------------------------------------------------------------------------
# Load function

source(file = here::here('function','get_time_line.R'))

# -------------------------------------------------------------------------
# Database manipulation (join burned and species)

# species count per year
db <- readxl::read_xlsx(path = here::here("database","vivian_data.xlsx")) %>% 
  dplyr::select("year" = starts_with("year"), species) %>% 
  dplyr::group_by(year) %>% dplyr::count() %>% 
  dplyr::rename("species" = n) %>% dplyr::ungroup(year) %>% 
  tibble::add_row(data.frame("year" = 1985, "species" = NA)) %>% 
  dplyr::arrange(year)

# burned cover count per year
db <- tibble::enframe(x = burned_list
                ,name = 'compart'
                ,value = 'burned') %>% 
  tidyr::unnest(burned) %>% dplyr::select(!compart) %>% 
  dplyr::filter(burned == "yes") %>% dplyr::select(!burned, year, "burned" = n) %>% 
  dplyr::inner_join(db, by = 'year')
    
# -------------------------------------------------------------------------
# Graphic

p1 <- get_time_line(
  database = db %>% dplyr::mutate(burned = burned/50000)
  ,name.v1 = 'Species records'
  ,name.v2 = 'Fire occurrence'
  ,col.v1 = "#619CFF"
  ,col.v2 = "tomato"
  ,x.lab = '\nYears'
  ,y.lab = 'Count occurrences\n'
  ,linewidth = 0.10
  ,y.ls = 30
  ,y.li = 0
  ,breaks = 5
  ,leg.linewidth = 0.5
  ,ticks.length = 0.25
  ,legend.position = c(0.7,0.9)
  ,x.title.size = 8
  ,y.title.size = 9
  ,axis.text.size = 6
  ,legend.text = 6
  ,x.text.axis = 0
  ,point.size = 0.6)

p1

# -------------------------------------------------------------------------
# Export graphic

w.p <- 3000
h.p <- 2000
resolution <- 500

png(here::here("figures","occorrences_count_v2.png")
   ,width = w.p
   ,height = h.p
   ,res = resolution 
   ,family = "serif")

p1

dev.off()

# -------------------------------------------------------------------------
# Export graphic

# Exportar em PDF
ggsave(here::here("figures","occorrences_count_v2.pdf")
       ,plot = p1
       ,width = 5*1.5
       ,height = 5
       ,units = "in"
       ,family = 'serif')

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
