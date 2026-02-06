##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' Extração de traits com uso de funções, fase 1 e 2, com a base de dados de exemplo.

# -------------------------------------------------------------------------
# Load packages

pacman::p_load(here, tidyverse, janitor, writexl)

# -------------------------------------------------------------------------
# Load Functions

source(here::here('script','traits','textmining_functions.R'))

# -------------------------------------------------------------------------
# Load database

db <- load(file = here::here('database'
                             ,'orquidea'
                             ,'tidy_data'
                             ,'db_webscrp_id_synm.rda')) %>% 
  get() %>% 
  dplyr::select(!synonym) %>% 
  unique() %>% 
  dplyr::select(id, sc_name, ctrl_descrp, free_descrp_pt)

rm("db_webscrp_id") # delete raw data

db_original <- db[c(1,9,10,14,19,20,42,48,57),c(1,2,3)]

db_original <- db_original %>%
  dplyr::mutate(
    status = map_chr(ctrl_descrp, validar_descricao)
  )

# -------------------------------------------------------------------------
# Aplicação à base original (linha por linha, condicional ao status)[Exemplo]
# -------------------------------------------------------------------------

traits_all <- db_original %>%
  
  dplyr::mutate(
    
    resultado = map2(
      ctrl_descrp,
      status,
      ~ if (.y == "extrair") {
        pipeline_extracao_traits(.x)
      } else {
        # retorna tibble vazia para manter integridade do map
        tibble::tibble()
      }
    )
    
  ) %>%
  
  tidyr::unnest(resultado, keep_empty = TRUE) %>%
  
  dplyr::select(
    id,
    sc_name,
    segmento_id,
    estrutura,
    segmento,
    estrutura_principal,
    subestrutura,
    trait_type,
    trait,
    min,
    max,
    value,
    unit
  ) %>%
  
  dplyr::arrange(
    id,
    sc_name,
    segmento_id,
    estrutura_principal,
    subestrutura,
    trait_type,
    trait
  )

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
