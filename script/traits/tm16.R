##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' PPEC - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Description
#' Extração de traits com uso de funções, fase 1 e 2, com a base de dados completo.
#' Rodar este script com a base de dados 'orquidea_data.rda'
#' ou com a base de dados "webscraping_final_06022026.rda".

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
                             ,'orquidea_data.rda')) %>% 
  get() %>% 
  dplyr::select(!synonym) %>% 
  unique() %>% 
  dplyr::select(id, sc_name, ctrl_descrp, free_descrp_pt)

nrow(db)

# delete raw data
rm("db_webscrp_id")

db <- db %>%
  dplyr::mutate(
    status = map_chr(ctrl_descrp, validar_descricao)
  )

db %>% 
  dplyr::filter(status == "extrair") %>% 
  nrow()

db$sc_name %>% 
  unique %>% length()

# observações: 
## Número total de linhas: 297
## Número de linhas extraíveis: 191
## % de recuperação: 191/297*100 = 64,31%

# -------------------------------------------------------------------------
# Aplicação à base original (linha por linha, condicional ao status)[Exemplo]
# -------------------------------------------------------------------------

traits_all <- db %>%
  
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
     id
    ,sc_name 
    ,segmento_id
    #,estrutura
    #,segmento
    ,estrutura_principal
    ,subestrutura
    ,trait_type
    ,trait
    ,min
    ,max
    ,value
    ,unit
  ) %>%
  
  dplyr::arrange(
     id
    ,sc_name 
    ,segmento_id
    ,estrutura_principal
    ,subestrutura
    ,trait_type
    ,trait
  ) %>% 
  dplyr::filter(!is.na(segmento_id)) %>% 
  dplyr::filter(!is.na(estrutura_principal)) %>% 
  dplyr::filter(!is.na(trait_type))

traits_all$sc_name %>% unique %>% length()

# número de espécies que foram extraídos os traits: 170

# export database
write_xlsx(traits_all,path = here::here("trait_textmining.xlsx"))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
